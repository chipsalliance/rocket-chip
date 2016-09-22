// See LICENSE for license details.

package uncore.tilelink2

import chisel3._
import chisel3.util._

// A bus agnostic register interface to a register-based device

case class RegMapperParams(indexBits: Int, maskBits: Int, extraBits: Int)

class RegMapperInput(params: RegMapperParams) extends GenericParameterizedBundle(params)
{
  val read  = Bool()
  val index = UInt(width = params.indexBits)
  val data  = UInt(width = params.maskBits*8)
  val mask  = UInt(width = params.maskBits)
  val extra = UInt(width = params.extraBits)
}

class RegMapperOutput(params: RegMapperParams) extends GenericParameterizedBundle(params)
{
  val read  = Bool()
  val data  = UInt(width = params.maskBits*8)
  val extra = UInt(width = params.extraBits)
}

object RegMapper
{
  // Create a generic register-based device
  def apply(bytes: Int, concurrency: Int, undefZero: Boolean, in: DecoupledIO[RegMapperInput], mapping: RegField.Map*) = {
    val regmap = mapping.toList.filter(!_._2.isEmpty)
    require (!regmap.isEmpty)

    // Ensure no register appears twice
    regmap.combinations(2).foreach { case Seq((reg1, _), (reg2, _)) =>
      require (reg1 != reg2)
    }
    // Don't be an asshole...
    regmap.foreach { reg => require (reg._1 >= 0) }
    // Make sure registers fit
    val inParams = in.bits.params
    val inBits = inParams.indexBits
    assert (regmap.map(_._1).max < (1 << inBits))

    val out = Wire(Irrevocable(new RegMapperOutput(inParams)))
    val front = Wire(Irrevocable(new RegMapperInput(inParams)))
    front.bits := in.bits

    // Must this device pipeline the control channel?
    val pipelined = regmap.map(_._2.map(_.pipelined)).flatten.reduce(_ || _)
    val depth = concurrency
    require (depth >= 0)
    require (!pipelined || depth > 0, "Register-based device with request/response handshaking needs concurrency > 0")
    val back = if (depth > 0) Queue(front, depth, pipe = depth == 1) else front

    // Convert to and from Bits
    def toBits(x: Int, tail: List[Boolean] = List.empty): List[Boolean] =
      if (x == 0) tail.reverse else toBits(x >> 1, ((x & 1) == 1) :: tail)
    def ofBits(bits: List[Boolean]) = bits.foldRight(0){ case (x,y) => (if (x) 1 else 0) | y << 1 }

    // Find the minimal mask that can decide the register map
    val mask = AddressDecoder(regmap.map(_._1))
    val maskMatch = ~UInt(mask, width = inBits)
    val maskFilter = toBits(mask)
    val maskBits = maskFilter.filter(x => x).size

    // Calculate size and indexes into the register map
    val regSize = 1 << maskBits
    def regIndexI(x: Int) = ofBits((maskFilter zip toBits(x)).filter(_._1).map(_._2))
    def regIndexU(x: UInt) = if (maskBits == 0) UInt(0) else
      Cat((maskFilter zip x.toBools).filter(_._1).map(_._2).reverse)

    // Protection flag for undefined registers
    val iRightReg = Array.fill(regSize) { Bool(true) }
    val oRightReg = Array.fill(regSize) { Bool(true) }

    // Flatten the regmap into (RegIndex:Int, Offset:Int, field:RegField)
    val flat = regmap.map { case (reg, fields) =>
      val offsets = fields.scanLeft(0)(_ + _.width).init
      val index = regIndexI(reg)
      val uint = UInt(reg, width = inBits)
      if (undefZero) {
        iRightReg(index) = ((front.bits.index ^ uint) & maskMatch) === UInt(0)
        oRightReg(index) = ((back .bits.index ^ uint) & maskMatch) === UInt(0)
      }
      // println("mapping 0x%x -> 0x%x for 0x%x/%d".format(reg, index, mask, maskBits))
      (offsets zip fields) map { case (o, f) => (index, o, f) }
    }.flatten

    // Forward declaration of all flow control signals
    val rivalid = Wire(Vec(flat.size, Bool()))
    val wivalid = Wire(Vec(flat.size, Bool()))
    val riready = Wire(Vec(flat.size, Bool()))
    val wiready = Wire(Vec(flat.size, Bool()))
    val rovalid = Wire(Vec(flat.size, Bool()))
    val wovalid = Wire(Vec(flat.size, Bool()))
    val roready = Wire(Vec(flat.size, Bool()))
    val woready = Wire(Vec(flat.size, Bool()))

    // Per-register list of all control signals needed for data to flow
    val rifire = Array.tabulate(regSize) { i => Seq(Bool(true)) }
    val wifire = Array.tabulate(regSize) { i => Seq(Bool(true)) }
    val rofire = Array.tabulate(regSize) { i => Seq(Bool(true)) }
    val wofire = Array.tabulate(regSize) { i => Seq(Bool(true)) }

    // The output values for each register
    val dataOut = Array.tabulate(regSize) { _ => UInt(0) }

    // Which bits are touched?
    val frontMask = FillInterleaved(8, front.bits.mask)
    val backMask  = FillInterleaved(8, back .bits.mask)

    // Connect the fields
    for (i <- 0 until flat.size) {
      val (reg, low, field) = flat(i)
      val high = low + field.width - 1
      // Confirm that no register is too big
      require (high < 8*bytes)
      val rimask = frontMask(high, low).orR()
      val wimask = frontMask(high, low).andR()
      val romask = backMask(high, low).orR()
      val womask = backMask(high, low).andR()
      val data = if (field.write.combinational) back.bits.data else front.bits.data
      val (f_riready, f_rovalid, f_data) = field.read.fn(rivalid(i) && rimask, roready(i) && romask)
      val (f_wiready, f_wovalid) = field.write.fn(wivalid(i) && wimask, woready(i) && womask, data(high, low))
      riready(i) := f_riready || !rimask
      wiready(i) := f_wiready || !wimask
      rovalid(i) := f_rovalid || !romask
      wovalid(i) := f_wovalid || !womask
      rifire(reg) = riready(i) +: rifire(reg)
      wifire(reg) = wiready(i) +: wifire(reg)
      rofire(reg) = rovalid(i) +: rofire(reg)
      wofire(reg) = wovalid(i) +: wofire(reg)
      dataOut(reg) = dataOut(reg) | ((f_data  << low) & (~UInt(0, width = high+1)))
    }

    // Is the selected register ready?
    val rifireMux = Vec(rifire.zipWithIndex.map { case (seq, i) => !iRightReg(i) || seq.reduce(_ && _)})
    val wifireMux = Vec(wifire.zipWithIndex.map { case (seq, i) => !iRightReg(i) || seq.reduce(_ && _)})
    val rofireMux = Vec(rofire.zipWithIndex.map { case (seq, i) => !oRightReg(i) || seq.reduce(_ && _)})
    val wofireMux = Vec(wofire.zipWithIndex.map { case (seq, i) => !oRightReg(i) || seq.reduce(_ && _)})
    val iindex = regIndexU(front.bits.index)
    val oindex = regIndexU(back .bits.index)
    val iready = Mux(front.bits.read, rifireMux(iindex), wifireMux(iindex))
    val oready = Mux(back .bits.read, rofireMux(oindex), wofireMux(oindex))

    // Connect the pipeline
    in.ready    := front.ready && iready
    front.valid := in.valid    && iready
    back.ready  := out.ready   && oready
    out.valid   := back.valid  && oready

    // Which register is touched?
    val frontSel = UIntToOH(iindex) & Cat(iRightReg.reverse)
    val backSel  = UIntToOH(oindex) & Cat(oRightReg.reverse)

    // Include the per-register one-hot selected criteria
    for (reg <- 0 until regSize) {
      rifire(reg) = (in.valid && front.ready &&  front.bits.read && frontSel(reg)) +: rifire(reg)
      wifire(reg) = (in.valid && front.ready && !front.bits.read && frontSel(reg)) +: wifire(reg)
      rofire(reg) = (back.valid && out.ready &&  back .bits.read && backSel (reg)) +: rofire(reg)
      wofire(reg) = (back.valid && out.ready && !back .bits.read && backSel (reg)) +: wofire(reg)
    }

    // Connect the field's ivalid and oready
    for (i <- 0 until flat.size) {
      val (reg, _, _ ) = flat(i)
      rivalid(i) := rifire(reg).filter(_ ne riready(i)).reduce(_ && _)
      wivalid(i) := wifire(reg).filter(_ ne wiready(i)).reduce(_ && _)
      roready(i) := rofire(reg).filter(_ ne rovalid(i)).reduce(_ && _)
      woready(i) := wofire(reg).filter(_ ne wovalid(i)).reduce(_ && _)
    }

    out.bits.read  := back.bits.read
    out.bits.data  := Mux(Vec(oRightReg)(oindex), Vec(dataOut)(oindex), UInt(0))
    out.bits.extra := back.bits.extra

    out
  }
}
