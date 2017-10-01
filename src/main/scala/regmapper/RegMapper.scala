// See LICENSE.SiFive for license details.

package freechips.rocketchip.regmapper

import Chisel._

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.{GenericParameterizedBundle, ReduceOthers}

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
    val bytemap = mapping.toList
    // Negative addresses are bad
    bytemap.foreach { byte => require (byte._1 >= 0) }

    // Transform all fields into bit offsets Seq[(bit, field)]
    val bitmap = bytemap.map { case (byte, fields) =>
      val bits = fields.scanLeft(byte * 8)(_ + _.width).init
      bits zip fields
    }.flatten.sortBy(_._1)

    // Detect overlaps
    (bitmap.init zip bitmap.tail) foreach { case ((lbit, lfield), (rbit, rfield)) =>
      require (lbit + lfield.width <= rbit, s"Register map overlaps at bit ${rbit}.")
    }

    // Group those fields into bus words Map[word, List[(bit, field)]]
    val wordmap = bitmap.groupBy(_._1 / (8*bytes))

    // Make sure registers fit
    val inParams = in.bits.params
    val inBits = inParams.indexBits
    assert (wordmap.keySet.max < (1 << inBits), "Register map does not fit in device")

    val out = Wire(Decoupled(new RegMapperOutput(inParams)))
    val front = Wire(Decoupled(new RegMapperInput(inParams)))
    front.bits := in.bits

    // Must this device pipeline the control channel?
    val pipelined = wordmap.values.map(_.map(_._2.pipelined)).flatten.reduce(_ || _)
    val depth = concurrency
    require (depth >= 0)
    require (!pipelined || depth > 0, "Register-based device with request/response handshaking needs concurrency > 0")
    val back = if (depth > 0) Queue(front, depth) else front

    // Convert to and from Bits
    def toBits(x: Int, tail: List[Boolean] = List.empty): List[Boolean] =
      if (x == 0) tail.reverse else toBits(x >> 1, ((x & 1) == 1) :: tail)
    def ofBits(bits: List[Boolean]) = bits.foldRight(0){ case (x,y) => (if (x) 1 else 0) | y << 1 }

    // Find the minimal mask that can decide the register map
    val mask = AddressDecoder(wordmap.keySet.toList)
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

    // Transform the wordmap into minimal decoded indexes, Seq[(index, bit, field)]
    val flat = wordmap.toList.map { case (word, fields) =>
      val index = regIndexI(word)
      val uint = UInt(word, width = inBits)
      if (undefZero) {
        iRightReg(index) = ((front.bits.index ^ uint) & maskMatch) === UInt(0)
        oRightReg(index) = ((back .bits.index ^ uint) & maskMatch) === UInt(0)
      }
      // Confirm that no field spans a word boundary
      fields foreach { case (bit, field) =>
        val off = bit - 8*bytes*word
        // println(s"Reg ${word}: [${off}, ${off+field.width})")
        require (off + field.width <= bytes * 8, s"Field at word ${word}*(${bytes}B) has bits [${off}, ${off+field.width}), which exceeds word limit.")
      }
      // println("mapping 0x%x -> 0x%x for 0x%x/%d".format(word, index, mask, maskBits))
      fields.map { case (bit, field) => (index, bit-8*bytes*word, field) }
    }.flatten

    // Forward declaration of all flow control signals
    val rivalid = Wire(Vec(flat.size, Bool()))
    val wivalid = Wire(Vec(flat.size, Bool()))
    val roready = Wire(Vec(flat.size, Bool()))
    val woready = Wire(Vec(flat.size, Bool()))

    // Per-register list of all control signals needed for data to flow
    val rifire = Array.fill(regSize) { Nil:List[(Bool, Bool)] }
    val wifire = Array.fill(regSize) { Nil:List[(Bool, Bool)] }
    val rofire = Array.fill(regSize) { Nil:List[(Bool, Bool)] }
    val wofire = Array.fill(regSize) { Nil:List[(Bool, Bool)] }

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
      def litOR(x: Bool, y: Bool) = if (x.isLit && x.litValue == 1) Bool(true) else x || y
      // Add this field to the ready-valid signals for the register
      rifire(reg) = (rivalid(i), litOR(f_riready, !rimask)) +: rifire(reg)
      wifire(reg) = (wivalid(i), litOR(f_wiready, !wimask)) +: wifire(reg)
      rofire(reg) = (roready(i), litOR(f_rovalid, !romask)) +: rofire(reg)
      wofire(reg) = (woready(i), litOR(f_wovalid, !womask)) +: wofire(reg)
      dataOut(reg) = dataOut(reg) | ((f_data << low) & (~UInt(0, width = high+1)))
    }

    // Which register is touched?
    val iindex = regIndexU(front.bits.index)
    val oindex = regIndexU(back .bits.index)
    val frontSel = UIntToOH(iindex).toBools
    val backSel  = UIntToOH(oindex).toBools

    // Compute: is the selected register ready? ... and cross-connect all ready-valids
    def mux(valid: Bool, select: Seq[Bool], guard: Seq[Bool], flow: Seq[Seq[(Bool, Bool)]]): Vec[Bool] =
      Vec(((select zip guard) zip flow).map { case ((s, g), f) =>
        val out = Wire(Bool())
        ReduceOthers((out, valid && s && g) +: f)
        out || !g
      })

    // Include the per-register one-hot selected criteria
    val rifireMux = mux(in.valid && front.ready &&  front.bits.read, frontSel, iRightReg, rifire)
    val wifireMux = mux(in.valid && front.ready && !front.bits.read, frontSel, iRightReg, wifire)
    val rofireMux = mux(back.valid && out.ready &&  back .bits.read, backSel,  oRightReg, rofire)
    val wofireMux = mux(back.valid && out.ready && !back .bits.read, backSel,  oRightReg, wofire)

    val iready = Mux(front.bits.read, rifireMux(iindex), wifireMux(iindex))
    val oready = Mux(back .bits.read, rofireMux(oindex), wofireMux(oindex))

    // Connect the pipeline
    in.ready    := front.ready && iready
    front.valid := in.valid    && iready
    back.ready  := out.ready   && oready
    out.valid   := back.valid  && oready

    out.bits.read  := back.bits.read
    out.bits.data  := Mux(Vec(oRightReg)(oindex), Vec(dataOut)(oindex), UInt(0))
    out.bits.extra := back.bits.extra

    out
  }
}
