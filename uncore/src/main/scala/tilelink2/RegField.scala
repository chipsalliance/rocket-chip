// See LICENSE for license details.

package uncore.tilelink2

import Chisel._

case class RegReadFn private(combinational: Boolean, fn: (Bool, Bool) => (Bool, Bool, UInt))
object RegReadFn
{
  // (ivalid: Bool, oready: Bool) => (iready: Bool, ovalid: Bool, data: UInt)
  // iready may combinationally depend on oready
  // all other combinational dependencies forbidden (e.g. ovalid <= ivalid)
  // effects must become visible only on the cycle after ovalid && oready
  implicit def apply(x: (Bool, Bool) => (Bool, Bool, UInt)) =
    new RegReadFn(false, x)
  // (ofire: Bool) => (data: UInt)
  // effects must become visible on the cycle after ofire
  implicit def apply(x: Bool => UInt) =
    new RegReadFn(true, { case (_, oready) =>
      (Bool(true), Bool(true), x(oready))
    })
  // read from a register
  implicit def apply(x: UInt) =
    new RegReadFn(true, { case (_, _) =>
      (Bool(true), Bool(true), x)
    })
  // noop
  implicit def apply(x: Unit) =
    new RegReadFn(true, { case (_, _) =>
      (Bool(true), Bool(true), UInt(0))
    })
}

case class RegWriteFn private(combinational: Boolean, fn: (Bool, Bool, UInt) => (Bool, Bool))
object RegWriteFn
{
  // (ivalid: Bool, oready: Bool, data: UInt) => (iready: Bool, ovalid: Bool)
  // iready may combinationally depend on both oready and data
  // all other combinational dependencies forbidden (e.g. ovalid <= ivalid)
  // effects must become visible only on the cycle after ovalid && oready
  implicit def apply(x: (Bool, Bool, UInt) => (Bool, Bool)) =
    new RegWriteFn(false, x)
  // (ofire: Bool, data: UInt) => ()
  // effects must become visible on the cycle after ofire
  implicit def apply(x: (Bool, UInt) => Unit) =
    new RegWriteFn(true, { case (_, oready, data) =>
      x(oready, data)
      (Bool(true), Bool(true))
    })
  // updates a register
  implicit def apply(x: UInt) =
    new RegWriteFn(true, { case (_, oready, data) =>
      when (oready) { x := data }
      (Bool(true), Bool(true))
    })
  // noop
  implicit def apply(x: Unit) =
    new RegWriteFn(true, { case (_, _, _) =>
      (Bool(true), Bool(true))
    })
}

case class RegField(width: Int, read: RegReadFn, write: RegWriteFn)
{
  require (width > 0)
  def pipelined = !read.combinational || !write.combinational
}

object RegField
{
  type Map = (Int, Seq[RegField])
  def apply(n: Int)            : RegField = apply(n, (), ())
  def apply(n: Int, rw: UInt)  : RegField = apply(n, rw, rw)
  def R(n: Int, r: RegReadFn)  : RegField = apply(n, r, ())
  def W(n: Int, w: RegWriteFn) : RegField = apply(n, (), w)
}

trait HasRegMap
{
  def regmap(mapping: RegField.Map*): Unit
}

case class RegFieldParams(indexBits: Int, maskBits: Int, extraBits: Int)

class RegFieldInput(params: RegFieldParams) extends GenericParameterizedBundle(params)
{
  val read  = Bool()
  val index = UInt(width = params.indexBits)
  val data  = UInt(width = params.maskBits*8)
  val mask  = UInt(width = params.maskBits)
  val extra = UInt(width = params.extraBits)
}

class RegFieldOutput(params: RegFieldParams) extends GenericParameterizedBundle(params)
{
  val read  = Bool()
  val data  = UInt(width = params.maskBits*8)
  val extra = UInt(width = params.extraBits)
}

object RegFieldHelper
{
  // Create a generic register-based device
  def apply(bytes: Int, concurrency: Option[Int], in: DecoupledIO[RegFieldInput], mapping: RegField.Map*) = {
    val regmap = mapping.toList
    require (!regmap.isEmpty)

    // Flatten the regmap into (Reg:Int, Offset:Int, field:RegField)
    val flat = regmap.map { case (reg, fields) =>
      val offsets = fields.scanLeft(0)(_ + _.width).init
      (offsets zip fields) map { case (o, f) => (reg, o, f) }
    }.flatten
    require (!flat.isEmpty)

    val endIndex = 1 << log2Ceil(regmap.map(_._1).max+1)
    val params = RegFieldParams(log2Up(endIndex), bytes, in.bits.params.extraBits)

    val out = Wire(Decoupled(new RegFieldOutput(params)))
    val front = Wire(Decoupled(new RegFieldInput(params)))
    front.bits := in.bits

    // Must this device pipeline the control channel?
    val pipelined = flat.map(_._3.pipelined).reduce(_ || _)
    val depth = concurrency.getOrElse(if (pipelined) 1 else 0)
    require (depth >= 0)
    require (!pipelined || depth > 0)
    val back = if (depth > 0) Queue(front, depth, pipe = depth == 1) else front

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
    val rifire = Array.tabulate(endIndex) { i => Seq(Bool(true)) }
    val wifire = Array.tabulate(endIndex) { i => Seq(Bool(true)) }
    val rofire = Array.tabulate(endIndex) { i => Seq(Bool(true)) }
    val wofire = Array.tabulate(endIndex) { i => Seq(Bool(true)) }

    // The output values for each register
    val dataOut = Array.tabulate(endIndex) { _ => UInt(0) }

    // Which bits are touched?
    val frontMask = FillInterleaved(8, front.bits.mask)
    val backMask  = FillInterleaved(8, back .bits.mask)

    // Connect the fields
    for (i <- 0 until flat.size) {
      val (reg, low, field) = flat(i)
      val high = low + field.width - 1
      // Confirm that no register is too big
      require (high <= 8*bytes)
      val rimask = frontMask(high, low).orR()
      val wimask = frontMask(high, low).andR()
      val romask = backMask(high, low).orR()
      val womask = backMask(high, low).andR()
      val data = if (field.write.combinational) back.bits.data else front.bits.data
      val (f_riready, f_rovalid, f_data) = field.read.fn(rivalid(i) && rimask, roready(i) && romask)
      val (f_wiready, f_wovalid) = field.write.fn(wivalid(i) && wimask, woready(i) && womask, data)
      riready(i) := f_riready || !rimask
      wiready(i) := f_wiready || !wimask
      rovalid(i) := f_rovalid || !romask
      wovalid(i) := f_wovalid || !womask
      rifire(reg) = riready(i) +: rifire(reg)
      wifire(reg) = wiready(i) +: wifire(reg)
      rofire(reg) = rovalid(i) +: rofire(reg)
      wofire(reg) = wovalid(i) +: wofire(reg)
      dataOut(reg) = dataOut(reg) | (f_data << low)
    }

    // Is the selected register ready?
    val rifireMux = Vec(rifire.map(_.reduce(_ && _)))
    val wifireMux = Vec(wifire.map(_.reduce(_ && _)))
    val rofireMux = Vec(rofire.map(_.reduce(_ && _)))
    val wofireMux = Vec(wofire.map(_.reduce(_ && _)))
    val iready = Mux(front.bits.read, rifireMux(front.bits.index), wifireMux(front.bits.index))
    val oready = Mux(back .bits.read, rofireMux(back .bits.index), wofireMux(back .bits.index))

    // Connect the pipeline
    in.ready    := front.ready && iready
    front.valid := in.valid    && iready
    back.ready  := out.ready   && oready
    out.valid   := back.valid  && oready

    // Which register is touched?
    val frontSel = UIntToOH(front.bits.index)
    val backSel  = UIntToOH(back.bits.index)

    // Include the per-register one-hot selected criteria
    for (reg <- 0 until endIndex) {
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
    out.bits.data  := Vec(dataOut)(back.bits.index)
    out.bits.extra := back.bits.extra

    (endIndex, out)
  }
}
