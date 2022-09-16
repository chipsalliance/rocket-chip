// See LICENSE.SiFive for license details.

package freechips.rocketchip.regmapper

import chisel3._
import chisel3.util.{DecoupledIO, ReadyValidIO}

import org.json4s.JsonDSL._
import org.json4s.JsonAST.JValue

import freechips.rocketchip.util.{SimpleRegIO}

case class RegReadFn private(combinational: Boolean, fn: (Bool, Bool) => (Bool, Bool, UInt))
object RegReadFn
{
  // (ivalid: Bool, oready: Bool) => (iready: Bool, ovalid: Bool, data: UInt)
  // iready may combinationally depend on oready
  // all other combinational dependencies forbidden (e.g. ovalid <= ivalid)
  // effects must become visible on the cycle after ovalid && oready
  // data is only inspected when ovalid && oready
  implicit def apply(x: (Bool, Bool) => (Bool, Bool, UInt)) =
    new RegReadFn(false, x)
  implicit def apply(x: RegisterReadIO[UInt]): RegReadFn =
    RegReadFn((ivalid, oready) => {
      x.request.valid := ivalid
      x.response.ready := oready
      (x.request.ready, x.response.valid, x.response.bits)
    })
  // (ready: Bool) => (valid: Bool, data: UInt)
  // valid must not combinationally depend on ready
  // effects must become visible on the cycle after valid && ready
  implicit def apply(x: Bool => (Bool, UInt)) =
    new RegReadFn(true, { case (_, oready) =>
      val (ovalid, data) = x(oready)
      (true.B, ovalid, data)
    })
  // read from a ReadyValidIO (only safe if there is a consistent source of data)
  implicit def apply(x: ReadyValidIO[UInt]):RegReadFn = RegReadFn(ready => { x.ready := ready; (x.valid, x.bits) })
  // read from a register
  implicit def apply(x: UInt):RegReadFn = RegReadFn(ready => (true.B, x))
  // noop
  implicit def apply(x: Unit):RegReadFn = RegReadFn(0.U)
}

case class RegWriteFn private(combinational: Boolean, fn: (Bool, Bool, UInt) => (Bool, Bool))
object RegWriteFn
{
  // (ivalid: Bool, oready: Bool, data: UInt) => (iready: Bool, ovalid: Bool)
  // iready may combinationally depend on both oready and data
  // all other combinational dependencies forbidden (e.g. ovalid <= ivalid)
  // effects must become visible on the cycle after ovalid && oready
  // data should only be used for an effect when ivalid && iready
  implicit def apply(x: (Bool, Bool, UInt) => (Bool, Bool)) =
    new RegWriteFn(false, x)
  implicit def apply(x: RegisterWriteIO[UInt]): RegWriteFn =
    RegWriteFn((ivalid, oready, data) => {
      x.request.valid := ivalid
      x.request.bits := data
      x.response.ready := oready
      (x.request.ready, x.response.valid)
    })
  // (valid: Bool, data: UInt) => (ready: Bool)
  // ready may combinationally depend on data (but not valid)
  // effects must become visible on the cycle after valid && ready
  implicit def apply(x: (Bool, UInt) => Bool) =
    // combinational => data valid on oready
    new RegWriteFn(true, { case (_, oready, data) =>
      (true.B, x(oready, data))
    })
  // write to a DecoupledIO (only safe if there is a consistent sink draining data)
  // NOTE: this is not an IrrevocableIO (even on TL2) because other fields could cause a lowered valid
  implicit def apply(x: DecoupledIO[UInt]): RegWriteFn = RegWriteFn((valid, data) => { x.valid := valid; x.bits := data; x.ready })
  // updates a register (or adds a mux to a wire)
  implicit def apply(x: UInt): RegWriteFn = RegWriteFn((valid, data) => { when (valid) { x := data }; true.B })
  // noop
  implicit def apply(x: Unit): RegWriteFn = RegWriteFn((valid, data) => { true.B })
}

case class RegField(width: Int, read: RegReadFn, write: RegWriteFn, desc: Option[RegFieldDesc])
{
  require (width >= 0, s"RegField width must be >= 0, not $width")

  def pipelined = !read.combinational || !write.combinational

  def readOnly = this.copy(write = (), desc = this.desc.map(_.copy(access = RegFieldAccessType.R)))

  def toJson(byteOffset: Int, bitOffset: Int): JValue = {
    ( ("byteOffset"   -> s"0x${byteOffset.toHexString}") ~
      ("bitOffset"    -> bitOffset) ~
      ("bitWidth"     -> width) ~
      ("name"         -> desc.map(_.name)) ~
      ("description"  -> desc.map{ d=> if (d.desc == "") None else Some(d.desc)}) ~
      ("resetValue"   -> desc.map{_.reset}) ~
      ("group"        -> desc.map{_.group}) ~
      ("groupDesc"    -> desc.map{_.groupDesc}) ~
      ("accessType"   -> desc.map {d => d.access.toString}) ~
      ("writeType"    -> desc.map {d => d.wrType.map(_.toString)}) ~
      ("readAction"   -> desc.map {d => d.rdAction.map(_.toString)}) ~
      ("volatile"     -> desc.map {d => if (d.volatile) Some(true) else None}) ~
      ("enumerations" -> desc.map {d =>
        Option(d.enumerations.map { case (key, (name, edesc)) =>
          (("value" -> key) ~ ("name" -> name) ~ ("description" -> edesc))
        }).filter(_.nonEmpty)}) )
  }
}

object RegField
{
  // Byte address => sequence of bitfields, lowest index => lowest address
  type Map = (Int, Seq[RegField])

  def apply(n: Int)                                                             : RegField = apply(n, (), (), Some(RegFieldDesc.reserved))
  def apply(n: Int, desc: RegFieldDesc)                                         : RegField = apply(n, (), (), Some(desc))

  def apply(n: Int, r: RegReadFn, w: RegWriteFn)                                : RegField = apply(n, r,  w,  None)
  def apply(n: Int, r: RegReadFn, w: RegWriteFn, desc: RegFieldDesc)            : RegField = apply(n, r,  w,  Some(desc))
  def apply(n: Int, rw: UInt)                                                   : RegField = apply(n, rw, rw, None)
  def apply(n: Int, rw: UInt, desc: RegFieldDesc)                               : RegField = apply(n, rw, rw, Some(desc))
  def r(n: Int, r: RegReadFn)                                                   : RegField = apply(n, r,  (), None)
  def r(n: Int, r: RegReadFn,  desc: RegFieldDesc)                              : RegField = apply(n, r,  (), Some(desc.copy(access = RegFieldAccessType.R)))
  def w(n: Int, w: RegWriteFn)                                                  : RegField = apply(n, (), w,  None)
  def w(n: Int, w: RegWriteFn, desc: RegFieldDesc)                              : RegField = apply(n, (), w,  Some(desc.copy(access = RegFieldAccessType.W)))

  // This RegField allows 'set' to set bits in 'reg'.
  // and to clear bits when the bus writes bits of value 1.
  // Setting takes priority over clearing.
  def w1ToClear(n: Int, reg: UInt, set: UInt, desc: Option[RegFieldDesc] = None): RegField =
    RegField(n, reg, RegWriteFn((valid, data) => { reg := (~((~reg) | Mux(valid, data, 0.U))) | set; true.B }),
      desc.map{_.copy(access = RegFieldAccessType.RW, wrType=Some(RegFieldWrType.ONE_TO_CLEAR), volatile = true)})

  // This RegField wraps an explicit register
  // (e.g. Black-Boxed Register) to create a R/W register.
  def rwReg(n: Int, bb: SimpleRegIO, desc: Option[RegFieldDesc] = None) : RegField =
    RegField(n, bb.q, RegWriteFn((valid, data) => {
      bb.en := valid
      bb.d := data
      true.B
    }), desc)

  // Create byte-sized read-write RegFields out of a large UInt register.
  // It is updated when any of the (implemented) bytes are written, the non-written
  // bytes are just copied over from their current value.
  // Because the RegField are all byte-sized, this is also suitable when a register is larger
  // than the intended bus width of the device (atomic updates are impossible). 
  def bytes(reg: UInt, numBytes: Int, desc: Option[RegFieldDesc]): Seq[RegField] = {
    require(reg.getWidth * 8 >= numBytes, "Can't break a ${reg.getWidth}-bit-wide register into only ${numBytes} bytes.")
    val numFullBytes = reg.getWidth/8
    val numPartialBytes  = if ((reg.getWidth % 8) > 0) 1 else 0
    val numPadBytes = numBytes - numFullBytes - numPartialBytes
    val pad = reg | 0.U((8*numBytes).W)
    val oldBytes = VecInit.tabulate(numBytes) { i => pad(8*(i+1)-1, 8*i) }
    val newBytes = WireDefault(oldBytes)
    val valids = WireDefault(VecInit.fill(numBytes) { false.B })
    when (valids.reduce(_ || _)) { reg := newBytes.asUInt }

    def wrFn(i: Int): RegWriteFn = RegWriteFn((valid, data) => {
      valids(i) := valid
      when (valid) {newBytes(i) := data}
      true.B
    })

    val fullBytes = Seq.tabulate(numFullBytes) { i =>
      val newDesc = desc.map {d => d.copy(name = d.name + s"_$i")}
      RegField(8, oldBytes(i), wrFn(i), newDesc)}
    val partialBytes = if (numPartialBytes > 0) {
      val newDesc = desc.map {d => d.copy(name = d.name + s"_$numFullBytes")}
      Seq(RegField(reg.getWidth % 8, oldBytes(numFullBytes), wrFn(numFullBytes), newDesc),
        RegField(8 - (reg.getWidth % 8)))
    } else Nil
    val padBytes = Seq.fill(numPadBytes){RegField(8)}
      fullBytes ++ partialBytes ++ padBytes
  }

  def bytes(reg: UInt, desc: Option[RegFieldDesc]): Seq[RegField] = {
    val width = reg.getWidth
    require (width % 8 == 0, s"RegField.bytes must be called on byte-sized reg, not ${width} bits")
    bytes(reg, width/8, desc)
  }

  def bytes(reg: UInt, numBytes: Int): Seq[RegField] = bytes(reg, numBytes, None)
  def bytes(reg: UInt): Seq[RegField] = bytes(reg, None)

}

trait HasRegMap
{
  def regmap(mapping: RegField.Map*): Unit
  val interrupts: Vec[Bool]
}

// See Example.scala for an example of how to use regmap
