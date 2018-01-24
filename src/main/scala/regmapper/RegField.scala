// See LICENSE.SiFive for license details.

package freechips.rocketchip.regmapper

import Chisel._
import chisel3.util.{ReadyValidIO}

import freechips.rocketchip.util.{SimpleRegIO}
import freechips.rocketchip.util.property._
import chisel3.internal.sourceinfo.{SourceInfo, SourceLine}

abstract class RegReadFnBase protected(val combinational: Boolean, val fn: (Bool, Bool) => (Bool, Bool, UInt))

case class RegReadFn private(override val combinational: Boolean, override val fn: (Bool, Bool) => (Bool, Bool, UInt)) extends RegReadFnBase(combinational, fn)
case class RegReadFnCover private(override val combinational: Boolean, override val fn: (Bool, Bool) => (Bool, Bool, UInt)) extends RegReadFnBase(combinational, fn)

object RegReadFnCover {
  def coverread(name: String, desc: String)(fn: (Bool, Bool) => (Bool, Bool, UInt))(implicit sourceInfo: SourceInfo): (Bool, Bool) => (Bool, Bool, UInt) = {
    case (ivalid: Bool, oready: Bool) => {
      val (iready, ovalid, data) = fn(ivalid, oready)
      cover(iready && ivalid, name+"_CSR_regread_start", desc+" RegField Read Request Initiate")
      cover(oready && ovalid, name+"_CSR_regread_out", desc+" RegField Read Request Complete")
      (iready, ovalid, data)
    }
  }

  def apply(name: String, desc: String)(regread: RegReadFn)(implicit sourceInfo: SourceInfo): RegReadFnCover = {
    new RegReadFnCover(
      combinational = regread.combinational,
      fn = coverread(name, desc)(regread.fn)
    )
  }
}

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
      (Bool(true), ovalid, data)
    })
  // read from a ReadyValidIO (only safe if there is a consistent source of data)
  implicit def apply(x: ReadyValidIO[UInt]):RegReadFn = RegReadFn(ready => { x.ready := ready; (x.valid, x.bits) })
  // read from a register
  implicit def apply(x: UInt):RegReadFn = RegReadFn(ready => (Bool(true), x))
  // noop
  implicit def apply(x: Unit):RegReadFn = RegReadFn(UInt(0))
}

abstract class RegWriteFnBase protected(val combinational: Boolean, val fn: (Bool, Bool, UInt) => (Bool, Bool))
case class RegWriteFn private(override val combinational: Boolean, override val fn: (Bool, Bool, UInt) => (Bool, Bool)) extends RegWriteFnBase(combinational, fn)
case class RegWriteFnCover private(override val combinational: Boolean, override val fn: (Bool, Bool, UInt) => (Bool, Bool)) extends RegWriteFnBase(combinational, fn)

object RegWriteFnCover {
  def coverwrite(name: String, desc: String)(fn: (Bool, Bool, UInt) => (Bool, Bool))(implicit sourceInfo: SourceInfo): (Bool, Bool, UInt) => (Bool, Bool) = {
    case (ivalid: Bool, oready: Bool, data: UInt) => {
      val (iready, ovalid) = fn(ivalid, oready, data)
      cover(iready && ivalid, name+"_CSR_regwrite_start", desc+" RegField Write Request Initiate")
      cover(oready && ovalid, name+"_CSR_regwrite_out", desc+" RegField Write Request Complete")
      (iready, ovalid)
    }
  }

  def apply(name: String, desc: String)(regwrite: RegWriteFn)(implicit sourceInfo: SourceInfo): RegWriteFnCover = {
    new RegWriteFnCover(
      combinational = regwrite.combinational,
      fn = coverwrite(name, desc)(regwrite.fn)
    )
  }
}

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
      (Bool(true), x(oready, data))
    })
  // write to a DecoupledIO (only safe if there is a consistent sink draining data)
  // NOTE: this is not an IrrevocableIO (even on TL2) because other fields could cause a lowered valid
  implicit def apply(x: DecoupledIO[UInt]): RegWriteFn = RegWriteFn((valid, data) => { x.valid := valid; x.bits := data; x.ready })
  // updates a register (or adds a mux to a wire)
  implicit def apply(x: UInt): RegWriteFn = RegWriteFn((valid, data) => { when (valid) { x := data }; Bool(true) })
  // noop
  implicit def apply(x: Unit): RegWriteFn = RegWriteFn((valid, data) => { Bool(true) })
}

case class RegField(width: Int, read: RegReadFnBase, write: RegWriteFnBase, name: String, description: String)
{
  require (width > 0, s"RegField width must be > 0, not $width")
  def pipelined = !read.combinational || !write.combinational
  def readOnly = this.copy(write = RegWriteFn(():Unit))
}

object RegField
{
  // Byte address => sequence of bitfields, lowest index => lowest address
  type Map = (Int, Seq[RegField])

  def apply(n: Int)(implicit sourceInfo: SourceInfo)                                                         : RegField = apply(n, RegReadFn((): Unit), RegWriteFn((): Unit), "", "")
  def apply(n: Int, r: RegReadFn, w: RegWriteFn)(implicit sourceInfo: SourceInfo)                            : RegField = apply(n, RegReadFnCover("", "")(r), RegWriteFnCover("", "")(w),   "", "")
  def apply(n: Int, rw: UInt)(implicit sourceInfo: SourceInfo)                                               : RegField = apply(n, RegReadFnCover("", "")(rw), RegWriteFnCover("", "")(rw), "", "")
  def apply(n: Int, rw: UInt, name: String, description: String)(implicit sourceInfo: SourceInfo)            : RegField = apply(n, RegReadFnCover(name, description)(rw), RegWriteFnCover(name, description)(rw), name, description)
  def apply(n: Int, r: RegReadFn, w: RegWriteFn, name: String, description: String)(implicit sourceInfo: SourceInfo) : RegField = apply(n, RegReadFnCover(name, description)(r), RegWriteFnCover(name, description)(w), name, description)
  def r(n: Int, r: RegReadFn,  name: String = "", description: String = "")(implicit sourceInfo: SourceInfo) : RegField = apply(n, RegReadFnCover(name, description)(r),  RegWriteFn(():Unit), name, description)
  def w(n: Int, w: RegWriteFn, name: String = "", description: String = "")(implicit sourceInfo: SourceInfo) : RegField = apply(n, RegReadFn((): Unit), RegWriteFnCover(name, description)(w),  name, description)

  // This RegField allows 'set' to set bits in 'reg'.
  // and to clear bits when the bus writes bits of value 1.
  // Setting takes priority over clearing.
  def w1ToClear(n: Int, reg: UInt, set: UInt)(implicit sourceInfo: SourceInfo): RegField =
    RegField(n, reg, RegWriteFn((valid, data) => { reg := ~(~reg | Mux(valid, data, UInt(0))) | set; Bool(true) }), "w1ToClear", "")

  // This RegField wraps an explicit register
  // (e.g. Black-Boxed Register) to create a R/W register.
  def rwReg(n: Int, bb: SimpleRegIO, name: String = "", description: String = "")(implicit sourceInfo: SourceInfo) : RegField =
    RegField(n, RegReadFn(bb.q), RegWriteFn((valid, data) => {
      bb.en := valid
      bb.d := data
      Bool(true)
    }), name, description)

  // Create byte-sized read-write RegFields out of a large UInt register.
  // It is updated when any of the bytes are written. Because the RegFields
  // are all byte-sized, this is also suitable when a register is larger
  // than the intended bus width of the device (atomic updates are impossible).
  def bytes(reg: UInt, numBytes: Int)(implicit sourceInfo: SourceInfo): Seq[RegField] = {
    val pad = reg | UInt(0, width = 8*numBytes)
    val oldBytes = Vec.tabulate(numBytes) { i => pad(8*(i+1)-1, 8*i) }
    val newBytes = Wire(init = oldBytes)
    val valids = Wire(init = Vec.fill(numBytes) { Bool(false) })
    when (valids.reduce(_ || _)) { reg := newBytes.asUInt }
    Seq.tabulate(numBytes) { i =>
      RegField(8, oldBytes(i),
        RegWriteFn((valid, data) => {
        valids(i) := valid
        when (valid) { newBytes(i) := data }
        Bool(true)
        }),
      "bytes", "")
    }}

  def bytes(reg: UInt)(implicit sourceInfo: SourceInfo): Seq[RegField] = {
    val width = reg.getWidth
    require (width % 8 == 0, s"RegField.bytes must be called on byte-sized reg, not ${width} bits")
    bytes(reg, width/8)
  }
}

trait HasRegMap
{
  def regmap(mapping: RegField.Map*): Unit
  val interrupts: Vec[Bool]
}

// See GPIO.scala for an example of how to use regmap
