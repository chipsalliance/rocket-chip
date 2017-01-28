// See LICENSE.SiFive for license details.

package regmapper

import Chisel._
import chisel3.util.{ReadyValidIO}
import util.{SimpleRegIO}

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
      (Bool(true), ovalid, data)
    })
  // read from a ReadyValidIO (only safe if there is a consistent source of data)
  implicit def apply(x: ReadyValidIO[UInt]):RegReadFn = RegReadFn(ready => { x.ready := ready; (x.valid, x.bits) })
  // read from a register
  implicit def apply(x: UInt):RegReadFn = RegReadFn(ready => (Bool(true), x))
  // noop
  implicit def apply(x: Unit):RegReadFn = RegReadFn(UInt(0))
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

case class RegField(width: Int, read: RegReadFn, write: RegWriteFn, name: String, description: String)
{
  require (width > 0)
  def pipelined = !read.combinational || !write.combinational
}

object RegField
{
  // Byte address => sequence of bitfields, lowest index => lowest address
  type Map = (Int, Seq[RegField])

  def apply(n: Int)                                                         : RegField = apply(n, (), (), "", "")
  def apply(n: Int, r: RegReadFn, w: RegWriteFn)                            : RegField = apply(n, r, w,   "", "")
  def apply(n: Int, rw: UInt)                                               : RegField = apply(n, rw, rw, "", "")
  def apply(n: Int, rw: UInt, name: String, description: String)            : RegField = apply(n, rw, rw, name, description)
  def r(n: Int, r: RegReadFn,  name: String = "", description: String = "") : RegField = apply(n, r,  (), name, description)
  def w(n: Int, w: RegWriteFn, name: String = "", description: String = "") : RegField = apply(n, (), w,  name, description)

  // This RegField allows 'set' to set bits in 'reg'.
  // and to clear bits when the bus writes bits of value 1.
  // Setting takes priority over clearing.
  def w1ToClear(n: Int, reg: UInt, set: UInt): RegField =
    RegField(n, reg, RegWriteFn((valid, data) => { reg := ~(~reg | Mux(valid, data, UInt(0))) | set; Bool(true) }))

  // This RegField wraps an explicit register
  // (e.g. Black-Boxed Register) to create a R/W register.
  def rwReg(n: Int, bb: SimpleRegIO): RegField =
    RegField(n, bb.q, RegWriteFn((valid, data) => {
      bb.en := valid
      bb.d := data
      Bool(true)
    }))
}

trait HasRegMap
{
  def regmap(mapping: RegField.Map*): Unit
  val interrupts: Vec[Bool]
}

// See GPIO.scala for an example of how to use regmap
