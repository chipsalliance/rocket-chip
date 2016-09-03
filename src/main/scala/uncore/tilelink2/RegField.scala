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

// See GPIO.scala for an example of how to use regmap
