// See LICENSE for license details.

package uncore.tilelink2

import Chisel._

case class RegField(width: Int, read: RegField.ReadFn, write: RegField.WriteFn)
object RegField
{
  type ReadFn = Bool => (Bool, UInt)
  type WriteFn = (Bool, UInt) => Bool
  type Map = (Int, Seq[RegField])

  def apply(n: Int)                        : RegField = apply(n, noR, noW)
  def apply(n: Int, rw: UInt)              : RegField = apply(n, regR(rw), regW(rw))
  def apply(n: Int, r: UInt,   w: UInt)    : RegField = apply(n, regR(r),  regW(w))
  def apply(n: Int, r: UInt,   w: WriteFn) : RegField = apply(n, regR(r),  w)
  def apply(n: Int, r: ReadFn, w: UInt)    : RegField = apply(n, r,        regW(w))
  def R(n: Int, r: ReadFn)                 : RegField = apply(n, r, noW)
  def R(n: Int, r: UInt)                   : RegField = R(n, regR(r))
  def W(n: Int, w: WriteFn)                : RegField = apply(n, noR, w)
  def W(n: Int, w: UInt)                   : RegField = W(n, regW(w))

  private val noR = (en: Bool) => (Bool(true), UInt(0))
  private val noW = (en: Bool, in: UInt) => Bool(true)
  private def regR(reg: UInt) = (en: Bool) => (Bool(true), reg)
  private def regW(reg: UInt) = (en: Bool, in: UInt) =>
  {
     when (en) { reg := in }
     Bool(true)
  }
}

trait HasRegMap
{
  def regmap(mapping: RegField.Map*): Unit
}
