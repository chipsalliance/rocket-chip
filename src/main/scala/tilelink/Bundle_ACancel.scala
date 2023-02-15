// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._
import freechips.rocketchip.util._
import scala.collection.immutable.ListMap
import freechips.rocketchip.util.EnhancedChisel3Assign

class TLBundle_ACancel(val params: TLBundleParameters) extends Record
{
  val a =  ReadyValidCancel(new TLBundleA(params))
  val b = Flipped(Decoupled(new TLBundleB(params)))
  val c =         Decoupled(new TLBundleC(params))
  val d = Flipped(Decoupled(new TLBundleD(params)))
  val e =         Decoupled(new TLBundleE(params))

  override def cloneType: this.type = (new TLBundle_ACancel(params)).asInstanceOf[this.type]
  val elements = ListMap("e" -> e, "d" -> d, "c" -> c, "b" -> b, "a" -> a)

  /** Down-converts a TLBundle_ACancel to a plain TLBundle, dropping early/late timing split. */
  def asDecoupled(): TLBundle = {
    val out = Wire(new TLBundle(params))
    out.a :<> a.asDecoupled()
    b :<> out.b
    out.c :<> c
    d :<> out.d
    out.e :<> e
    out
  }

  /** Down-converts a TLBundle_ACancel to a plain TLBundle, dropping early/late timing split.
    * This differs from [[asDecoupled]]: this is uni-directional, suitable solely for monitoring. */
  def monitorAndNotCancel(): TLBundle = {
    val out = Wire(new TLBundle(params))
    out.a.valid := a.validQual()
    out.a.bits  := a.bits
    out.a.ready := a.ready
    out.b := b
    out.c := c
    out.d := d
    out.e := e
    out
  }
}

object TLBundle_ACancel
{
  def apply(params: TLBundleParameters) = new TLBundle_ACancel(params)

  /** Up-converts a ReadyValid to a ReadyValidCancel, assuming conservative timing. */
  def apply(in: TLBundle) = {
    val out = Wire(new TLBundle_ACancel(in.params))
    out.a.earlyValid := in.a.valid
    out.a.lateCancel := false.B
    out.a.bits := in.a.bits
    in.a.ready := out.a.ready
    in.b :<> out.b
    out.c :<> in.c
    in.d :<> out.d
    out.e :<> in.e
    out
  }
}
