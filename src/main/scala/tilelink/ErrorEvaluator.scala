// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

// An ErrorEvaluator is used for test harnesses.
// It creates errors in transactions which overlap an address in pattern.
// If testOn is true, it will assert fail if these transactions do not already error.
// If testOff is true, it will assert fail if these transactions otherwise error.
// This helps when building unit tests to confirm that errors are propagated correctly.
class TLErrorEvaluator(pattern: Seq[AddressSet], testOn: Boolean, testOff: Boolean)(implicit p: Parameters) extends LazyModule
{
  val node = TLAdapterNode()
  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in

      // Does the request overlap the pattern ?
      val abase = in.a.bits.address
      val amask = UIntToOH1(in.a.bits.size, log2Up(edgeIn.maxTransfer))
      val overlaps = pattern.map { case a =>
        val pbase = UInt(a.base)
        val pmask = UInt(a.mask & ((BigInt(1) << edgeIn.bundle.addressBits) - 1))
        (amask | pmask | ~(abase ^ pbase)).andR
      }.reduce(_ || _)

      val (d_first, d_last, _) = edgeOut.firstlast(out.d)

      val inject = Mem(edgeIn.client.endSourceId, Bool())
      when (in.a.fire()) { inject.write(in.a.bits.source, overlaps) }

      val bypass = in.a.fire() && in.a.bits.source === in.d.bits.source
      val d_inject = Mux(bypass, overlaps, inject.read(in.d.bits.source)) holdUnless d_first
      in.d.bits.error := out.d.bits.error || (d_last && d_inject)

      assert (Bool(!testOn)  || !out.d.fire() || !d_last || !d_inject ||  out.d.bits.error, "Error flag was not set!")
      assert (Bool(!testOff) || !out.d.fire() || !d_last ||  d_inject || !out.d.bits.error, "Error flag was set!")
    }
  }
}

object TLErrorEvaluator
{
  def apply(pattern: Seq[AddressSet], testOn: Boolean = false, testOff: Boolean = false)(implicit p: Parameters): TLNode =
    LazyModule(new TLErrorEvaluator(pattern, testOn, testOff)).node
}
