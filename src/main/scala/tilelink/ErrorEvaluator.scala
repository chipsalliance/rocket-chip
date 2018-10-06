// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

// Check if a request satisfies some interesting property
class RequestPattern(test: TLBundleA => Bool)
{
  def apply(a: TLBundleA) = test(a)
}

object RequestPattern
{
  // Does the request address range overlap a particular pattern of addresses?
  def overlaps(pattern: Seq[AddressSet])(a: TLBundleA) = {
    val amask = UIntToOH1(a.size, a.params.addressBits)
    val abase = a.address
    pattern.map { case p =>
      val pbase = UInt(p.base)
      val pmask = UInt(p.mask & ((BigInt(1) << a.params.addressBits) - 1))
      (amask | pmask | ~(abase ^ pbase)).andR
    }.reduce(_ || _)
  }

  implicit def apply(pattern: Seq[AddressSet]): RequestPattern = new RequestPattern(overlaps(pattern) _)
}

// An ErrorEvaluator is used for test harnesses.
// It creates errors in transactions which match the provided test function.
// If testOn is true, it will assert fail if these transactions do not already error.
// If testOff is true, it will assert fail if these transactions otherwise error.
// This helps when building unit tests to confirm that errors are propagated correctly.
class TLErrorEvaluator(test: RequestPattern, testOn: Boolean, testOff: Boolean, deny: Boolean = false)(implicit p: Parameters) extends LazyModule
{
  val node = TLAdapterNode(managerFn = { mp => mp.copy(managers =
    mp.managers.map { m => m.copy(mayDenyPut = true, mayDenyGet = deny || m.mayDenyGet) }) })

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in

      // Should the request receive an error ?
      val inject_map = Mem(edgeIn.client.endSourceId, Bool())
      val inject_now = test(in.a.bits)

      val (d_first, d_last, _) = edgeOut.firstlast(out.d)
      val d_hasData = edgeOut.hasData(out.d.bits)

      when (in.a.fire()) { inject_map.write(in.a.bits.source, inject_now) }

      val bypass = Bool(edgeOut.manager.minLatency == 0) && in.a.fire() && in.a.bits.source === in.d.bits.source
      val d_inject = Mux(bypass, inject_now, inject_map.read(in.d.bits.source)) holdUnless d_first
      in.d.bits.corrupt := out.d.bits.corrupt || (d_inject &&   d_hasData)
      in.d.bits.denied  := out.d.bits.denied  || (d_inject && (!d_hasData || Bool(deny)))

      val r_detect = Reg(Bool())
      val d_detect = (!d_first && r_detect) || (Bool(!deny) && out.d.bits.corrupt) || out.d.bits.denied
      when (out.d.fire()) { r_detect := d_detect }

      val d_hint = out.d.bits.opcode === TLMessages.HintAck // even illegal hints can succeed
      assert (Bool(!testOn)  || !out.d.fire() || !d_last || !d_inject ||  d_detect || d_hint, "Denied/Corrupt flag was not set!")
      assert (Bool(!testOff) || !out.d.fire() || !d_last ||  d_inject || !d_detect, "Denied/Corrupt flag was set!")
    }
  }
}

object TLErrorEvaluator
{
  def apply(test: RequestPattern, testOn: Boolean = false, testOff: Boolean = false, deny: Boolean = false)(implicit p: Parameters): TLNode =
  {
    val errors = LazyModule(new TLErrorEvaluator(test, testOn, testOff, deny))
    errors.node
  }
}
