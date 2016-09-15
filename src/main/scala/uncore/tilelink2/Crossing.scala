// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import junctions._

class TLAsyncCrossing(depth: Int = 8, sync: Int = 3) extends LazyModule
{
  val node = TLIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in        = node.bundleIn
      val in_clock  = Clock(INPUT)
      val in_reset  = Bool(INPUT)
      val out       = node.bundleOut
      val out_clock = Clock(INPUT)
      val out_reset = Bool(INPUT)
    }

    // Transfer all TL2 bundles from/to the same domains
    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>
      out.a <> AsyncIrrevocableCrossing(io.in_clock, io.in_reset, in.a, io.out_clock, io.out_reset, depth, sync)
      in.d <> AsyncIrrevocableCrossing(io.out_clock, io.out_reset, out.d, io.in_clock, io.in_reset, depth, sync)

      if (edgeOut.manager.anySupportAcquire && edgeOut.client.anySupportProbe) {
        in.b <> AsyncIrrevocableCrossing(io.out_clock, io.out_reset, out.b, io.in_clock, io.in_reset, depth, sync)
        out.c <> AsyncIrrevocableCrossing(io.in_clock, io.in_reset, in.c, io.out_clock, io.out_reset, depth, sync)
        out.e <> AsyncIrrevocableCrossing(io.in_clock, io.in_reset, in.e, io.out_clock, io.out_reset, depth, sync)
      } else {
        in.b.valid := Bool(false)
        in.c.ready := Bool(true)
        in.e.ready := Bool(true)
        out.b.ready := Bool(true)
        out.c.valid := Bool(false)
        out.e.valid := Bool(false)
      }
    }
  }
}
