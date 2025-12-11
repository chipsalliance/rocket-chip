// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import chisel3._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.ValName
import org.chipsalliance.diplomacy.lazymodule._
import org.chipsalliance.diplomacy.nodes.NodeHandle
import freechips.rocketchip.diplomacy.{AddressSet, RegionType}
import freechips.rocketchip.tilelink._

abstract class TLBusBypassBase(beatBytes: Int, deadlock: Boolean = false, bufferError: Boolean = true, maxAtomic: Int = 16, maxTransfer: Int = 4096)
  (implicit p: Parameters) extends LazyModule
{
  protected val nodeIn = TLIdentityNode()
  protected val nodeOut = TLIdentityNode()
  val node = NodeHandle(nodeIn, nodeOut)

  protected val bar = LazyModule(new TLBusBypassBar(dFn = { mp =>
    mp.v1copy(managers = mp.managers.map { m =>
      m.v1copy(
        mayDenyPut = m.mayDenyPut || !deadlock,
        mayDenyGet = m.mayDenyGet || !deadlock)
    })
  }))
  protected val everything = Seq(AddressSet(0, BigInt("ffffffffffffffffffffffffffffffff", 16))) // 128-bit
  protected val params = DevNullParams(everything, maxAtomic, maxTransfer, region=RegionType.TRACKED)
  protected val error = if (deadlock) LazyModule(new TLDeadlock(params, beatBytes))
                        else LazyModule(new TLError(params, bufferError, beatBytes))

  // order matters because the parameters and bypass
  // assume that the non-bypassed connection is
  // the last connection to the bar, so keep nodeOut last.
  bar.node := nodeIn
  error.node := bar.node
  nodeOut := bar.node
}

class TLBusBypass(beatBytes: Int, bufferError: Boolean = false, maxAtomic: Int = 16, maxTransfer: Int = 4096)(implicit p: Parameters)
    extends TLBusBypassBase(beatBytes, deadlock = false, bufferError = bufferError, maxAtomic = maxAtomic, maxTransfer = maxTransfer)
{
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val bypass = Input(Bool())
    })
    bar.module.io.bypass := io.bypass
  }
}

class TLBypassNode(dFn: TLSlavePortParameters => TLSlavePortParameters)(implicit valName: ValName) extends TLCustomNode
{
  def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
    require (iStars == 0 && oStars == 0, "TLBypass node does not support :=* or :*=")
    require (iKnown == 1, "TLBypass node expects exactly one input")
    require (oKnown == 2, "TLBypass node expects exactly two outputs")
    (0, 0)
  }
  def mapParamsD(n: Int, p: Seq[TLMasterPortParameters]): Seq[TLMasterPortParameters] = { p ++ p }
  def mapParamsU(n: Int, p: Seq[TLSlavePortParameters]): Seq[TLSlavePortParameters] = { Seq(dFn(p.last).v1copy(minLatency = p.map(_.minLatency).min))}
}

class TLBusBypassBar(dFn: TLSlavePortParameters => TLSlavePortParameters)(implicit p: Parameters) extends LazyModule
{
  val node = new TLBypassNode(dFn)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val bypass = Input(Bool())
      val pending = Output(Bool())
    })

    val (in, edgeIn) = node.in(0)
    val Seq((out0, edgeOut0), (out1, edgeOut1)) = node.out

    require (edgeOut0.manager.beatBytes == edgeOut1.manager.beatBytes,
      s"BusBypass slave device widths mismatch (${edgeOut0.manager.managers.map(_.name)} has ${edgeOut0.manager.beatBytes}B vs ${edgeOut1.manager.managers.map(_.name)} has ${edgeOut1.manager.beatBytes}B)")

    // We need to be locked to the given bypass direction until all transactions stop
    val in_reset = RegNext(false.B, init = true.B)
    val bypass_reg = Reg(Bool())
    val bypass = Mux(in_reset, io.bypass, bypass_reg)
    val (flight, next_flight) = edgeIn.inFlight(in)

    io.pending := (flight > 0.U)
    when (in_reset || (next_flight === 0.U)) { bypass_reg := io.bypass }
    val stall = (bypass =/= io.bypass) && edgeIn.first(in.a)

    out0.a.valid := !stall && in.a.valid &&  bypass
    out1.a.valid := !stall && in.a.valid && !bypass
    in.a.ready   := !stall && Mux(bypass, out0.a.ready, out1.a.ready)
    out0.a.bits  := in.a.bits
    out1.a.bits  := in.a.bits

    out0.d.ready := in.d.ready &&  bypass
    out1.d.ready := in.d.ready && !bypass
    in.d.valid   := Mux(bypass, out0.d.valid, out1.d.valid)
    def cast(x: TLBundleD) = { val out = WireDefault(in.d.bits); out <> x; out }
    in.d.bits := Mux(bypass, cast(out0.d.bits), cast(out1.d.bits))

    if (edgeIn.manager.anySupportAcquireB && edgeIn.client.anySupportProbe) {
      out0.b.ready := in.b.ready &&  bypass
      out1.b.ready := in.b.ready && !bypass
      in.b.valid   := Mux(bypass, out0.b.valid, out1.b.valid)
      def cast(x: TLBundleB) = { val out = Wire(in.b.bits); out <> x; out }
      in.b.bits := Mux(bypass, cast(out0.b.bits), cast(out1.b.bits))

      out0.c.valid := in.c.valid &&  bypass
      out1.c.valid := in.c.valid && !bypass
      in.c.ready   := Mux(bypass, out0.c.ready, out1.c.ready)
      out0.c.bits  := in.c.bits
      out1.c.bits  := in.c.bits

      out0.e.valid := in.e.valid &&  bypass
      out1.e.valid := in.e.valid && !bypass
      in.e.ready   := Mux(bypass, out0.e.ready, out1.e.ready)
      out0.e.bits  := in.e.bits
      out1.e.bits  := in.e.bits
    } else {
      in.b.valid := false.B
      in.c.ready := true.B
      in.e.ready := true.B

      out0.b.ready := true.B
      out0.c.valid := false.B
      out0.e.valid := false.B

      out1.b.ready := true.B
      out1.c.valid := false.B
      out1.e.valid := false.B
    }
  }
}
