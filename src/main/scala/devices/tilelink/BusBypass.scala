// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import scala.math.min

abstract class TLBusBypassBase(beatBytes: Int, deadlock: Boolean = false)(implicit p: Parameters) extends LazyModule
{
  protected val nodeIn = TLIdentityNode()
  protected val nodeOut = TLIdentityNode()
  val node = NodeHandle(nodeIn, nodeOut)

  protected val bar = LazyModule(new TLBusBypassBar(dFn = { mp =>
    mp.copy(managers = mp.managers.map { m =>
      m.copy(
        mayDenyPut = m.mayDenyPut || !deadlock,
        mayDenyGet = m.mayDenyGet || !deadlock)
    })
  }))
  protected val everything = Seq(AddressSet(0, BigInt("ffffffffffffffffffffffffffffffff", 16))) // 128-bit
  protected val params = DevNullParams(everything, maxAtomic=16, maxTransfer=4096, region=RegionType.TRACKED)
  protected val error = if (deadlock) LazyModule(new TLDeadlock(params, beatBytes))
                        else LazyModule(new TLError(params, beatBytes))

  // order matters
  bar.node := nodeIn
  error.node := bar.node
  nodeOut := bar.node
}

class TLBusBypass(beatBytes: Int)(implicit p: Parameters) extends TLBusBypassBase(beatBytes)
{
  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val bypass = Bool(INPUT)
    })
    bar.module.io.bypass := io.bypass
  }
}

class TLBypassNode(dFn: TLManagerPortParameters => TLManagerPortParameters)(implicit valName: ValName) extends TLCustomNode
{
  def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
    require (iStars == 0 && oStars == 0, "TLBypass node does not support :=* or :*=")
    require (iKnown == 1, "TLBypass node expects exactly one input")
    require (oKnown == 2, "TLBypass node expects exactly two outputs")
    (0, 0)
  }
  def mapParamsD(n: Int, p: Seq[TLClientPortParameters]): Seq[TLClientPortParameters] = { p ++ p }
  def mapParamsU(n: Int, p: Seq[TLManagerPortParameters]): Seq[TLManagerPortParameters] = { Seq(dFn(p.last)) }
}

class TLBusBypassBar(dFn: TLManagerPortParameters => TLManagerPortParameters)(implicit p: Parameters) extends LazyModule
{
  val node = new TLBypassNode(dFn)

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val bypass = Bool(INPUT)
      val pending = Bool(OUTPUT)
    })

    val (in, edgeIn) = node.in(0)
    val Seq((out0, edgeOut0), (out1, edgeOut1)) = node.out

    require (edgeOut0.manager.beatBytes == edgeOut1.manager.beatBytes,
      s"BusBypass slave device widths mismatch (${edgeOut0.manager.managers.map(_.name)} has ${edgeOut0.manager.beatBytes}B vs ${edgeOut1.manager.managers.map(_.name)} has ${edgeOut1.manager.beatBytes}B)")

    // We need to be locked to the given bypass direction until all transactions stop
    val bypass = RegInit(io.bypass) // synchronous reset required
    val (flight, next_flight) = edgeIn.inFlight(in)

    io.pending := (flight > 0.U)
    when (next_flight === UInt(0)) { bypass := io.bypass }
    val stall = (bypass =/= io.bypass) && edgeIn.first(in.a)

    out0.a.valid := !stall && in.a.valid &&  bypass
    out1.a.valid := !stall && in.a.valid && !bypass
    in.a.ready   := !stall && Mux(bypass, out0.a.ready, out1.a.ready)
    out0.a.bits  := in.a.bits
    out1.a.bits  := in.a.bits

    out0.d.ready := in.d.ready &&  bypass
    out1.d.ready := in.d.ready && !bypass
    in.d.valid   := Mux(bypass, out0.d.valid, out1.d.valid)
    def cast(x: TLBundleD) = { val out = Wire(in.d.bits); out <> x; out }
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
      in.b.valid := Bool(false)
      in.c.ready := Bool(true)
      in.e.ready := Bool(true)

      out0.b.ready := Bool(true)
      out0.c.valid := Bool(false)
      out0.e.valid := Bool(false)

      out1.b.ready := Bool(true)
      out1.c.valid := Bool(false)
      out1.e.valid := Bool(false)
    }
  }
}
