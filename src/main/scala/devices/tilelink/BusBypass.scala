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

  protected val bar = LazyModule(new TLBusBypassBar)
  protected val everything = Seq(AddressSet(0, BigInt("ffffffffffffffffffffffffffffffff", 16))) // 128-bit
  protected val params = ErrorParams(everything, maxAtomic=16, maxTransfer=4096)
  protected val error = if (deadlock) LazyModule(new DeadlockDevice(params, beatBytes))
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

class TLBypassNode(implicit valName: ValName) extends TLCustomNode
{
  def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
    require (iStars == 0 && oStars == 0, "TLBypass node does not support :=* or :*=")
    require (iKnown == 1, "TLBypass node expects exactly one input")
    require (oKnown == 2, "TLBypass node expects exactly one output")
    (0, 0)
  }
  def mapParamsD(n: Int, p: Seq[TLClientPortParameters]): Seq[TLClientPortParameters] = { p ++ p }
  def mapParamsU(n: Int, p: Seq[TLManagerPortParameters]): Seq[TLManagerPortParameters] = { p.tail }
}

class TLBusBypassBar(implicit p: Parameters) extends LazyModule
{
  val node = new TLBypassNode()

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val bypass = Bool(INPUT)
      val pending = Bool(OUTPUT)
    })

    val (in, edgeIn) = node.in(0)
    val Seq((out0, edgeOut0), (out1, edgeOut1)) = node.out

    require (edgeOut0.manager.beatBytes == edgeOut1.manager.beatBytes,
      s"BusBypass slave device widths mismatch (${edgeOut0.manager.managers.map(_.name)} has ${edgeOut0.manager.beatBytes}B vs ${edgeOut1.manager.managers.map(_.name)} has ${edgeOut1.manager.beatBytes}B)")

    val bce = edgeIn.manager.anySupportAcquireB && edgeIn.client.anySupportProbe

    // We need to be locked to the given bypass direction until all transactions stop
    val flight = RegInit(UInt(0, width = log2Ceil(3*edgeIn.client.endSourceId+1)))
    val bypass = RegInit(io.bypass) // synchronous reset required

    io.pending := (flight > 0.U)

    val (a_first, a_last, _) = edgeIn.firstlast(in.a)
    val (b_first, b_last, _) = edgeIn.firstlast(in.b)
    val (c_first, c_last, _) = edgeIn.firstlast(in.c)
    val (d_first, d_last, _) = edgeIn.firstlast(in.d)
    val (e_first, e_last, _) = edgeIn.firstlast(in.e)

    val (a_request, a_response) = (edgeIn.isRequest(in.a.bits), edgeIn.isResponse(in.a.bits))
    val (b_request, b_response) = (edgeIn.isRequest(in.b.bits), edgeIn.isResponse(in.b.bits))
    val (c_request, c_response) = (edgeIn.isRequest(in.c.bits), edgeIn.isResponse(in.c.bits))
    val (d_request, d_response) = (edgeIn.isRequest(in.d.bits), edgeIn.isResponse(in.d.bits))
    val (e_request, e_response) = (edgeIn.isRequest(in.e.bits), edgeIn.isResponse(in.e.bits))

    val a_inc = in.a.fire() && a_first && a_request
    val b_inc = in.b.fire() && b_first && b_request
    val c_inc = in.c.fire() && c_first && c_request
    val d_inc = in.d.fire() && d_first && d_request
    val e_inc = in.e.fire() && e_first && e_request
    val inc = Cat(Seq(a_inc, d_inc) ++ (if (bce) Seq(b_inc, c_inc, e_inc) else Nil))

    val a_dec = in.a.fire() && a_last && a_response
    val b_dec = in.b.fire() && b_last && b_response
    val c_dec = in.c.fire() && c_last && c_response
    val d_dec = in.d.fire() && d_last && d_response
    val e_dec = in.e.fire() && e_last && e_response
    val dec = Cat(Seq(a_dec, d_dec) ++ (if (bce) Seq(b_dec, c_dec, e_dec) else Nil))

    val next_flight = flight + PopCount(inc) - PopCount(dec)
    flight := next_flight

    when (next_flight === UInt(0)) { bypass := io.bypass }
    val stall = (bypass != io.bypass) && a_first

    out0.a.valid := !stall && in.a.valid &&  bypass
    out1.a.valid := !stall && in.a.valid && !bypass
    in.a.ready   := !stall && Mux(bypass, out0.a.ready, out1.a.ready)
    out0.a.bits  := in.a.bits
    out1.a.bits  := in.a.bits

    out0.d.ready := in.d.ready &&  bypass
    out1.d.ready := in.d.ready && !bypass
    in.d.valid   := Mux(bypass, out0.d.valid, out1.d.valid)

    // Argh. The Bundles are not identical, so Mux on bits does not work
    in.d.bits.opcode := Mux(bypass, out0.d.bits.opcode, out1.d.bits.opcode)
    in.d.bits.param  := Mux(bypass, out0.d.bits.param,  out1.d.bits.param)
    in.d.bits.size   := Mux(bypass, out0.d.bits.size,   out1.d.bits.size)
    in.d.bits.source := Mux(bypass, out0.d.bits.source, out1.d.bits.source)
    in.d.bits.sink   := Mux(bypass, out0.d.bits.sink,   out1.d.bits.sink)
    in.d.bits.data   := Mux(bypass, out0.d.bits.data,   out1.d.bits.data)
    in.d.bits.error  := Mux(bypass, out0.d.bits.error,  out1.d.bits.error)

    if (bce) {
      out0.b.ready := in.b.ready &&  bypass
      out1.b.ready := in.b.ready && !bypass
      in.b.valid   := Mux(bypass, out0.b.valid, out1.b.valid)

      in.b.bits.opcode := Mux(bypass, out0.b.bits.opcode, out1.b.bits.opcode)
      in.b.bits.param  := Mux(bypass, out0.b.bits.param,  out1.b.bits.param)
      in.b.bits.size   := Mux(bypass, out0.b.bits.size,   out1.b.bits.size)
      in.b.bits.source := Mux(bypass, out0.b.bits.source, out1.b.bits.source)
      in.b.bits.address:= Mux(bypass, out0.b.bits.address,out1.b.bits.address)
      in.b.bits.mask   := Mux(bypass, out0.b.bits.mask,   out1.b.bits.mask)
      in.b.bits.data   := Mux(bypass, out0.b.bits.data,   out1.b.bits.data)

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
