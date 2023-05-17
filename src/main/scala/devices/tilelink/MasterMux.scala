// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import chisel3._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class MasterMuxNode(uFn: Seq[TLMasterPortParameters] => TLMasterPortParameters)(implicit valName: ValName) extends TLCustomNode
{
  def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
    require (iStars == 0 && oStars == 0, "MasterMux node does not support :=* or :*=")
    require (iKnown == 2, "MasterMux node expects exactly two inputs")
    require (oKnown == 1, "MasterMux node expects exactly one output")
    (0, 0)
  }
  def mapParamsD(n: Int, p: Seq[TLMasterPortParameters]): Seq[TLMasterPortParameters] = { Seq(uFn(p)) }
  def mapParamsU(n: Int, p: Seq[TLSlavePortParameters]): Seq[TLSlavePortParameters] = { p ++ p }
}

class MuteMaster(name: String = "MuteMaster", maxProbe: Int = 0)(implicit p: Parameters) extends LazyModule
{
  val node = TLClientNode(Seq(TLMasterPortParameters.v1(clients = Seq(TLMasterParameters.v1(
    name = name,
    supportsProbe = if (maxProbe > 0) TransferSizes(1, maxProbe) else TransferSizes.none)))))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val Seq((out, edgeOut)) = node.out
    out.a.valid := false.B
    out.b.ready := out.c.ready
    out.c.valid := out.b.valid
    out.d.ready := true.B
    out.e.valid := false.B

    out.c.bits := edgeOut.ProbeAck(out.b.bits, TLPermissions.NtoN)
  }
}

class MasterMux(uFn: Seq[TLMasterPortParameters] => TLMasterPortParameters)(implicit p: Parameters) extends LazyModule
{
  val node = new MasterMuxNode(uFn)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val bypass = Input(Bool())
      val pending = Output(Bool())
    })

    val Seq((in0, edgeIn0), (in1, edgeIn1)) = node.in
    val Seq((out, edgeOut)) = node.out

    // We need to be locked to the given bypass direction until all transactions stop
    val bypass = RegInit(io.bypass) // synchronous reset required
    val (flight, next_flight) = edgeOut.inFlight(out)

    io.pending := (flight > 0.U)
    when (next_flight === 0.U) { bypass := io.bypass }
    val stall = (bypass =/= io.bypass) && edgeOut.first(out.a)

    in0.a.ready := !stall && out.a.ready &&  bypass
    in1.a.ready := !stall && out.a.ready && !bypass
    out.a.valid := !stall && Mux(bypass, in0.a.valid, in1.a.valid)
    def castA(x: TLBundleA) = { val ret = Wire(x.cloneType); ret <> x; ret }
    out.a.bits := Mux(bypass, castA(in0.a.bits), castA(in1.a.bits))

    out.d.ready := Mux(bypass, in0.d.ready, in1.d.ready)
    in0.d.valid := out.d.valid &&  bypass
    in1.d.valid := out.d.valid && !bypass
    in0.d.bits := out.d.bits
    in1.d.bits := out.d.bits

    if (edgeOut.manager.anySupportAcquireB && edgeOut.client.anySupportProbe) {
      out.b.ready := Mux(bypass, in0.b.ready, in1.b.ready)
      in0.b.valid := out.b.valid &&  bypass
      in1.b.valid := out.b.valid && !bypass
      in0.b.bits := out.b.bits
      in1.b.bits := out.b.bits

      in0.c.ready := out.c.ready &&  bypass
      in1.c.ready := out.c.ready && !bypass
      out.c.valid := Mux(bypass, in0.c.valid, in1.c.valid)
      def castC(x: TLBundleC) = { val ret = Wire(out.c.bits); ret <> x; ret }
      out.c.bits := Mux(bypass, castC(in0.c.bits), castC(in1.c.bits))

      in0.e.ready := out.e.ready &&  bypass
      in1.e.ready := out.e.ready && !bypass
      out.e.valid := Mux(bypass, in0.e.valid, in1.e.valid)
      def castE(x: TLBundleE) = { val ret = Wire(out.e.bits); ret <> x; ret }
      out.e.bits := Mux(bypass, castE(in0.e.bits), castE(in1.e.bits))
    } else {
      in0.b.valid := false.B
      in0.c.ready := true.B
      in0.e.ready := true.B

      in1.b.valid := false.B
      in1.c.ready := true.B
      in1.e.ready := true.B

      out.b.ready := true.B
      out.c.valid := false.B
      out.e.valid := false.B
    }
  }
}

// Synthesizable unit tests
import freechips.rocketchip.unittest._

class TLMasterMuxTester(txns: Int)(implicit p: Parameters) extends LazyModule {
  val fuzz1 = LazyModule(new TLFuzzer(txns))
  val fuzz2 = LazyModule(new TLFuzzer(txns))
  val model1 = LazyModule(new TLRAMModel("MasterMux1"))
  val model2 = LazyModule(new TLRAMModel("MasterMux2"))
  val mux = LazyModule(new MasterMux(uFn = _.head))
  val ram = LazyModule(new TLRAM(AddressSet(0, 0x3ff), beatBytes = 4))
  mux.node := TLFilter(TLFilter.mSelectIntersect(AddressSet( 0, ~16))) := model1.node := fuzz1.node
  mux.node := TLFilter(TLFilter.mSelectIntersect(AddressSet(16, ~16))) := model2.node := fuzz2.node
  ram.node := TLFragmenter(4, 16) := mux.node
  // how to test probe + release?

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz1.module.io.finished && fuzz2.module.io.finished
    mux.module.io.bypass := LFSR64(true.B)(0)
  }
}

class TLMasterMuxTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new TLMasterMuxTester(txns)).module)
  io <> dut.io
}
