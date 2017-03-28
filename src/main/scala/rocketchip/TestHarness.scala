// See LICENSE.SiFive for license details.

package rocketchip

import Chisel._
import config._
import junctions._
import diplomacy._
import coreplex._
import uncore.axi4._
import jtag.JTAGIO

class TestHarness()(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val success = Bool(OUTPUT)
  }
  val dut = Module(LazyModule(new ExampleRocketTop).module)

  dut.io.interrupts := UInt(0)

  val channels = p(coreplex.BankedL2Config).nMemoryChannels
  if (channels > 0) Module(LazyModule(new SimAXIMem(channels)).module).io.axi4 <> dut.io.mem_axi4

  if (!p(IncludeJtagDTM)) {
    val dtm = Module(new SimDTM).connect(clock, reset, dut.io.debug.get, io.success)
  } else {
    val jtag = Module(new JTAGVPI).connect(dut.io.jtag.get, dut.io.jtag_reset.get, reset, io.success)
  }

  val mmio_sim = Module(LazyModule(new SimAXIMem(1, 4096)).module)
  mmio_sim.io.axi4 <> dut.io.mmio_axi4

  val l2_axi4 = dut.io.l2_frontend_bus_axi4(0)
  l2_axi4.ar.valid := Bool(false)
  l2_axi4.aw.valid := Bool(false)
  l2_axi4.w .valid := Bool(false)
  l2_axi4.r .ready := Bool(true)
  l2_axi4.b .ready := Bool(true)
}

class SimAXIMem(channels: Int, forceSize: BigInt = 0)(implicit p: Parameters) extends LazyModule {
  val config = p(ExtMem)
  val totalSize = if (forceSize > 0) forceSize else BigInt(config.size)
  val size = totalSize / channels
  require(totalSize % channels == 0)

  val node = AXI4BlindInputNode(Seq.fill(channels) {
    AXI4MasterPortParameters(Seq(AXI4MasterParameters(IdRange(0, 1 << config.idBits))))})

  for (i <- 0 until channels) {
    val sram = LazyModule(new AXI4RAM(AddressSet(0, size-1), beatBytes = config.beatBytes))
    sram.node := AXI4Buffer()(AXI4Fragmenter(maxInFlight = 4)(node))
  }

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val axi4 = node.bundleIn
    }
  }
}

class SimDTM(implicit p: Parameters) extends BlackBox {
  val io = new Bundle {
    val clk = Clock(INPUT)
    val reset = Bool(INPUT)
    val debug = new uncore.devices.DMIIO
    val exit = UInt(OUTPUT, 32)
  }

  def connect(tbclk: Clock, tbreset: Bool, dutio: uncore.devices.ClockedDMIIO, tbsuccess: Bool) = {
    io.clk := tbclk
    io.reset := tbreset
    dutio <> io.debug
    dutio.dmiClock := tbclk
    dutio.dmiReset := tbreset

    tbsuccess := io.exit === UInt(1)
    when (io.exit >= UInt(2)) {
      printf("*** FAILED *** (exit code = %d)\n", io.exit >> UInt(1))
      stop(1)
    }
  }
}

class JTAGVPI(implicit val p: Parameters) extends BlackBox {
  val io = new Bundle {
    val jtag = new JTAGIO(hasTRSTn = false)
    val enable = Bool(INPUT)
    val init_done = Bool(INPUT)
  }

  def connect(dutio: JTAGIO, jtag_reset: Bool, tbreset: Bool, tbsuccess: Bool) = {
    dutio <> io.jtag

    dutio.TRSTn.foreach{ _:= false.B}
    jtag_reset := tbreset

    io.enable    := ~tbreset
    io.init_done := ~tbreset

    // Success is determined by the gdbserver
    // which is controlling this simulation.
    tbsuccess := Bool(false)
  }
}
