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
  dut.reset := reset | dut.debug.ndreset

  dut.tieOffInterrupts()
  dut.connectSimAXIMem()
  dut.connectSimAXIMMIO()
  dut.tieOffAXI4SlavePort()
  dut.connectDebug(clock, reset, io.success)
}

class SimAXIMem(channels: Int, forceSize: BigInt = 0)(implicit p: Parameters) extends LazyModule {
  val config = p(ExtMem)
  val totalSize = if (forceSize > 0) forceSize else BigInt(config.size)
  val size = totalSize / channels
  require(totalSize % channels == 0)

  val node = AXI4BlindInputNode(Seq.fill(channels) {
    AXI4MasterPortParameters(Seq(AXI4MasterParameters(
      name = "dut",
      id   = IdRange(0, 1 << config.idBits))))})

  for (i <- 0 until channels) {
    val sram = LazyModule(new AXI4RAM(AddressSet(0, size-1), beatBytes = config.beatBytes))
    sram.node := AXI4Buffer()(AXI4Fragmenter()(node))
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
    dutio.dmi <> io.debug
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
