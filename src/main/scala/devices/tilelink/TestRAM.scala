// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.diplomacy.{AddressSet, RegionType, TransferSizes}
import freechips.rocketchip.resources.{MemoryDevice}
import freechips.rocketchip.tilelink.{TLDelayer, TLFuzzer, TLManagerNode, TLMessages, TLRAMModel, TLManagerParameters, TLManagerPortParameters}

// Do not use this for synthesis! Only for simulation.
class TLTestRAM(address: AddressSet, executable: Boolean = true, beatBytes: Int = 4, trackCorruption: Boolean = true)(implicit p: Parameters) extends LazyModule
{
  val device = new MemoryDevice

  val node = TLManagerNode(Seq(TLManagerPortParameters.v1(
    Seq(TLManagerParameters.v1(
      address            = List(address),
      resources          = device.reg,
      regionType         = RegionType.UNCACHED,
      executable         = executable,
      supportsGet        = TransferSizes(1, beatBytes),
      supportsPutPartial = TransferSizes(1, beatBytes),
      supportsPutFull    = TransferSizes(1, beatBytes),
      fifoId             = Some(0))), // requests are handled in order
    beatBytes  = beatBytes)))

  // We require the address range to include an entire beat (for the write mask)
  require ((address.mask & (beatBytes-1)) == beatBytes-1)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    def bigBits(x: BigInt, tail: List[Boolean] = List.empty[Boolean]): List[Boolean] =
      if (x == 0) tail.reverse else bigBits(x >> 1, ((x & 1) == 1) :: tail)
    val mask = bigBits(address.mask >> log2Ceil(beatBytes))

    val (in, edge) = node.in(0)

    val addrBits = (mask zip edge.addr_hi(in.a.bits).asBools).filter(_._1).map(_._2)
    val memAddress = Cat(addrBits.reverse)
    val mem = Mem(1 << addrBits.size, Vec(beatBytes, Bits(8.W)))
    val bad = Mem(1 << addrBits.size, Bool())

    // "Flow control"
    in.a.ready := in.d.ready
    in.d.valid := in.a.valid

    val hasData = edge.hasData(in.a.bits)
    val wdata = VecInit(Seq.tabulate(beatBytes) {i => in.a.bits.data(8*(i+1)-1, 8*i)})

    in.d.bits := edge.AccessAck(in.a.bits)
    in.d.bits.data := Cat(mem(memAddress).reverse)
    in.d.bits.corrupt := !hasData && bad(memAddress) && trackCorruption.B
    in.d.bits.opcode := Mux(hasData, TLMessages.AccessAck, TLMessages.AccessAckData)
    when (in.a.fire && hasData) {
      mem.write(memAddress, wdata, in.a.bits.mask.asBools)
      bad.write(memAddress, in.a.bits.corrupt)
    }

    // Tie off unused channels
    in.b.valid := false.B
    in.c.ready := true.B
    in.e.ready := true.B
  }
}

// Synthesizable unit testing
import freechips.rocketchip.unittest._

class TLRAMZeroDelay(ramBeatBytes: Int, txns: Int)(implicit p: Parameters) extends LazyModule {
  val fuzz = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("ZeroDelay"))
  val ram  = LazyModule(new TLTestRAM(AddressSet(0x0, 0x3ff), beatBytes = ramBeatBytes))

  ram.node := TLDelayer(0.25) := model.node := fuzz.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class TLRAMZeroDelayTest(ramBeatBytes: Int, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new TLRAMZeroDelay(ramBeatBytes, txns)).module)
  io.finished := dut.io.finished
  dut.io.start := io.start
}
