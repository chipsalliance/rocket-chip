// See LICENSE.SiFive for license details.

package uncore.tilelink2

import Chisel._
import config._
import diplomacy._
import util._

// Do not use this for synthesis! Only for simulation.
class TLTestRAM(address: AddressSet, executable: Boolean = true, beatBytes: Int = 4)(implicit p: Parameters) extends LazyModule
{
  val device = new MemoryDevice

  val node = TLManagerNode(Seq(TLManagerPortParameters(
    Seq(TLManagerParameters(
      address            = List(address),
      resources          = device.reg("mem"),
      regionType         = RegionType.UNCACHED,
      executable         = executable,
      supportsGet        = TransferSizes(1, beatBytes),
      supportsPutPartial = TransferSizes(1, beatBytes),
      supportsPutFull    = TransferSizes(1, beatBytes),
      fifoId             = Some(0))), // requests are handled in order
    beatBytes  = beatBytes)))

  // We require the address range to include an entire beat (for the write mask)
  require ((address.mask & (beatBytes-1)) == beatBytes-1)

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in = node.bundleIn
    }

    def bigBits(x: BigInt, tail: List[Boolean] = List.empty[Boolean]): List[Boolean] =
      if (x == 0) tail.reverse else bigBits(x >> 1, ((x & 1) == 1) :: tail)
    val mask = bigBits(address.mask >> log2Ceil(beatBytes))

    val in = io.in(0)
    val edge = node.edgesIn(0)

    val addrBits = (mask zip edge.addr_hi(in.a.bits).toBools).filter(_._1).map(_._2)
    val memAddress = Cat(addrBits.reverse)
    val mem = Mem(1 << addrBits.size, Vec(beatBytes, Bits(width = 8)))

    // "Flow control"
    in.a.ready := in.d.ready
    in.d.valid := in.a.valid

    val hasData = edge.hasData(in.a.bits)
    val wdata = Vec.tabulate(beatBytes) { i => in.a.bits.data(8*(i+1)-1, 8*i) }

    in.d.bits := edge.AccessAck(in.a.bits, UInt(0))
    in.d.bits.data := Cat(mem(memAddress).reverse)
    in.d.bits.opcode := Mux(hasData, TLMessages.AccessAck, TLMessages.AccessAckData)
    when (in.a.fire() && hasData) { mem.write(memAddress, wdata, in.a.bits.mask.toBools) }

    // Tie off unused channels
    in.b.valid := Bool(false)
    in.c.ready := Bool(true)
    in.e.ready := Bool(true)
  }
}

/** Synthesizeable unit testing */
import unittest._

class TLRAMZeroDelay(ramBeatBytes: Int, txns: Int)(implicit p: Parameters) extends LazyModule {
  val fuzz = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("ZeroDelay"))
  val ram  = LazyModule(new TLTestRAM(AddressSet(0x0, 0x3ff), beatBytes = ramBeatBytes))

  model.node := fuzz.node
  ram.node := TLDelayer(0.25)(model.node)

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := fuzz.module.io.finished
  }
}

class TLRAMZeroDelayTest(ramBeatBytes: Int, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  io.finished := Module(LazyModule(new TLRAMZeroDelay(ramBeatBytes, txns)).module).io.finished
}
