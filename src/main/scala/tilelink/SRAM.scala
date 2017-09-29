// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import chisel3.experimental.chiselName
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class TLRAM(
    address: AddressSet,
    executable: Boolean = true,
    beatBytes: Int = 4,
    devName: Option[String] = None,
    errors: Seq[AddressSet] = Nil)
  (implicit p: Parameters) extends DiplomaticSRAM(address, beatBytes, devName)
{
  val node = TLManagerNode(Seq(TLManagerPortParameters(
    Seq(TLManagerParameters(
      address            = List(address) ++ errors,
      resources          = resources,
      regionType         = RegionType.UNCACHED,
      executable         = executable,
      supportsGet        = TransferSizes(1, beatBytes),
      supportsPutPartial = TransferSizes(1, beatBytes),
      supportsPutFull    = TransferSizes(1, beatBytes),
      fifoId             = Some(0))), // requests are handled in order
    beatBytes  = beatBytes,
    minLatency = 1))) // no bypass needed for this device

  lazy val module = new LazyModuleImp(this) {
    val (in, edge) = node.in(0)

    val addrBits = (mask zip edge.addr_hi(in.a.bits).toBools).filter(_._1).map(_._2)
    val a_legal = address.contains(in.a.bits.address)
    val memAddress = Cat(addrBits.reverse)
    val mem = makeSinglePortedByteWriteSeqMem(1 << addrBits.size)

    val d_full = RegInit(Bool(false))
    val d_read = Reg(Bool())
    val d_size = Reg(UInt())
    val d_source = Reg(UInt())
    val d_data = Wire(UInt())
    val d_legal = Reg(Bool())

    // Flow control
    when (in.d.fire()) { d_full := Bool(false) }
    when (in.a.fire()) { d_full := Bool(true)  }
    in.d.valid := d_full
    in.a.ready := in.d.ready || !d_full

    in.d.bits := edge.AccessAck(d_source, d_size, !d_legal)
    // avoid data-bus Mux
    in.d.bits.data := d_data
    in.d.bits.opcode := Mux(d_read, TLMessages.AccessAckData, TLMessages.AccessAck)

    val read = in.a.bits.opcode === TLMessages.Get
    val rdata = Wire(Vec(beatBytes, Bits(width = 8)))
    val wdata = Vec.tabulate(beatBytes) { i => in.a.bits.data(8*(i+1)-1, 8*i) }
    d_data := Cat(rdata.reverse)
    when (in.a.fire()) {
      d_read   := read
      d_size   := in.a.bits.size
      d_source := in.a.bits.source
      d_legal  := a_legal
    }

    // exactly this pattern is required to get a RWM memory
    when (in.a.fire() && !read && a_legal) {
      mem.write(memAddress, wdata, in.a.bits.mask.toBools)
    }
    val ren = in.a.fire() && read
    rdata := mem.readAndHold(memAddress, ren)

    // Tie off unused channels
    in.b.valid := Bool(false)
    in.c.ready := Bool(true)
    in.e.ready := Bool(true)
  }
}

/** Synthesizeable unit testing */
import freechips.rocketchip.unittest._

class TLRAMSimple(ramBeatBytes: Int, txns: Int)(implicit p: Parameters) extends LazyModule {
  val fuzz = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("SRAMSimple"))
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x3ff), beatBytes = ramBeatBytes))

  model.node := fuzz.node
  ram.node := TLDelayer(0.25)(model.node)

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class TLRAMSimpleTest(ramBeatBytes: Int, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  io.finished := Module(LazyModule(new TLRAMSimple(ramBeatBytes, txns)).module).io.finished
}
