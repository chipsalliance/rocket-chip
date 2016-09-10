// See LICENSE for license details.
package uncore.tilelink2

import Chisel._
import chisel3.util.LFSR16
import junctions.unittests._

class TLClient extends LazyModule
{
  val node = TLClientNode(TLClientParameters())

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val out = node.bundleOut
      val finished = Bool()
    }

    require (node.edgesOut(0).manager.beatBytes == 16)

    val addr = RegInit(UInt(0, width = 13))
    val size = RegInit(UInt(0, width = 4))
    val put = RegInit(Bool(false))
    val width = 12
    val count = RegInit(UInt(0, width = width))
    val limit = ~(SInt(-1, width=width).asUInt << size)(width-1, 0) >> 4

    val out = io.out(0)
    val edge = node.edgesOut(0)
    val data = Cat(Seq.tabulate(16) { i => UInt(i) | (count(3,0) + UInt(1)) << 4 } .reverse)

    val (legalg, gbits) = edge.Get(UInt(0), addr, size)
    val (legalp, pbits) = edge.Put(UInt(0), addr, size, data)
    val legal = Mux(put, legalp, legalg)
    val bits = Mux(put, pbits, gbits)

    out.a.valid := legal
    out.a.bits  := bits
    out.b.ready := Bool(true)
    out.c.valid := Bool(false)
    out.d.ready := Bool(true)
    out.e.valid := Bool(false)
    io.finished := Bool(true)//count === limit

    when (out.a.fire()) { count := count + UInt(1) }
    when (!legal || (out.a.fire() && Mux(put, count === limit, Bool(true)))) {
      count := UInt(0)
      size  := size + UInt(1)
      put   := LFSR16()(0)
      addr  := addr + UInt(0x100)
      when (size === UInt(8)) { size := UInt(0) }
    }
  }
}

class Bar extends LazyModule
{
  val node = TLOutputNode()

  val client = LazyModule(new TLClient)
  val xbar = LazyModule(new TLXbar)

  connect(client.node -> xbar.node)
  connect(xbar.node -> node)

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val out = node.bundleOut
      val finished = Bool()
    }
    io.finished := client.module.io.finished
  }
}

class TLXbarGPIORAM extends LazyModule
{
  val ram  = LazyModule(new TLRAM(AddressSet(0, 0xfff)))
  val xbar = LazyModule(new TLXbar)
  val bar = LazyModule(new Bar)

  connect(TLWidthWidget(TLHintHandler(bar.node), 16) -> xbar.node)
  connect(TLFragmenter(TLBuffer(xbar.node), 4, 256) -> ram.node)

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := bar.module.io.finished
  }
}

class TLXbarGPIORAMTest extends UnitTest {
  val dut = LazyModule(new TLXbarGPIORAM).module
  io.finished := dut.io.finished
}

