// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class MaskROMParams(address: BigInt, name: String, depth: Int = 2048, width: Int = 32)

class TLMaskROM(c: MaskROMParams)(implicit p: Parameters) extends LazyModule {
  val beatBytes = c.width/8
  val node = TLManagerNode(Seq(TLManagerPortParameters(
    Seq(TLManagerParameters(
      address            = AddressSet.misaligned(c.address, c.depth*beatBytes),
      resources          = new SimpleDevice("rom", Seq("sifive,maskrom0")).reg("mem"),
      regionType         = RegionType.UNCACHED,
      executable         = true,
      supportsGet        = TransferSizes(1, beatBytes),
      fifoId             = Some(0))), // requests are handled in order
    beatBytes = beatBytes)))

  lazy val module = new LazyModuleImp(this) {
    val (in, edge)= node.in(0)

    val rom = ROMGenerator(ROMConfig(c.name, c.depth, c.width))
    rom.io.clock := clock
    rom.io.address := edge.addr_hi(in.a.bits.address - UInt(c.address))(log2Ceil(c.depth)-1, 0)
    rom.io.oe := Bool(true) // active high tri state enable
    rom.io.me := in.a.fire()

    val d_full = RegInit(Bool(false))
    val d_size = Reg(UInt())
    val d_source = Reg(UInt())
    val d_data = rom.io.q holdUnless RegNext(in.a.fire())

    // Flow control
    when (in.d.fire()) { d_full := Bool(false) }
    when (in.a.fire()) { d_full := Bool(true)  }
    in.d.valid := d_full
    in.a.ready := in.d.ready || !d_full

    when (in.a.fire()) {
      d_size   := in.a.bits.size
      d_source := in.a.bits.source
    }

    in.d.bits := edge.AccessAck(d_source, d_size, d_data)

    // Tie off unused channels
    in.b.valid := Bool(false)
    in.c.ready := Bool(true)
    in.e.ready := Bool(true)
  }
}

object MaskROM {
  def attach(params: MaskROMParams, bus: TLBusWrapper)(implicit p: Parameters): TLMaskROM = {
    val maskROM = LazyModule(new TLMaskROM(params))
    maskROM.node := bus.coupleTo("MaskROM") {
      TLFragmenter(maskROM.beatBytes, bus.blockBytes) :*= TLWidthWidget(bus) := _
    }
    InModuleBody { maskROM.module.clock := bus.module.clock }
    InModuleBody { maskROM.module.reset := bus.module.reset }
    maskROM
  }
}
