// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.diplomacy.{RegionType, AddressSet, TransferSizes}
import freechips.rocketchip.resources.{SimpleDevice}
import freechips.rocketchip.subsystem.{Attachable, HierarchicalLocation, TLBusWrapperLocation}
import freechips.rocketchip.tilelink.{TLFragmenter, TLManagerNode, TLManagerParameters, TLManagerPortParameters, TLWidthWidget}
import freechips.rocketchip.util.{ROMConfig, ROMGenerator}

import freechips.rocketchip.util.DataToAugmentedData

case class MaskROMParams(address: BigInt, name: String, depth: Int = 2048, width: Int = 32)

class TLMaskROM(c: MaskROMParams)(implicit p: Parameters) extends LazyModule {
  val beatBytes = c.width/8
  val node = TLManagerNode(Seq(TLManagerPortParameters.v1(
    Seq(TLManagerParameters.v1(
      address            = AddressSet.misaligned(c.address, c.depth*beatBytes),
      resources          = new SimpleDevice("rom", Seq("sifive,maskrom0")).reg("mem"),
      regionType         = RegionType.UNCACHED,
      executable         = true,
      supportsGet        = TransferSizes(1, beatBytes),
      fifoId             = Some(0))), // requests are handled in order
    beatBytes = beatBytes)))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, edge)= node.in(0)

    val rom = ROMGenerator(ROMConfig(c.name, c.depth, c.width))
    rom.io.clock := clock
    rom.io.address := edge.addr_hi(in.a.bits.address - c.address.U)(log2Ceil(c.depth)-1, 0)
    rom.io.oe := true.B // active high tri state enable
    rom.io.me := in.a.fire

    val d_full = RegInit(false.B)
    val d_size = Reg(UInt())
    val d_source = Reg(UInt())
    val d_data = rom.io.q holdUnless RegNext(in.a.fire)

    // Flow control
    when (in.d.fire) { d_full := false.B }
    when (in.a.fire) { d_full := true.B  }
    in.d.valid := d_full
    in.a.ready := in.d.ready || !d_full

    when (in.a.fire) {
      d_size   := in.a.bits.size
      d_source := in.a.bits.source
    }

    in.d.bits := edge.AccessAck(d_source, d_size, d_data)

    // Tie off unused channels
    in.b.valid := false.B
    in.c.ready := true.B
    in.e.ready := true.B
  }
}

case class MaskROMLocated(loc: HierarchicalLocation) extends Field[Seq[MaskROMParams]](Nil)

object MaskROM {
  def attach(params: MaskROMParams, subsystem: Attachable, where: TLBusWrapperLocation)
            (implicit p: Parameters): TLMaskROM = {
    val bus = subsystem.locateTLBusWrapper(where)
    val maskROM = LazyModule(new TLMaskROM(params))
    maskROM.node := bus.coupleTo("MaskROM") {
      TLFragmenter(maskROM.beatBytes, bus.blockBytes) :*= TLWidthWidget(bus) := _
    }
    InModuleBody { maskROM.module.clock := bus.module.clock }
    InModuleBody { maskROM.module.reset := bus.module.reset }
    maskROM
  }
}
