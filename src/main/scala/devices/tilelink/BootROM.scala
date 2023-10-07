// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import chisel3._
import chisel3.util.log2Ceil
import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.subsystem.{BaseSubsystem, HierarchicalLocation, HasTiles, TLBusWrapperLocation}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.prci.{ClockSinkDomain}

import java.nio.ByteBuffer
import java.nio.file.{Files, Paths}

/** Size, location and contents of the boot rom. */
case class BootROMParams(
  address: BigInt = 0x10000,
  size: Int = 0x10000,
  hang: BigInt = 0x10040, // The hang parameter is used as the power-on reset vector
  contentFileName: String)

class TLROM(val base: BigInt, val size: Int, contentsDelayed: => Seq[Byte], executable: Boolean = true, beatBytes: Int = 4,
  resources: Seq[Resource] = new SimpleDevice("rom", Seq("sifive,rom0")).reg("mem"))(implicit p: Parameters) extends LazyModule
{
  val node = TLManagerNode(Seq(TLSlavePortParameters.v1(
    Seq(TLSlaveParameters.v1(
      address     = List(AddressSet(base, size-1)),
      resources   = resources,
      regionType  = RegionType.UNCACHED,
      executable  = executable,
      supportsGet = TransferSizes(1, beatBytes),
      fifoId      = Some(0))),
    beatBytes = beatBytes)))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val contents = contentsDelayed
    val wrapSize = 1 << log2Ceil(contents.size)
    require (wrapSize <= size)

    val (in, edge) = node.in(0)

    val words = (contents ++ Seq.fill(wrapSize-contents.size)(0.toByte)).grouped(beatBytes).toSeq
    val bigs = words.map(_.foldRight(BigInt(0)){ case (x,y) => (x.toInt & 0xff) | y << 8})
    val rom = VecInit(bigs.map(_.U((8*beatBytes).W)))

    in.d.valid := in.a.valid
    in.a.ready := in.d.ready

    val index = in.a.bits.address(log2Ceil(wrapSize)-1,log2Ceil(beatBytes))
    val high = if (wrapSize == size) 0.U else in.a.bits.address(log2Ceil(size)-1, log2Ceil(wrapSize))
    in.d.bits := edge.AccessAck(in.a.bits, Mux(high.orR, 0.U, rom(index)))

    // Tie off unused channels
    in.b.valid := false.B
    in.c.ready := true.B
    in.e.ready := true.B
  }
}

case class BootROMLocated(loc: HierarchicalLocation) extends Field[Option[BootROMParams]](None)

object BootROM {
  /** BootROM.attach not only instantiates a TLROM and attaches it to the tilelink interconnect
    *    at a configurable location, but also drives the tiles' reset vectors to point
    *    at its 'hang' address parameter value.
    */
  def attach(params: BootROMParams, subsystem: BaseSubsystem with HasTiles, where: TLBusWrapperLocation)
            (implicit p: Parameters): TLROM = {
    val tlbus = subsystem.locateTLBusWrapper(where)
    val bootROMDomainWrapper = LazyModule(new ClockSinkDomain(take = None))
    bootROMDomainWrapper.clockNode := tlbus.fixedClockNode

    val bootROMResetVectorSourceNode = BundleBridgeSource[UInt]()
    lazy val contents = {
      val romdata = Files.readAllBytes(Paths.get(params.contentFileName))
      val rom = ByteBuffer.wrap(romdata)
      rom.array() ++ subsystem.dtb.contents
    }

    val bootrom = bootROMDomainWrapper {
      LazyModule(new TLROM(params.address, params.size, contents, true, tlbus.beatBytes))
    }

    bootrom.node := tlbus.coupleTo("bootrom"){ TLFragmenter(tlbus) := _ }
    // Drive the `subsystem` reset vector to the `hang` address of this Boot ROM.
    subsystem.tileResetVectorNexusNode := bootROMResetVectorSourceNode
    InModuleBody {
      val reset_vector_source = bootROMResetVectorSourceNode.bundle
      require(reset_vector_source.getWidth >= params.hang.bitLength,
        s"BootROM defined with a reset vector (${params.hang})too large for physical address space (${reset_vector_source.getWidth})")
      bootROMResetVectorSourceNode.bundle := params.hang.U
    }
    bootrom
  }
}
