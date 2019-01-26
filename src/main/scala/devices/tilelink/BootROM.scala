// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.subsystem.{BaseSubsystem, HasResetVectorWire}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Paths}

/** Size, location and contents of the boot rom. */
case class BootROMParams(
  address: BigInt = 0x10000,
  size: Int = 0x10000,
  hang: BigInt = 0x10040,
  contentFileName: String)
case object BootROMParams extends Field[BootROMParams]

class TLROM(val base: BigInt, val size: Int, contentsDelayed: => Seq[Byte], executable: Boolean = true, beatBytes: Int = 4,
  resources: Seq[Resource] = new SimpleDevice("rom", Seq("sifive,rom0")).reg("mem"))(implicit p: Parameters) extends LazyModule
{
  val node = TLManagerNode(Seq(TLManagerPortParameters(
    Seq(TLManagerParameters(
      address     = List(AddressSet(base, size-1)),
      resources   = resources,
      regionType  = RegionType.UNCACHED,
      executable  = executable,
      supportsGet = TransferSizes(1, beatBytes),
      fifoId      = Some(0))),
    beatBytes = beatBytes)))

  lazy val module = new LazyModuleImp(this) {
    val contents = contentsDelayed
    val wrapSize = 1 << log2Ceil(contents.size)
    require (wrapSize <= size)

    val (in, edge) = node.in(0)

    val words = (contents ++ Seq.fill(wrapSize-contents.size)(0.toByte)).grouped(beatBytes).toSeq
    val bigs = words.map(_.foldRight(BigInt(0)){ case (x,y) => (x.toInt & 0xff) | y << 8})
    val rom = Vec(bigs.map(x => UInt(x, width = 8*beatBytes)))

    in.d.valid := in.a.valid
    in.a.ready := in.d.ready

    val index = in.a.bits.address(log2Ceil(wrapSize)-1,log2Ceil(beatBytes))
    val high = if (wrapSize == size) UInt(0) else in.a.bits.address(log2Ceil(size)-1, log2Ceil(wrapSize))
    in.d.bits := edge.AccessAck(in.a.bits, Mux(high.orR, UInt(0), rom(index)))

    // Tie off unused channels
    in.b.valid := Bool(false)
    in.c.ready := Bool(true)
    in.e.ready := Bool(true)
  }
}

/** Adds a boot ROM that contains the DTB describing the system's subsystem. */
trait HasPeripheryBootROM { this: BaseSubsystem =>
  val dtb: DTB
  private val params = p(BootROMParams)
  private lazy val contents = {
    val romdata = Files.readAllBytes(Paths.get(params.contentFileName))
    val rom = ByteBuffer.wrap(romdata)
    rom.array() ++ dtb.contents
  }
  def resetVector: BigInt = params.hang

  val bootrom = LazyModule(new TLROM(params.address, params.size, contents, true, cbus.beatBytes))

  bootrom.node := cbus.coupleTo("bootrom"){ TLFragmenter(cbus) := _ }
}

/** Subsystem will power-on running at 0x10040 (BootROM) */
trait HasPeripheryBootROMModuleImp extends LazyModuleImp
    with HasResetVectorWire {
  val outer: HasPeripheryBootROM
  global_reset_vector := outer.resetVector.U
}
