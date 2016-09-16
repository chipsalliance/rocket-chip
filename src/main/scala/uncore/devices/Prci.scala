// See LICENSE for license details.

package uncore.devices

import Chisel._
import rocket.Util._
import junctions._
import junctions.NastiConstants._
import uncore.tilelink2._
import uncore.util._
import scala.math.{min,max}
import cde.{Parameters, Field}

/** Number of tiles */
case object NTiles extends Field[Int]

class CoreplexLocalInterrupts extends Bundle {
  val mtip = Bool()
  val msip = Bool()
}

case class CoreplexLocalInterrupterConfig(beatBytes: Int, address: BigInt = 0x44000000) {
  def msipOffset(hart: Int) = hart * msipBytes
  def msipAddress(hart: Int) = address + msipOffset(hart)
  def timecmpOffset(hart: Int) = 0x4000 + hart * timecmpBytes
  def timecmpAddress(hart: Int) = address + timecmpOffset(hart)
  def timeOffset = 0xbff8
  def timeAddress = address + timeOffset
  def msipBytes = 4
  def timecmpBytes = 8
  def size = 0x10000
}

trait MixCoreplexLocalInterrupterParameters {
  val params: (CoreplexLocalInterrupterConfig, Parameters)
  val c = params._1
  implicit val p = params._2
}

trait CoreplexLocalInterrupterBundle extends Bundle with MixCoreplexLocalInterrupterParameters {
  val tiles = Vec(p(NTiles), new CoreplexLocalInterrupts).asOutput
  val rtcTick = Bool(INPUT)
}

trait CoreplexLocalInterrupterModule extends Module with HasRegMap with MixCoreplexLocalInterrupterParameters {
  val io: CoreplexLocalInterrupterBundle

  val timeWidth = 64
  val time = Reg(init=UInt(0, width = timeWidth))
  when (io.rtcTick) { time := time + UInt(1) }

  val timecmp = Seq.fill(p(NTiles)) { Reg(UInt(width = timeWidth)) }
  val ipi     = Seq.fill(p(NTiles)) { RegInit(UInt(0, width = 1)) }

  for ((tile, i) <- io.tiles zipWithIndex) {
    tile.msip := ipi(i)(0)
    tile.mtip := time >= timecmp(i)
  }

  def pad = RegField(8) // each use is a new field
  val ipi_fields = ipi.map(r => Seq(RegField(1, r), RegField(7), pad, pad, pad)).flatten
  val timecmp_fields = timecmp.map(RegField.bytes(_)).flatten
  val time_fields = Seq.fill(c.timeOffset % c.beatBytes)(pad) ++ RegField.bytes(time)

  /* 0000 msip hart 0
   * 0004 msip hart 1
   * 4000 mtimecmp hart 0 lo
   * 4004 mtimecmp hart 0 hi
   * 4008 mtimecmp hart 1 lo
   * 400c mtimecmp hart 1 hi
   * bff8 mtime lo
   * bffc mtime hi
   */
  val ipi_base     = 0
  val timecmp_base = c.timecmpOffset(0) / c.beatBytes
  val time_base    = c.timeOffset / c.beatBytes

  regmap((
    RegField.split(ipi_fields,     ipi_base,     c.beatBytes) ++
    RegField.split(timecmp_fields, timecmp_base, c.beatBytes) ++
    RegField.split(time_fields,    time_base,    c.beatBytes)):_*)
}

/** Power, Reset, Clock, Interrupt */
// Magic TL2 Incantation to create a TL2 Slave
class CoreplexLocalInterrupter(c: CoreplexLocalInterrupterConfig)(implicit val p: Parameters)
  extends TLRegisterRouter(c.address, 0, c.size, None, c.beatBytes, false)(
  new TLRegBundle((c, p), _)    with CoreplexLocalInterrupterBundle)(
  new TLRegModule((c, p), _, _) with CoreplexLocalInterrupterModule)
