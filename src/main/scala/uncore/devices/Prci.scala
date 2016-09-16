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

class PRCITileIO(implicit p: Parameters) extends Bundle {
  val reset = Bool(OUTPUT)
  val interrupts = new Bundle {
    val mtip = Bool()
    val msip = Bool()
  }

  override def cloneType: this.type = new PRCITileIO().asInstanceOf[this.type]
}

object PRCI {
  def msip(hart: Int) = hart * msipBytes
  def timecmp(hart: Int) = 0x4000 + hart * timecmpBytes
  def time = 0xbff8
  def msipBytes = 4
  def timecmpBytes = 8
  def size = 0xc000
}

case class PRCIConfig(beatBytes: Int, address: BigInt = 0x44000000)

trait MixPRCIParameters {
  val params: (PRCIConfig, Parameters)
  val c = params._1
  implicit val p = params._2
}

trait PRCIBundle extends Bundle with MixPRCIParameters {
  val tiles = Vec(p(NTiles), new PRCITileIO)
  val rtcTick = Bool(INPUT)
}

trait PRCIModule extends Module with HasRegMap with MixPRCIParameters {
  val io: PRCIBundle

  val timeWidth = 64
  val time = Reg(init=UInt(0, width = timeWidth))
  when (io.rtcTick) { time := time + UInt(1) }

  val timecmp = Seq.fill(p(NTiles)) { Reg(UInt(width = timeWidth)) }
  val ipi     = Seq.fill(p(NTiles)) { RegInit(UInt(0, width = 1)) }

  for ((tile, i) <- io.tiles zipWithIndex) {
    tile.interrupts.msip := ipi(i)(0)
    tile.interrupts.mtip := time >= timecmp(i)
    tile.reset := reset
  }

  def pad = RegField(8) // each use is a new field
  val ipi_fields = ipi.map(r => Seq(RegField(1, r), RegField(7), pad, pad, pad)).flatten
  val timecmp_fields = timecmp.map(RegField.bytes(_)).flatten
  val time_fields = Seq.fill(PRCI.time%c.beatBytes)(pad) ++ RegField.bytes(time)

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
  val timecmp_base = PRCI.timecmp(0) / c.beatBytes
  val time_base    = PRCI.time       / c.beatBytes

  regmap((
    RegField.split(ipi_fields,     ipi_base,     c.beatBytes) ++
    RegField.split(timecmp_fields, timecmp_base, c.beatBytes) ++
    RegField.split(time_fields,    time_base,    c.beatBytes)):_*)
}

/** Power, Reset, Clock, Interrupt */
// Magic TL2 Incantation to create a TL2 Slave
class PRCI(c: PRCIConfig)(implicit val p: Parameters)
  extends TLRegisterRouter(c.address, 0, 0x10000, None, c.beatBytes, false)(
  new TLRegBundle((c, p), _)    with PRCIBundle)(
  new TLRegModule((c, p), _, _) with PRCIModule)
