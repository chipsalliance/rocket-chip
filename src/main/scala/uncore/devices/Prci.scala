// See LICENSE for license details.

package uncore.devices

import Chisel._
import junctions._
import junctions.NastiConstants._
import regmapper._
import uncore.tilelink2._
import uncore.util._
import util._
import scala.math.{min,max}
import cde.{Parameters, Field}

/** Number of tiles */
case object NTiles extends Field[Int]

class CoreplexLocalInterrupts extends Bundle {
  val mtip = Bool()
  val msip = Bool()
}

case class CoreplexLocalInterrupterConfig(beatBytes: Int, address: BigInt = 0x02000000) {
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
  val regWidth = 32
  // demand atomic accesses for RV64
  require(c.beatBytes >= (p(rocket.XLen) min timeWidth)/8)

  val time = Seq.fill(timeWidth/regWidth)(Reg(init=UInt(0, width = regWidth)))
  when (io.rtcTick) {
    val newTime = time.asUInt + UInt(1)
    for ((reg, i) <- time zip (0 until timeWidth by regWidth))
      reg := newTime >> i
  }

  val timecmp = Seq.fill(p(NTiles)) { Seq.fill(timeWidth/regWidth)(Reg(UInt(width = regWidth))) }
  val ipi = Seq.fill(p(NTiles)) { RegInit(UInt(0, width = 1)) }

  for ((tile, i) <- io.tiles zipWithIndex) {
    tile.msip := ipi(i)(0)
    tile.mtip := time.asUInt >= timecmp(i).asUInt
  }

  /* 0000 msip hart 0
   * 0004 msip hart 1
   * 4000 mtimecmp hart 0 lo
   * 4004 mtimecmp hart 0 hi
   * 4008 mtimecmp hart 1 lo
   * 400c mtimecmp hart 1 hi
   * bff8 mtime lo
   * bffc mtime hi
   */

  regmap(
    0                  -> makeRegFields(ipi),
    c.timecmpOffset(0) -> makeRegFields(timecmp.flatten),
    c.timeOffset       -> makeRegFields(time))

  def makeRegFields(s: Seq[UInt]) = s.map(r => RegField(regWidth, r))
}

/** Power, Reset, Clock, Interrupt */
// Magic TL2 Incantation to create a TL2 Slave
class CoreplexLocalInterrupter(c: CoreplexLocalInterrupterConfig)(implicit val p: Parameters)
  extends TLRegisterRouter(c.address, 0, c.size, 0, c.beatBytes, false)(
  new TLRegBundle((c, p), _)    with CoreplexLocalInterrupterBundle)(
  new TLRegModule((c, p), _, _) with CoreplexLocalInterrupterModule)
