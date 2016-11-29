// See LICENSE.SiFive for license details.

package uncore.devices

import Chisel._
import junctions._
import junctions.NastiConstants._
import regmapper._
import diplomacy._
import uncore.tilelink2._
import uncore.util._
import util._
import scala.math.{min,max}
import config._

/** Number of tiles */
case object NTiles extends Field[Int]

class CoreplexLocalInterrupts extends Bundle {
  val mtip = Bool()
  val msip = Bool()
}

object ClintConsts
{
  def msipOffset(hart: Int) = hart * msipBytes
  def timecmpOffset(hart: Int) = 0x4000 + hart * timecmpBytes
  def timeOffset = 0xbff8
  def msipBytes = 4
  def timecmpBytes = 8
  def size = 0x10000
}

trait MixCoreplexLocalInterrupterParameters {
  val params: Parameters
  implicit val p = params
}

trait CoreplexLocalInterrupterBundle extends Bundle with MixCoreplexLocalInterrupterParameters {
  val tiles = Vec(p(NTiles), new CoreplexLocalInterrupts).asOutput
  val rtcTick = Bool(INPUT)
}

trait CoreplexLocalInterrupterModule extends Module with HasRegMap with MixCoreplexLocalInterrupterParameters {
  val io: CoreplexLocalInterrupterBundle
  val address: AddressSet

  val timeWidth = 64
  val regWidth = 32

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
    0                            -> makeRegFields(ipi),
    ClintConsts.timecmpOffset(0) -> makeRegFields(timecmp.flatten),
    ClintConsts.timeOffset       -> makeRegFields(time))

  def makeRegFields(s: Seq[UInt]) = s.map(r => RegField(regWidth, r))
}

/** Power, Reset, Clock, Interrupt */
// Magic TL2 Incantation to create a TL2 Slave
class CoreplexLocalInterrupter(address: BigInt = 0x02000000)(implicit val p: Parameters)
  extends TLRegisterRouter(address, size = ClintConsts.size, beatBytes = p(rocket.XLen)/8, undefZero = false)(
  new TLRegBundle(p, _)    with CoreplexLocalInterrupterBundle)(
  new TLRegModule(p, _, _) with CoreplexLocalInterrupterModule)
{
  val globalConfigString = Seq(
    s"rtc {\n",
    s"  addr 0x${(address + ClintConsts.timeOffset).toString(16)};\n",
    s"};\n").mkString
  val hartConfigStrings = (0 until p(NTiles)).map { i => Seq(
    s"      timecmp 0x${(address + ClintConsts.timecmpOffset(i)).toString(16)};\n",
    s"      ipi 0x${(address + ClintConsts.msipOffset(i)).toString(16)};\n").mkString
  }
}
