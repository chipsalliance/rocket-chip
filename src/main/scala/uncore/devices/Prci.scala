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
import tile.XLen

/** Number of tiles */
case object NTiles extends Field[Int]

object ClintConsts
{
  def msipOffset(hart: Int) = hart * msipBytes
  def timecmpOffset(hart: Int) = 0x4000 + hart * timecmpBytes
  def timeOffset = 0xbff8
  def msipBytes = 4
  def timecmpBytes = 8
  def size = 0x10000
  def timeWidth = 64
  def regWidth = 32
  def ints = 2
}

class CoreplexLocalInterrupter(address: BigInt = 0x02000000)(implicit p: Parameters) extends LazyModule
{
  import ClintConsts._

  // clint0 => at most 4095 devices
  val device = new SimpleDevice("clint", Seq("riscv,clint0")) {
    override val alwaysExtended = true
  }

  val node = TLRegisterNode(
    address   = AddressSet(address, size-1),
    device    = device,
    beatBytes = p(XLen)/8)

  val intnode = IntNexusNode(
    numSourcePorts = 0 to 1024,
    numSinkPorts   = 0 to 0,
    sourceFn       = { _ => IntSourcePortParameters(Seq(IntSourceParameters(ints, Seq(Resource(device, "int"))))) },
    sinkFn         = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) })

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val rtcTick = Bool(INPUT)
      val int = intnode.bundleOut
      val in = node.bundleIn
    }

    val time = Seq.fill(timeWidth/regWidth)(Reg(init=UInt(0, width = regWidth)))
    when (io.rtcTick) {
      val newTime = time.asUInt + UInt(1)
      for ((reg, i) <- time zip (0 until timeWidth by regWidth))
        reg := newTime >> i
    }

    val timecmp = Seq.fill(p(NTiles)) { Seq.fill(timeWidth/regWidth)(Reg(UInt(width = regWidth))) }
    val ipi = Seq.fill(p(NTiles)) { RegInit(UInt(0, width = 1)) }

    io.int.zipWithIndex.foreach { case (int, i) =>
      int(0) := ipi(i)(0) // msip
      int(1) := time.asUInt >= timecmp(i).asUInt // mtip
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

    def makeRegFields(s: Seq[UInt]) = s.map(r => RegField(regWidth, r))

    node.regmap(
      0                -> makeRegFields(ipi),
      timecmpOffset(0) -> makeRegFields(timecmp.flatten),
      timeOffset       -> makeRegFields(time))
  }
}
