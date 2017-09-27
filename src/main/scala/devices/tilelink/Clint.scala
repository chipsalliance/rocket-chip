// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.coreplex.HasPeripheryBus
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tile.XLen
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import scala.math.{min,max}

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

case class ClintParams(baseAddress: BigInt = 0x02000000, intStages: Int = 0)
{
  def address = AddressSet(baseAddress, ClintConsts.size-1)
}

case object ClintKey extends Field(ClintParams())

class CoreplexLocalInterrupter(params: ClintParams)(implicit p: Parameters) extends LazyModule
{
  import ClintConsts._

  // clint0 => at most 4095 devices
  val device = new SimpleDevice("clint", Seq("riscv,clint0")) {
    override val alwaysExtended = true
  }

  val node = TLRegisterNode(
    address   = Seq(params.address),
    device    = device,
    beatBytes = p(XLen)/8)

  val intnode = IntNexusNode(
    numSourcePorts = 0 to 1024,
    numSinkPorts   = 0 to 0,
    sourceFn       = { _ => IntSourcePortParameters(Seq(IntSourceParameters(ints, Seq(Resource(device, "int"))))) },
    sinkFn         = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) })

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val rtcTick = Bool(INPUT)
    })

    val time = Seq.fill(timeWidth/regWidth)(Reg(init=UInt(0, width = regWidth)))
    when (io.rtcTick) {
      val newTime = time.asUInt + UInt(1)
      for ((reg, i) <- time zip (0 until timeWidth by regWidth))
        reg := newTime >> i
    }

    val nTiles = intnode.out.size
    val timecmp = Seq.fill(nTiles) { Seq.fill(timeWidth/regWidth)(Reg(UInt(width = regWidth))) }
    val ipi = Seq.fill(nTiles) { RegInit(UInt(0, width = 1)) }

    val (intnode_out, _) = intnode.out.unzip
    intnode_out.zipWithIndex.foreach { case (int, i) =>
      int(0) := ShiftRegister(ipi(i)(0), params.intStages) // msip
      int(1) := ShiftRegister(time.asUInt >= timecmp(i).asUInt, params.intStages) // mtip
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

/** Trait that will connect a Clint to a coreplex */
trait HasPeripheryClint extends HasPeripheryBus {
  val clint = LazyModule(new CoreplexLocalInterrupter(p(ClintKey)))
  clint.node := pbus.toVariableWidthSlaves
}
