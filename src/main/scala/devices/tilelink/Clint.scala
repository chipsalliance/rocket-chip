// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.coreplex.HasPeripheryBus
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tile.XLen
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
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
  def ipiWidth = 32
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
    sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(ints, Seq(Resource(device, "int"))))) },
    sinkFn   = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
    outputRequiresInput = false)

  lazy val module = new LazyModuleImp(this) {
    require (intnode.edges.in.size == 0, "CLINT only produces interrupts; it does not accept them")

    val io = IO(new Bundle {
      val rtcTick = Bool(INPUT)
    })

    val time = RegInit(UInt(0, width = timeWidth))
    when (io.rtcTick) { time := time + UInt(1) }

    val nTiles = intnode.out.size
    val timecmp = Seq.fill(nTiles) { Reg(UInt(width = timeWidth)) }
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

    node.regmap(
      0                -> ipi.map(r => RegField(ipiWidth, r)),
      timecmpOffset(0) -> timecmp.flatMap(RegField.bytes(_)),
      timeOffset       -> RegField.bytes(time))
  }
}

/** Trait that will connect a Clint to a coreplex */
trait HasPeripheryClint extends HasPeripheryBus {
  val clint = LazyModule(new CoreplexLocalInterrupter(p(ClintKey)))
  clint.node := pbus.toVariableWidthSlaves
}
