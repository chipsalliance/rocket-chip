// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import chisel3._
import chisel3.util.ShiftRegister
import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

object MTIMERConsts
{
  def mtimecmpOffset(hart: Int) = hart * mtimecmpBytes
  def mtimecmpBytes       = 8
  def mtimeWidth          = 64
  def mtimecmpSize        = 0x7ff8
  def mtimecmpSizeAligned = 0x8000
  def mtimeSize           = 0x8
  def mtimeSizeAligned    = 0x1000
  def ints                = 1
}

// Notice: Remember to ensure that the size and address meet the requirements of the AddressSet, if you use ACLINT 
case class MTIMERParams(mtimecmpBaseAddress: BigInt = 0x02008000, mtimeBaseAddress: BigInt = 0x02010000, intStages: Int = 0)
{
  def mtimecmpAddress = AddressSet(mtimecmpBaseAddress, MTIMERConsts.mtimecmpSizeAligned - 1)

  def mtimeAddress = AddressSet(mtimeBaseAddress, MTIMERConsts.mtimeSizeAligned - 1)
}

case class MTIMERAttachParams(
  slaveWhere: TLBusWrapperLocation = CBUS
)

case object MTIMERAttachKey extends Field(MTIMERAttachParams())

class MTIMER(params: MTIMERParams, beatBytes: Int)(implicit p: Parameters) extends LazyModule
{
  import MTIMERConsts._ 

  val device: SimpleDevice = new SimpleDevice("mtimer", Seq("riscv,aclint-mtimer")) {
    override val alwaysExtended = true
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      val mtimeReg = ResourceAddress(Seq(params.mtimeAddress), ResourcePermissions(false, false, false, false, false))
      val mtimecmpReg = ResourceAddress(Seq(params.mtimecmpAddress), ResourcePermissions(false, false, false, false, false))
      // The default `reg' in dts will merge both AddressSets
      // and the software can not tell where is mtime/mtimecmp
      val extra = Map(
        // override `reg'
        // aclint dt-binding defines the order of mtime and mtimecmp
        "reg" -> Seq(mtimeReg, mtimecmpReg)
      )
      Description(name, mapping ++ extra)
    }
  }

  val mtimecmpNode: TLRegisterNode = TLRegisterNode(
    address     = Seq(params.mtimecmpAddress),
    device      = device,
    beatBytes   = beatBytes
  )

  val mtimeNode: TLRegisterNode = TLRegisterNode(
    address     = Seq(params.mtimeAddress),
    device      = device,
    beatBytes   = beatBytes
  )

  val intnode: IntNexusNode = IntNexusNode(
    sourceFn       = { _ => IntSourcePortParameters(Seq(IntSourceParameters(ints, Seq(Resource(device, "int")))))},
    sinkFn         = { _ => IntSinkPortParameters(Seq(IntSinkParameters()))},
    outputRequiresInput = false
  )

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    Annotated.params(this, params)
    require (intnode.edges.in.size == 0, "MTIMER Device only produces interrupts; it does not accept them")

    val io = IO(new Bundle{
      val rtcTick = Input(Bool())
    })

    val (timecmpRegGroup, timeRegGroup): (Seq[RegField], Seq[RegField]) = MTIMER(io.rtcTick, intnode, 0, params.intStages)

    /* 
     * Two base addresses:
     * 0 mtimecmp hart 0 lo
     * 4 mtimecmp hart 0 hi
     * 8 mtimecmp hart 1 lo
     * c mtimecmp hart 1 hi
     *
     * 0 mtime lo
     * 4 mtime hi
     */
    mtimecmpNode.regmap(0 -> timecmpRegGroup)
    mtimeNode.regmap(0 -> timeRegGroup)
  }
}

object MTIMER {
  def apply(rtcTick: Bool, intnode: IntNexusNode, intIndex: Int, intStages: Int) = {
    import MTIMERConsts._ 

    val time = RegInit(0.U(mtimeWidth.W))
    when(rtcTick) { time := time + 1.U }

    val nTiles = intnode.out.size
    val timecmp = Seq.fill(nTiles) { Reg(UInt(mtimeWidth.W)) }

    val (intnode_out, _) = intnode.out.unzip
    intnode_out.zipWithIndex.foreach { case (int, i) =>
      int(intIndex) := ShiftRegister(time.asUInt >= timecmp(i).asUInt, intStages)
    }

    val timecmpRegGroup = timecmp.zipWithIndex.flatMap{ case (t, i) => RegFieldGroup(s"mtimecmp_$i", Some(s"MTIMECMP for hart $i"),
      RegField.bytes(t, Some(RegFieldDesc(s"mtimecmp_$i", "", reset=None))))}

    val timeRegGroup = RegFieldGroup("mtime", Some("Timer Register"),
      RegField.bytes(time, Some(RegFieldDesc("mtime", "", reset=Some(0), volatile=true))))

    (timecmpRegGroup, timeRegGroup)
  }
}