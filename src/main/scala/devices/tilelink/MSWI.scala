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

object SWIConsts
{
  def sipOffset(hart: Int) = hart * sipBytes
  def sipBytes = 4
  def size = 0x4000
  def ipiWidth = 32
  def ints = 1
  def clintSize = 0x10000
}

case class MSWIParams(BaseAddress: BigInt = 0x02000000, intStages: Int = 0)
{
  def address = AddressSet(BaseAddress, SWIConsts.size - 1)
}

case class MSWIAttachParams(
  slaveWhere: TLBusWrapperLocation = CBUS
)

case object MSWIAttachKey extends Field(MSWIAttachParams())

class MSWI(mswiParams: MSWIParams, mtimerParams: MTIMERParams, isACLINT: Boolean = false, beatBytes: Int)(implicit p: Parameters) extends LazyModule 
{
  import SWIConsts._ 

  val device = if (isACLINT) { 
      new SimpleDevice("mswi", Seq("riscv,aclint-mswi")) {
        override val alwaysExtended = true
      }
    } else {
      new SimpleDevice("clint", Seq("riscv,clint0")) {
        override val alwaysExtended = true
      }
    }

  val node: TLRegisterNode = if (isACLINT) {
    TLRegisterNode(
      address     = Seq(mswiParams.address),
      device      = device,
      beatBytes   = beatBytes
    )
  } else {
    TLRegisterNode(
      address     = Seq(AddressSet(mswiParams.address.base, clintSize - 1)),
      device      = device,
      beatBytes   = beatBytes
    )
  }

  val ints = if (isACLINT) {
    SWIConsts.ints
  } else {
    SWIConsts.ints + MTIMERConsts.ints
  }

  val intnode : IntNexusNode = IntNexusNode(
    sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(ints, Seq(Resource(device, "int")))))},
    sinkFn   = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
    outputRequiresInput = false
  )

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    if (isACLINT) {
      Annotated.params(this, mswiParams)
    } else {
      Annotated.params(this, mswiParams)
      Annotated.params(this, mtimerParams)
    }

    require (intnode.edges.in.size == 0, "MSWI only produces interrupts; it does not accept them")

    val mswiRegGroup: Seq[RegField] = SWI("m", intnode, mswiParams.intStages)

    val io = IO(new Bundle {
        val rtcTick = (!isACLINT).option(Input(Bool()))
    })
    
    val mtimerRegGroup: Option[(Seq[RegField], Seq[RegField])] = (!isACLINT).option(MTIMER(io.rtcTick.get, intnode, 1, mtimerParams.intStages))

    /* aclint:
     * 0 msip hart 0
     * 4 msip hart 1
     * 
     * clint:
     * 0000 msip hart 0
     * 0004 msip hart 1
     * 4000 mtimecmp hart 0 lo
     * 4004 mtimecmp hart 0 hi
     * 4008 mtimecmp hart 1 lo
     * 400c mtimecmp hart 1 hi
     * bff8 mtime lo
     * bffc mtime hi
     */
    val mtimerRegMapping = if (!isACLINT) Seq(
      0x4000 -> mtimerRegGroup.get._1,
      0xbff8 -> mtimerRegGroup.get._2
    ) else Nil

    val mswiRegMapping = Seq(
      0 -> mswiRegGroup
    )

    val mapping = mswiRegMapping ++ mtimerRegMapping
    node.regmap(mapping:_*)    
  }
}

object SWI {
  def apply(mswiType: String, intnode: IntNexusNode, intStages: Int) = {
    import SWIConsts._ 

    val nTiles  = intnode.out.size
    val ipi     = Seq.fill(nTiles) { RegInit(0.U(1.W)) }
    
    val (intnode_out, _) = intnode.out.unzip
    intnode_out.zipWithIndex.foreach { case (int, i) =>
      int(0) := ShiftRegister(ipi(i)(0), intStages)
    }

    val swiRegGroup = RegFieldGroup(mswiType + "sip", Some(mswiType.toUpperCase + "SIP Bits"), ipi.zipWithIndex.flatMap { case (r, i) => 
      RegField(1, r, RegFieldDesc(mswiType + s"sip_$i", mswiType.toUpperCase + s"SIP bit for Hart $i", reset=Some(0))) :: RegField(SWIConsts.ipiWidth - 1) :: Nil })
  
    swiRegGroup
  }
}