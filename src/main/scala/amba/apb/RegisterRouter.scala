// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.interrupts.{IntSourceNode, IntSourcePortSimple}

case class APBRegisterNode(address: AddressSet, concurrency: Int = 0, beatBytes: Int = 4, undefZero: Boolean = true, executable: Boolean = false)(implicit valName: ValName)
  extends SinkNode(APBImp)(Seq(APBSlavePortParameters(
    Seq(APBSlaveParameters(
      address       = Seq(address),
      executable    = executable,
      supportsWrite = true,
      supportsRead  = true)),
    beatBytes  = beatBytes)))
{
  require (address.contiguous)

  // Calling this method causes the matching APB bundle to be
  // configured to route all requests to the listed RegFields.
  def regmap(mapping: RegField.Map*) = {
    val (apb, _) = this.in(0)

    val indexBits = log2Up((address.mask+1)/beatBytes)
    val params = RegMapperParams(indexBits, beatBytes)
    val in = Wire(Decoupled(new RegMapperInput(params)))
    val out = RegMapper(beatBytes, concurrency, undefZero, in, mapping:_*)

    // Only send the request to the RR once
    val taken = RegInit(false.B)
    when (in.fire)  { taken := true.B  }
    when (out.fire) { taken := false.B }

    in.bits.read  := !apb.pwrite
    in.bits.index := apb.paddr >> log2Ceil(beatBytes)
    in.bits.data  := apb.pwdata
    in.bits.mask  := Mux(apb.pwrite, apb.pstrb, ((1<<beatBytes) - 1).U)

    in.valid := apb.psel && !taken
    out.ready := apb.penable

    apb.pready  := out.valid
    apb.pslverr := false.B
    apb.prdata  := out.bits.data
  }
}

// These convenience methods below combine to make it possible to create a APB
// register mapped device from a totally abstract register mapped device.

@deprecated("Use HasAPBControlRegMap+HasInterruptSources traits in place of APBRegisterRouter+APBRegBundle+APBRegModule", "rocket-chip 1.3")
abstract class APBRegisterRouterBase(address: AddressSet, interrupts: Int, concurrency: Int, beatBytes: Int, undefZero: Boolean, executable: Boolean)(implicit p: Parameters) extends LazyModule
{
  val node = APBRegisterNode(address, concurrency, beatBytes, undefZero, executable)
  val intnode = IntSourceNode(IntSourcePortSimple(num = interrupts))
}

@deprecated("APBRegBundleArg is no longer necessary, use IO(...) to make any additional IOs", "rocket-chip 1.3")
case class APBRegBundleArg()(implicit val p: Parameters)

@deprecated("AXI4RegBundleBase is no longer necessary, use IO(...) to make any additional IOs", "rocket-chip 1.3")
class APBRegBundleBase(arg: APBRegBundleArg) extends Bundle
{
  implicit val p = arg.p
}

@deprecated("Use HasAPBControlRegMap+HasInterruptSources traits in place of APBRegisterRouter+APBRegBundle+APBRegModule", "rocket-chip 1.3")
class APBRegBundle[P](val params: P, arg: APBRegBundleArg) extends APBRegBundleBase(arg)

@deprecated("Use HasAPBControlRegMap+HasInterruptSources traits in place of APBRegisterRouter+APBRegBundle+APBRegModule", "rocket-chip 1.3")
class APBRegModule[P, B <: APBRegBundleBase](val params: P, bundleBuilder: => B, router: APBRegisterRouterBase)
  extends LazyModuleImp(router) with HasRegMap
{
  val io = IO(bundleBuilder)
  val interrupts = if (router.intnode.out.isEmpty) Vec(0, Bool()) else router.intnode.out(0)._1
  def regmap(mapping: RegField.Map*) = router.node.regmap(mapping:_*)
}

@deprecated("Use HasAPBControlRegMap+HasInterruptSources traits in place of APBRegisterRouter+APBRegBundle+APBRegModule", "rocket-chip 1.3")
class APBRegisterRouter[B <: APBRegBundleBase, M <: LazyModuleImp]
   (val base: BigInt, val interrupts: Int = 0, val size: BigInt = 4096, val concurrency: Int = 0, val beatBytes: Int = 4, undefZero: Boolean = true, executable: Boolean = false)
   (bundleBuilder: APBRegBundleArg => B)
   (moduleBuilder: (=> B, APBRegisterRouterBase) => M)(implicit p: Parameters)
  extends APBRegisterRouterBase(AddressSet(base, size-1), interrupts, concurrency, beatBytes, undefZero, executable)
{
  require (isPow2(size))
  // require (size >= 4096) ... not absolutely required, but highly recommended

  lazy val module = moduleBuilder(bundleBuilder(APBRegBundleArg()), this)
}

/** Mix this trait into a RegisterRouter to be able to attach its register map to an AXI4 bus */
trait HasAPBControlRegMap { this: RegisterRouter =>
  // Externally, this node should be used to connect the register control port to a bus
  val controlNode = APBRegisterNode(
    address = address.head,
    concurrency = concurrency,
    beatBytes = beatBytes,
    undefZero = undefZero,
    executable = executable)

  // Backwards-compatibility default node accessor with no clock crossing
  lazy val node: APBInwardNode = controlNode

  // Internally, this function should be used to populate the control port with registers
  protected def regmap(mapping: RegField.Map*): Unit = { controlNode.regmap(mapping:_*) }
}
