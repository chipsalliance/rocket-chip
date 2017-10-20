// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.interrupts.{IntSourceNode, IntSourcePortSimple}
import freechips.rocketchip.util.HeterogeneousBag
import scala.math.{min,max}

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
    val params = RegMapperParams(indexBits, beatBytes, 1)
    val in = Wire(Decoupled(new RegMapperInput(params)))
    val out = RegMapper(beatBytes, concurrency, undefZero, in, mapping:_*)

    // Only send the request to the RR once
    val taken = RegInit(Bool(false))
    when (in.fire())  { taken := Bool(true)  }
    when (out.fire()) { taken := Bool(false) }

    in.bits.read  := !apb.pwrite
    in.bits.index := apb.paddr >> log2Ceil(beatBytes)
    in.bits.data  := apb.pwdata
    in.bits.mask  := Mux(apb.pwrite, apb.pstrb, UInt((1<<beatBytes) - 1))
    in.bits.extra := UInt(0)

    in.valid := apb.psel && !taken
    out.ready := apb.penable

    apb.pready  := out.valid
    apb.pslverr := Bool(false)
    apb.prdata  := out.bits.data
  }
}

// These convenience methods below combine to make it possible to create a APB
// register mapped device from a totally abstract register mapped device.

abstract class APBRegisterRouterBase(address: AddressSet, interrupts: Int, concurrency: Int, beatBytes: Int, undefZero: Boolean, executable: Boolean)(implicit p: Parameters) extends LazyModule
{
  val node = APBRegisterNode(address, concurrency, beatBytes, undefZero, executable)
  val intnode = IntSourceNode(IntSourcePortSimple(num = interrupts))
}

case class APBRegBundleArg()(implicit val p: Parameters)

class APBRegBundleBase(arg: APBRegBundleArg) extends Bundle
{
  implicit val p = arg.p
}

class APBRegBundle[P](val params: P, arg: APBRegBundleArg) extends APBRegBundleBase(arg)

class APBRegModule[P, B <: APBRegBundleBase](val params: P, bundleBuilder: => B, router: APBRegisterRouterBase)
  extends LazyModuleImp(router) with HasRegMap
{
  val io = IO(bundleBuilder)
  val interrupts = if (router.intnode.out.isEmpty) Vec(0, Bool()) else router.intnode.out(0)._1
  def regmap(mapping: RegField.Map*) = router.node.regmap(mapping:_*)
}

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
