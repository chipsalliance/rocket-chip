// See LICENSE.SiFive for license details.

package uncore.apb

import Chisel._
import config._
import diplomacy._
import regmapper._
import scala.math.{min,max}

class APBRegisterNode(address: AddressSet, concurrency: Int = 0, beatBytes: Int = 4, undefZero: Boolean = true, executable: Boolean = false)
  extends APBSlaveNode(Seq(APBSlavePortParameters(
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
    val apb = bundleIn(0)

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

object APBRegisterNode
{
  def apply(address: AddressSet, concurrency: Int = 0, beatBytes: Int = 4, undefZero: Boolean = true, executable: Boolean = false) =
    new APBRegisterNode(address, concurrency, beatBytes, undefZero, executable)
}

// These convenience methods below combine to make it possible to create a APB
// register mapped device from a totally abstract register mapped device.

abstract class APBRegisterRouterBase(address: AddressSet, interrupts: Int, concurrency: Int, beatBytes: Int, undefZero: Boolean, executable: Boolean)(implicit p: Parameters) extends LazyModule
{
  val node = APBRegisterNode(address, concurrency, beatBytes, undefZero, executable)
  val intnode = uncore.tilelink2.IntSourceNode(uncore.tilelink2.IntSourcePortSimple(num = interrupts))
}

case class APBRegBundleArg(interrupts: util.HeterogeneousBag[Vec[Bool]], in: util.HeterogeneousBag[APBBundle])(implicit val p: Parameters)

class APBRegBundleBase(arg: APBRegBundleArg) extends Bundle
{
  implicit val p = arg.p
  val interrupts = arg.interrupts
  val in = arg.in
}

class APBRegBundle[P](val params: P, arg: APBRegBundleArg) extends APBRegBundleBase(arg)

class APBRegModule[P, B <: APBRegBundleBase](val params: P, bundleBuilder: => B, router: APBRegisterRouterBase)
  extends LazyModuleImp(router) with HasRegMap
{
  val io = bundleBuilder
  val interrupts = if (io.interrupts.isEmpty) Vec(0, Bool()) else io.interrupts(0)
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

  lazy val module = moduleBuilder(bundleBuilder(APBRegBundleArg(intnode.bundleOut, node.bundleIn)), this)
}
