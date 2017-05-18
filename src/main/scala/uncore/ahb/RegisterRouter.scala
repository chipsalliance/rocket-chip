// See LICENSE.SiFive for license details.

package uncore.ahb

import Chisel._
import config._
import diplomacy._
import regmapper._
import scala.math.{min,max}

class AHBRegisterNode(address: AddressSet, concurrency: Int = 0, beatBytes: Int = 4, undefZero: Boolean = true, executable: Boolean = false)
  extends AHBSlaveNode(Seq(AHBSlavePortParameters(
    Seq(AHBSlaveParameters(
      address       = Seq(address),
      executable    = executable,
      supportsWrite = TransferSizes(1, min(address.alignment.toInt, beatBytes * AHBParameters.maxTransfer)),
      supportsRead  = TransferSizes(1, min(address.alignment.toInt, beatBytes * AHBParameters.maxTransfer)))),
    beatBytes  = beatBytes)))
{
  require (address.contiguous)

  // Calling this method causes the matching AHB bundle to be
  // configured to route all requests to the listed RegFields.
  def regmap(mapping: RegField.Map*) = {
    val ahb = bundleIn(0)

    val indexBits = log2Up((address.mask+1)/beatBytes)
    val params = RegMapperParams(indexBits, beatBytes, 1)
    val in = Wire(Decoupled(new RegMapperInput(params)))
    val out = RegMapper(beatBytes, concurrency, undefZero, in, mapping:_*)

    val d_phase = RegInit(Bool(false))
    val d_taken = Reg(Bool())
    val d_read  = Reg(Bool())
    val d_index = Reg(UInt(width = indexBits))
    val d_mask  = Reg(UInt(width = beatBytes))

    // Only send the request to the RR once
    d_taken := d_phase && in.ready
    in.valid := d_phase && !d_taken

    in.bits.read  := d_read
    in.bits.index := d_index
    in.bits.data  := ahb.hwdata
    in.bits.mask  := d_mask
    in.bits.extra := UInt(0)

    when (ahb.hready) { d_phase := Bool(false) }
    ahb.hreadyout := !d_phase || out.valid
    ahb.hresp     := AHBParameters.RESP_OKAY
    ahb.hrdata    := out.bits.data

    val request = ahb.htrans === AHBParameters.TRANS_NONSEQ || ahb.htrans === AHBParameters.TRANS_SEQ
    when (ahb.hready && ahb.hsel && request) {
      assert (!in.valid || in.ready)
      d_phase := Bool(true)
      d_taken := Bool(false)
      d_read  := !ahb.hwrite
      d_index := ahb.haddr >> log2Ceil(beatBytes)
      d_mask  := uncore.tilelink2.maskGen(ahb.haddr, ahb.hsize, beatBytes)
    }

    out.ready := Bool(true)
    assert (d_phase || !out.valid)
  }
}

object AHBRegisterNode
{
  def apply(address: AddressSet, concurrency: Int = 0, beatBytes: Int = 4, undefZero: Boolean = true, executable: Boolean = false) =
    new AHBRegisterNode(address, concurrency, beatBytes, undefZero, executable)
}

// These convenience methods below combine to make it possible to create a AHB
// register mapped device from a totally abstract register mapped device.

abstract class AHBRegisterRouterBase(address: AddressSet, interrupts: Int, concurrency: Int, beatBytes: Int, undefZero: Boolean, executable: Boolean)(implicit p: Parameters) extends LazyModule
{
  val node = AHBRegisterNode(address, concurrency, beatBytes, undefZero, executable)
  val intnode = uncore.tilelink2.IntSourceNode(uncore.tilelink2.IntSourcePortSimple(num = interrupts))
}

case class AHBRegBundleArg(interrupts: util.HeterogeneousBag[Vec[Bool]], in: util.HeterogeneousBag[AHBBundle])(implicit val p: Parameters)

class AHBRegBundleBase(arg: AHBRegBundleArg) extends Bundle
{
  implicit val p = arg.p
  val interrupts = arg.interrupts
  val in = arg.in
}

class AHBRegBundle[P](val params: P, arg: AHBRegBundleArg) extends AHBRegBundleBase(arg)

class AHBRegModule[P, B <: AHBRegBundleBase](val params: P, bundleBuilder: => B, router: AHBRegisterRouterBase)
  extends LazyModuleImp(router) with HasRegMap
{
  val io = bundleBuilder
  val interrupts = if (io.interrupts.isEmpty) Vec(0, Bool()) else io.interrupts(0)
  def regmap(mapping: RegField.Map*) = router.node.regmap(mapping:_*)
}

class AHBRegisterRouter[B <: AHBRegBundleBase, M <: LazyModuleImp]
   (val base: BigInt, val interrupts: Int = 0, val size: BigInt = 4096, val concurrency: Int = 0, val beatBytes: Int = 4, undefZero: Boolean = true, executable: Boolean = false)
   (bundleBuilder: AHBRegBundleArg => B)
   (moduleBuilder: (=> B, AHBRegisterRouterBase) => M)(implicit p: Parameters)
  extends AHBRegisterRouterBase(AddressSet(base, size-1), interrupts, concurrency, beatBytes, undefZero, executable)
{
  require (isPow2(size))
  // require (size >= 4096) ... not absolutely required, but highly recommended

  lazy val module = moduleBuilder(bundleBuilder(AHBRegBundleArg(intnode.bundleOut, node.bundleIn)), this)
}
