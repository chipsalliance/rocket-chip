// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.interrupts.{IntSourceNode, IntSourcePortSimple}
import freechips.rocketchip.util._
import scala.math.min

case class AHBRegisterNode(address: AddressSet, concurrency: Int = 0, beatBytes: Int = 4, undefZero: Boolean = true, executable: Boolean = false)(implicit valName: ValName)
  extends SinkNode(AHBImpSlave)(Seq(AHBSlavePortParameters(
    Seq(AHBSlaveParameters(
      address       = Seq(address),
      executable    = executable,
      supportsWrite = TransferSizes(1, min(address.alignment.toInt, beatBytes * AHBParameters.maxTransfer)),
      supportsRead  = TransferSizes(1, min(address.alignment.toInt, beatBytes * AHBParameters.maxTransfer)))),
    beatBytes  = beatBytes,
    lite = true)))
{
  require (address.contiguous)

  // Calling this method causes the matching AHB bundle to be
  // configured to route all requests to the listed RegFields.
  def regmap(mapping: RegField.Map*) = {
    val (ahb, _) = this.in(0)

    val indexBits = log2Up((address.mask+1)/beatBytes)
    val params = RegMapperParams(indexBits, beatBytes)
    val in = Wire(Decoupled(new RegMapperInput(params)))
    val out = RegMapper(beatBytes, concurrency, undefZero, in, mapping:_*)

    val d_phase = RegInit(false.B)
    val d_taken = Reg(Bool())
    val d_read  = Reg(Bool())
    val d_index = Reg(UInt(indexBits.W))
    val d_mask  = Reg(UInt(beatBytes.W))

    // Only send the request to the RR once
    d_taken := d_phase && in.ready
    in.valid := d_phase && !d_taken

    in.bits.read  := d_read
    in.bits.index := d_index
    in.bits.data  := ahb.hwdata
    in.bits.mask  := d_mask

    when (ahb.hready) { d_phase := false.B }
    ahb.hreadyout := !d_phase || out.valid
    ahb.hresp     := AHBParameters.RESP_OKAY
    ahb.hrdata    := out.bits.data

    val request = ahb.htrans === AHBParameters.TRANS_NONSEQ || ahb.htrans === AHBParameters.TRANS_SEQ
    when (ahb.hready && ahb.hsel && request) {
      assert (!in.valid || in.ready)
      d_phase := true.B
      d_taken := false.B
      d_read  := !ahb.hwrite
      d_index := ahb.haddr >> log2Ceil(beatBytes)
      d_mask  := MaskGen(ahb.haddr, ahb.hsize, beatBytes)
    }

    out.ready := true.B
    assert (d_phase || !out.valid)
  }
}

// These convenience methods below combine to make it possible to create a AHB
// register mapped device from a totally abstract register mapped device.

@deprecated("Use HasAHBControlRegMap+HasInterruptSources traits in place of AHBRegisterRouter+AHBRegBundle+AHBRegModule", "rocket-chip 1.3")
abstract class AHBRegisterRouterBase(address: AddressSet, interrupts: Int, concurrency: Int, beatBytes: Int, undefZero: Boolean, executable: Boolean)(implicit p: Parameters) extends LazyModule
{
  val node = AHBRegisterNode(address, concurrency, beatBytes, undefZero, executable)
  val intnode = IntSourceNode(IntSourcePortSimple(num = interrupts))
}

@deprecated("AHBRegBundleArg is no longer necessary, use IO(...) to make any additional IOs", "rocket-chip 1.3")
case class AHBRegBundleArg()(implicit val p: Parameters)

@deprecated("AHBRegBundleBase is no longer necessary, use IO(...) to make any additional IOs", "rocket-chip 1.3")
class AHBRegBundleBase(arg: AHBRegBundleArg) extends Bundle
{
  implicit val p = arg.p
}

@deprecated("Use HasAHBControlRegMap+HasInterruptSources traits in place of AHBRegisterRouter+AHBRegBundle+AHBRegModule", "rocket-chip 1.3")
class AHBRegBundle[P](val params: P, arg: AHBRegBundleArg) extends AHBRegBundleBase(arg)

@deprecated("Use HasAHBControlRegMap+HasInterruptSources traits in place of AHBRegisterRouter+AHBRegBundle+AHBRegModule", "rocket-chip 1.3")
class AHBRegModule[P, B <: AHBRegBundleBase](val params: P, bundleBuilder: => B, router: AHBRegisterRouterBase)
  extends LazyModuleImp(router) with HasRegMap
{
  val io = IO(bundleBuilder)
  val interrupts = if (router.intnode.out.isEmpty) Vec(0, Bool()) else router.intnode.out(0)._1
  def regmap(mapping: RegField.Map*) = router.node.regmap(mapping:_*)
}

@deprecated("Use HasAHBControlRegMap+HasInterruptSources traits in place of AHBRegisterRouter+AHBRegBundle+AHBRegModule", "rocket-chip 1.3")
class AHBRegisterRouter[B <: AHBRegBundleBase, M <: LazyModuleImp]
   (val base: BigInt, val interrupts: Int = 0, val size: BigInt = 4096, val concurrency: Int = 0, val beatBytes: Int = 4, undefZero: Boolean = true, executable: Boolean = false)
   (bundleBuilder: AHBRegBundleArg => B)
   (moduleBuilder: (=> B, AHBRegisterRouterBase) => M)(implicit p: Parameters)
  extends AHBRegisterRouterBase(AddressSet(base, size-1), interrupts, concurrency, beatBytes, undefZero, executable)
{
  require (isPow2(size))
  // require (size >= 4096) ... not absolutely required, but highly recommended

  lazy val module = moduleBuilder(bundleBuilder(AHBRegBundleArg()), this)
}

/** Mix this trait into a RegisterRouter to be able to attach its register map to an AXI4 bus */
trait HasAHBControlRegMap { this: RegisterRouter =>
  // Externally, this node should be used to connect the register control port to a bus
  val controlNode = AHBRegisterNode(
    address = address.head,
    concurrency = concurrency,
    beatBytes = beatBytes,
    undefZero = undefZero,
    executable = executable)

  // Backwards-compatibility default node accessor with no clock crossing
  lazy val node: AHBRegisterNode = controlNode

  // Internally, this function should be used to populate the control port with registers
  protected def regmap(mapping: RegField.Map*): Unit = { controlNode.regmap(mapping:_*) }
}
