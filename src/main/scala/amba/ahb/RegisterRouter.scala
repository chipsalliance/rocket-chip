// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import Chisel._
import chisel3.experimental.RawModule
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.model.{OMMemoryRegion, OMRegister, OMRegisterMap}
import freechips.rocketchip.regmapper._
import freechips.rocketchip.interrupts.{IntSourceNode, IntSourcePortSimple}
import freechips.rocketchip.util.{HeterogeneousBag, MaskGen}
import freechips.rocketchip.util.{ElaborationArtefacts, GenRegDescsAnno, HeterogeneousBag}
import scala.math.{min,max}

case class AHBRegisterNode(
  address:     AddressSet,
  device:      Device,
  deviceKey:   String  = "reg/control",
  concurrency: Int     = 0,
  beatBytes:   Int     = 4,
  undefZero:   Boolean = true,
  executable:  Boolean = false)(
  implicit valName: ValName)
  extends SinkNode(AHBImp)(Seq(AHBSlavePortParameters(
    Seq(AHBSlaveParameters(
      address       = Seq(address),
      resources     = Seq(Resource(device, deviceKey)),
      executable    = executable,
      supportsWrite = TransferSizes(1, min(address.alignment.toInt, beatBytes * AHBParameters.maxTransfer)),
      supportsRead  = TransferSizes(1, min(address.alignment.toInt, beatBytes * AHBParameters.maxTransfer)))),
    beatBytes  = beatBytes)))
{
  require (address.contiguous)

  // Calling this method causes the matching AHB bundle to be
  // configured to route all requests to the listed RegFields.
  def regmap(mapping: RegField.Map*) = {
    val (ahb, _) = this.in(0)

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
      d_mask  := MaskGen(ahb.haddr, ahb.hsize, beatBytes)
    }

    out.ready := Bool(true)
    assert (d_phase || !out.valid)

    genRegDescsJson(mapping:_*)
    genOMRegMap(mapping:_*)
  }
    def genOMRegMap(mapping: RegField.Map*): OMRegisterMap = {
    OMRegister.convert(mapping = mapping:_*)
  }

  def genRegDescsJson(mapping: RegField.Map*) {
    // Dump out the register map for documentation purposes.
    val base = address.base
    val baseHex = s"0x${base.toInt.toHexString}"
    val name = s"deviceAt${baseHex}" //TODO: It would be better to name this other than "Device at ...."
    val json = GenRegDescsAnno.serialize(base, name, mapping:_*)
    var suffix = 0
    while( ElaborationArtefacts.contains(s"${baseHex}.${suffix}.regmap.json")) {
      suffix = suffix + 1
    }
    ElaborationArtefacts.add(s"${baseHex}.${suffix}.regmap.json", json)

    val module = Module.currentModule.get.asInstanceOf[RawModule]
    GenRegDescsAnno.anno(
      module,
      base,
      mapping:_*)
  }
}

// These convenience methods below combine to make it possible to create a AHB
// register mapped device from a totally abstract register mapped device.

abstract class AHBRegisterRouterBase(devname: String, devcompat: Seq[String], address: AddressSet, interrupts: Int, concurrency: Int, beatBytes: Int, undefZero: Boolean, executable: Boolean)(implicit p: Parameters) extends LazyModule
{
  // Allow devices to extend the DTS mapping
  def extraResources(resources: ResourceBindings) = Map[String, Seq[ResourceValue]]()
  val device = new SimpleDevice(devname, devcompat) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping ++ extraResources(resources))
    }
  }

  val node = AHBRegisterNode(address, device, "reg/control", concurrency, beatBytes, undefZero, executable)
  val intnode = IntSourceNode(IntSourcePortSimple(num = interrupts))
}

case class AHBRegBundleArg()(implicit val p: Parameters)

class AHBRegBundleBase(arg: AHBRegBundleArg) extends Bundle
{
  implicit val p = arg.p
}

class AHBRegBundle[P](val params: P, arg: AHBRegBundleArg) extends AHBRegBundleBase(arg)

class AHBRegModule[P, B <: AHBRegBundleBase](val params: P, bundleBuilder: => B, router: AHBRegisterRouterBase)
  extends LazyModuleImp(router) with HasRegMap
{
  val io = IO(bundleBuilder)
  val interrupts = if (router.intnode.out.isEmpty) Vec(0, Bool()) else router.intnode.out(0)._1
  def regmap(mapping: RegField.Map*) = router.node.regmap(mapping:_*)
}

class AHBRegisterRouter[B <: AHBRegBundleBase, M <: LazyModuleImp] (
  val base:        BigInt,
  val devname:     String,
  val devcompat:   Seq[String],
  val interrupts:  Int     = 0,
  val size:        BigInt  = 4096,
  val concurrency: Int     = 0,
  val beatBytes:   Int     = 4,
  val undefZero:   Boolean = true,
  val executable:  Boolean = false)
  (bundleBuilder: AHBRegBundleArg => B)
  (moduleBuilder: (=> B, AHBRegisterRouterBase) => M)(implicit p: Parameters)
    extends AHBRegisterRouterBase(devname, devcompat, AddressSet(base, size-1), interrupts, concurrency, beatBytes, undefZero, executable)
{
  require (isPow2(size))
  // require (size >= 4096) ... not absolutely required, but highly recommended

  lazy val module = moduleBuilder(bundleBuilder(AHBRegBundleArg()), this)
}

/** Mix this trait into a RegisterRouter to be able to attach its register map to an AXI4 bus */
trait HasAHBControlRegMap { this: RegisterRouter[_] =>
//trait HasAXI4ControlRegMap { this: RAXI4RegisterRouterBase =>
  protected val controlNode = AHBRegisterNode(
    address     = address.head,
    device      = device,
    deviceKey   = "reg/control",
    concurrency = concurrency,
    beatBytes   = beatBytes,
    undefZero   = undefZero,
    executable  = executable)

  // Externally, this helper should be used to connect the register control port to a bus
  val controlXing = this.controlNode

  // Internally, this function should be used to populate the control port with registers
  protected def regmap(mapping: RegField.Map*) { controlNode.regmap(mapping:_*) }
}
