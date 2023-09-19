// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.interrupts.{IntSourceNode, IntSourcePortSimple}
import freechips.rocketchip.util._

case object AXI4RRId extends ControlKey[UInt]("extra_id")
case class AXI4RRIdField(width: Int) extends SimpleBundleField(AXI4RRId)(Output(UInt((1 max width).W)), 0.U)

case class AXI4RegisterNode(address: AddressSet, concurrency: Int = 0, beatBytes: Int = 4, undefZero: Boolean = true, executable: Boolean = false)(implicit valName: ValName)
  extends SinkNode(AXI4Imp)(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address       = Seq(address),
      executable    = executable,
      supportsWrite = TransferSizes(1, beatBytes),
      supportsRead  = TransferSizes(1, beatBytes),
      interleavedId = Some(0))),
    beatBytes  = beatBytes,
    minLatency = 1)))
{
  require (address.contiguous)

  // Calling this method causes the matching AXI4 bundle to be
  // configured to route all requests to the listed RegFields.
  def regmap(mapping: RegField.Map*) = {
    val (io, _) = this.in(0)
    val ar = io.ar
    val aw = io.aw
    val w  = io.w
    val r  = io.r
    val b  = io.b

    val fields = AXI4RRIdField(ar.bits.params.idBits) +: ar.bits.params.echoFields
    val params = RegMapperParams(log2Up((address.mask+1)/beatBytes), beatBytes, fields)
    val in = Wire(Decoupled(new RegMapperInput(params)))
    val ar_extra = Wire(BundleMap(params.extraFields))
    val aw_extra = Wire(BundleMap(params.extraFields))

    // Prefer to execute reads first
    in.valid := ar.valid || (aw.valid && w.valid)
    ar.ready := in.ready
    aw.ready := in.ready && !ar.valid && w .valid
    w .ready := in.ready && !ar.valid && aw.valid

    ar_extra.waiveAll :<= ar.bits.echo
    aw_extra.waiveAll :<= aw.bits.echo
    ar_extra(AXI4RRId) := ar.bits.id
    aw_extra(AXI4RRId) := aw.bits.id
    val addr = Mux(ar.valid, ar.bits.addr, aw.bits.addr)
    val mask = MaskGen(ar.bits.addr, ar.bits.size, beatBytes)

    in.bits.read  := ar.valid
    in.bits.index := addr >> log2Ceil(beatBytes)
    in.bits.data  := w.bits.data
    in.bits.mask  := Mux(ar.valid, mask, w.bits.strb)
    in.bits.extra := Mux(ar.valid, ar_extra, aw_extra)

    // Invoke the register map builder and make it Irrevocable
    val out = Queue.irrevocable(
      RegMapper(beatBytes, concurrency, undefZero, in, mapping:_*),
      entries = 2)

    // No flow control needed
    out.ready := Mux(out.bits.read, r.ready, b.ready)
    r.valid := out.valid &&  out.bits.read
    b.valid := out.valid && !out.bits.read

    r.bits.id   := out.bits.extra(AXI4RRId)
    r.bits.data := out.bits.data
    r.bits.last := true.B
    r.bits.resp := AXI4Parameters.RESP_OKAY
    r.bits.echo :<= out.bits.extra.waiveAll

    b.bits.id   := out.bits.extra(AXI4RRId)
    b.bits.resp := AXI4Parameters.RESP_OKAY
    b.bits.echo :<= out.bits.extra.waiveAll
  }
}

// These convenience methods below combine to make it possible to create a AXI4
// register mapped device from a totally abstract register mapped device.

@deprecated("Use HasAXI4ControlRegMap+HasInterruptSources traits in place of AXI4RegisterRouter+AXI4RegBundle+AXI4RegModule", "rocket-chip 1.3")
abstract class AXI4RegisterRouterBase(address: AddressSet, interrupts: Int, concurrency: Int, beatBytes: Int, undefZero: Boolean, executable: Boolean)(implicit p: Parameters) extends LazyModule
{
  val node = AXI4RegisterNode(address, concurrency, beatBytes, undefZero, executable)
  val intnode = IntSourceNode(IntSourcePortSimple(num = interrupts))
}

@deprecated("AXI4RegBundleArg is no longer necessary, use IO(...) to make any additional IOs", "rocket-chip 1.3")
case class AXI4RegBundleArg()(implicit val p: Parameters)

@deprecated("AXI4RegBundleBase is no longer necessary, use IO(...) to make any additional IOs", "rocket-chip 1.3")
class AXI4RegBundleBase(arg: AXI4RegBundleArg) extends Bundle
{
  implicit val p = arg.p
}

@deprecated("Use HasAXI4ControlRegMap+HasInterruptSources traits in place of AXI4RegisterRouter+AXI4RegBundle+AXI4RegModule", "rocket-chip 1.3")
class AXI4RegBundle[P](val params: P, arg: AXI4RegBundleArg) extends AXI4RegBundleBase(arg)

@deprecated("Use HasAXI4ControlRegMap+HasInterruptSources traits in place of AXI4RegisterRouter+AXI4RegBundle+AXI4RegModule", "rocket-chip 1.3")
class AXI4RegModule[P, B <: AXI4RegBundleBase](val params: P, bundleBuilder: => B, router: AXI4RegisterRouterBase)
  extends LazyModuleImp(router) with HasRegMap
{
  val io = IO(bundleBuilder)
  val interrupts = if (router.intnode.out.isEmpty) Vec(0, Bool()) else router.intnode.out(0)._1
  def regmap(mapping: RegField.Map*) = router.node.regmap(mapping:_*)
}

@deprecated("Use HasAXI4ControlRegMap+HasInterruptSources traits in place of AXI4RegisterRouter+AXI4RegBundle+AXI4RegModule", "rocket-chip 1.3")
class AXI4RegisterRouter[B <: AXI4RegBundleBase, M <: LazyModuleImp]
   (val base: BigInt, val interrupts: Int = 0, val size: BigInt = 4096, val concurrency: Int = 0, val beatBytes: Int = 4, undefZero: Boolean = true, executable: Boolean = false)
   (bundleBuilder: AXI4RegBundleArg => B)
   (moduleBuilder: (=> B, AXI4RegisterRouterBase) => M)(implicit p: Parameters)
  extends AXI4RegisterRouterBase(AddressSet(base, size-1), interrupts, concurrency, beatBytes, undefZero, executable)
{
  require (isPow2(size))
  // require (size >= 4096) ... not absolutely required, but highly recommended

  lazy val module = moduleBuilder(bundleBuilder(AXI4RegBundleArg()), this)
}

/** Mix this trait into a RegisterRouter to be able to attach its register map to an AXI4 bus */
trait HasAXI4ControlRegMap { this: RegisterRouter =>
  protected val controlNode = AXI4RegisterNode(
    address = address.head,
    concurrency = concurrency,
    beatBytes = beatBytes,
    undefZero = undefZero,
    executable = executable)

  // Externally, this helper should be used to connect the register control port to a bus
  val controlXing: AXI4InwardClockCrossingHelper = this.crossIn(controlNode)

  // Backwards-compatibility default node accessor with no clock crossing
  lazy val node: AXI4InwardNode = controlXing(NoCrossing)

  // Internally, this function should be used to populate the control port with registers
  protected def regmap(mapping: RegField.Map*): Unit = { controlNode.regmap(mapping:_*) }
}
