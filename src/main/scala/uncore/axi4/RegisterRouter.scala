// See LICENSE.SiFive for license details.

package uncore.axi4

import Chisel._
import config._
import diplomacy._
import regmapper._
import scala.math.{min,max}

class AXI4RegisterNode(address: AddressSet, concurrency: Int = 0, beatBytes: Int = 4, undefZero: Boolean = true, executable: Boolean = false)
  extends AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address       = Seq(address),
      executable    = executable,
      supportsWrite = TransferSizes(1, beatBytes),
      supportsRead  = TransferSizes(1, beatBytes),
      interleavedId = Some(0))),
    beatBytes  = beatBytes,
    minLatency = min(concurrency, 1)))) // the Queue adds at most one cycle
{
  require (address.contiguous)

  // Calling this method causes the matching AXI4 bundle to be
  // configured to route all requests to the listed RegFields.
  def regmap(mapping: RegField.Map*) = {
    val ar = bundleIn(0).ar
    val aw = bundleIn(0).aw
    val w  = bundleIn(0).w
    val r  = bundleIn(0).r
    val b  = bundleIn(0).b

    val params = RegMapperParams(log2Up((address.mask+1)/beatBytes), beatBytes, ar.bits.params.idBits)
    val in = Wire(Decoupled(new RegMapperInput(params)))

    // Prefer to execute reads first
    in.valid := ar.valid || (aw.valid && w.valid)
    ar.ready := in.ready
    aw.ready := in.ready && !ar.valid && w .valid
    w .ready := in.ready && !ar.valid && aw.valid

    val addr  = Mux(ar.valid, ar.bits.addr, aw.bits.addr)
    val in_id = Mux(ar.valid, ar.bits.id,   aw.bits.id)
    val mask = uncore.tilelink2.maskGen(ar.bits.addr, ar.bits.size, beatBytes)

    in.bits.read  := ar.valid
    in.bits.index := addr >> log2Ceil(beatBytes)
    in.bits.data  := w.bits.data
    in.bits.mask  := Mux(ar.valid, mask, w.bits.strb)
    in.bits.extra := in_id

    // Invoke the register map builder and make it Irrevocable
    val out = Queue.irrevocable(
      RegMapper(beatBytes, concurrency, undefZero, in, mapping:_*),
      entries = 1, flow = true)

    // No flow control needed
    out.ready := Mux(out.bits.read, r.ready, b.ready)
    r.valid := out.valid &&  out.bits.read
    b.valid := out.valid && !out.bits.read

    val out_id = if (r.bits.params.idBits == 0) UInt(0) else out.bits.extra

    r.bits.id   := out_id
    r.bits.data := out.bits.data
    r.bits.last := Bool(true)
    r.bits.resp := AXI4Parameters.RESP_OKAY
    b.bits.id   := out_id
    b.bits.resp := AXI4Parameters.RESP_OKAY
  }
}

object AXI4RegisterNode
{
  def apply(address: AddressSet, concurrency: Int = 0, beatBytes: Int = 4, undefZero: Boolean = true, executable: Boolean = false) =
    new AXI4RegisterNode(address, concurrency, beatBytes, undefZero, executable)
}

// These convenience methods below combine to make it possible to create a AXI4
// register mapped device from a totally abstract register mapped device.

abstract class AXI4RegisterRouterBase(address: AddressSet, interrupts: Int, concurrency: Int, beatBytes: Int, undefZero: Boolean, executable: Boolean)(implicit p: Parameters) extends LazyModule
{
  val node = AXI4RegisterNode(address, concurrency, beatBytes, undefZero, executable)
  val intnode = uncore.tilelink2.IntSourceNode(uncore.tilelink2.IntSourcePortSimple(num = interrupts))
}

case class AXI4RegBundleArg(interrupts: util.HeterogeneousBag[Vec[Bool]], in: util.HeterogeneousBag[AXI4Bundle])(implicit val p: Parameters)

class AXI4RegBundleBase(arg: AXI4RegBundleArg) extends Bundle
{
  implicit val p = arg.p
  val interrupts = arg.interrupts
  val in = arg.in
}

class AXI4RegBundle[P](val params: P, arg: AXI4RegBundleArg) extends AXI4RegBundleBase(arg)

class AXI4RegModule[P, B <: AXI4RegBundleBase](val params: P, bundleBuilder: => B, router: AXI4RegisterRouterBase)
  extends LazyModuleImp(router) with HasRegMap
{
  val io = bundleBuilder
  val interrupts = if (io.interrupts.isEmpty) Vec(0, Bool()) else io.interrupts(0)
  def regmap(mapping: RegField.Map*) = router.node.regmap(mapping:_*)
}

class AXI4RegisterRouter[B <: AXI4RegBundleBase, M <: LazyModuleImp]
   (val base: BigInt, val interrupts: Int = 0, val size: BigInt = 4096, val concurrency: Int = 0, val beatBytes: Int = 4, undefZero: Boolean = true, executable: Boolean = false)
   (bundleBuilder: AXI4RegBundleArg => B)
   (moduleBuilder: (=> B, AXI4RegisterRouterBase) => M)(implicit p: Parameters)
  extends AXI4RegisterRouterBase(AddressSet(base, size-1), interrupts, concurrency, beatBytes, undefZero, executable)
{
  require (isPow2(size))
  // require (size >= 4096) ... not absolutely required, but highly recommended

  lazy val module = moduleBuilder(bundleBuilder(AXI4RegBundleArg(intnode.bundleOut, node.bundleIn)), this)
}
