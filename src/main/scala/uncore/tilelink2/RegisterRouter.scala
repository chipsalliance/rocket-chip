// See LICENSE.SiFive for license details.

package uncore.tilelink2

import Chisel._
import config._
import diplomacy._
import regmapper._
import scala.math.{min,max}

class TLRegisterNode(
    address:     AddressSet,
    device:      Device,
    deviceKey:   String  = "reg",
    concurrency: Int     = 0,
    beatBytes:   Int     = 4,
    undefZero:   Boolean = true,
    executable:  Boolean = false)
  extends TLManagerNode(Seq(TLManagerPortParameters(
    Seq(TLManagerParameters(
      address            = Seq(address),
      resources          = Seq(Resource(device, deviceKey)),
      executable         = executable,
      supportsGet        = TransferSizes(1, beatBytes),
      supportsPutPartial = TransferSizes(1, beatBytes),
      supportsPutFull    = TransferSizes(1, beatBytes),
      fifoId             = Some(0))), // requests are handled in order
    beatBytes  = beatBytes,
    minLatency = min(concurrency, 1)))) // the Queue adds at most one cycle
{
  require (address.contiguous)

  // Calling this method causes the matching TL2 bundle to be
  // configured to route all requests to the listed RegFields.
  def regmap(mapping: RegField.Map*) = {
    val a = bundleIn(0).a
    val d = bundleIn(0).d
    val edge = edgesIn(0)

    // Please forgive me ...
    val baseEnd = 0
    val (sizeEnd,   sizeOff)   = (edge.bundle.sizeBits   + baseEnd, baseEnd)
    val (sourceEnd, sourceOff) = (edge.bundle.sourceBits + sizeEnd, sizeEnd)
    val (addrLoEnd, addrLoOff) = (log2Up(beatBytes)      + sourceEnd, sourceEnd)

    val params = RegMapperParams(log2Up((address.mask+1)/beatBytes), beatBytes, addrLoEnd)
    val in = Wire(Decoupled(new RegMapperInput(params)))
    in.bits.read  := a.bits.opcode === TLMessages.Get
    in.bits.index := edge.addr_hi(a.bits)
    in.bits.data  := a.bits.data
    in.bits.mask  := a.bits.mask
    in.bits.extra := Cat(edge.addr_lo(a.bits), a.bits.source, a.bits.size)

    // Invoke the register map builder
    val out = RegMapper(beatBytes, concurrency, undefZero, in, mapping:_*)

    // No flow control needed
    in.valid  := a.valid
    a.ready   := in.ready
    d.valid   := out.valid
    out.ready := d.ready

    // We must restore the size and addr_lo to enable width adapters to work
    d.bits := edge.AccessAck(
      fromAddress = out.bits.extra(addrLoEnd-1, addrLoOff),
      fromSink    = UInt(0), // our unique sink id
      toSource    = out.bits.extra(sourceEnd-1, sourceOff),
      lgSize      = out.bits.extra(sizeEnd-1, sizeOff))

    // avoid a Mux on the data bus by manually overriding two fields
    d.bits.data := out.bits.data
    d.bits.opcode := Mux(out.bits.read, TLMessages.AccessAckData, TLMessages.AccessAck)

    // Tie off unused channels
    bundleIn(0).b.valid := Bool(false)
    bundleIn(0).c.ready := Bool(true)
    bundleIn(0).e.ready := Bool(true)
  }
}

object TLRegisterNode
{
  def apply(
      address:     AddressSet,
      device:      Device,
      deviceKey:   String  = "reg",
      concurrency: Int     = 0,
      beatBytes:   Int     = 4,
      undefZero:   Boolean = true,
      executable:  Boolean = false) =
    new TLRegisterNode(address, device, deviceKey, concurrency, beatBytes, undefZero, executable)
}

// These convenience methods below combine to make it possible to create a TL2 
// register mapped device from a totally abstract register mapped device.
// See GPIO.scala in this directory for an example

abstract class TLRegisterRouterBase(devname: String, devcompat: Seq[String], val address: AddressSet, interrupts: Int, concurrency: Int, beatBytes: Int, undefZero: Boolean, executable: Boolean)(implicit p: Parameters) extends LazyModule
{
  val device = new SimpleDevice(devname, devcompat)
  val node = TLRegisterNode(address, device, "reg", concurrency, beatBytes, undefZero, executable)
  val intnode = IntSourceNode(IntSourcePortSimple(num = interrupts, resources = Seq(Resource(device, "int"))))
}

case class TLRegBundleArg(interrupts: util.HeterogeneousBag[Vec[Bool]], in: util.HeterogeneousBag[TLBundle])(implicit val p: Parameters)

class TLRegBundleBase(arg: TLRegBundleArg) extends Bundle
{
  implicit val p = arg.p
  val interrupts = arg.interrupts
  val in = arg.in
}

class TLRegBundle[P](val params: P, arg: TLRegBundleArg)(implicit p: Parameters) extends TLRegBundleBase(arg)

class TLRegModule[P, B <: TLRegBundleBase](val params: P, bundleBuilder: => B, router: TLRegisterRouterBase)
  extends LazyModuleImp(router) with HasRegMap
{
  val io = bundleBuilder
  val interrupts = if (io.interrupts.isEmpty) Vec(0, Bool()) else io.interrupts(0)
  val address = router.address
  def regmap(mapping: RegField.Map*) = router.node.regmap(mapping:_*)
}

class TLRegisterRouter[B <: TLRegBundleBase, M <: LazyModuleImp](
     val base:        BigInt,
     val devname:     String,
     val devcompat:   Seq[String],
     val interrupts:  Int     = 0,
     val size:        BigInt  = 4096,
     val concurrency: Int     = 0,
     val beatBytes:   Int     = 4,
     val undefZero:   Boolean = true,
     val executable:  Boolean = false)
   (bundleBuilder: TLRegBundleArg => B)
   (moduleBuilder: (=> B, TLRegisterRouterBase) => M)(implicit p: Parameters)
  extends TLRegisterRouterBase(devname, devcompat, AddressSet(base, size-1), interrupts, concurrency, beatBytes, undefZero, executable)
{
  require (isPow2(size))
  // require (size >= 4096) ... not absolutely required, but highly recommended

  lazy val module = moduleBuilder(bundleBuilder(TLRegBundleArg(intnode.bundleOut, node.bundleIn)), this)
}
