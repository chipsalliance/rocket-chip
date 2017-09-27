// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.util.HeterogeneousBag
import scala.math.{min,max}

case class TLRegisterNode(
    address:     Seq[AddressSet],
    device:      Device,
    deviceKey:   String  = "reg/control",
    concurrency: Int     = 0,
    beatBytes:   Int     = 4,
    undefZero:   Boolean = true,
    executable:  Boolean = false)(
    implicit valName: ValName)
  extends SinkNode(TLImp)(Seq(TLManagerPortParameters(
    Seq(TLManagerParameters(
      address            = address,
      resources          = Seq(Resource(device, deviceKey)),
      executable         = executable,
      supportsGet        = TransferSizes(1, beatBytes),
      supportsPutPartial = TransferSizes(1, beatBytes),
      supportsPutFull    = TransferSizes(1, beatBytes),
      fifoId             = Some(0))), // requests are handled in order
    beatBytes  = beatBytes,
    minLatency = min(concurrency, 1)))) // the Queue adds at most one cycle
{
  val size = 1 << log2Ceil(1 + address.map(_.max).max - address.map(_.base).min)
  require (size >= beatBytes)
  address.foreach { case a =>
    require (a.widen(size-1).base == address.head.widen(size-1).base,
      s"TLRegisterNode addresses (${address}) must be aligned to its size ${size}")
  }

  // Calling this method causes the matching TL2 bundle to be
  // configured to route all requests to the listed RegFields.
  def regmap(mapping: RegField.Map*) = {
    val (bundleIn, edge) = this.in(0)
    val a = bundleIn.a
    val d = bundleIn.d

    // Please forgive me ...
    val baseEnd = 0
    val (sizeEnd,   sizeOff)   = (edge.bundle.sizeBits   + baseEnd, baseEnd)
    val (sourceEnd, sourceOff) = (edge.bundle.sourceBits + sizeEnd, sizeEnd)

    val params = RegMapperParams(log2Up(size/beatBytes), beatBytes, sourceEnd)
    val in = Wire(Decoupled(new RegMapperInput(params)))
    in.bits.read  := a.bits.opcode === TLMessages.Get
    in.bits.index := edge.addr_hi(a.bits)
    in.bits.data  := a.bits.data
    in.bits.mask  := a.bits.mask
    in.bits.extra := Cat(a.bits.source, a.bits.size)

    // Invoke the register map builder
    val out = RegMapper(beatBytes, concurrency, undefZero, in, mapping:_*)

    // No flow control needed
    in.valid  := a.valid
    a.ready   := in.ready
    d.valid   := out.valid
    out.ready := d.ready

    // We must restore the size to enable width adapters to work
    d.bits := edge.AccessAck(
      toSource    = out.bits.extra(sourceEnd-1, sourceOff),
      lgSize      = out.bits.extra(sizeEnd-1, sizeOff))

    // avoid a Mux on the data bus by manually overriding two fields
    d.bits.data := out.bits.data
    d.bits.opcode := Mux(out.bits.read, TLMessages.AccessAckData, TLMessages.AccessAck)

    // Tie off unused channels
    bundleIn.b.valid := Bool(false)
    bundleIn.c.ready := Bool(true)
    bundleIn.e.ready := Bool(true)
  }
}

// register mapped device from a totally abstract register mapped device.
// See GPIO.scala in this directory for an example

abstract class TLRegisterRouterBase(devname: String, devcompat: Seq[String], val address: AddressSet, interrupts: Int, concurrency: Int, beatBytes: Int, undefZero: Boolean, executable: Boolean)(implicit p: Parameters) extends LazyModule
{
  val device = new SimpleDevice(devname, devcompat)
  val node = TLRegisterNode(Seq(address), device, "reg/control", concurrency, beatBytes, undefZero, executable)
  val intnode = IntSourceNode(IntSourcePortSimple(num = interrupts, resources = Seq(Resource(device, "int"))))
}

case class TLRegBundleArg()(implicit val p: Parameters)

class TLRegBundleBase(arg: TLRegBundleArg) extends Bundle
{
  implicit val p = arg.p
}

class TLRegBundle[P](val params: P, arg: TLRegBundleArg)(implicit p: Parameters) extends TLRegBundleBase(arg)

class TLRegModule[P, B <: TLRegBundleBase](val params: P, bundleBuilder: => B, router: TLRegisterRouterBase)
  extends LazyModuleImp(router) with HasRegMap
{
  val io = IO(bundleBuilder)
  val interrupts = if (router.intnode.out.isEmpty) Vec(0, Bool()) else router.intnode.out(0)._1
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

  lazy val module = moduleBuilder(bundleBuilder(TLRegBundleArg()), this)
}

// !!! eliminate third trait
