// See LICENSE for license details.

package uncore.tilelink2

import Chisel._

class TLRegisterNode(address: AddressSet, concurrency: Option[Int] = None, beatBytes: Int = 4)
  extends TLManagerNode(beatBytes, TLManagerParameters(
    address            = Seq(address),
    supportsGet        = TransferSizes(1, beatBytes),
    supportsPutPartial = TransferSizes(1, beatBytes),
    supportsPutFull    = TransferSizes(1, beatBytes),
    fifoId             = Some(0))) // requests are handled in order
{
  require (!address.strided)

  // Calling this method causes the matching TL2 bundle to be
  // configured to route all requests to the listed RegFields.
  def regmap(mapping: RegField.Map*) = {
    val a = bundleIn(0).a
    val d = bundleIn(0).d
    val edge = edgesIn(0)
 
    val params = RegFieldParams(log2Up(address.mask+1), beatBytes, edge.bundle.sourceBits + edge.bundle.sizeBits)
    val in = Wire(Decoupled(new RegFieldInput(params)))
    in.bits.read  := a.bits.opcode === TLMessages.Get
    in.bits.index := a.bits.address >> log2Ceil(beatBytes)
    in.bits.data  := a.bits.data
    in.bits.mask  := a.bits.wmask
    in.bits.extra := Cat(a.bits.source, a.bits.size)

    // Invoke the register map builder
    val (endIndex, out) = RegFieldHelper(beatBytes, concurrency, in, mapping:_*)

    // All registers must fit inside the device address space
    require (address.mask >= (endIndex-1)*beatBytes)

    // No flow control needed
    in.valid  := a.valid
    a.ready   := in.ready
    d.valid   := out.valid
    out.ready := d.ready

    val sizeBits = edge.bundle.sizeBits
    d.bits := edge.AccessAck(out.bits.extra >> sizeBits, out.bits.extra(sizeBits-1, 0))
    // avoid a Mux on the data bus by manually overriding two fields
    d.bits.data := out.bits.data
    d.bits.opcode := Mux(out.bits.read, TLMessages.AccessAckData, TLMessages.AccessAck)
  }
}

object TLRegisterNode
{
  def apply(address: AddressSet, concurrency: Option[Int] = None, beatBytes: Int = 4) =
    new TLRegisterNode(address, concurrency, beatBytes)
}

// These convenience methods below combine to make it possible to create a TL2 
// register mapped device from a totally abstract register mapped device.
// See GPIO.scala in this directory for an example

abstract class TLRegFactory(address: AddressSet, concurrency: Option[Int], beatBytes: Int) extends TLSimpleFactory
{
  val node = TLRegisterNode(address, concurrency, beatBytes)
}

class TLRegBundle[P](val params: P, val in: Vec[TLBundle]) extends Bundle

class TLRegModule[P, B <: Bundle](val params: P, bundleBuilder: => B, factory: TLRegFactory)
  extends TLModule(factory) with HasRegMap
{
  val io = bundleBuilder
  def regmap(mapping: RegField.Map*) = factory.node.regmap(mapping:_*)
}

class TLRegisterRouter[B <: Bundle, M <: TLModule]
   (address: Option[BigInt] = None, size: BigInt = 4096, concurrency: Option[Int] = None, beatBytes: Int = 4)
   (bundleBuilder: Vec[TLBundle] => B)
   (moduleBuilder: (=> B, TLRegFactory) => M)
  extends TLRegFactory(AddressSet(size-1, address), concurrency, beatBytes)
{
  require (size % 4096 == 0) // devices should be 4K aligned
  require (isPow2(size))
  require (size >= 4096)

  lazy val module = Module(moduleBuilder(bundleBuilder(node.bundleIn), this))
}
