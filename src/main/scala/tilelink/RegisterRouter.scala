// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy._
import org.chipsalliance.diplomacy.nodes._

import freechips.rocketchip.diplomacy.{AddressSet, TransferSizes}
import freechips.rocketchip.resources.{Device, Resource, ResourceBindings}
import freechips.rocketchip.prci.{NoCrossing}
import freechips.rocketchip.regmapper.{RegField, RegMapper, RegMapperParams, RegMapperInput, RegisterRouter}
import freechips.rocketchip.util.{BundleField, ControlKey, ElaborationArtefacts, GenRegDescsAnno}

import scala.math.min

class TLRegisterRouterExtraBundle(val sourceBits: Int, val sizeBits: Int) extends Bundle {
  val source = UInt((sourceBits max 1).W)
  val size   = UInt((sizeBits max 1).W)
}

case object TLRegisterRouterExtra extends ControlKey[TLRegisterRouterExtraBundle]("tlrr_extra")
case class TLRegisterRouterExtraField(sourceBits: Int, sizeBits: Int) extends BundleField[TLRegisterRouterExtraBundle](TLRegisterRouterExtra, Output(new TLRegisterRouterExtraBundle(sourceBits, sizeBits)), x => {
  x.size   := 0.U
  x.source := 0.U
})

/** TLRegisterNode is a specialized TL SinkNode that encapsulates MMIO registers.
  * It provides functionality for describing and outputting metdata about the registers in several formats.
  * It also provides a concrete implementation of a regmap function that will be used
  * to wire a map of internal registers associated with this node to the node's interconnect port.
  */
case class TLRegisterNode(
    address:     Seq[AddressSet],
    device:      Device,
    deviceKey:   String  = "reg/control",
    concurrency: Int     = 0,
    beatBytes:   Int     = 4,
    undefZero:   Boolean = true,
    executable:  Boolean = false)(
    implicit valName: ValName)
  extends SinkNode(TLImp)(Seq(TLManagerPortParameters.v1(
    Seq(TLManagerParameters.v1(
      address            = address,
      resources          = Seq(Resource(device, deviceKey)),
      executable         = executable,
      supportsGet        = TransferSizes(1, beatBytes),
      supportsPutPartial = TransferSizes(1, beatBytes),
      supportsPutFull    = TransferSizes(1, beatBytes),
      fifoId             = Some(0))), // requests are handled in order
    beatBytes  = beatBytes,
    minLatency = min(concurrency, 1)))) with TLFormatNode // the Queue adds at most one cycle
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

    val fields = TLRegisterRouterExtraField(edge.bundle.sourceBits, edge.bundle.sizeBits) +: a.bits.params.echoFields
    val params = RegMapperParams(log2Up(size/beatBytes), beatBytes, fields)
    val in = Wire(Decoupled(new RegMapperInput(params)))
    in.bits.read  := a.bits.opcode === TLMessages.Get
    in.bits.index := edge.addr_hi(a.bits)
    in.bits.data  := a.bits.data
    in.bits.mask  := a.bits.mask
    Connectable.waiveUnmatched(in.bits.extra, a.bits.echo) match {
      case (lhs, rhs) => lhs :<= rhs
    }

    val a_extra = in.bits.extra(TLRegisterRouterExtra)
    a_extra.source := a.bits.source
    a_extra.size   := a.bits.size

    // Invoke the register map builder
    val out = RegMapper(beatBytes, concurrency, undefZero, in, mapping:_*)

    // No flow control needed
    in.valid  := a.valid
    a.ready   := in.ready
    d.valid   := out.valid
    out.ready := d.ready

    // We must restore the size to enable width adapters to work
    val d_extra = out.bits.extra(TLRegisterRouterExtra)
    d.bits := edge.AccessAck(toSource = d_extra.source, lgSize = d_extra.size)

    // avoid a Mux on the data bus by manually overriding two fields
    d.bits.data := out.bits.data
    Connectable.waiveUnmatched(d.bits.echo, out.bits.extra) match {
      case (lhs, rhs) => lhs :<= rhs
    }

    d.bits.opcode := Mux(out.bits.read, TLMessages.AccessAckData, TLMessages.AccessAck)

    // Tie off unused channels
    bundleIn.b.valid := false.B
    bundleIn.c.ready := true.B
    bundleIn.e.ready := true.B

    genRegDescsJson(mapping:_*)
  }

  def genRegDescsJson(mapping: RegField.Map*): Unit = {
    // Dump out the register map for documentation purposes.
    val base = address.head.base
    val baseHex = s"0x${base.toInt.toHexString}"
    val name = s"${device.describe(ResourceBindings()).name}.At${baseHex}"
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

/** Mix HasTLControlRegMap into any subclass of RegisterRouter to gain helper functions for attaching a device control register map to TileLink.
  * - The intended use case is that controlNode will diplomatically publish a SW-visible device's memory-mapped control registers.
  * - Use the clock crossing helper controlXing to externally connect controlNode to a TileLink interconnect. 
  * - Use the mapping helper function regmap to internally fill out the space of device control registers.
  */
trait HasTLControlRegMap { this: RegisterRouter =>
  protected val controlNode = TLRegisterNode(
    address = address,
    device = device,
    deviceKey = "reg/control",
    concurrency = concurrency,
    beatBytes = beatBytes,
    undefZero = undefZero,
    executable = executable)

  // Externally, this helper should be used to connect the register control port to a bus
  val controlXing: TLInwardClockCrossingHelper = this.crossIn(controlNode)

  // Backwards-compatibility default node accessor with no clock crossing
  lazy val node: TLInwardNode = controlXing(NoCrossing)

  // Internally, this function should be used to populate the control port with registers
  protected def regmap(mapping: RegField.Map*): Unit = { controlNode.regmap(mapping:_*) }
}
