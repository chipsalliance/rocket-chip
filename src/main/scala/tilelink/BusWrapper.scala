// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci._
import freechips.rocketchip.util._

/** Specifies widths of various attachement points in the SoC */
trait HasTLBusParams {
  def beatBytes: Int
  def blockBytes: Int

  def beatBits: Int = beatBytes * 8
  def blockBits: Int = blockBytes * 8
  def blockBeats: Int = blockBytes / beatBytes
  def blockOffset: Int = log2Up(blockBytes)

  def dtsFrequency: Option[BigInt]
  def fixedClockOpt = dtsFrequency.map(f => ClockParameters(freqMHz = f.toDouble / 1000000.0))

  require (isPow2(beatBytes))
  require (isPow2(blockBytes))
}

abstract class TLBusWrapper(params: HasTLBusParams, val busName: String)(implicit p: Parameters)
    extends ClockDomain with HasTLBusParams {

  private val clockGroupAggregator = LazyModule(new ClockGroupAggregator(busName)).suggestName(busName + "_clock_groups")
  private val clockGroup = LazyModule(new ClockGroup(busName))
  val clockGroupNode = clockGroupAggregator.node // other bus clock groups attach here
  val clockNode = clockGroup.node
  val fixedClockNode = FixedClockBroadcast(fixedClockOpt) // device clocks attach here
  private val clockSinkNode = ClockSinkNode(List(ClockSinkParameters(take = fixedClockOpt)))

  clockGroup.node := clockGroupAggregator.node
  fixedClockNode := clockGroup.node // first member of group is always domain's own clock
  clockSinkNode := fixedClockNode

  def clockBundle = clockSinkNode.in.head._1
  def beatBytes = params.beatBytes
  def blockBytes = params.blockBytes
  def dtsFrequency = params.dtsFrequency
  val dtsClk = fixedClockNode.fixedClockResources(s"${busName}_clock").flatten.headOption

  /* If you violate this requirement, you will have a rough time.
   * The codebase is riddled with the assumption that this is true.
   */
  require(blockBytes >= beatBytes)

  def inwardNode: TLInwardNode
  def outwardNode: TLOutwardNode
  def busView: TLEdge
  def unifyManagers: List[TLManagerParameters] = ManagerUnification(busView.manager.managers)
  def crossOutHelper = this.crossOut(outwardNode)(ValName("bus_xing"))
  def crossInHelper = this.crossIn(inwardNode)(ValName("bus_xing"))

  def to[T](name: String)(body: => T): T = {
    this { LazyScope(s"coupler_to_${name}") { body } }
  }

  def from[T](name: String)(body: => T): T = {
    this { LazyScope(s"coupler_from_${name}") { body } }
  }

  def coupleTo[T](name: String)(gen: TLOutwardNode => T): T =
    to(name) { gen(outwardNode) }

  def coupleFrom[T](name: String)(gen: TLInwardNode => T): T =
    from(name) { gen(inwardNode) }

  def crossToBus(bus: TLBusWrapper, xType: ClockCrossingType)(implicit asyncClockGroupNode: ClockGroupEphemeralNode): NoHandle = {
    bus.clockGroupNode := asyncMux(xType, asyncClockGroupNode, this.clockGroupNode)
    coupleTo(s"bus_named_${bus.busName}") {
      bus.crossInHelper(xType) :*= TLWidthWidget(beatBytes) :*= _
    }
  }

  def crossFromBus(bus: TLBusWrapper, xType: ClockCrossingType)(implicit asyncClockGroupNode: ClockGroupEphemeralNode): NoHandle = {
    bus.clockGroupNode := asyncMux(xType, asyncClockGroupNode, this.clockGroupNode)
    coupleFrom(s"bus_named_${bus.busName}") {
      _ :=* TLWidthWidget(bus.beatBytes) :=* bus.crossOutHelper(xType)
    }
  }
}

trait CanAttachTLSlaves extends HasTLBusParams { this: TLBusWrapper =>
  @deprecated("Replace with e.g. bus.coupleTo(s\"slave_named_${name}\"){ slave.controlXing(NoCrossing) :*= _ }", "rocket-chip 1.3")
  def toSlave[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLNameNode(name)): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) { gen :*= TLBuffer(buffer) :*= outwardNode }
  }

  @deprecated("Replace with e.g. bus.coupleTo(s\"slave_named_${name}\"){ node :*= TLFragmenter(bus.beatBytes, bus.blockBytes) :*= _ }", "rocket-chip 1.3")
  def toVariableWidthSlaveNode(name: Option[String] = None, buffer: BufferParams = BufferParams.none)(node: TLInwardNode) {
    toVariableWidthSlaveNodeOption(name, buffer)(Some(node))
  }

  @deprecated("Replace with e.g. node.foreach { n => bus.coupleTo(s\"slave_named_${name}\"){ b => n :*= TLFragmenter(bus.beatBytes, bus.blockBytes) :*= b } }", "rocket-chip 1.3")
  def toVariableWidthSlaveNodeOption(name: Option[String] = None, buffer: BufferParams = BufferParams.none)(node: Option[TLInwardNode]) {
    node foreach { n => to("slave" named name) {
      n :*= TLFragmenter(beatBytes, blockBytes) :*= TLBuffer(buffer) :*= outwardNode
    }}
  }

  @deprecated("Replace with e.g. bus.coupleTo(s\"slave_named_${name}\"){ slave.controlXing(NoCrossing) :*= TLFragmenter(bus.beatBytes, bus.blockBytes) :*= _ }", "rocket-chip 1.3")
  def toVariableWidthSlave[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLNameNode(name)): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) {
      gen :*= TLFragmenter(beatBytes, blockBytes) :*= TLBuffer(buffer) :*= outwardNode
    }
  }

  @deprecated("Replace with e.g. bus.coupleTo(s\"slave_named_${name}\"){ slave.control :*= TLWidthWidget(bus.beatBytes) :*= _ }", "rocket-chip 1.3")
  def toFixedWidthSlaveNode(name: Option[String] = None, buffer: BufferParams = BufferParams.none)(gen: TLInwardNode) {
    to("slave" named name) { gen :*= TLWidthWidget(beatBytes) :*= TLBuffer(buffer) :*= outwardNode }
  }

  @deprecated("Replace with e.g. bus.coupleTo(s\"slave_named_${name}\"){ slave.controlXing(NoCrossing) :*= TLWidthWidget(bus.beatBytes) :*= _ }", "rocket-chip 1.3")
  def toFixedWidthSlave[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLNameNode(name)): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) { gen :*= TLWidthWidget(beatBytes) :*= TLBuffer(buffer) :*= outwardNode }
  }

  def toFixedWidthSingleBeatSlaveNode
      (widthBytes: Int, name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: TLInwardNode) {
    to("slave" named name) {
      gen :*= TLFragmenter(widthBytes, blockBytes) :*= TLWidthWidget(beatBytes) :*= TLBuffer(buffer) :*= outwardNode
    }
  }

  @deprecated("Replace with e.g. bus.coupleTo(s\"slave_named_${name}\"){ slave.controlXing(NoCrossing) :*= TLFragmenter(widthBytes, bus.blockBytes) :*= TLWidthWidget(bus.beatBytes) :*= _ }", "rocket-chip 1.3")
  def toFixedWidthSingleBeatSlave[D,U,E,B <: Data]
      (widthBytes: Int, name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLNameNode(name)): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) {
      gen :*= TLFragmenter(widthBytes, blockBytes) :*= TLWidthWidget(beatBytes) :*= TLBuffer(buffer) :*= outwardNode
    }
  }

  @deprecated("Replace with e.g. bus.coupleTo(s\"slave_named_${name}\"){ slave.controlXing(NoCrossing) :*= TLFragmenter(bus.beatBytes, <maxXferBytes>) :*= _ }", "rocket-chip 1.3")
  def toLargeBurstSlave[D,U,E,B <: Data]
      (maxXferBytes: Int, name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLNameNode(name)): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) {
      gen :*= TLFragmenter(beatBytes, maxXferBytes) :*= TLBuffer(buffer) :*= outwardNode
    }
  }

  @deprecated("Replace with e.g. bus.coupleTo(s\"slave_named_${name}\"){ slave.controlXing(NoCrossing) :*= TLFragmenter(bus.beatBytes, <maxXferBytes>) :*= _ }", "rocket-chip 1.3")
  def toFixedWidthPort[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLNameNode(name)): OutwardNodeHandle[D,U,E,B] = {
    to("port" named name) {
      gen := TLWidthWidget(beatBytes) :*= TLBuffer(buffer) :*= outwardNode
    }
  }
}

trait CanAttachTLMasters extends HasTLBusParams { this: TLBusWrapper =>
  @deprecated("Replace with e.g. bus.coupleFrom(s\"master_named_${name}\"){ _ :=* TLFIFOFixer(TLFIFOFixer.all) :=* node }", "rocket-chip 1.3")
  def fromMasterNode
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: TLOutwardNode) {
    from("master" named name) {
      inwardNode :=* TLBuffer(buffer) :=* TLFIFOFixer(TLFIFOFixer.all) :=* gen
    }
  }

  @deprecated("Replace with e.g. bus.coupleFrom(s\"master_named_${name}\"){ _ :=* TLFIFOFixer(TLFIFOFixer.all) :=* master.node }", "rocket-chip 1.3")
  def fromMaster[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[D,U,E,B,TLClientPortParameters,TLManagerPortParameters,TLEdgeOut,TLBundle] =
        TLNameNode(name)): InwardNodeHandle[D,U,E,B] = {
    from("master" named name) {
      inwardNode :=* TLBuffer(buffer) :=* TLFIFOFixer(TLFIFOFixer.all) :=* gen
    }
  }

  // TODO also deprecate this once debug module isn't using it
  //@deprecated("Replace with e.g. bus.coupleFrom(s\"port_named_${name}\"){ _ :=* TLFIFOFixer(TLFIFOFixer.all) :=* master.node }", "rocket-chip 1.3")
  def fromPort[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[D,U,E,B,TLClientPortParameters,TLManagerPortParameters,TLEdgeOut,TLBundle] =
        TLNameNode(name)): InwardNodeHandle[D,U,E,B] = {
    from("port" named name) {
      inwardNode :=* TLBuffer(buffer) :=* TLFIFOFixer(TLFIFOFixer.all) :=* gen
    }
  }

  @deprecated("Replace with e.g. bus.coupleFrom(s\"coherent_master_named_${name}\"){ _ :=* TLFIFOFixer(TLFIFOFixer.all) :=* master.node }", "rocket-chip 1.3")
  def fromCoherentMaster[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[D,U,E,B,TLClientPortParameters,TLManagerPortParameters,TLEdgeOut,TLBundle] =
        TLNameNode(name)): InwardNodeHandle[D,U,E,B] = {
    from("coherent_master" named name) {
      inwardNode :=* TLBuffer(buffer) :=* TLFIFOFixer(TLFIFOFixer.all) :=* gen
    }
  }
}

trait HasTLXbarPhy { this: TLBusWrapper =>
  private val xbar = LazyModule(new TLXbar).suggestName(busName + "_xbar")

  def inwardNode: TLInwardNode = xbar.node
  def outwardNode: TLOutwardNode = xbar.node
  def busView: TLEdge = xbar.node.edges.in.head
}
