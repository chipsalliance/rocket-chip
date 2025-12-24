// See LICENSE.SiFive for license details.

package freechips.rocketchip

import chisel3.Data
import chisel3.experimental.{SourceInfo, SourceLine}
import org.chipsalliance.cde.config.Parameters

import scala.language.implicitConversions

/** Rocketchip Specific Diplomacy code. All other Diplomacy core functionality has been moved to standalone diplomacy
  */
package object diplomacy {
  def bitIndexes(x: BigInt, tail: Seq[Int] = Nil): Seq[Int] = {
    require (x >= 0)
    if (x == 0) {
      tail.reverse
    } else {
      val lowest = x.lowestSetBit
      bitIndexes(x.clearBit(lowest), lowest +: tail)
    }
  }

  // TODO - Remove compatibility layer for deprecated diplomacy api once all local references are moved to standalone diplomacy lib.
  // package.scala
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  def sourceLine(sourceInfo: SourceInfo, prefix: String = " (", suffix: String = ")") = sourceInfo match {
    case SourceLine(filename, line, col) => s"$prefix$filename:$line:$col$suffix"
    case _                               => ""
  }
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  def EnableMonitors[T](
    body:       Parameters => T
  )(
    implicit p: Parameters
  ) = _root_.org.chipsalliance.diplomacy.EnableMonitors(body)(p)
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  def DisableMonitors[T](
    body:       Parameters => T
  )(
    implicit p: Parameters
  ) = _root_.org.chipsalliance.diplomacy.DisableMonitors(body)(p)
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  def FlipRendering[T](
    body:       Parameters => T
  )(
    implicit p: Parameters
  ) = _root_.org.chipsalliance.diplomacy.FlipRendering(body)(p)
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  implicit def moduleValue[T](value: ModuleValue[T]): T = _root_.org.chipsalliance.diplomacy.moduleValue(value)

// Clone.scala
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val CloneLazyModule = _root_.org.chipsalliance.diplomacy.lazymodule.CloneLazyModule

// ValName.scala
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type ValName = _root_.org.chipsalliance.diplomacy.ValName
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  def ValName(value:                String)          = _root_.org.chipsalliance.diplomacy.ValName(value)
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  implicit def SourcecodeNameExt(x: sourcecode.Name): _root_.org.chipsalliance.diplomacy.SourcecodeNameExt = _root_.org.chipsalliance.diplomacy.SourcecodeNameExt(x)

// LazyModule.scala
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type LazyModule = _root_.org.chipsalliance.diplomacy.lazymodule.LazyModule
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val LazyModule = _root_.org.chipsalliance.diplomacy.lazymodule.LazyModule
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type LazyModuleImpLike = _root_.org.chipsalliance.diplomacy.lazymodule.LazyModuleImpLike
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type LazyModuleImp     = _root_.org.chipsalliance.diplomacy.lazymodule.LazyModuleImp
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type LazyRawModuleImp  = _root_.org.chipsalliance.diplomacy.lazymodule.LazyRawModuleImp
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type SimpleLazyModule  = _root_.org.chipsalliance.diplomacy.lazymodule.SimpleLazyModule
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type LazyScope         = _root_.org.chipsalliance.diplomacy.lazymodule.LazyScope
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val LazyScope = _root_.org.chipsalliance.diplomacy.lazymodule.LazyScope
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type ModuleValue[T] = _root_.org.chipsalliance.diplomacy.lazymodule.ModuleValue[T]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val InModuleBody = _root_.org.chipsalliance.diplomacy.lazymodule.InModuleBody

// Nodes.scala
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type Dangle = _root_.org.chipsalliance.diplomacy.nodes.Dangle
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val Dangle = _root_.org.chipsalliance.diplomacy.nodes.Dangle
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type HalfEdge = _root_.org.chipsalliance.diplomacy.nodes.HalfEdge
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val HalfEdge        = _root_.org.chipsalliance.diplomacy.nodes.HalfEdge
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val MonitorsEnabled = _root_.org.chipsalliance.diplomacy.nodes.MonitorsEnabled
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val RenderFlipped   = _root_.org.chipsalliance.diplomacy.nodes.RenderFlipped
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val RenderedEdge    = _root_.org.chipsalliance.diplomacy.nodes.RenderedEdge
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type RenderedEdge                           = _root_.org.chipsalliance.diplomacy.nodes.RenderedEdge
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type InwardNodeImp[DI, UI, EI, BI <: Data]  = _root_.org.chipsalliance.diplomacy.nodes.InwardNodeImp[DI, UI, EI, BI]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type OutwardNodeImp[DO, UO, EO, BO <: Data] = _root_.org.chipsalliance.diplomacy.nodes.OutwardNodeImp[DO, UO, EO, BO]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type NodeImp[D, U, EO, EI, B <: Data]       = _root_.org.chipsalliance.diplomacy.nodes.NodeImp[D, U, EO, EI, B]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type SimpleNodeImp[D, U, E, B <: Data]      = _root_.org.chipsalliance.diplomacy.nodes.SimpleNodeImp[D, U, E, B]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type BaseNode                               = _root_.org.chipsalliance.diplomacy.nodes.BaseNode
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val BaseNode = _root_.org.chipsalliance.diplomacy.nodes.BaseNode
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type FormatEdge                                   = _root_.org.chipsalliance.diplomacy.nodes.FormatEdge
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type FormatNode[I <: FormatEdge, O <: FormatEdge] = _root_.org.chipsalliance.diplomacy.nodes.FormatNode[I, O]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type NoHandle                                     = _root_.org.chipsalliance.diplomacy.nodes.NoHandle
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val NoHandleObject = _root_.org.chipsalliance.diplomacy.nodes.NoHandleObject
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type NodeHandle[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data] =
    _root_.org.chipsalliance.diplomacy.nodes.NodeHandle[DI, UI, EI, BI, DO, UO, EO, BO]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val NodeHandle = _root_.org.chipsalliance.diplomacy.nodes.NodeHandle
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type NodeHandlePair[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data] =
    _root_.org.chipsalliance.diplomacy.nodes.NodeHandlePair[DI, UI, EI, BI, DO, UO, EO, BO]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type InwardNodeHandle[DI, UI, EI, BI <: Data]                       =
    _root_.org.chipsalliance.diplomacy.nodes.InwardNodeHandle[DI, UI, EI, BI]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type NodeBinding                                                    = _root_.org.chipsalliance.diplomacy.nodes.NodeBinding
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val BIND_ONCE  = _root_.org.chipsalliance.diplomacy.nodes.BIND_ONCE
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val BIND_QUERY = _root_.org.chipsalliance.diplomacy.nodes.BIND_QUERY
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val BIND_STAR  = _root_.org.chipsalliance.diplomacy.nodes.BIND_STAR
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val BIND_FLEX  = _root_.org.chipsalliance.diplomacy.nodes.BIND_FLEX
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type InwardNode[DI, UI, BI <: Data]            = _root_.org.chipsalliance.diplomacy.nodes.InwardNode[DI, UI, BI]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type OutwardNodeHandle[DO, UO, EO, BO <: Data] =
    _root_.org.chipsalliance.diplomacy.nodes.OutwardNodeHandle[DO, UO, EO, BO]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type OutwardNode[DO, UO, BO <: Data]           = _root_.org.chipsalliance.diplomacy.nodes.OutwardNode[DO, UO, BO]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type CycleException                            = _root_.org.chipsalliance.diplomacy.nodes.CycleException
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type StarCycleException                        = _root_.org.chipsalliance.diplomacy.nodes.StarCycleException
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type DownwardCycleException                    = _root_.org.chipsalliance.diplomacy.nodes.DownwardCycleException
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type UpwardCycleException                      = _root_.org.chipsalliance.diplomacy.nodes.UpwardCycleException
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val Edges = _root_.org.chipsalliance.diplomacy.nodes.Edges
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type Edges[EI, EO]                                                     = _root_.org.chipsalliance.diplomacy.nodes.Edges[EI, EO]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type MixedCustomNode[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data]   =
    _root_.org.chipsalliance.diplomacy.nodes.MixedCustomNode[DI, UI, EI, BI, DO, UO, EO, BO]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type CustomNode[D, U, EO, EI, B <: Data]                               = _root_.org.chipsalliance.diplomacy.nodes.CustomNode[D, U, EO, EI, B]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type MixedJunctionNode[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data] =
    _root_.org.chipsalliance.diplomacy.nodes.MixedJunctionNode[DI, UI, EI, BI, DO, UO, EO, BO]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type JunctionNode[D, U, EO, EI, B <: Data]                             = _root_.org.chipsalliance.diplomacy.nodes.JunctionNode[D, U, EO, EI, B]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type MixedAdapterNode[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data]  =
    _root_.org.chipsalliance.diplomacy.nodes.MixedAdapterNode[DI, UI, EI, BI, DO, UO, EO, BO]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type AdapterNode[D, U, EO, EI, B <: Data]                              = _root_.org.chipsalliance.diplomacy.nodes.AdapterNode[D, U, EO, EI, B]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type IdentityNode[D, U, EO, EI, B <: Data]                             = _root_.org.chipsalliance.diplomacy.nodes.IdentityNode[D, U, EO, EI, B]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type EphemeralNode[D, U, EO, EI, B <: Data]                            = _root_.org.chipsalliance.diplomacy.nodes.EphemeralNode[D, U, EO, EI, B]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type MixedNexusNode[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data]    =
    _root_.org.chipsalliance.diplomacy.nodes.MixedNexusNode[DI, UI, EI, BI, DO, UO, EO, BO]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type NexusNode[D, U, EO, EI, B <: Data]                                = _root_.org.chipsalliance.diplomacy.nodes.NexusNode[D, U, EO, EI, B]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type SourceNode[D, U, EO, EI, B <: Data]                               = _root_.org.chipsalliance.diplomacy.nodes.SourceNode[D, U, EO, EI, B]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type SinkNode[D, U, EO, EI, B <: Data]                                 = _root_.org.chipsalliance.diplomacy.nodes.SinkNode[D, U, EO, EI, B]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type SimpleNodeHandle[D, U, E, B <: Data]                              = _root_.org.chipsalliance.diplomacy.nodes.SimpleNodeHandle[D, U, E, B]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type AnyMixedNode                                                      = _root_.org.chipsalliance.diplomacy.nodes.AnyMixedNode

// BundleBridge.scala
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type BundleBridgeParams[T <: Data] = _root_.org.chipsalliance.diplomacy.bundlebridge.BundleBridgeParams[T]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val BundleBridgeParams = _root_.org.chipsalliance.diplomacy.bundlebridge.BundleBridgeParams
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type BundleBridgeEdgeParams[T <: Data] = _root_.org.chipsalliance.diplomacy.bundlebridge.BundleBridgeEdgeParams[T]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type BundleBridgeImp[T <: Data]        = _root_.org.chipsalliance.diplomacy.bundlebridge.BundleBridgeImp[T]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type BundleBridgeSink[T <: Data]       = _root_.org.chipsalliance.diplomacy.bundlebridge.BundleBridgeSink[T]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val BundleBridgeSink = _root_.org.chipsalliance.diplomacy.bundlebridge.BundleBridgeSink
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type BundleBridgeSource[T <: Data] = _root_.org.chipsalliance.diplomacy.bundlebridge.BundleBridgeSource[T]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val BundleBridgeSource = _root_.org.chipsalliance.diplomacy.bundlebridge.BundleBridgeSource
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type BundleBridgeIdentityNode[T <: Data] = _root_.org.chipsalliance.diplomacy.bundlebridge.BundleBridgeIdentityNode[T]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val BundleBridgeIdentityNode = _root_.org.chipsalliance.diplomacy.bundlebridge.BundleBridgeIdentityNode
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type BundleBridgeEphemeralNode[T <: Data] =
    _root_.org.chipsalliance.diplomacy.bundlebridge.BundleBridgeEphemeralNode[T]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val BundleBridgeEphemeralNode                     = _root_.org.chipsalliance.diplomacy.bundlebridge.BundleBridgeEphemeralNode
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  def BundleBridgeNameNode[T <: Data](name: String) =
    _root_.org.chipsalliance.diplomacy.bundlebridge.BundleBridgeNameNode[T](name)
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type BundleBridgeNexusNode[T <: Data] = _root_.org.chipsalliance.diplomacy.bundlebridge.BundleBridgeNexusNode[T]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type BundleBridgeNexus[T <: Data]     = _root_.org.chipsalliance.diplomacy.bundlebridge.BundleBridgeNexus[T]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  val BundleBridgeNexus = _root_.org.chipsalliance.diplomacy.bundlebridge.BundleBridgeNexus
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  def BundleBroadcast[T <: Data](
    name:                Option[String] = None,
    registered:          Boolean = false,
    default:             Option[() => T] = None,
    inputRequiresOutput: Boolean = false, // when false, connecting a source does not mandate connecting a sink
    shouldBeInlined:     Boolean = true
  )(
    implicit p:          Parameters
  ) = _root_.org.chipsalliance.diplomacy.bundlebridge
    .BundleBroadcast[T](name, registered, default, inputRequiresOutput, shouldBeInlined)(p)
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type BundleBridgeInwardNode[T <: Data] = _root_.org.chipsalliance.diplomacy.bundlebridge.BundleBridgeInwardNode[T]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type BundleBridgeOutwardNode[T <: Data] = _root_.org.chipsalliance.diplomacy.bundlebridge.BundleBridgeOutwardNode[T]
  @deprecated("Diplomacy has been split to a standalone library", "rocketchip 2.0.0")
  type BundleBridgeNode[T <: Data]        = _root_.org.chipsalliance.diplomacy.bundlebridge.BundleBridgeNode[T]

// Resources.scala
  @deprecated("Use freechips.rocketchip.resources.ResourcePermissions", "rocketchip 2.0.0")
  type ResourcePermissions = freechips.rocketchip.resources.ResourcePermissions
  @deprecated("Use freechips.rocketchip.resources.Resource", "rocketchip 2.0.0")
  type Resource = freechips.rocketchip.resources.Resource
  @deprecated("Use freechips.rocketchip.resources.Resource", "rocketchip 2.0.0")
  val Resource = freechips.rocketchip.resources.Resource
  @deprecated("Use freechips.rocketchip.resources.ResourceAnchors", "rocketchip 2.0.0")
  val ResourceAnchors = freechips.rocketchip.resources.ResourceAnchors
  @deprecated("Use freechips.rocketchip.resources.ResourceAlias", "rocketchip 2.0.0")
  type ResourceAlias = freechips.rocketchip.resources.ResourceAlias
  @deprecated("Use freechips.rocketchip.resources.ResourceAlias", "rocketchip 2.0.0")
  val ResourceAlias = freechips.rocketchip.resources.ResourceAlias
  @deprecated("Use freechips.rocketchip.resources.ResourceMapping", "rocketchip 2.0.0")
  type ResourceMapping = freechips.rocketchip.resources.ResourceMapping
  @deprecated("Use freechips.rocketchip.resources.ResourceMapping", "rocketchip 2.0.0")
  val ResourceMapping = freechips.rocketchip.resources.ResourceMapping
  @deprecated("Use freechips.rocketchip.resources.ResourceMap", "rocketchip 2.0.0")
  type ResourceMap = freechips.rocketchip.resources.ResourceMap
  @deprecated("Use freechips.rocketchip.resources.ResourceMap", "rocketchip 2.0.0")
  val ResourceMap = freechips.rocketchip.resources.ResourceMap
  @deprecated("Use freechips.rocketchip.resources.ResourceReference", "rocketchip 2.0.0")
  type ResourceReference = freechips.rocketchip.resources.ResourceReference
  @deprecated("Use freechips.rocketchip.resources.ResourceReference", "rocketchip 2.0.0")
  val ResourceReference = freechips.rocketchip.resources.ResourceReference
  @deprecated("Use freechips.rocketchip.resources.ResourceAddress", "rocketchip 2.0.e0")
  type ResourceAddress = freechips.rocketchip.resources.ResourceAddress
  @deprecated("Use freechips.rocketchip.resources.ResourceAddress", "rocketchip 2.0.0")
  val ResourceAddress = freechips.rocketchip.resources.ResourceAddress
  @deprecated("Use freechips.rocketchip.resources.ResourceValue", "rocketchip 2.0.0")
  type ResourceValue = freechips.rocketchip.resources.ResourceValue
  @deprecated("Use freechips.rocketchip.resources.ResourceBinding", "rocketchip 2.0.0")
  val ResourceBinding = freechips.rocketchip.resources.ResourceBinding
  @deprecated("Use freechips.rocketchip.resources.ResourceBindings", "rocketchip 2.0.0")
  type ResourceBindings = freechips.rocketchip.resources.ResourceBindings
  @deprecated("Use freechips.rocketchip.resources.BindingScope", "rocketchip 2.0.0")
  type BindingScope = freechips.rocketchip.resources.BindingScope
  @deprecated("Use freechips.rocketchip.resources.Binding", "rocketchip 2.0.0")
  type Binding = freechips.rocketchip.resources.Binding
  @deprecated("Use freechips.rocketchip.resources.Binding", "rocketchip 2.0.0")
  val Binding = freechips.rocketchip.resources.Binding
  @deprecated("Use freechips.rocketchip.resources.ResourceInt", "rocketchip 2.0.0")
  type ResourceInt = freechips.rocketchip.resources.ResourceInt
  @deprecated("Use freechips.rocketchip.resources.ResourceInt", "rocketchip 2.0.0")
  val ResourceInt = freechips.rocketchip.resources.ResourceInt
  @deprecated("Use freechips.rocketchip.resources.ResourceString", "rocketchip 2.0.0")
  type ResourceString = freechips.rocketchip.resources.ResourceString
  @deprecated("Use freechips.rocketchip.resources.ResourceString", "rocketchip 2.0.0")
  val ResourceString = freechips.rocketchip.resources.ResourceString
  @deprecated("Use freechips.rocketchip.resources.SimpleDevice", "rocketchip 2.0.0")
  type SimpleDevice = freechips.rocketchip.resources.SimpleDevice
  @deprecated("Use freechips.rocketchip.resources.MemoryDevice", "rocketchip 2.0.0")
  type MemoryDevice = freechips.rocketchip.resources.MemoryDevice
  @deprecated("Use freechips.rocketchip.resources.Device", "rocketchip 2.0.0")
  type Device = freechips.rocketchip.resources.Device
  @deprecated("Use freechips.rocketchip.resources.Description", "rocketchip 2.0.0")
  type Description = freechips.rocketchip.resources.Description
  @deprecated("Use freechips.rocketchip.resources.Description", "rocketchip 2.0.0")
  val Description = freechips.rocketchip.resources.Description
  @deprecated("Use freechips.rocketchip.resources.SimpleBus", "rocketchip 2.0.0")
  type SimpleBus = freechips.rocketchip.resources.SimpleBus
}
