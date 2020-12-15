// See LICENSE.SiFive for license details.

package freechips.rocketchip

import chisel3.internal.sourceinfo.SourceInfo
import chisel3.Data
import _root_.diplomacy.config.Parameters

import scala.language.implicitConversions


/** Diplomacy is a set of abstractions for describing directed, acyclic graphs
  * where parameters will be negotiated between nodes. These abstractions are
  * expressed in the form of abstract classes, traits, and type parameters, which
  * comprises nearly all of the types defined in this package.
  *
  * The [[NodeImp]] ("node implementation") is the main abstract type that associates
  * the type parameters of all other abstract types. Defining a concrete
  * implementation of [[NodeImp]] will therefore determine concrete types for all
  * type parameters. For example, passing in a concrete instance of NodeImp to a
  * SourceNode will fully determine concrete types for all of a SourceNode's type
  * parameters.
  *
  * Specific applications of Diplomacy are expected to either extend these types
  * or to specify concrete types for the type parameters. This allows for
  * creating and associating application-specific node, edge, parameter, and bundle types.
  *
  *
  * =Concepts, metaphors, and mnemonics to help with understanding Diplomacy code=
  *
  * ==Parameter Types==
  *  
  *  There are several types of parameters involved in diplomacy code.
  *  
  *  - Upward-flowing/Downward-flowing Parameters: These parameter types flow along edges and can be considered as the
  *    pre-negotiated, unresolved parameters.
  *  - Edge Parameters: These parameters are the result of the diplomatic negotiation and that is resolved for each edge.
  *    They are metadata, or an abstract concept of the connection represented by their edge, and contain any sort.
  *    These are an abstract concept which carry any sort of conceptual information that is useful to pass along the graph edge.
  *    For example, the full address map visible from a given edge and the supported access types for each memory region.
  *  - "p" Parameters: These are parameters of type [[Parameters]] which are implicit and generally available
  *    in various stages of the diplomacy codebase.
  *    Often they are captured from the context in which a particular diplomatic node or edge is created or bound.
  *  - Bundle Parameters: These parameters are used for actual hardware generation of the [[chisel3.Data]]s which connect
  *    diplomatic components. In contrast to edge parameters, this may carry information like the width of an address
  *    or opcode field.
  *    While they are derived from Edge parameters holding all metadata computed over an edge,
  *    Bundle parameters often contain only concrete information required to create the hardware type,
  *    such as [[freechips.rocketchip.tilelink.TLBundleParameters]] and [[freechips.rocketchip.amba.axi4.AXI4BundleParameters]]
  *  
  * ==Inward/Outward vs. Upward/Downward==
  *
  * Diplomacy defines two dimensions: inward/outward and upward/downward.
  *
  * Inward/outward refer to the direction of edges from the perspective of a given node.
  * For a given node:
  * - Inward refers to edges that point into itself.
  * - Outward refers to edges that point out from itself.
  *  
  * Therefore, a given edge is always described as inward to one node and as outward from another.
  *
  * Upward/downward refer to the direction of the overall directed acyclic graph.
  * Because each each edge is directed, we say that the directions flow from sources (nodes that only have outward edges)
  * downwards to sinks (nodes that only have inward edges), or from sinks upwards to sources.
  * These terms are used in parameter negotiation, where parameters flow both 
  * upwards and downwards on edges. Note that diplomacy avoids terms like "master/slave",
  * "producer/consumer", though these can be defined by the concrete implementations of diplomatic systems.
  * Such terms imply something about the transactional behavior of agents within a protocol,
  * whereas source/sink and up/down refer only to the structure of the graph and the flow of parameters.
  * - Upward refers to the flow of parameters along edges in the upwards direction.
  * - Downward refers to a flow of parameters along edges in the downwards direction.
  *
  * A useful mnemonic for distinguishing between upward and downward is to imagine
  * a diplomatic graph as a literal network of rivers where water flows in the direction of the edges,
  * and parameters that move in the upstream direction,
  * while downward refers to parameters that move in the downstream direction.
  *
  * ==Acronyms==
  *  
  * Diplomacy has some commonly used acronyms described below:
  *   D[IO], U[IO], E[IO], B[IO] are the types of parameters which will be propagated.
  *   D: Downwards -- parameters passed in the same direction as the edge.
  *   U: Upwards -- parameters passed in the opposite direction as the edge.
  *   E: Edge -- resolved (negotiated) parameters describing conceptual information on graph edges.
  *   B: Bundle should extends from [[chisel3.Data]].
  *
  *  {{{
  *
  *
  *         Upwards (a.k.a. towards Sources)          ↓
  *                                                   ↓
  *         inward edge of (parameter) type EI        ↓
  *         created from parameters of type UI and DI ↓
  *         will result in a Bundle of type BI        ↓
  *                                                   ↓
  *                                                ^  ↓  *
  *                                                .  ↓  *
  *                ┌───────────────────────────────.──↓──*───────────────────────────────┐
  *                │                               .  ↓  *                    BaseNode   │
  *                │                               .  ↓  *                    (NodeImp)  │
  *                │                               .  ↓  *                               │
  *                │  ┌────────────────────────────.──↓──*────────────────────────────┐  │
  *                │  │                            .  ↓  *  InwardNode (InwardNodeImp)│  │
  *                │  │                         (mixI)↓  *                            │  │
  *                │  │                            .  ↓  *                            │  │
  *                │  │  Upward-flowing inwards    .  ↓  * Downward-Flowing inwards   │  │
  *                │  │  parameters of type UI     .  ↓  * parameters of type DI      │  │
  *                │  └────────────────────────────.──↓──*────────────────────────────┘  │
  *                │                               .  ↓  *                               │
  *                │                               .  I  v                               │
  *                │                     (mapParamsU)    (mapParamsD)                    │
  *                │                               ^  O  +                               │
  *                │                               :  ↓  +                               │
  *                │  ┌────────────────────────────:──↓──+────────────────────────────┐  │
  *                │  │                            :  ↓  + OutwardNode(OutwardNodeImp)│  │
  *                │  │                            :  ↓ (mixO)                        │  │
  *                │  │                            :  ↓  +                            │  │
  *                │  │  Upward-flowing outwards   :  ↓  + Downward-Flowing outward   │  │
  *                │  │    parameters of type UO   :  ↓  + parameters of type DO      │  │
  *                │  └────────────────────────────:──↓──+────────────────────────────┘  │
  *                │                               :  ↓  +                               │
  *                │                               :  ↓  +                               │
  *                └───────────────────────────────.──↓──*───────────────────────────────┘
  *                                                :  ↓  *
  *                                                :  ↓  v
  *                                                   ↓ outward edge of (parameter) type EO
  *                                                   ↓ created from parameters of type UO and DO
  *                                                   ↓ will result in a Bundle of type BO
  *                                                   ↓
  *                                                   ↓ Downwards (a.k.a. towards Sinks)
  *
  * }}}
  * 
  * == Handles ==
  *
  * Two Diplomatic nodes can be bound together using the `:=` operator or one of
  * its sibling operators. Binding is asymmetric, and the binding operation will
  * connect the outward of one node to the inward of the other.
  *
  * For example, the expression `a := b` will connect the outward of `b` to the
  * inward of `a`.
  *
  * We would like the `:=` operator to have additional properties that make it
  * intuitive to use:
  *
  * 1. It should be chainable, so that `a := b := c` will have the intuitive effect
  *    of binding c to b and b to a. This requires that the return type of `:=` be the
  *    same as its arguments, because the result of one `:=` operation must be
  *    valid as an argument to other `:=` operations.
  *
  * 2. It should be associative, so that `(a := b) := c` is equivalent to `a := (b := c).`
  *    This means that the order in which the bind operations execute does
  *    not matter, even if split across multiple files.
  *
  * 3. `a := b` should only be allowed if and only if `b` allows outward edges and `a`
  *    allows inward edges. This should be preserved even when chaining
  *    operations, and it should ideally be enforced at compile time.
  *
  * [[NodeHandle]] are a way of satisfying all of these properties. A Handle represents
  * the aggregation of a chain of Nodes, and it preserves information about
  * the connectability of the innermost and the outermost sides of the chain.
  *
  * If `b` supports inward edges, then `a := b` returns a [[NodeHandle]] that supports inward
  * edges that go into `b`. If `a` supports outward edges, then `a := b` returns a
  * [[NodeHandle]] that supports outward edges coming out of `a`.
  *
  * ==Node Terms==
  *
  * These are some conventionally used terms for diplomatic Nodes,
  * which describe different common subtypes that certain protocol implementation might utilize:
  *   - Mixed: implies that the inward and outward NodeImp are not the same (some sort of protocol conversion is occurring between the two implementations).
  *   - Adapter: the number of inward and outward edges must be the same.
  *   - Nexus: the number of nodes connecting from either side are unknown until the graph is constructed and can differ from one another.
  *   - Identity: modifies neither the parameters nor the protocol-specific circuitry for the edges that pass through it.
  *   - Source: cannot have inward edges.
  *   - Sink: cannot have outward edges.
  *   - Junction: the number of inward and outward edges must have a fixed ratio to one another
  *   - Ephemeral: a temporary placeholder used for connectivity operations
  */
package object diplomacy
{
  /* compatibility mode to stand alone diplomacy. */
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type LazyModule = _root_.diplomacy.LazyModule
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val LazyModule = _root_.diplomacy.LazyModule
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type LazyModuleImpLike = _root_.diplomacy.LazyModuleImpLike
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type LazyModuleImp = _root_.diplomacy.LazyModuleImp
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type LazyRawModuleImp = _root_.diplomacy.LazyRawModuleImp
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type SimpleLazyModule = _root_.diplomacy.SimpleLazyModule
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type LazyScope = _root_.diplomacy.LazyScope
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val LazyScope = _root_.diplomacy.LazyScope
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type HalfEdge = _root_.diplomacy.HalfEdge
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val HalfEdge = _root_.diplomacy.HalfEdge
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type Dangle = _root_.diplomacy.Dangle
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val Dangle = _root_.diplomacy.Dangle
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type AutoBundle = _root_.diplomacy.AutoBundle
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type ModuleValue[T] = _root_.diplomacy.ModuleValue[T]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val InModuleBody = _root_.diplomacy.InModuleBody
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val MonitorsEnabled = _root_.diplomacy.MonitorsEnabled
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val RenderFlipped = _root_.diplomacy.RenderFlipped
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val RenderedEdge = _root_.diplomacy.RenderedEdge
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type RenderedEdge = _root_.diplomacy.RenderedEdge
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type InwardNodeImp[DI, UI, EI, BI <: Data] = _root_.diplomacy.InwardNodeImp[DI, UI, EI, BI]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type OutwardNodeImp[DO, UO, EO, BO <: Data] = _root_.diplomacy.OutwardNodeImp[DO, UO, EO, BO]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type NodeImp[D, U, EO, EI, B <: Data] = _root_.diplomacy.NodeImp[D, U, EO, EI, B]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type SimpleNodeImp[D, U, E, B <: Data] = _root_.diplomacy.SimpleNodeImp[D, U, E, B]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type BaseNode = _root_.diplomacy.BaseNode
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val BaseNode = _root_.diplomacy.BaseNode
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type FormatEdge = _root_.diplomacy.FormatEdge
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type FormatNode[I <: FormatEdge, O <: FormatEdge] = _root_.diplomacy.FormatNode[I, O]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type NoHandle = _root_.diplomacy.NoHandle
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val NoHandleObject = _root_.diplomacy.NoHandleObject
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type NodeHandle[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data] = _root_.diplomacy.NodeHandle[DI, UI, EI, BI, DO, UO, EO, BO]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val NodeHandle = _root_.diplomacy.NodeHandle
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type NodeHandlePair[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data] = _root_.diplomacy.NodeHandlePair[DI, UI, EI, BI, DO, UO, EO, BO]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type InwardNodeHandle[DI, UI, EI, BI <: Data] = _root_.diplomacy.InwardNodeHandle[DI, UI, EI, BI]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type NodeBinding = _root_.diplomacy.NodeBinding
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val BIND_ONCE = _root_.diplomacy.BIND_ONCE
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val BIND_QUERY = _root_.diplomacy.BIND_QUERY
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val BIND_STAR = _root_.diplomacy.BIND_STAR
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val BIND_FLEX = _root_.diplomacy.BIND_FLEX
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type InwardNode[DI, UI, BI <: Data] = _root_.diplomacy.InwardNode[DI, UI, BI]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type OutwardNodeHandle[DO, UO, EO, BO <: Data] = _root_.diplomacy.OutwardNodeHandle[DO, UO, EO, BO]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type OutwardNode[DO, UO, BO <: Data] = _root_.diplomacy.OutwardNode[DO, UO, BO]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type CycleException = _root_.diplomacy.CycleException
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type StarCycleException = _root_.diplomacy.StarCycleException
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type DownwardCycleException = _root_.diplomacy.DownwardCycleException
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type UpwardCycleException = _root_.diplomacy.UpwardCycleException
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val Edges = _root_.diplomacy.Edges
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type Edges[EI, EO] = _root_.diplomacy.Edges[EI, EO]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type MixedNode[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data] = _root_.diplomacy.MixedNode[DI, UI, EI, BI, DO, UO, EO, BO]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type MixedCustomNode[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data] = _root_.diplomacy.MixedCustomNode[DI, UI, EI, BI, DO, UO, EO, BO]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type CustomNode[D, U, EO, EI, B <: Data] = _root_.diplomacy.CustomNode[D, U, EO, EI, B]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type MixedJunctionNode[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data] = _root_.diplomacy.MixedJunctionNode[DI, UI, EI, BI, DO, UO, EO, BO]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type JunctionNode[D, U, EO, EI, B <: Data] = _root_.diplomacy.JunctionNode[D, U, EO, EI, B]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type MixedAdapterNode[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data] = _root_.diplomacy.MixedAdapterNode[DI, UI, EI, BI, DO, UO, EO, BO]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type AdapterNode[D, U, EO, EI, B <: Data] = _root_.diplomacy.AdapterNode[D, U, EO, EI, B]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type IdentityNode[D, U, EO, EI, B <: Data] = _root_.diplomacy.IdentityNode[D, U, EO, EI, B]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type EphemeralNode[D, U, EO, EI, B <: Data] = _root_.diplomacy.EphemeralNode[D, U, EO, EI, B]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type MixedNexusNode[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data] = _root_.diplomacy.MixedNexusNode[DI, UI, EI, BI, DO, UO, EO, BO]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type NexusNode[D, U, EO, EI, B <: Data] = _root_.diplomacy.NexusNode[D, U, EO, EI, B]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type SourceNode[D, U, EO, EI, B <: Data] = _root_.diplomacy.SourceNode[D, U, EO, EI, B]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type SinkNode[D, U, EO, EI, B <: Data] = _root_.diplomacy.SinkNode[D, U, EO, EI, B]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type BundleBridgeParams[T <: Data] = _root_.diplomacy.BundleBridgeParams[T]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val BundleBridgeParams = _root_.diplomacy.BundleBridgeParams
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type BundleBridgeEdgeParams[T <: Data] = _root_.diplomacy.BundleBridgeEdgeParams[T]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type BundleBridgeImp[T <: Data] = _root_.diplomacy.BundleBridgeImp[T]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type BundleBridgeSink[T <: Data] = _root_.diplomacy.BundleBridgeSink[T]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val BundleBridgeSink = _root_.diplomacy.BundleBridgeSink
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type BundleBridgeSource[T <: Data] = _root_.diplomacy.BundleBridgeSource[T]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val BundleBridgeSource = _root_.diplomacy.BundleBridgeSource
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type BundleBridgeIdentityNode[T <: Data] = _root_.diplomacy.BundleBridgeIdentityNode[T]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val BundleBridgeIdentityNode = _root_.diplomacy.BundleBridgeIdentityNode
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type BundleBridgeEphemeralNode[T <: Data] = _root_.diplomacy.BundleBridgeEphemeralNode[T]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val BundleBridgeEphemeralNode = _root_.diplomacy.BundleBridgeEphemeralNode
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val BundleBridgeNameNode = _root_.diplomacy.BundleBridgeNameNode
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type BundleBridgeNexusNode[T <: Data] = _root_.diplomacy.BundleBridgeNexusNode[T]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type BundleBridgeNexus[T <: Data] = _root_.diplomacy.BundleBridgeNexus[T]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val BundleBridgeNexus = _root_.diplomacy.BundleBridgeNexus
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val BundleBroadcast = _root_.diplomacy.BundleBroadcast
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type ValName = _root_.diplomacy.ValName
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  val ValName = _root_.diplomacy.ValName
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type SimpleNodeHandle[D, U, E, B <: Chisel.Data] = _root_.diplomacy.SimpleNodeHandle[D, U, E, B]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type AnyMixedNode = _root_.diplomacy.AnyMixedNode
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type BundleBridgeInwardNode[T <: Data] = _root_.diplomacy.BundleBridgeInwardNode[T]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type BundleBridgeOutwardNode[T <: Data] = _root_.diplomacy.BundleBridgeOutwardNode[T]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  type BundleBridgeNode[T <: Data] = _root_.diplomacy.BundleBridgeNode[T]
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  def sourceLine(sourceInfo: SourceInfo, prefix: String = " (", suffix: String = ")") = _root_.diplomacy.sourceLine(sourceInfo, prefix, suffix)
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  def EnableMonitors[T](body: Parameters => T)(implicit p: Parameters) = _root_.diplomacy.EnableMonitors(body)(p)
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  def DisableMonitors[T](body: Parameters => T)(implicit p: Parameters) = _root_.diplomacy.DisableMonitors(body)(p)
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  def FlipRendering[T](body: Parameters => T)(implicit p: Parameters) = _root_.diplomacy.FlipRendering(body)(p)
  @deprecated("diplomacy and config is moved from rocket-chip to standalone package.", "1.2")
  implicit def moduleValue[T](value: ModuleValue[T]): T = _root_.diplomacy.moduleValue(value)

  def bitIndexes(x: BigInt, tail: Seq[Int] = Nil): Seq[Int] = {
    require (x >= 0)
    if (x == 0) {
      tail.reverse
    } else {
      val lowest = x.lowestSetBit
      bitIndexes(x.clearBit(lowest), lowest +: tail)
    }
  }

  implicit class BigIntHexContext(private val sc: StringContext) extends AnyVal {
    def x(args: Any*): BigInt = {
      val orig = sc.s(args: _*)
      BigInt(orig.replace("_", ""), 16)
    }
  }

  type PropertyOption = Option[(String, Seq[ResourceValue])]
  type PropertyMap = Iterable[(String, Seq[ResourceValue])]

  implicit class IntToProperty(x: Int) {
    def asProperty: Seq[ResourceValue] = Seq(ResourceInt(BigInt(x)))
  }

  implicit class BigIntToProperty(x: BigInt) {
    def asProperty: Seq[ResourceValue] = Seq(ResourceInt(x))
  }

  implicit class StringToProperty(x: String) {
    def asProperty: Seq[ResourceValue] = Seq(ResourceString(x))
  }

  implicit class DeviceToProperty(x: Device) {
    def asProperty: Seq[ResourceValue] = Seq(ResourceReference(x.label))
  }

  implicit def noCrossing(value: NoCrossing.type): ClockCrossingType = SynchronousCrossing(BufferParams.none)
}
