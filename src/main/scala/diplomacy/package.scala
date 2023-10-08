// See LICENSE.SiFive for license details.

package freechips.rocketchip

import chisel3.experimental.{SourceInfo, SourceLine}
import chisel3.Data
import org.chipsalliance.cde.config.Parameters
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
  type SimpleNodeHandle[D, U, E, B <: Data] = NodeHandle[D, U, E, B, D, U, E, B]
  type AnyMixedNode = MixedNode[_, _, _, _ <: Data, _, _, _, _ <: Data]

  def sourceLine(sourceInfo: SourceInfo, prefix: String = " (", suffix: String = ")") = sourceInfo match {
    case SourceLine(filename, line, col) => s"$prefix$filename:$line:$col$suffix"
    case _ => ""
  }

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

  def EnableMonitors[T](body: Parameters => T)(implicit p: Parameters) = body(p.alterPartial {
    case MonitorsEnabled => true
  })
  def DisableMonitors[T](body: Parameters => T)(implicit p: Parameters) = body(p.alterPartial {
    case MonitorsEnabled => false
  })
  def FlipRendering[T](body: Parameters => T)(implicit p: Parameters) = body(p.alterPartial {
    case RenderFlipped => !p(RenderFlipped)
  })

  implicit def moduleValue[T](value: ModuleValue[T]): T = value.getWrappedValue

  implicit def noCrossing(value: NoCrossing.type): ClockCrossingType = SynchronousCrossing(BufferParams.none)

  type BundleBridgeInwardNode[T <: Data] = InwardNodeHandle[BundleBridgeParams[T], BundleBridgeParams[T], BundleBridgeEdgeParams[T], T]
  type BundleBridgeOutwardNode[T <: Data] = OutwardNodeHandle[BundleBridgeParams[T], BundleBridgeParams[T], BundleBridgeEdgeParams[T], T]
  type BundleBridgeNode[T <: Data] = NodeHandle[BundleBridgeParams[T], BundleBridgeParams[T], BundleBridgeEdgeParams[T], T, BundleBridgeParams[T], BundleBridgeParams[T], BundleBridgeEdgeParams[T], T]
}
