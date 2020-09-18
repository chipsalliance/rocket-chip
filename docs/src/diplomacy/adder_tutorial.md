# The Littlest Diplomacy Walkthrough

- [What is Diplomacy?](#what-is-diplomacy)
- [Diplomatic Adder and Test Bench](#diplomatic-adder-and-test-bench)
  * [Parameter negotiation and passing](#parameter-negotiation-and-passing)
    + [Parameters](#parameters)
    + [Node Implementation](#node-implementation)
    + [Nodes](#nodes)
  * [Creating the `LazyModule`s](#creating-the-lazymodules)
  * [Creating the Top](#creating-the-top)
  * [The Generated Verilog](#the-generated-verilog)
- [Other Resources](#other-resources)

## What is Diplomacy?

From the [Diplomatic Design Patterns: A TileLink Case Study](https://carrv.github.io/2017/papers/cook-diplomacy-carrv2017.pdf):
> Diplomacy is a parameter negotiation framework for generating parameterized 
  protocol implementations.

The goal of this walkthrough is to demonstrate an extremely simple Diplomacy
protocol. In this walkthrough, we will demonstrate how to create a parameterized
adder and its associated testing modules.

## Diplomatic Adder and Test Bench

Let's begin by describing the desired circuit. We want a 2-to-1 **adder**, and
to test it, we would like a **top-level test bench**, two **drivers**, and a simple
**monitor**.

![adder test bench module hierarchy](diagrams/adder_modules.pdf)

Let's say we would also like to parameterize the widths of these modules' ports.
For the sake of demonstration, when negotiating parameters, our protocol will
expect both our drivers to provide addends of the same widths. It will use the
smaller of the two widths from the drivers versus the monitor, which is the opposite
behavior of typical Chisel width inference.

```scala mdoc:invisible
import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.stage.ChiselStage
import chisel3.util.random.FibonacciLFSR
import freechips.rocketchip.diplomacy.{SimpleNodeImp, RenderedEdge, ValName, SourceNode,
                                       NexusNode, SinkNode, LazyModule, LazyModuleImp}
```

### Parameter negotiation and passing

We have a width parameter we would like our modules to share. Any module that
wants to send or receive parameter information must have **one or more nodes**.

Between each pair of nodes, there may be **one or more edges**. The edge does
the negotiation work to ensure parameter agreement between the nodes it connects.
Edges are directed, and if we draw an edge's direction with an arrow we can then
define the tail of the arrow as the **source** of the edge and the head of the
arrow as the **sink** of the edge.

Let's pass around our parameters like so:

![adder diplomatic nodes](diagrams/adder_nodes.pdf)

Diplomacy expects the user to define a graph of directed edges between nodes,
and this graph must be **acyclic**. This type of graph is called a "directed,
acyclic multigraph." The direction towards the sinks is **downward**, and the
direction towards the sources is **upward**.

#### Parameters

In our example, we want all of our circuitry to agree upon a Scala `Int` to use
as a wire width. For clarity, we will define distinct case classes for our
parameters, although they all contain the same type of information.

```scala mdoc
case class UpwardParam(width: Int)
case class DownwardParam(width: Int)
case class EdgeParam(width: Int)
```

#### Node Implementation

In our "node implementation", or `NodeImp`, we describe how parameters flow
throughout our graph and how parameters are negotiated between nodes.

We will use `SimpleNodeImp`, which performs the same parameter negotiation and
passes the same bundles along an edge, regardless of whether the edge points
into our out of the node.

Our **edge parameter** (`E`) describes the data type that needs to be passed along
the edges in our graph. In our case, we only need to keep track of one `Int` that
describes our describes the final, negotiated width used to actually create our
hardware wires. This will live inside our `EdgeParam` class.

We also specify a **bundle parameter** (`B`), which is the type of data that will
resolve into parameterized hardware ports between our modules. For us, this is
a Chisel `UInt` with the width described by our edge parameter (see the `bundle`
function).

```scala mdoc
// PARAMETER TYPES:                       D              U            E          B
object AdderNodeImp extends SimpleNodeImp[DownwardParam, UpwardParam, EdgeParam, UInt] {
  def edge(pd: DownwardParam, pu: UpwardParam, p: Parameters, sourceInfo: SourceInfo) = {
    if (pd.width < pu.width) EdgeParam(pd.width) else EdgeParam(pu.width)
  }
  def bundle(e: EdgeParam) = UInt(e.width.W)
  def render(e: EdgeParam) = RenderedEdge("blue", s"width = ${e.width}")
}
```

Notice that the `edge` function does the actual negotiation between nodes. In our
case, it compares the parameters traveling both upwards and downwards, and chooses
the one with the smaller width as the final result to make visible from each node.

The `render` function is required by `NodeImp`s and holds metadata for rendering
a view of the negotiated information in some graphical format.

#### Nodes

Nodes can receive or generate parameters. A module parameterized through
Diplomacy must have one or more nodes in order to receive or send parameters.
Here, we will create some different types of nodes for use in the modules we
would like to parameterize.

In this section, in addition to **upward** (toward a source) and **downward**
(toward a sink) directionality, we will also discuss **inward** and **outward**
directionality. Again, let's picture a directed edge as an arrow. For a node,
edges that are pointing into the node are **inward edges** _respective to that
particular node_, and edges who are pointing away from the node are **outward**.

![inward and outward directionality](diagrams/edge_inout.pdf)

Let's start off with creating a node for our driver. We will make it a
`SourceNode` with the node implementation shown in the previous section, since
`SourceNode`s only generate downward-flowing parameters along **outward edges**.

For our `AdderDriverNode`, `widths` of type `Seq[DownwardParam]` represents the
desired widths for the output wires of the module instantiating this node
(`AdderDriver`). We are using a `Seq` here because each node can drive multiple
outputs. In our case, each node will be connected to both the adder and the monitor.

```scala mdoc
/** node for [[AdderDriver]] (source) */
class AdderDriverNode(widths: Seq[DownwardParam])(implicit valName: ValName)
  extends SourceNode(AdderNodeImp)(widths)
```

Our monitor node follows the same pattern, this time extending `SinkNode`, a
type of node that only generates upward-flowing parameters along **inward edges**.
The parameter it takes in is `width` of type `UpwardParam`.

```scala mdoc
/** node for [[AdderMonitor]] (sink) */
class AdderMonitorNode(width: UpwardParam)(implicit valName: ValName)
  extends SinkNode(AdderNodeImp)(Seq(width))
```

Our adder node will take inputs from two `AdderDriverNode`s (the two addends)
and pass one output (the sum) into the monitor. Because the number of inputs and
outputs differ, we will make this node extend a `NexusNode`.

`dFn` maps **downward** parameters along **inward edges** into **downward**
parameters along **outward edges**. Similarly, `uFn` maps **upward**
parameters along **outward edges** into **upward** parameters along **inward
edges**.

```scala mdoc
/** node for [[Adder]] (nexus) */
class AdderNode(dFn: Seq[DownwardParam] => DownwardParam,
                uFn: Seq[UpwardParam] => UpwardParam)(implicit valName: ValName)
  extends NexusNode(AdderNodeImp)(dFn, uFn)
```

There are several other node types, which can be found in [Nodes.scala](https://github.com/chipsalliance/rocket-chip/blob/master/src/main/scala/diplomacy/Nodes.scala).

### Creating the `LazyModule`s

"Lazy" evaluation refers to deferring the evaluation of an expression until
it is needed.

Parameter negotiation in Diplomacy is done lazily, after the Diplomacy graph
is created. Because of this, the concrete hardware that we want to parameterize
must also be generated lazily. Diplomacy provides the `LazyModule` construct for
writing lazily-evaluated hardware modules.

Let's use `LazyModule` to define our `Adder`. Notice that the creation of the
components used to define Diplomacy graph (in this case, the node), is non-lazy.
The desired hardware for the module must be written inside `LazyModuleImp`.

For this example, we expect our drivers to drive addends of equal bit widths
into the adder, meaning we expect all of our downward flowing parameters to be
equivalent. We also expect a single monitor, so all of our upward flowing
parameters should also be equivalent. Thus, we can require these properties in
our `AdderNode` and simply pass on downward the first `DownwardParam` we see.
Following the same logic, we can pass upward the first `UpwardParam` we see.

```scala mdoc
/** adder DUT (nexus) */
class Adder(implicit p: Parameters) extends LazyModule {
  val node = new AdderNode (
    { case dps: Seq[DownwardParam] =>
      require(dps.forall(dp => dp.width == dps.head.width), "inward, downward adder widths must be equivalent")
      dps.head
    },
    { case ups: Seq[UpwardParam] =>
      require(ups.forall(up => up.width == ups.head.width), "outward, upward adder widths must be equivalent")
      ups.head
    }
  )
  lazy val module = new LazyModuleImp(this) {
    require(node.in.size >= 2)
    node.out.head._1 := node.in.unzip._1.reduce(_ + _)
  }

  override lazy val desiredName = "Adder"
}
```

Our `AdderDriver` randomly generates an addend of `finalWidth` bits each cycle, and
passes it to `numOutputs` sources.

```scala mdoc
/** driver (source)
  * drives one random number on multiple outputs */
class AdderDriver(width: Int, numOutputs: Int)(implicit p: Parameters) extends LazyModule {
  val node = new AdderDriverNode(Seq.fill(numOutputs)(DownwardParam(width)))

  lazy val module = new LazyModuleImp(this) {
    // check that node parameters converge after negotiation
    val negotiatedWidths = node.edges.out.map(_.width)
    require(negotiatedWidths.forall(_ == negotiatedWidths.head), "outputs must all have agreed on same width")
    val finalWidth = negotiatedWidths.head

    // generate random addend (notice the use of the negotiated width)
    val randomAddend = FibonacciLFSR.maxPeriod(finalWidth)

    // drive signals
    node.out.foreach { case (addend, _) => addend := randomAddend }
  }

  override lazy val desiredName = "AdderDriver"
}
```

Our `AdderMonitor` prints out the results of the `Adder` each cycle and
signals an error if the `Adder` returns an incorrect result. It has two
`AdderMonitorNode`s for taking in addends from the `AdderDriver`, and one
`AdderMonitorNode` to take in the sum from the `Adder`.

```scala mdoc
/** monitor (sink) */
class AdderMonitor(width: Int, numOperands: Int)(implicit p: Parameters) extends LazyModule {
  val nodeSeq = Seq.fill(numOperands) { new AdderMonitorNode(UpwardParam(width)) }
  val nodeSum = new AdderMonitorNode(UpwardParam(width))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val error = Output(Bool())
    })

    // print operation
    printf(nodeSeq.map(node => p"${node.in.head._1}").reduce(_ + p" + " + _) + p" = ${nodeSum.in.head._1}")

    // basic correctness checking
    io.error := nodeSum.in.head._1 =/= nodeSeq.map(_.in.head._1).reduce(_ + _)
  }

  override lazy val desiredName = "AdderMonitor"
}
```

### Creating the Top

Our top-level module will be a test bench that instantiate our `Adder` and its
peripherals. Notice that different `width` parameters are passed into Diplomacy
via `drivers` and `checker`. As described in our `AdderNodeImp`, we expect the
smaller of the two widths to actually be used.

In this module, we also bind our nodes. Sinks are on the left-hand side, while
sources are on the right.

```scala mdoc
/** top-level connector */
class AdderTestHarness()(implicit p: Parameters) extends LazyModule {
  val numOperands = 2
  val adder = LazyModule(new Adder)
  // 8 will be the downward-traveling widths from our drivers
  val drivers = Seq.fill(numOperands) { LazyModule(new AdderDriver(width = 8, numOutputs = 2)) }
  // 4 will be the upward-traveling width from our monitor
  val monitor = LazyModule(new AdderMonitor(width = 4, numOperands = numOperands))

  // create edges via binding operators between nodes in order to define a complete graph
  drivers.foreach{ driver => adder.node := driver.node }

  drivers.zip(monitor.nodeSeq).foreach { case (driver, monitorNode) => monitorNode := driver.node }
  monitor.nodeSum := adder.node

  lazy val module = new LazyModuleImp(this) {
    when(monitor.module.io.error) {
      printf("something went wrong")
    }
  }

  override lazy val desiredName = "AdderTestHarness"
}
```

We have used the most typical node binding operator (`:=`) here, but other types
of bindings can be found in [Nodes.scala](https://github.com/chipsalliance/rocket-chip/blob/master/src/main/scala/diplomacy/Nodes.scala).

### The Generated Verilog

Now we are ready to generate the Verilog for our circuit.

```scala mdoc:silent
val verilog = (new ChiselStage).emitVerilog(
  LazyModule(new AdderTestHarness()(Parameters.empty)).module
)
```

Below is the generated Verilog for our modules! Note the parameterized ports
in `Adder`, `AdderDriver`, and `AdderChecker` all get the lower width (4).
The LFSR is also parameterized to 4 bits.

```scala mdoc:passthrough
println(s"```verilog\n$verilog```")
```

## Other Resources

[Rocket Chip Diplomacy Library](https://github.com/chipsalliance/rocket-chip/tree/master/src/main/scala/diplomacy)

[A Crash Course in the Diplomacy Framework](https://www.youtube.com/watch?v=4VfMCO4q26g)

[TileLink and Diplomacy Reference](https://chipyard.readthedocs.io/en/latest/TileLink-Diplomacy-Reference/index.html)
