# Select Library
Chisel provides a [Select library](https://github.com/freechipsproject/chisel3/blob/master/src/main/scala/chisel3/aop/Select.scala)
that provides a set of methods for finding specific sets of hardware nodes in a
Chisel design after it has been elaborated. However, Diplomacy abstracts away a
lot of the underlying Chisel hardware, so the low-level Chisel `Select`
combinators can be fragile and difficult to use in designs using Diplomacy. To
help with this, Rocket Chip provides its own `Select` library that operates on
`LazyModule`s and `Node`s instead of `Module`s and `Wire`s.

We will use the following `LazyModule`s in the examples below.
```scala mdoc
import chisel3.Bool
import freechips.rocketchip.aop.Select
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{
  BundleBridgeSink,
  BundleBridgeSource,
  LazyModule,
  LazyModuleImp,
  SimpleLazyModule
}

class Top(implicit p: Parameters) extends LazyModule {
  val a = LazyModule(new A)
  val foo = LazyModule(new Foo)

  val aInput = BundleBridgeSource[Bool](() => Bool())
  a.input := aInput

  val aOutput = a.output.makeSink

  val fooInput = BundleBridgeSource[Bool](() => Bool())
  foo.input := fooInput

  val fooOutput = foo.output.makeSink

  lazy val module = new LazyModuleImp(this) {
    aInput.makeIO
    fooOutput.makeIO
    fooInput.bundle := aOutput.bundle
  }
}

class Foo(implicit p: Parameters) extends SimpleLazyModule {
  val bar = LazyModule(new A)

  val input = bar.input
  val output = bar.output
}

class A(implicit p: Parameters) extends LazyModule {
  val b = LazyModule(new Leaf)
  val c = LazyModule(new Leaf)

  val input = b.input
  val output = c.output

  val bOutput = b.output.makeSink
  val cInput = BundleBridgeSource[Bool](() => Bool())
  c.input := cInput
  lazy val module = new LazyModuleImp(this) {
    cInput.bundle := bOutput.bundle
  }
}

class Leaf(implicit p: Parameters) extends LazyModule {
  val input = BundleBridgeSink[Bool]()
  val output = BundleBridgeSource[Bool](() => Bool())

  lazy val module = new LazyModuleImp(this) {
    output.bundle := input.bundle
  }
}

val top = LazyModule(new Top()(Parameters.empty))
```

```scala mdoc:invisible
// Select methods can only be used after instantiation
chisel3.stage.ChiselStage.elaborate(top.module)
```

The instance `top` has the following hierarchy:
```
        top:Top
        /      \
      a:A      foo:Foo
     /  \        \
b:Leaf  c:Leaf   bar:A
                  /  \
              b:Leaf  c:Leaf
```

## Examples
`Select.collectDeep` takes a `LazyModule` and a partial function. It
recursively applies the partial to the module and all of its children. The
following example uses `Select.collectDeep` to collect all `Leaf` modules in
`top`.
```scala mdoc
Select.collectDeep(top) {
  case l: Leaf => l.pathName
}
```

`Select.filterCollectDeep` takes a `LazyModule`, a filter function, and a
partial function. It recursively applies the partial to the module and all of
its children if the filter function returns true. When the filter function
returns `false`, the recursion stops.  The following example uses
`Select.filterCollectDeep` to collect all `Leaf` modules in `top` that do not
belong to the `foo:Foo` subhierarchy.
```scala mdoc
Select.filterCollectDeep(top) {
  case _: Foo => false
  case _ => true
} {
  case l: Leaf => l.pathName
}
```

`LazyModule`s have a `getNodes` method that return all the nodes instantiated
within that module. This can be combined with the `Select.collectInwardEdges`
to select `LazyModule`s based on their connectivity.
`Select.collectInwardEdges` takes a `BaseNode` and a partial function. It
applies the function to all the `InwardEdge`s of the node. There is also a
`Select.collectOutwardEdges` that does the same for outward edges. The example
below uses `Select.filterCollectDeep`, `LazyModule.getNodes`, and
`Select.collectInwardEdges` to find all instances of `Leaf` modules in `top`
that are connected to an `A` module and do not belong to the `foo:Foo`
subhierarchy.
```scala mdoc
Select.filterCollectDeep (top) {
  case _: Foo => false
  case _ => true
} {
  case a: A =>
    // Function.unlift used to convert `InwardEdge => Option[String]` to `PartialFunction[InwardEdge, String]
    a.getNodes.flatMap(Select.collectInwardEdges(_)(Function.unlift { edge =>
      edge.node.lazyModule match {
        case l: Leaf => Some(l.pathName)
        case _ => None
      }
    }))
}
```
