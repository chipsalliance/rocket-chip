// See LICENSE.SiFive for license details.

package coreplex

import Chisel._
import config._
import diplomacy._
import uncore.tilelink2._
import uncore.util._
import util._

trait CoreplexNetwork extends HasCoreplexParameters {
  val module: CoreplexNetworkModule
  def bindingTree: ResourceMap

  val l1tol2 = LazyModule(new TLXbar)
  val l1tol2_beatBytes = l1tol2Config.beatBytes
  val l1tol2_lineBytes = p(CacheBlockBytes)

  val cbus = LazyModule(new TLXbar)
  val cbus_beatBytes = cbusConfig.beatBytes
  val cbus_lineBytes = l1tol2_lineBytes

  val intBar = LazyModule(new IntXbar)

  val mmio = TLOutputNode()
  val mmioInt = IntInputNode()
  val l2in = TLInputNode()

  intBar.intnode := mmioInt

  // Allows a variable number of inputs from outside to the Xbar
  l1tol2.node :=* l2in

  cbus.node :=
    TLBuffer()(
    TLAtomicAutomata(arithmetic = true)( // disable once TLB uses TL2 metadata
    TLWidthWidget(l1tol2_beatBytes)(
    l1tol2.node)))

  mmio :=
    TLWidthWidget(l1tol2_beatBytes)(
    l1tol2.node)

  val root = new Device {
    def describe(resources: ResourceBindings): Description = {
      val width = resources("width").map(_.value)
      Description("/", Map(
        "#address-cells" -> width,
        "#size-cells"    -> width,
        "model"          -> Seq(ResourceString(p(DTSModel))),
        "compatible"     -> (p(DTSModel) +: p(DTSCompat)).map(s => ResourceString(s + "-dev"))))
    }
  }

  val soc = new Device {
    def describe(resources: ResourceBindings): Description = {
      val width = resources("width").map(_.value)
      Description("soc", Map(
        "#address-cells" -> width,
        "#size-cells"    -> width,
        "compatible"     -> (p(DTSModel) +: p(DTSCompat)).map(s => ResourceString(s + "-soc")),
        "ranges"         -> Nil))
    }
  }

  val cpus = new Device {
    def describe(resources: ResourceBindings): Description = {
      Description("cpus", Map(
        "#address-cells"     -> Seq(ResourceInt(1)),
        "#size-cells"        -> Seq(ResourceInt(0)),
        "timebase-frequency" -> Seq(ResourceInt(p(DTSTimebase)))))
    }
  }

  ResourceBinding {
    val managers = l1tol2.node.edgesIn.headOption.map(_.manager.managers).getOrElse(Nil)
    val max = managers.flatMap(_.address).map(_.max).max
    val width = ResourceInt((log2Ceil(max)+31) / 32)
    Resource(root, "width").bind(width)
    Resource(soc,  "width").bind(width)
    Resource(cpus, "null").bind(ResourceString(""))

    managers.foreach { case manager =>
      val value = manager.toResource
      manager.resources.foreach { case resource =>
        resource.bind(value)
      }
    }
  }
}

trait CoreplexNetworkBundle extends HasCoreplexParameters {
  val outer: CoreplexNetwork

  val mmio = outer.mmio.bundleOut
  val interrupts = outer.mmioInt.bundleIn
  val l2in = outer.l2in.bundleIn
}

trait CoreplexNetworkModule extends HasCoreplexParameters {
  val outer: CoreplexNetwork
  val io: CoreplexNetworkBundle

  println("Generated Address Map")
  for (manager <- outer.l1tol2.node.edgesIn(0).manager.managers) {
    val prot = (if (manager.supportsGet)     "R" else "") +
               (if (manager.supportsPutFull) "W" else "") +
               (if (manager.executable)      "X" else "") +
               (if (manager.supportsAcquireB) " [C]" else "")
    AddressRange.fromSets(manager.address).foreach { r =>
      println(f"\t${manager.name}%s ${r.base}%x - ${r.base+r.size}%x, $prot")
    }
  }
  println("")
}

/////

trait BankedL2CoherenceManagers extends CoreplexNetwork {
  val module: BankedL2CoherenceManagersModule

  require (isPow2(l2Config.nMemoryChannels) || l2Config.nMemoryChannels == 0)
  require (isPow2(l2Config.nBanksPerChannel))
  require (isPow2(l1tol2_lineBytes))

  private val (in, out) = l2Config.coherenceManager(p, this)
  private val mask = ~BigInt((l2Config.nBanks-1) * l1tol2_lineBytes)
  val mem = Seq.tabulate(l2Config.nMemoryChannels) { channel =>
    val node = TLOutputNode()
    for (bank <- 0 until l2Config.nBanksPerChannel) {
      val offset = (bank * l2Config.nMemoryChannels) + channel
      in := l1tol2.node
      node := TLFilter(AddressSet(offset * l1tol2_lineBytes, mask))(out)
    }
    node
  }
}

trait BankedL2CoherenceManagersBundle extends CoreplexNetworkBundle {
  val outer: BankedL2CoherenceManagers
  val mem = HeterogeneousBag(outer.mem.map(_.bundleOut))
}

trait BankedL2CoherenceManagersModule extends CoreplexNetworkModule {
  val outer: BankedL2CoherenceManagers
  val io: BankedL2CoherenceManagersBundle
}
