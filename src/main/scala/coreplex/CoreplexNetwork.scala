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

  // TODO: do we need one of these? val cbus = LazyModule(new TLXbar) // Locally-visible peripheral devices
  val sbus = LazyModule(new TLXbar) // Globally-visible high-bandwidth devices
  val pbus = LazyModule(new TLXbar) // Globally-visible low-bandwidth devices
  val tile_splitter = LazyModule(new TLSplitter) // Allows cycle-free connection to external networks
  val int_xbar = LazyModule(new IntXbar) // Interrupt crossbar

  val mmio = TLOutputNode() // Exernal memory-mapped IO slaves
  val mmioInt = IntInputNode() // Exernal devices' interrupts
  val l2in = TLInputNode() // External masters talking to the frontside of the shared cache
  val l2out = TLOutputNode() // External slaves hanging off the backside of the shared cache

  int_xbar.intnode := mmioInt

  // Allows a variable number of inputs from outside to the Xbar
  private val l2in_buffer = LazyModule(new TLBuffer)
  private val l2in_fifo = LazyModule(new TLFIFOFixer)
  sbus.node :=* l2in_fifo.node
  sbus.node :=* tile_splitter.node
  l2in_fifo.node :=* l2in_buffer.node
  l2in_buffer.node :=* l2in

  private val l2out_buffer = LazyModule(new TLBuffer(BufferParams.flow, BufferParams.none))
  l2out :*= l2out_buffer.node
  l2out_buffer.node :*= sbus.node

  pbus.node :=
    TLBuffer()(
    TLAtomicAutomata(arithmetic = true)( // disable once TLB uses TL2 metadata
    TLWidthWidget(sbusBeatBytes)(
    sbus.node)))

  mmio :=
    TLWidthWidget(sbusBeatBytes)(
    sbus.node)

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
        "compatible"     -> ((p(DTSModel) +: p(DTSCompat)).map(s => ResourceString(s + "-soc")) :+ ResourceString("simple-bus")),
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

  // Make topManagers an Option[] so as to avoid LM name reflection evaluating it...
  lazy val topManagers = Some(ManagerUnification(tile_splitter.node.edgesIn.headOption.map(_.manager.managers).getOrElse(Nil)))
  ResourceBinding {
    val managers = topManagers.get
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
  val l2out = outer.l2out.bundleOut
}

trait CoreplexNetworkModule extends HasCoreplexParameters {
  val outer: CoreplexNetwork
  val io: CoreplexNetworkBundle

  println("Generated Address Map")
  private val aw = (outer.p(rocket.PAddrBits)-1)/4 + 1
  private val fmt = s"\t%${aw}x - %${aw}x %c%c%c%c %s"

  private def collect(path: List[String], value: ResourceValue): List[(String, ResourceAddress)] = {
    value match {
      case r: ResourceAddress => List((path(1), r))
      case b: ResourceMapping => List((path(1), ResourceAddress(b.address, b.permissions)))
      case ResourceMap(value, _) => value.toList.flatMap { case (key, seq) => seq.flatMap(r => collect(key :: path, r)) }
      case _ => Nil
    }
  }
  private val ranges = collect(Nil, outer.bindingTree).groupBy(_._2).toList.flatMap { case (key, seq) =>
    AddressRange.fromSets(key.address).map { r => (r, key.permissions, seq.map(_._1)) }
  }.sortBy(_._1)
  private val json = ranges.map { case (range, ResourcePermissions(r, w, x, c), names) =>
    println(fmt.format(
      range.base,
      range.base+range.size,
      if (r) 'R' else ' ',
      if (w) 'W' else ' ',
      if (x) 'X' else ' ',
      if (c) 'C' else ' ',
      names.mkString(", ")))
    s"""{"base":[${range.base}],"size":[${range.size}],"r":[$r],"w":[$w],"x":[$x],"c":[$c],"names":[${names.map('"'+_+'"').mkString(",")}]}"""
  }
  println("")
  ElaborationArtefacts.add("memmap.json", s"""{"mapping":[${json.mkString(",")}]}""")

  // Confirm that all of memory was described by DTS
  private val dtsRanges = AddressRange.unify(ranges.map(_._1))
  private val allRanges = AddressRange.unify(outer.topManagers.get.flatMap { m => AddressRange.fromSets(m.address) })

  if (dtsRanges != allRanges) {
    println("Address map described by DTS differs from physical implementation:")
    AddressRange.subtract(allRanges, dtsRanges).foreach { case r =>
      println(s"\texists, but undescribed by DTS: ${r}")
    }
    AddressRange.subtract(dtsRanges, allRanges).foreach { case r =>
      println(s"\tdoes not exist, but described by DTS: ${r}")
    }
    println("")
  }
}

/////

trait BankedL2CoherenceManagers extends CoreplexNetwork {
  val module: BankedL2CoherenceManagersModule

  require (isPow2(l2Config.nMemoryChannels) || l2Config.nMemoryChannels == 0)
  require (isPow2(l2Config.nBanksPerChannel))
  require (isPow2(sbusBlockBytes))

  private val (in, out) = l2Config.coherenceManager(p, this)
  private val mask = ~BigInt((l2Config.nBanks-1) * sbusBlockBytes)
  val mem = Seq.tabulate(l2Config.nMemoryChannels) { channel =>
    val node = TLOutputNode()
    for (bank <- 0 until l2Config.nBanksPerChannel) {
      val offset = (bank * l2Config.nMemoryChannels) + channel
      in := sbus.node
      node := TLFilter(AddressSet(offset * sbusBlockBytes, mask))(out)
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
