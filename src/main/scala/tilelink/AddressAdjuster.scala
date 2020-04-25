// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

// forceLocal -> used to ensure special devices (like debug) remain reacheable at chip_id=0 even if in params.region
class AddressAdjuster(val params: ReplicatedRegion, val forceLocal: Seq[AddressSet] = Nil)(implicit p: Parameters) extends LazyModule {
  val mask = params.replicationMask
  // Which bits are in the mask?
  val bits = AddressSet.enumerateBits(mask)
  // Which ids must we route within that mask?
  val ids  = AddressSet.enumerateMask(mask)
  // Find the intersection of the mask with some region
  private def masked(region: Seq[AddressSet], offset: BigInt = 0): Seq[AddressSet] = {
    region.flatMap { _.intersect(AddressSet(offset, ~mask)) }
  }

  // forceLocal better only go one place (the low index)
  forceLocal.foreach { as => require((as.max & mask) == 0) }

  // Address Adjustment requires many things about the downstream devices, captured here as helper functions:

  // Report whether a region of addresses fully contains a particular manager
  def isDeviceContainedBy(region: Seq[AddressSet], m: TLSlaveParameters): Boolean = {
    val addr = masked(m.address)
    val any_in  = region.exists { f => addr.exists { a => f.overlaps(a) } }
    val any_out = region.exists { f => addr.exists { a => !f.contains(a) } }
    // Ensure device is either completely inside or outside this region
    require (!any_in || !any_out,
      s"Address adjuster cannot have partially contained devices, but for ${m.name}: $region vs ${m.address}")
    any_in
  }

  // Confirm that bits of an address are repeated according to the mask
  def requireMaskRepetition(managers: Seq[TLSlaveParameters]): Unit = managers.map { m =>
    val sorted = m.address.sorted
    bits.foreach { b =>
      val flipped = m.address.map(a => AddressSet((a.base ^ b) & ~a.mask, a.mask)).sorted
      require (sorted == flipped, s"AddressSets for ${m.name} (${sorted}) do not repeat with bit ${b} (${flipped})")
    }
  }

  // Confirm that everything supported by the remote PMA (which will be the final PMA) can be taken to the error device
  def requireErrorSupport(errorDev: TLSlaveParameters, managers: Seq[TLSlaveParameters]): Unit = managers.map { m =>
    require (errorDev.supportsAcquireT  .contains(m.supportsAcquireT  ), s"Error device cannot cover ${m.name}'s AcquireT")
    require (errorDev.supportsAcquireB  .contains(m.supportsAcquireB  ), s"Error device cannot cover ${m.name}'s AcquireB")
    require (errorDev.supportsArithmetic.contains(m.supportsArithmetic), s"Error device cannot cover ${m.name}'s Arithmetic")
    require (errorDev.supportsLogical   .contains(m.supportsLogical   ), s"Error device cannot cover ${m.name}'s Logical")
    require (errorDev.supportsGet       .contains(m.supportsGet       ), s"Error device cannot cover ${m.name}'s Get")
    require (errorDev.supportsPutFull   .contains(m.supportsPutFull   ), s"Error device cannot cover ${m.name}'s PutFull")
    require (errorDev.supportsPutPartial.contains(m.supportsPutPartial), s"Error device cannot cover ${m.name}'s PutPartial")
    require (errorDev.supportsHint      .contains(m.supportsHint      ), s"Error device cannot cover ${m.name}'s Hint")
  }

  // Confirm that a subset of managers have homogeneous FIFO ids
  def requireFifoHomogeneity(managers: Seq[TLSlaveParameters]): Unit = managers.map { m =>
    require(m.fifoId.isDefined && m.fifoId == managers.head.fifoId,
      s"${m.name} had fifoId ${m.fifoId}, " +
      s"which was not homogeneous (${managers.map(s => (s.name, s.fifoId))}) ")
  }

  // Confirm that a particular manager r can successfully handle all operations targetting another manager l
  def requireContainerSupport(l: TLSlaveParameters, r: TLSlaveParameters): Unit = {
    require (l.regionType >= r.regionType,  s"Device ${l.name} cannot be ${l.regionType} when ${r.name} is ${r.regionType}")
    require (!l.executable || r.executable, s"Device ${l.name} cannot be executable if ${r.name} is not")
    require (!l.mayDenyPut || r.mayDenyPut, s"Device ${l.name} cannot deny Put if ${r.name} does not")
    require (!l.mayDenyGet || r.mayDenyGet, s"Device ${l.name} cannot deny Get if ${r.name} does not")
    require (l.alwaysGrantsT || !r.alwaysGrantsT, s"Device ${l.name} must always Grant toT if ${r.name} does")
    // Confirm no capabilities are lost
    require (!l.supportsAcquireT   || r.supportsAcquireT,   s"Device ${l.name} (${l.address}) loses AcquireT suppport because ${r.name} does not support it")
    require (!l.supportsAcquireB   || r.supportsAcquireB,   s"Device ${l.name} (${l.address}) loses AcquireB support because ${r.name} does not support it")
    require (!l.supportsArithmetic || r.supportsArithmetic, s"Device ${l.name} (${l.address}) loses Arithmetic support because ${r.name} does not support it")
    require (!l.supportsLogical    || r.supportsLogical,    s"Device ${l.name} (${l.address}) loses Logical support because ${r.name} does not support it")
    require (!l.supportsGet        || r.supportsGet,        s"Device ${l.name} (${l.address}) loses Get support because ${r.name} does not support it")
    require (!l.supportsPutFull    || r.supportsPutFull,    s"Device ${l.name} (${l.address}) loses PutFull support because ${r.name} does not support it")
    require (!l.supportsPutPartial || r.supportsPutPartial, s"Device ${l.name} (${l.address}) loses PutPartial support because ${r.name} does not support it")
    require (!l.supportsHint       || r.supportsHint,       s"Device ${l.name} (${l.address}) loses Hint support because ${r.name} does not support it")
  }

  // Utility debug printer
  def printManagers(kind: String, managers: Seq[TLSlaveParameters]): Unit = {
    println(s"$kind:")
    println(managers.map(m => s"\t${m.name} ${m.address.head} ${m.fifoId}").mkString("\n"))
  }

  // Now we create a custom node that joins the local and remote manager parameters, changing the PMAs of devices in the adjustable region
  val node: TLJunctionNode = TLJunctionNode(
    clientFn  = { cp => Seq.fill(node.dRatio) { cp(0) } },
    managerFn = { mp =>
      require (node.uRatio == 1)
      require (node.dRatio == 2)

      val local  = mp(0)
      val remote = mp(1)

      // Subdivide the managers into four cases: (adjustable vs fixed) x (local vs remote)
      val adjustableLocalManagers  = local.managers.filter(m =>  isDeviceContainedBy(Seq(params.region), m))
      val fixedLocalManagers       = local.managers.filter(m => !isDeviceContainedBy(Seq(params.region), m))

      val adjustableRemoteManagers = remote.managers.flatMap { m =>
        val intersection = m.address.flatMap(a => params.region.intersect(a))
        if (intersection.isEmpty) None else Some(m.v1copy(address = intersection))
      }

      val fixedRemoteManagers = remote.managers.flatMap { m =>
        val subtraction = m.address.flatMap(a => a.subtract(params.region))
        if (subtraction.isEmpty) None else Some(m.v1copy(address = subtraction))
      }

      if (false) {
        printManagers("Adjustable Local", adjustableLocalManagers)
        printManagers("Adjustable Remote",adjustableRemoteManagers)
        printManagers("Fixed Local", fixedLocalManagers)
        printManagers("Fixed Remote",fixedRemoteManagers)
      }

      // For address adjustment to be possible, we have to calculate some specific things about the downstream addess map:

      // Find the downstream error device
      val errorDevs = local.managers.filter(_.nodePath.last.lazyModule.className == "TLError")
      require (!errorDevs.isEmpty, s"There is no TLError reachable from ${name}. One must be instantiated.")
      val errorDev = errorDevs.head

      // Find all the holes in local routing
      val holes = {
        val ra = masked(adjustableRemoteManagers.flatMap(_.address))
        val la = masked(adjustableLocalManagers .flatMap(_.address))
        la.foldLeft(ra) { case (holes, la) => holes.flatMap(_.subtract(la)) }
      }

      // Confirm that the PMAs of all adjustable devices are replicated according to the mask
      requireMaskRepetition(adjustableLocalManagers ++ adjustableRemoteManagers)

      // Confirm that the error device can supply all the same capabilities as the remote path
      requireErrorSupport(errorDev, adjustableRemoteManagers)

      // Confirm that each subset of adjustable managers have homogeneous FIFO ids
      requireFifoHomogeneity(adjustableLocalManagers)
      requireFifoHomogeneity(adjustableRemoteManagers)

      // Actually rewrite the PMAs for the adjustable local devices
      val newLocals = adjustableLocalManagers.map { l =>
        // Ensure that every local device has a matching remote device
        val container = adjustableRemoteManagers.find { r => l.address.forall { la => r.address.exists(_.contains(la)) } }
        require (!container.isEmpty, s"There is no remote manager which contains the addresses of ${l.name} (${l.address})")
        val r = container.get
        requireContainerSupport(l, r)

        // The address can be dynamically adjusted to anywhere in the adjustable region, but we take the 0 setting as default for DTS output
        // Any address space holes in the local adjustable region will be plugged with the error device.
        // All other PMAs are replaced with the capabilities of the remote path, since that's all we can know statically.
        // Capabilities supported by the remote but not the local will result in dynamic re-reouting to the error device.
        l.v1copy(
          address            = AddressSet.unify(masked(l.address) ++ (if (l == errorDev) holes else Nil)),
          regionType         = r.regionType,
          executable         = r.executable,
          supportsAcquireT   = r.supportsAcquireT,
          supportsAcquireB   = r.supportsAcquireB,
          supportsArithmetic = r.supportsArithmetic,
          supportsLogical    = r.supportsLogical,
          supportsGet        = r.supportsGet,
          supportsPutFull    = r.supportsPutFull,
          supportsPutPartial = r.supportsPutPartial,
          supportsHint       = r.supportsHint,
          mayDenyGet         = r.mayDenyGet,
          mayDenyPut         = r.mayDenyPut,
          alwaysGrantsT      = r.alwaysGrantsT,
          fifoId             = Some(if (isDeviceContainedBy(forceLocal, l)) ids.size else 0))
      }

      // Actually rewrite the PMAs for the adjustable remote region too, to account for the differing FIFO domains under the mask
      val newRemotes = ids.tail.zipWithIndex.flatMap { case (id, i) => adjustableRemoteManagers.map { r =>
        r.v1copy(
          address = AddressSet.unify(masked(r.address, offset = id)),
          fifoId = Some(i+1))
      } }

      // Relable the FIFO domains for certain manager subsets
      val fifoIdFactory = TLXbar.relabeler()
      def relabelFifo(managers: Seq[TLSlaveParameters]): Seq[TLSlaveParameters] = {
        val fifoIdMapper = fifoIdFactory()
        managers.map(m => m.v1copy(fifoId = m.fifoId.map(fifoIdMapper(_))))
      }

      val newManagerList =
        relabelFifo(newLocals ++ newRemotes) ++
        relabelFifo(fixedLocalManagers) ++
        relabelFifo(fixedRemoteManagers)

      Seq(local.v1copy(
        managers   = newManagerList,
        endSinkId  = local.endSinkId + remote.endSinkId,
        minLatency = local.minLatency min remote.minLatency))
    })

  val prefix = BundleBridgeSink[UInt]()

  lazy val module = new LazyModuleImp(this) {
    val edgesInSize = node.edges.in.size
    val edgesOutSize = node.edges.out.size
    require (edgesOutSize % 2 == 0,
      s"AddressAdjuster requires an even number of  out edges, but got $edgesOutSize")
    require (edgesOutSize / 2  == edgesInSize,
      s"AddressAdjuster requires two out edges for every one in edge, but: out($edgesOutSize}) / 2 != in($edgesInSize)")

    node.inoutGrouped.collect { case (Seq((parent, parentEdge)), Seq((local, localEdge), (remote, remoteEdge))) =>

      require (localEdge.manager.beatBytes == remoteEdge.manager.beatBytes,
        s"Port width mismatch ${localEdge.manager.beatBytes} (${localEdge.manager.managers.map(_.name)}) != ${remoteEdge.manager.beatBytes} (${remoteEdge.manager.managers.map(_.name)})")

      // Which address within the mask routes to local devices?
      val local_prefix = RegNext(prefix.bundle)
      assert (params.isLegalPrefix(local_prefix))

      def containsAddress(region: Seq[AddressSet], addr: UInt): Bool = region.foldLeft(false.B)(_ || _.contains(addr))
      val totalContainment = parentEdge.slave.slaves.forall(_.address.forall(params.region contains _))
      def isAdjustable(addr: UInt) = params.region.contains(addr) || totalContainment.B
      def isDynamicallyLocal(addr: UInt) = local_prefix === (addr & mask.U) || containsAddress(forceLocal, addr)
      val staticLocal = AddressSet.unify(localEdge.manager.managers.flatMap(_.address).filter(a => !params.region.contains(a)))
      def isStaticallyLocal(addr: UInt) = containsAddress(staticLocal, addr)
      def routeLocal(addr: UInt): Bool = Mux(isAdjustable(addr), isDynamicallyLocal(addr), isStaticallyLocal(addr))

      // Route A by address, but reroute unsupported operations
      val a_local = routeLocal(parent.a.bits.address)
      parent.a.ready := Mux(a_local, local.a.ready, remote.a.ready)
      local .a.valid := parent.a.valid &&  a_local
      remote.a.valid := parent.a.valid && !a_local
      local .a.bits  := parent.a.bits
      remote.a.bits  := parent.a.bits

      val dynamicLocal = AddressSet.unify(localEdge.manager.managers.flatMap(_.address).filter(a => params.region.contains(a)))
      val a_legal = containsAddress(dynamicLocal, parent.a.bits.address)

      val dynamicLocalManagers = localEdge.manager.v1copy(
        managers = localEdge.manager.managers.filter(m => isDeviceContainedBy(Seq(params.region), m)))

      val acquire_ok =
        Mux(parent.a.bits.param === TLPermissions.toT,
          dynamicLocalManagers.supportsAcquireTFast(parent.a.bits.address, parent.a.bits.size),
          dynamicLocalManagers.supportsAcquireBFast(parent.a.bits.address, parent.a.bits.size))

      val a_support = VecInit(
        dynamicLocalManagers.supportsPutFullFast   (parent.a.bits.address, parent.a.bits.size),
        dynamicLocalManagers.supportsPutPartialFast(parent.a.bits.address, parent.a.bits.size),
        dynamicLocalManagers.supportsArithmeticFast(parent.a.bits.address, parent.a.bits.size),
        dynamicLocalManagers.supportsLogicalFast   (parent.a.bits.address, parent.a.bits.size),
        dynamicLocalManagers.supportsGetFast       (parent.a.bits.address, parent.a.bits.size),
        dynamicLocalManagers.supportsHintFast      (parent.a.bits.address, parent.a.bits.size),
        acquire_ok, acquire_ok)(parent.a.bits.opcode)

      val errorSet = localEdge.manager.managers.filter(_.nodePath.last.lazyModule.className == "TLError").head.address.head
      val a_ok = !isAdjustable(parent.a.bits.address) || (a_legal && a_support)
      local.a.bits.address := Cat(
        Mux(a_ok, parent.a.bits.address, errorSet.base.U) >> log2Ceil(errorSet.alignment),
        parent.a.bits.address(log2Ceil(errorSet.alignment)-1, 0))

      // Rewrite sink in D
      val sink_threshold = localEdge.manager.endSinkId.U // more likely to be 0 than remote.endSinkId
      val local_d  = Wire(chiselTypeOf(parent.d)) // type-cast, because 'sink' width differs
      local.d.ready := local_d.ready
      local_d.valid := local.d.valid
      local_d.bits  := local.d.bits
      val remote_d = Wire(chiselTypeOf(parent.d))
      remote.d.ready := remote_d.ready
      remote_d.valid := remote.d.valid
      remote_d.bits := remote.d.bits
      remote_d.bits.sink := remote.d.bits.sink +& sink_threshold
      TLArbiter.robin(parentEdge, parent.d, local_d, remote_d)

      if (parentEdge.manager.anySupportAcquireB && parentEdge.client.anySupportProbe) {
        // Merge probe channels
        assert (!local .b.valid || ((local .b.bits.address & mask.U) === local_prefix))
        assert (!remote.b.valid || ((remote.b.bits.address & mask.U) =/= local_prefix))
        TLArbiter.robin(parentEdge, parent.b, local.b, remote.b)

        // Route C by address
        val c_local = routeLocal(parent.c.bits.address)
        parent.c.ready := Mux(c_local, local.c.ready, remote.c.ready)
        local .c.valid := parent.c.valid &&  c_local
        remote.c.valid := parent.c.valid && !c_local
        local .c.bits  := parent.c.bits
        remote.c.bits  := parent.c.bits

        // Route E by sink
        val e_local = parent.e.bits.sink < sink_threshold
        parent.e.ready := Mux(e_local, local.e.ready, remote.e.ready)
        local .e.valid := parent.e.valid &&  e_local
        remote.e.valid := parent.e.valid && !e_local
        local .e.bits  := parent.e.bits
        remote.e.bits  := parent.e.bits
        remote.e.bits.sink := parent.e.bits.sink - sink_threshold
      }
    }
  }
}
