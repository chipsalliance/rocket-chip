// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

// mask=0 -> passthrough
class AddressAdjuster(mask: BigInt)(implicit p: Parameters) extends LazyModule {
  // Which bits are in the mask?
  val bits = AddressSet.enumerateBits(mask)
  // Which idss must we route within that mask?
  val ids  = AddressSet.enumerateMask(mask)

  val node = TLNexusNode(
    clientFn = { cp => cp(0) },
    managerFn = { mp =>
      require (mp.size == 2)
      val remote = mp(0)
      val local  = mp(1)

      // Confirm that the PMAs of devices are replicated according to the mask
      def checkMask(m: TLManagerParameters) = {
        val sorted = m.address.sorted
        bits.foreach { b =>
          val flipped = m.address.map(a => AddressSet((a.base ^ b) & ~a.mask, a.mask)).sorted
          require (sorted == flipped, s"AddressSets for ${m.name} (${sorted}) do not repeat with bit ${b} (${flipped})")
        }
      }
      local .managers.foreach { z => checkMask(z) }
      remote.managers.foreach { z => checkMask(z) }

      // Find the downstream error device
      val errorDevs = local.managers.filter(_.nodePath.last.lazyModule.className == "TLError")
      require (!errorDevs.isEmpty, s"There is no TLError reachable from ${name}. One must be instantiated.")
      val errorDev = errorDevs.head

      // Confirm that everything supported by the remote PMA (which will be the final PMA) can be taken to the error device
      remote.managers.foreach { r =>
        require (errorDev.supportsAcquireT  .contains(r.supportsAcquireT  ), s"Error device cannot cover ${r.name}'s AcquireT")
        require (errorDev.supportsAcquireB  .contains(r.supportsAcquireB  ), s"Error device cannot cover ${r.name}'s AcquireB")
        require (errorDev.supportsArithmetic.contains(r.supportsArithmetic), s"Error device cannot cover ${r.name}'s Arithmetic")
        require (errorDev.supportsLogical   .contains(r.supportsLogical   ), s"Error device cannot cover ${r.name}'s Logical")
        require (errorDev.supportsGet       .contains(r.supportsGet       ), s"Error device cannot cover ${r.name}'s Get")
        require (errorDev.supportsPutFull   .contains(r.supportsPutFull   ), s"Error device cannot cover ${r.name}'s PutFull")
        require (errorDev.supportsPutPartial.contains(r.supportsPutPartial), s"Error device cannot cover ${r.name}'s PutPartial")
        require (errorDev.supportsHint      .contains(r.supportsHint      ), s"Error device cannot cover ${r.name}'s Hint")
      }

      // Confirm that both ports are FIFOFixed, so our rewrite of the domains is legal
      remote.requireFifo()
      local.requireFifo()

      // Find all the holes in local routing
      val holes = {
        val ra = remote.managers.flatMap(_.address).flatMap(_.intersect(AddressSet(0, ~mask)))
        val la = local .managers.flatMap(_.address).flatMap(_.intersect(AddressSet(0, ~mask)))
        la.foldLeft(ra) { case (holes, la) => holes.flatMap(_.subtract(la)) }
      }

      // Ensure that every local device has a matching remote device
      val newLocal = local.managers.map { l =>
        val container = remote.managers.find { r => l.address.forall { la => r.address.exists(_.contains(la)) } }
        require (!container.isEmpty, s"There is no remote manager which contains the addresses of ${l.name} (${l.address})")
        val r = container.get
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
        l.copy(
          // take the 0 setting as default for DTS output
          address            = AddressSet.unify(l.address.flatMap(_.intersect(AddressSet(0, ~mask))) ++
                                               (if (l == errorDev) holes else Nil)),
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
          fifoId             = Some(0))
      }

      val newRemote = ids.tail.zipWithIndex.flatMap { case (id, i) => remote.managers.map { r =>
        r.copy(
          address = AddressSet.unify(r.address.flatMap(_.intersect(AddressSet(id, ~mask)))),
          fifoId = Some(i+1))
      } }

      local.copy(
        managers   = newLocal ++ newRemote,
        endSinkId  = local.endSinkId + remote.endSinkId,
        minLatency = local.minLatency min remote.minLatency)
    })

  val chip_id = BundleBridgeSink[UInt]()

  lazy val module = new LazyModuleImp(this) {
    require (node.edges.in.size == 1)
    require (node.edges.out.size == 2)

    val (parent, parentEdge) = node.in(0)
    val (remote, remoteEdge) = node.out(0)
    val (local,  localEdge)  = node.out(1)
    require (localEdge.manager.beatBytes == remoteEdge.manager.beatBytes,
      s"Port width mismatch ${localEdge.manager.beatBytes} (${localEdge.manager.managers.map(_.name)}) != ${remoteEdge.manager.beatBytes} (${remoteEdge.manager.managers.map(_.name)})")

    // Which address within the mask routes to local devices?
    val local_address = (bits zip chip_id.bundle.toBools).foldLeft(0.U) {
      case (acc, (bit, sel)) => acc | Mux(sel, bit.U, 0.U)
    }

    // Route A by address, but reroute unsupported operations
    val a_local = local_address === (parent.a.bits.address & mask.U)
    parent.a.ready := Mux(a_local, local.a.ready, remote.a.ready)
    local .a.valid := parent.a.valid &&  a_local
    remote.a.valid := parent.a.valid && !a_local
    local .a.bits  := parent.a.bits
    remote.a.bits  := parent.a.bits

    val a_routable  = AddressSet.unify(localEdge.manager.managers.flatMap(_.address))
    val a_contained = a_routable.map(_.contains(parent.a.bits.address)).reduce(_ || _)

    val acquireOk =
      Mux(parent.a.bits.param === TLPermissions.toT,
        localEdge.manager.supportsAcquireTFast(parent.a.bits.address, parent.a.bits.size),
        localEdge.manager.supportsAcquireBFast(parent.a.bits.address, parent.a.bits.size))

    val a_support = VecInit(
      localEdge.manager.supportsPutFullFast   (parent.a.bits.address, parent.a.bits.size),
      localEdge.manager.supportsPutPartialFast(parent.a.bits.address, parent.a.bits.size),
      localEdge.manager.supportsArithmeticFast(parent.a.bits.address, parent.a.bits.size),
      localEdge.manager.supportsLogicalFast   (parent.a.bits.address, parent.a.bits.size),
      localEdge.manager.supportsGetFast       (parent.a.bits.address, parent.a.bits.size),
      localEdge.manager.supportsHintFast      (parent.a.bits.address, parent.a.bits.size),
      acquireOk, acquireOk)(parent.a.bits.opcode)

    val errorSet = localEdge.manager.managers.filter(_.nodePath.last.lazyModule.className == "TLError").head.address.head
    val a_error = !a_contained || !a_support
    local.a.bits.address := Cat(
      Mux(!a_error, parent.a.bits.address, errorSet.base.U) >> log2Ceil(errorSet.alignment),
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
      assert (!local .b.valid || ((local .b.bits.address & mask.U) === local_address))
      assert (!remote.b.valid || ((remote.b.bits.address & mask.U) =/= local_address))
      TLArbiter.robin(parentEdge, parent.b, local.b, remote.b)

      // Route C by address
      val c_local = local_address === (parent.c.bits.address & mask.U)
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
