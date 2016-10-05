// See LICENSE for license details.

package uncore.tilelink

import Chisel._
import uncore.util._
import cde.{Parameters, Field}

case object LNEndpoints extends Field[Int]
case object LNHeaderBits extends Field[Int]

class PhysicalHeader(n: Int) extends Bundle {
  val src = UInt(width = log2Up(n))
  val dst = UInt(width = log2Up(n))
}

class PhysicalNetworkIO[T <: Data](n: Int, dType: T) extends Bundle {
  val header = new PhysicalHeader(n)
  val payload = dType.cloneType
  override def cloneType = new PhysicalNetworkIO(n,dType).asInstanceOf[this.type]
}

class BasicCrossbarIO[T <: Data](n: Int, dType: T) extends Bundle {
  val in  = Vec(n, Decoupled(new PhysicalNetworkIO(n,dType))).flip
  val out = Vec(n, Decoupled(new PhysicalNetworkIO(n,dType)))
}

abstract class PhysicalNetwork extends Module

case class CrossbarConfig[T <: Data](n: Int, dType: T, count: Int = 1, needsLock: Option[PhysicalNetworkIO[T] => Bool] = None)

abstract class AbstractCrossbar[T <: Data](conf: CrossbarConfig[T]) extends PhysicalNetwork {
  val io = new BasicCrossbarIO(conf.n, conf.dType)
}

class BasicBus[T <: Data](conf: CrossbarConfig[T]) extends AbstractCrossbar(conf) {
  val arb = Module(new LockingRRArbiter(io.in(0).bits, conf.n, conf.count, conf.needsLock))
  arb.io.in <> io.in

  arb.io.out.ready := io.out(arb.io.out.bits.header.dst).ready
  for ((out, i) <- io.out zipWithIndex) {
    out.valid := arb.io.out.valid && arb.io.out.bits.header.dst === UInt(i)
    out.bits := arb.io.out.bits
  }
}

class BasicCrossbar[T <: Data](conf: CrossbarConfig[T]) extends AbstractCrossbar(conf) {
  io.in.foreach { _.ready := Bool(false) }

  io.out.zipWithIndex.map{ case (out, i) => {
    val rrarb = Module(new LockingRRArbiter(io.in(0).bits, conf.n, conf.count, conf.needsLock))
    (rrarb.io.in, io.in).zipped.map{ case (arb, in) => {
      val destined = in.bits.header.dst === UInt(i)
      arb.valid := in.valid && destined
      arb.bits := in.bits
      when (arb.ready && destined) { in.ready := Bool(true) }
    }}
    out <> rrarb.io.out
  }}
}

abstract class LogicalNetwork extends Module

class LogicalHeader(implicit p: Parameters) extends util.ParameterizedBundle()(p) {
  val src = UInt(width = p(LNHeaderBits))
  val dst = UInt(width = p(LNHeaderBits))
}

class LogicalNetworkIO[T <: Data](dType: T)(implicit p: Parameters) extends Bundle {
  val header = new LogicalHeader
  val payload = dType.cloneType
  override def cloneType = new LogicalNetworkIO(dType)(p).asInstanceOf[this.type]
}

object DecoupledLogicalNetworkIOWrapper {
  def apply[T <: Data](
        in: DecoupledIO[T],
        src: UInt = UInt(0),
        dst: UInt = UInt(0))
      (implicit p: Parameters): DecoupledIO[LogicalNetworkIO[T]] = {
    val out = Wire(Decoupled(new LogicalNetworkIO(in.bits)))
    out.valid := in.valid
    out.bits.payload := in.bits
    out.bits.header.dst := dst
    out.bits.header.src := src
    in.ready := out.ready
    out
  }
}

object DecoupledLogicalNetworkIOUnwrapper {
  def apply[T <: Data](in: DecoupledIO[LogicalNetworkIO[T]])
                      (implicit p: Parameters): DecoupledIO[T] = {
    val out = Wire(Decoupled(in.bits.payload))
    out.valid := in.valid
    out.bits := in.bits.payload
    in.ready := out.ready
    out
  }
}

object DefaultFromPhysicalShim {
  def apply[T <: Data](in: DecoupledIO[PhysicalNetworkIO[T]])
                      (implicit p: Parameters): DecoupledIO[LogicalNetworkIO[T]] = {
    val out = Wire(Decoupled(new LogicalNetworkIO(in.bits.payload)))
    out.bits.header := in.bits.header
    out.bits.payload := in.bits.payload
    out.valid := in.valid
    in.ready := out.ready
    out
  }
}

object DefaultToPhysicalShim {
  def apply[T <: Data](n: Int, in: DecoupledIO[LogicalNetworkIO[T]])
                      (implicit p: Parameters): DecoupledIO[PhysicalNetworkIO[T]] = {
    val out = Wire(Decoupled(new PhysicalNetworkIO(n, in.bits.payload)))
    out.bits.header := in.bits.header
    out.bits.payload := in.bits.payload
    out.valid := in.valid
    in.ready := out.ready
    out
  }
}

/** A helper module that automatically issues [[uncore.Finish]] messages in repsonse
  * to [[uncore.Grant]] that it receives from a manager and forwards to a client
  */
class FinishUnit(srcId: Int = 0, outstanding: Int = 2)(implicit p: Parameters) extends TLModule()(p)
    with HasDataBeatCounters {
  val io = new Bundle {
    val grant = Decoupled(new LogicalNetworkIO(new Grant)).flip
    val refill = Decoupled(new Grant)
    val finish = Decoupled(new LogicalNetworkIO(new Finish))
    val ready = Bool(OUTPUT)
  }

  val g = io.grant.bits.payload

  if(tlNetworkPreservesPointToPointOrdering) {
    io.finish.valid := Bool(false)
    io.refill.valid := io.grant.valid
    io.refill.bits := g
    io.grant.ready := io.refill.ready
    io.ready := Bool(true)
  } else {
    // We only want to send Finishes after we have collected all beats of
    // a multibeat Grant. But Grants from multiple managers or transactions may
    // get interleaved, so we could need a counter for each.
    val done = if(tlNetworkDoesNotInterleaveBeats) {
      connectIncomingDataBeatCounterWithHeader(io.grant)
    } else {
      val entries = 1 << tlClientXactIdBits
      def getId(g: LogicalNetworkIO[Grant]) = g.payload.client_xact_id
      assert(getId(io.grant.bits) <= UInt(entries), "Not enough grant beat counters, only " + entries + " entries.")
      connectIncomingDataBeatCountersWithHeader(io.grant, entries, getId).reduce(_||_)
    }
    val q = Module(new FinishQueue(outstanding))
    q.io.enq.valid := io.grant.fire() && g.requiresAck() && (!g.hasMultibeatData() || done)
    q.io.enq.bits <> g.makeFinish()
    q.io.enq.bits.manager_id := io.grant.bits.header.src

    io.finish.bits.header.src := UInt(srcId)
    io.finish.bits.header.dst := q.io.deq.bits.manager_id
    io.finish.bits.payload <> q.io.deq.bits
    io.finish.valid := q.io.deq.valid
    q.io.deq.ready := io.finish.ready

    io.refill.valid := (q.io.enq.ready || !g.requiresAck()) && io.grant.valid
    io.refill.bits := g
    io.grant.ready := (q.io.enq.ready || !g.requiresAck()) && io.refill.ready
    io.ready := q.io.enq.ready
  }
}

class FinishQueue(entries: Int)(implicit p: Parameters) extends Queue(new FinishToDst()(p), entries)

/** A port to convert [[uncore.ClientTileLinkIO]].flip into [[uncore.TileLinkIO]]
  *
  * Creates network headers for [[uncore.Acquire]] and [[uncore.Release]] messages,
  * calculating header.dst and filling in header.src.
  * Strips headers from [[uncore.Probe Probes]].
  * Passes [[uncore.GrantFromSrc]] and accepts [[uncore.FinishFromDst]] in response,
  * setting up the headers for each.
  *
  * @param clientId network port id of this agent
  * @param addrConvert how a physical address maps to a destination manager port id
  */
class ClientTileLinkNetworkPort(clientId: Int, addrConvert: UInt => UInt)
                               (implicit p: Parameters) extends TLModule()(p) {
  val io = new Bundle {
    val client = new ClientTileLinkIO().flip
    val network = new TileLinkIO
  }

  val acq_with_header = ClientTileLinkHeaderCreator(io.client.acquire, clientId, addrConvert)
  val rel_with_header = ClientTileLinkHeaderCreator(io.client.release, clientId, addrConvert)
  val fin_with_header = ClientTileLinkHeaderCreator(io.client.finish, clientId)
  val prb_without_header = DecoupledLogicalNetworkIOUnwrapper(io.network.probe)
  val gnt_without_header = DecoupledLogicalNetworkIOUnwrapper(io.network.grant)

  io.network.acquire <> acq_with_header
  io.network.release <> rel_with_header
  io.network.finish <> fin_with_header
  io.client.probe <> prb_without_header
  io.client.grant.bits.manager_id := io.network.grant.bits.header.src
  io.client.grant <> gnt_without_header
}

/** A port to convert [[uncore.ClientUncachedTileLinkIO]].flip into [[uncore.TileLinkIO]]
  *
  * Creates network headers for [[uncore.Acquire]] and [[uncore.Release]] messages,
  * calculating header.dst and filling in header.src.
  * Responds to [[uncore.Grant]] by automatically issuing [[uncore.Finish]] to the granting managers.
  *
  * @param clientId network port id of this agent
  * @param addrConvert how a physical address maps to a destination manager port id
  */
class ClientUncachedTileLinkNetworkPort(clientId: Int, addrConvert: UInt => UInt)
                               (implicit p: Parameters) extends TLModule()(p) {
  val io = new Bundle {
    val client = new ClientUncachedTileLinkIO().flip
    val network = new TileLinkIO
  }

  val finisher = Module(new FinishUnit(clientId))
  finisher.io.grant <> io.network.grant
  io.network.finish <> finisher.io.finish

  val acq_with_header = ClientTileLinkHeaderCreator(io.client.acquire, clientId, addrConvert)
  val gnt_without_header = finisher.io.refill

  io.network.acquire.bits := acq_with_header.bits
  io.network.acquire.valid := acq_with_header.valid && finisher.io.ready
  acq_with_header.ready := io.network.acquire.ready && finisher.io.ready
  io.client.grant <> gnt_without_header
  io.network.probe.ready :=  Bool(false)
  io.network.release.valid := Bool(false)
}

object ClientTileLinkHeaderCreator {
  def apply[T <: ClientToManagerChannel with HasManagerId](
        in: DecoupledIO[T],
        clientId: Int)
      (implicit p: Parameters): DecoupledIO[LogicalNetworkIO[T]] = {
    val out = Wire(new DecoupledIO(new LogicalNetworkIO(in.bits)))
    out.bits.payload := in.bits
    out.bits.header.src := UInt(clientId)
    out.bits.header.dst := in.bits.manager_id
    out.valid := in.valid
    in.ready := out.ready
    out
  }
  def apply[T <: ClientToManagerChannel with HasCacheBlockAddress](
        in: DecoupledIO[T],
        clientId: Int,
        addrConvert: UInt => UInt)
      (implicit p: Parameters): DecoupledIO[LogicalNetworkIO[T]] = {
    val out = Wire(new DecoupledIO(new LogicalNetworkIO(in.bits)))
    out.bits.payload := in.bits
    out.bits.header.src := UInt(clientId)
    out.bits.header.dst := addrConvert(in.bits.addr_block)
    out.valid := in.valid
    in.ready := out.ready
    out
  }
}

/** A port to convert [[uncore.ManagerTileLinkIO]].flip into [[uncore.TileLinkIO]].flip
  *
  * Creates network headers for [[uncore.Probe]] and [[uncore.Grant]] messagess,
  * calculating header.dst and filling in header.src.
  * Strips headers from [[uncore.Acquire]], [[uncore.Release]] and [[uncore.Finish]],
  * but supplies client_id instead.
  *
  * @param managerId the network port id of this agent
  * @param idConvert how a sharer id maps to a destination client port id
  */
class ManagerTileLinkNetworkPort(managerId: Int, idConvert: UInt => UInt)
                                (implicit p: Parameters) extends TLModule()(p) {
  val io = new Bundle {
    val manager = new ManagerTileLinkIO().flip
    val network = new TileLinkIO().flip
  }
  io.network.grant <> ManagerTileLinkHeaderCreator(io.manager.grant, managerId, (u: UInt) => u)
  io.network.probe <> ManagerTileLinkHeaderCreator(io.manager.probe, managerId, idConvert)
  io.manager.acquire <> DecoupledLogicalNetworkIOUnwrapper(io.network.acquire)
  io.manager.acquire.bits.client_id := io.network.acquire.bits.header.src
  io.manager.release <> DecoupledLogicalNetworkIOUnwrapper(io.network.release)
  io.manager.release.bits.client_id := io.network.release.bits.header.src
  io.manager.finish <> DecoupledLogicalNetworkIOUnwrapper(io.network.finish)
}

object ManagerTileLinkHeaderCreator {
  def apply[T <: ManagerToClientChannel with HasClientId](
        in: DecoupledIO[T],
        managerId: Int,
        idConvert: UInt => UInt)
      (implicit p: Parameters): DecoupledIO[LogicalNetworkIO[T]] = {
    val out = Wire(new DecoupledIO(new LogicalNetworkIO(in.bits)))
    out.bits.payload := in.bits
    out.bits.header.src := UInt(managerId)
    out.bits.header.dst := idConvert(in.bits.client_id)
    out.valid := in.valid
    in.ready := out.ready
    out
  }
}
