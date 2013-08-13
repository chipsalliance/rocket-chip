package uncore
import Chisel._

case class TileLinkConfiguration(co: CoherencePolicyWithUncached, ln: LogicalNetworkConfiguration, masterXactIdBits: Int, clientXactIdBits: Int, dataBits: Int) 

abstract trait TileLinkSubBundle extends Bundle {
  implicit val conf: TileLinkConfiguration
  override def clone = this.getClass.getConstructors.head.newInstance(conf).asInstanceOf[this.type]
}

trait HasPhysicalAddress extends TileLinkSubBundle {
  val addr = UInt(width = PADDR_BITS - OFFSET_BITS)
}

trait HasClientTransactionId extends TileLinkSubBundle {
  val client_xact_id = Bits(width = conf.clientXactIdBits)
}

trait HasMasterTransactionId extends TileLinkSubBundle {
  val master_xact_id = Bits(width = conf.masterXactIdBits)
}

trait HasTileLinkData extends TileLinkSubBundle {
  val data = Bits(width = conf.dataBits)
}

trait SourcedMessage extends Bundle
trait ClientSourcedMessage extends SourcedMessage
trait MasterSourcedMessage extends SourcedMessage

object Acquire 
{
  def apply(a_type: Bits, addr: UInt, client_xact_id: UInt)(implicit conf: TileLinkConfiguration): Acquire = {
    val acq = new Acquire
    acq.a_type := a_type
    acq.addr := addr
    acq.client_xact_id := client_xact_id
    acq.write_mask := Bits(0)
    acq.subword_addr := Bits(0)
    acq.atomic_opcode := Bits(0)
    acq
  }
  def apply(a_type: Bits, addr: UInt, client_xact_id: UInt, write_mask: Bits)(implicit conf: TileLinkConfiguration): Acquire = {
    val acq = apply(a_type, addr, client_xact_id)
    acq.write_mask := write_mask
    acq
  }
  def apply(a_type: Bits, addr: UInt, client_xact_id: UInt, subword_addr: UInt, atomic_opcode: UInt)(implicit conf: TileLinkConfiguration): Acquire = {
    val acq = apply(a_type, addr, client_xact_id)
    acq.subword_addr := subword_addr
    acq.atomic_opcode := atomic_opcode
    acq
  }
}
class Acquire(implicit val conf: TileLinkConfiguration) extends ClientSourcedMessage with HasPhysicalAddress with HasClientTransactionId {
  val a_type = Bits(width = conf.co.acquireTypeWidth)
  val write_mask = Bits(width = ACQUIRE_WRITE_MASK_BITS)
  val subword_addr = Bits(width = ACQUIRE_SUBWORD_ADDR_BITS)
  val atomic_opcode = Bits(width = ACQUIRE_ATOMIC_OP_BITS)
}

class AcquireData(implicit val conf: TileLinkConfiguration) extends ClientSourcedMessage with HasTileLinkData

class Probe(implicit val conf: TileLinkConfiguration) extends MasterSourcedMessage with HasPhysicalAddress with HasMasterTransactionId {
  val p_type = Bits(width = conf.co.probeTypeWidth)
}

object Release
{
  def apply(r_type: Bits, addr: UInt, client_xact_id: UInt, master_xact_id: UInt)(implicit conf: TileLinkConfiguration) = {
    val rel = new Release
    rel.r_type := r_type
    rel.addr := addr
    rel.client_xact_id := client_xact_id
    rel.master_xact_id := master_xact_id
    rel
  }
}
class Release(implicit val conf: TileLinkConfiguration) extends ClientSourcedMessage with HasPhysicalAddress with HasClientTransactionId with HasMasterTransactionId {
  val r_type = Bits(width = conf.co.releaseTypeWidth)
}

class ReleaseData(implicit val conf: TileLinkConfiguration) extends ClientSourcedMessage with HasTileLinkData

class Grant(implicit val conf: TileLinkConfiguration) extends MasterSourcedMessage with HasTileLinkData with HasClientTransactionId with HasMasterTransactionId {
  val g_type = Bits(width = conf.co.grantTypeWidth)
}

class GrantAck(implicit val conf: TileLinkConfiguration) extends ClientSourcedMessage with HasMasterTransactionId

trait DirectionalIO
trait ClientSourcedIO extends DirectionalIO
trait MasterSourcedIO extends DirectionalIO
class ClientSourcedFIFOIO[T <: Data]()(data: => T) extends DecoupledIO(data) with ClientSourcedIO {
  override def clone = { new ClientSourcedFIFOIO()(data).asInstanceOf[this.type] }
}
class ClientSourcedDataIO[M <: Data, D <: Data]()(meta: => M, data: => D)  extends PairedDataIO()(meta,data) with ClientSourcedIO {
  override def clone = { new ClientSourcedDataIO()(meta,data).asInstanceOf[this.type] }
}
class MasterSourcedFIFOIO[T <: Data]()(data: => T) extends DecoupledIO(data) with MasterSourcedIO {
  flip()
  override def clone = { new MasterSourcedFIFOIO()(data).asInstanceOf[this.type] }
}
class MasterSourcedDataIO[M <: Data, D <: Data]()(meta: => M, data: => D)  extends PairedDataIO()(meta,data) with MasterSourcedIO {
  flip()
  override def clone = { new MasterSourcedDataIO()(meta,data).asInstanceOf[this.type] }
}

class UncachedTileLinkIO(implicit conf: TileLinkConfiguration) extends Bundle {
  implicit val ln = conf.ln
  val acquire   = new ClientSourcedDataIO()(new LogicalNetworkIO()(new Acquire), new LogicalNetworkIO()(new AcquireData))
  val grant     = new MasterSourcedFIFOIO()(new LogicalNetworkIO()(new Grant))
  val grant_ack = new ClientSourcedFIFOIO()(new LogicalNetworkIO()(new GrantAck))
  override def clone = { new UncachedTileLinkIO().asInstanceOf[this.type] }
}

class TileLinkIO(implicit conf: TileLinkConfiguration) extends UncachedTileLinkIO()(conf) { 
  val probe     = new MasterSourcedFIFOIO()(new LogicalNetworkIO()(new Probe))
  val release   = new ClientSourcedDataIO()(new LogicalNetworkIO()(new Release), new LogicalNetworkIO()(new ReleaseData))
  override def clone = { new TileLinkIO().asInstanceOf[this.type] }
}

/*
 * TODO: Merge the below classes into children of an abstract class in Chisel 2.0
abstract class UncachedTileLinkIOArbiter(n: Int, co: CoherencePolicy)(implicit conf: LogicalNetworkConfiguration) extends Module {
  def acquireClientXactId(in: Acquire, id: Int): Bits
  def grantClientXactId(in: Grant): Bits
  def arbIdx(in: Grant): UInt
}
*/

class UncachedTileLinkIOArbiterThatAppendsArbiterId(n: Int)(implicit conf: TileLinkConfiguration) extends Module {
  implicit val (ln, co) = (conf.ln, conf.co)
  def acquireClientXactId(in: Acquire, id: Int) = Cat(in.client_xact_id, UInt(id, log2Up(n)))
  def grantClientXactId(in: Grant) = in.client_xact_id >> UInt(log2Up(n))
  def arbIdx(in: Grant) = in.client_xact_id(log2Up(n)-1,0).toUInt

  val io = new Bundle {
    val in = Vec.fill(n){new UncachedTileLinkIO}.flip
    val out = new UncachedTileLinkIO
  }
  def acqHasData(acq: LogicalNetworkIO[Acquire]) = co.messageHasData(acq.payload)
  val acq_arb = Module(new PairedLockingRRArbiter(n, REFILL_CYCLES, acqHasData _)((new LogicalNetworkIO){new Acquire},(new LogicalNetworkIO){new AcquireData}))
  io.out.acquire <> acq_arb.io.out
  io.in.map(_.acquire).zipWithIndex.zip(acq_arb.io.in).map{ case ((req,id), arb) => {
    arb.data <> req.data
    arb.meta.valid := req.meta.valid
    arb.meta.bits := req.meta.bits
    arb.meta.bits.payload.client_xact_id := acquireClientXactId(req.meta.bits.payload, id)
    req.meta.ready := arb.meta.ready
  }}

  val grant_ack_arb = Module(new RRArbiter((new LogicalNetworkIO){new GrantAck},n))
  io.out.grant_ack <> grant_ack_arb.io.out
  grant_ack_arb.io.in zip io.in map { case (arb, req) => arb <> req.grant_ack }

  io.out.grant.ready := Bool(false)
  for (i <- 0 until n) {
    io.in(i).grant.valid := Bool(false)
    when (arbIdx(io.out.grant.bits.payload) === UInt(i)) {
      io.in(i).grant.valid := io.out.grant.valid
      io.out.grant.ready := io.in(i).grant.ready
    }
    io.in(i).grant.bits := io.out.grant.bits
    io.in(i).grant.bits.payload.client_xact_id := grantClientXactId(io.out.grant.bits.payload) 
  }
}

class UncachedTileLinkIOArbiterThatPassesId(n: Int)(implicit conf: TileLinkConfiguration) extends Module {
  implicit val (ln, co) = (conf.ln, conf.co)
  def acquireClientXactId(in: Acquire, id: Int) = in.client_xact_id
  def grantClientXactId(in: Grant) = in.client_xact_id
  def arbIdx(in: Grant): UInt = in.client_xact_id

  val io = new Bundle {
    val in = Vec.fill(n){new UncachedTileLinkIO}.flip
    val out = new UncachedTileLinkIO
  }
  def acqHasData(acq: LogicalNetworkIO[Acquire]) = co.messageHasData(acq.payload)
  val acq_arb = Module(new PairedLockingRRArbiter(n, REFILL_CYCLES, acqHasData _)((new LogicalNetworkIO){new Acquire},(new LogicalNetworkIO){new AcquireData}))
  io.out.acquire <> acq_arb.io.out
  io.in.map(_.acquire).zipWithIndex.zip(acq_arb.io.in).map{ case ((req,id), arb) => {
    arb.data <> req.data
    arb.meta.valid := req.meta.valid
    arb.meta.bits := req.meta.bits
    arb.meta.bits.payload.client_xact_id := acquireClientXactId(req.meta.bits.payload, id)
    req.meta.ready := arb.meta.ready
  }}

  val grant_ack_arb = Module(new RRArbiter((new LogicalNetworkIO){new GrantAck},n))
  io.out.grant_ack <> grant_ack_arb.io.out
  grant_ack_arb.io.in zip io.in map { case (arb, req) => arb <> req.grant_ack }

  io.out.grant.ready := Bool(false)
  for (i <- 0 until n) {
    io.in(i).grant.valid := Bool(false)
    when (arbIdx(io.out.grant.bits.payload) === UInt(i)) {
      io.in(i).grant.valid := io.out.grant.valid
      io.out.grant.ready := io.in(i).grant.ready
    }
    io.in(i).grant.bits := io.out.grant.bits
    io.in(i).grant.bits.payload.client_xact_id := grantClientXactId(io.out.grant.bits.payload) 
  }
}

class UncachedTileLinkIOArbiterThatUsesNewId(n: Int)(implicit conf: TileLinkConfiguration) extends Module {
  implicit val (ln, co) = (conf.ln, conf.co)
  def acquireClientXactId(in: Acquire, id: Int) = UInt(id, log2Up(n))
  def grantClientXactId(in: Grant) = UInt(0) // DNC 
  def arbIdx(in: Grant) = in.client_xact_id

  val io = new Bundle {
    val in = Vec.fill(n){new UncachedTileLinkIO}.flip
    val out = new UncachedTileLinkIO
  }
  def acqHasData(acq: LogicalNetworkIO[Acquire]) = co.messageHasData(acq.payload)
  val acq_arb = Module(new PairedLockingRRArbiter(n, REFILL_CYCLES, acqHasData _)((new LogicalNetworkIO){new Acquire},(new LogicalNetworkIO){new AcquireData}))
  io.out.acquire <> acq_arb.io.out
  io.in.map(_.acquire).zipWithIndex.zip(acq_arb.io.in).map{ case ((req,id), arb) => {
    arb.data <> req.data
    arb.meta.valid := req.meta.valid
    arb.meta.bits := req.meta.bits
    arb.meta.bits.payload.client_xact_id := acquireClientXactId(req.meta.bits.payload, id)
    req.meta.ready := arb.meta.ready
  }}

  val grant_ack_arb = Module(new RRArbiter((new LogicalNetworkIO){new GrantAck},n))
  io.out.grant_ack <> grant_ack_arb.io.out
  grant_ack_arb.io.in zip io.in map { case (arb, req) => arb <> req.grant_ack }

  io.out.grant.ready := Bool(false)
  for (i <- 0 until n) {
    io.in(i).grant.valid := Bool(false)
    when (arbIdx(io.out.grant.bits.payload) === UInt(i)) {
      io.in(i).grant.valid := io.out.grant.valid
      io.out.grant.ready := io.in(i).grant.ready
    }
    io.in(i).grant.bits := io.out.grant.bits
    io.in(i).grant.bits.payload.client_xact_id := grantClientXactId(io.out.grant.bits.payload) 
  }
}

