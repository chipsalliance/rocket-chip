package uncore

import Chisel._
import Constants._

trait HasPhysicalAddress extends Bundle {
  val addr = UFix(width = PADDR_BITS - OFFSET_BITS)
}

trait HasClientTransactionId extends Bundle {
  val client_xact_id = Bits(width = CLIENT_XACT_ID_MAX_BITS)
}

trait HasMasterTransactionId extends Bundle {
  val master_xact_id = Bits(width = MASTER_XACT_ID_MAX_BITS)
}

trait HasMemData extends Bundle {
  val data = Bits(width = MEM_DATA_BITS)
}

class MemData extends Bundle with HasMemData

class MemReqCmd extends Bundle with HasPhysicalAddress {
  val rw = Bool()
  val tag = Bits(width = MEM_TAG_BITS)
}

class MemResp extends Bundle with HasMemData {
  val tag = Bits(width = MEM_TAG_BITS)
}

class ioMem extends Bundle {
  val req_cmd  = (new FIFOIO) { new MemReqCmd() }
  val req_data = (new FIFOIO) { new MemData() }
  val resp     = (new FIFOIO) { new MemResp() }.flip
}

class ioMemPipe extends Bundle {
  val req_cmd  = (new FIFOIO) { new MemReqCmd() }
  val req_data = (new FIFOIO) { new MemData() }
  val resp     = (new PipeIO) { new MemResp() }.flip
}

trait SourcedMessage extends Bundle
trait ClientSourcedMessage extends SourcedMessage
trait MasterSourcedMessage extends SourcedMessage

class Acquire extends ClientSourcedMessage with HasPhysicalAddress with HasClientTransactionId {
  val a_type = Bits(width = ACQUIRE_TYPE_MAX_BITS)
  val write_mask = Bits(width = ACQUIRE_WRITE_MASK_BITS)
  val subword_addr = Bits(width = ACQUIRE_SUBWORD_ADDR_BITS)
  val atomic_opcode = Bits(width = ACQUIRE_ATOMIC_OP_BITS)
}

object Acquire 
{
  def apply(a_type: Bits, addr: UFix, client_xact_id: UFix) = {
    val acq = new Acquire
    acq.a_type := a_type
    acq.addr := addr
    acq.client_xact_id := client_xact_id
    acq.write_mask := Bits(0, width = ACQUIRE_WRITE_MASK_BITS)
    acq.subword_addr := Bits(0, width = ACQUIRE_SUBWORD_ADDR_BITS)
    acq.atomic_opcode := Bits(0, width = ACQUIRE_ATOMIC_OP_BITS)
    acq
  }
  def apply(a_type: Bits, addr: UFix, client_xact_id: UFix, write_mask: Bits) = {
    val acq = new Acquire
    acq.a_type := a_type
    acq.addr := addr
    acq.client_xact_id := client_xact_id
    acq.write_mask := write_mask
    acq.subword_addr := Bits(0, width = ACQUIRE_SUBWORD_ADDR_BITS)
    acq.atomic_opcode := Bits(0, width = ACQUIRE_ATOMIC_OP_BITS)
    acq
  }
  def apply(a_type: Bits, addr: UFix, client_xact_id: UFix, subword_addr: UFix, atomic_opcode: UFix) = {
    val acq = new Acquire
    acq.a_type := a_type
    acq.addr := addr
    acq.client_xact_id := client_xact_id
    acq.subword_addr := subword_addr
    acq.atomic_opcode := atomic_opcode
    acq.write_mask := Bits(0, width = ACQUIRE_WRITE_MASK_BITS)
    acq
  }
}

class AcquireData extends ClientSourcedMessage with HasMemData

class Probe extends MasterSourcedMessage with HasPhysicalAddress with HasMasterTransactionId {
  val p_type = Bits(width = PROBE_TYPE_MAX_BITS)
}

object Release
{
  def apply(r_type: Bits, addr: UFix, client_xact_id: UFix, master_xact_id: UFix) = {
    val rel = new Release
    rel.r_type := r_type
    rel.addr := addr
    rel.client_xact_id := client_xact_id
    rel.master_xact_id := master_xact_id
    rel
  }
}
class Release extends ClientSourcedMessage with HasPhysicalAddress with HasClientTransactionId with HasMasterTransactionId {
  val r_type = Bits(width = RELEASE_TYPE_MAX_BITS)
}

class ReleaseData extends ClientSourcedMessage with HasMemData

class Grant extends MasterSourcedMessage with HasMemData with HasClientTransactionId with HasMasterTransactionId {
  val g_type = Bits(width = GRANT_TYPE_MAX_BITS)
}

class GrantAck extends ClientSourcedMessage with HasMasterTransactionId 

trait DirectionalIO
trait ClientSourcedIO extends DirectionalIO
trait MasterSourcedIO extends DirectionalIO
class ClientSourcedFIFOIO[T <: Data]()(data: => T) extends FIFOIO()(data) with ClientSourcedIO {
  override def clone = { new ClientSourcedFIFOIO()(data).asInstanceOf[this.type] }
}
class ClientSourcedDataIO[M <: Data, D <: Data]()(meta: => M, data: => D)  extends PairedDataIO()(meta,data) with ClientSourcedIO {
  override def clone = { new ClientSourcedDataIO()(meta,data).asInstanceOf[this.type] }
}
class MasterSourcedFIFOIO[T <: Data]()(data: => T) extends FIFOIO()(data) with MasterSourcedIO {
  flip()
  override def clone = { new MasterSourcedFIFOIO()(data).asInstanceOf[this.type] }
}
class MasterSourcedDataIO[M <: Data, D <: Data]()(meta: => M, data: => D)  extends PairedDataIO()(meta,data) with MasterSourcedIO {
  flip()
  override def clone = { new MasterSourcedDataIO()(meta,data).asInstanceOf[this.type] }
}

class UncachedTileLinkIO(implicit conf: LogicalNetworkConfiguration) extends Bundle {
  val acquire   = new ClientSourcedDataIO()(new LogicalNetworkIO()(new Acquire), new LogicalNetworkIO()(new AcquireData))
  val grant     = new MasterSourcedFIFOIO()(new LogicalNetworkIO()(new Grant))
  val grant_ack = new ClientSourcedFIFOIO()(new LogicalNetworkIO()(new GrantAck))
  override def clone = { new UncachedTileLinkIO().asInstanceOf[this.type] }
}

class TileLinkIO(implicit conf: LogicalNetworkConfiguration) extends UncachedTileLinkIO()(conf) { 
  val probe     = new MasterSourcedFIFOIO()(new LogicalNetworkIO()(new Probe))
  val release   = new ClientSourcedDataIO()(new LogicalNetworkIO()(new Release), new LogicalNetworkIO()(new ReleaseData))
  override def clone = { new TileLinkIO().asInstanceOf[this.type] }
}

/*
 * TODO: Merge the below classes into children of an abstract class in Chisel 2.0
abstract class UncachedTileLinkIOArbiter(n: Int, co: CoherencePolicy)(implicit conf: LogicalNetworkConfiguration) extends Component {
  def acquireClientXactId(in: Acquire, id: Int): Bits
  def grantClientXactId(in: Grant): Bits
  def arbIdx(in: Grant): UFix
}
*/

class UncachedTileLinkIOArbiterThatAppendsArbiterId(n: Int, co: CoherencePolicy)(implicit conf: LogicalNetworkConfiguration) extends Component {
  def acquireClientXactId(in: Acquire, id: Int) = Cat(in.client_xact_id, UFix(id, log2Up(n)))
  def grantClientXactId(in: Grant) = in.client_xact_id >> UFix(log2Up(n))
  def arbIdx(in: Grant) = in.client_xact_id(log2Up(n)-1,0).toUFix

  val io = new Bundle {
    val in = Vec(n) { new UncachedTileLinkIO }.flip
    val out = new UncachedTileLinkIO
  }
  def acqHasData(acq: LogicalNetworkIO[Acquire]) = co.messageHasData(acq.payload)
  val acq_arb = new PairedLockingRRArbiter(n, REFILL_CYCLES, acqHasData _)((new LogicalNetworkIO){new Acquire},(new LogicalNetworkIO){new AcquireData})
  io.out.acquire <> acq_arb.io.out
  io.in.map(_.acquire).zipWithIndex.zip(acq_arb.io.in).map{ case ((req,id), arb) => {
    arb.data <> req.data
    arb.meta.valid := req.meta.valid
    arb.meta.bits := req.meta.bits
    arb.meta.bits.payload.client_xact_id := acquireClientXactId(req.meta.bits.payload, id)
    req.meta.ready := arb.meta.ready
  }}

  val grant_ack_arb = (new RRArbiter(n)){ (new LogicalNetworkIO){new GrantAck} }
  io.out.grant_ack <> grant_ack_arb.io.out
  grant_ack_arb.io.in zip io.in map { case (arb, req) => arb <> req.grant_ack }

  io.out.grant.ready := Bool(false)
  for (i <- 0 until n) {
    io.in(i).grant.valid := Bool(false)
    when (arbIdx(io.out.grant.bits.payload) === UFix(i)) {
      io.in(i).grant.valid := io.out.grant.valid
      io.out.grant.ready := io.in(i).grant.ready
    }
    io.in(i).grant.bits := io.out.grant.bits
    io.in(i).grant.bits.payload.client_xact_id := grantClientXactId(io.out.grant.bits.payload) 
  }
}

class UncachedTileLinkIOArbiterThatPassesId(n: Int, co: CoherencePolicy)(implicit conf: LogicalNetworkConfiguration) extends Component {
  def acquireClientXactId(in: Acquire, id: Int) = in.client_xact_id
  def grantClientXactId(in: Grant) = in.client_xact_id
  def arbIdx(in: Grant): UFix = in.client_xact_id

  val io = new Bundle {
    val in = Vec(n) { new UncachedTileLinkIO }.flip
    val out = new UncachedTileLinkIO
  }
  def acqHasData(acq: LogicalNetworkIO[Acquire]) = co.messageHasData(acq.payload)
  val acq_arb = new PairedLockingRRArbiter(n, REFILL_CYCLES, acqHasData _)((new LogicalNetworkIO){new Acquire},(new LogicalNetworkIO){new AcquireData})
  io.out.acquire <> acq_arb.io.out
  io.in.map(_.acquire).zipWithIndex.zip(acq_arb.io.in).map{ case ((req,id), arb) => {
    arb.data <> req.data
    arb.meta.valid := req.meta.valid
    arb.meta.bits := req.meta.bits
    arb.meta.bits.payload.client_xact_id := acquireClientXactId(req.meta.bits.payload, id)
    req.meta.ready := arb.meta.ready
  }}

  val grant_ack_arb = (new RRArbiter(n)){ (new LogicalNetworkIO){new GrantAck} }
  io.out.grant_ack <> grant_ack_arb.io.out
  grant_ack_arb.io.in zip io.in map { case (arb, req) => arb <> req.grant_ack }

  io.out.grant.ready := Bool(false)
  for (i <- 0 until n) {
    io.in(i).grant.valid := Bool(false)
    when (arbIdx(io.out.grant.bits.payload) === UFix(i)) {
      io.in(i).grant.valid := io.out.grant.valid
      io.out.grant.ready := io.in(i).grant.ready
    }
    io.in(i).grant.bits := io.out.grant.bits
    io.in(i).grant.bits.payload.client_xact_id := grantClientXactId(io.out.grant.bits.payload) 
  }
}

class UncachedTileLinkIOArbiterThatUsesNewId(n: Int, co: CoherencePolicy)(implicit conf: LogicalNetworkConfiguration) extends Component {
  def acquireClientXactId(in: Acquire, id: Int) = UFix(id, log2Up(n))
  def grantClientXactId(in: Grant) = UFix(0) // DNC 
  def arbIdx(in: Grant) = in.client_xact_id

  val io = new Bundle {
    val in = Vec(n) { new UncachedTileLinkIO }.flip
    val out = new UncachedTileLinkIO
  }
  def acqHasData(acq: LogicalNetworkIO[Acquire]) = co.messageHasData(acq.payload)
  val acq_arb = new PairedLockingRRArbiter(n, REFILL_CYCLES, acqHasData _)((new LogicalNetworkIO){new Acquire},(new LogicalNetworkIO){new AcquireData})
  io.out.acquire <> acq_arb.io.out
  io.in.map(_.acquire).zipWithIndex.zip(acq_arb.io.in).map{ case ((req,id), arb) => {
    arb.data <> req.data
    arb.meta.valid := req.meta.valid
    arb.meta.bits := req.meta.bits
    arb.meta.bits.payload.client_xact_id := acquireClientXactId(req.meta.bits.payload, id)
    req.meta.ready := arb.meta.ready
  }}

  val grant_ack_arb = (new RRArbiter(n)){ (new LogicalNetworkIO){new GrantAck} }
  io.out.grant_ack <> grant_ack_arb.io.out
  grant_ack_arb.io.in zip io.in map { case (arb, req) => arb <> req.grant_ack }

  io.out.grant.ready := Bool(false)
  for (i <- 0 until n) {
    io.in(i).grant.valid := Bool(false)
    when (arbIdx(io.out.grant.bits.payload) === UFix(i)) {
      io.in(i).grant.valid := io.out.grant.valid
      io.out.grant.ready := io.in(i).grant.ready
    }
    io.in(i).grant.bits := io.out.grant.bits
    io.in(i).grant.bits.payload.client_xact_id := grantClientXactId(io.out.grant.bits.payload) 
  }
}

