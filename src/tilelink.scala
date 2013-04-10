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

abstract class DirectionalFIFOIO[T <: Data]()(data: => T) extends FIFOIO()(data)
class ClientSourcedIO[T <: Data]()(data: => T)  extends DirectionalFIFOIO()(data) 
class ClientSourcedDataIO[T <: Data]()(data: => T)  extends ClientSourcedIO()(data) 
class MasterSourcedIO[T <: Data]()(data: => T) extends DirectionalFIFOIO()(data) {flip()}

class UncachedTileLinkIO(implicit conf: LogicalNetworkConfiguration) extends Bundle {
  val acquire      = (new ClientSourcedIO){(new LogicalNetworkIO){new Acquire }}
  val acquire_data = (new ClientSourcedDataIO){(new LogicalNetworkIO){new AcquireData }}
  val grant       = (new MasterSourcedIO) {(new LogicalNetworkIO){new Grant }}
  val grant_ack    = (new ClientSourcedIO){(new LogicalNetworkIO){new GrantAck }}
  override def clone = { new UncachedTileLinkIO().asInstanceOf[this.type] }
}

class TileLinkIO(implicit conf: LogicalNetworkConfiguration) extends UncachedTileLinkIO()(conf) { 
  val probe        = (new MasterSourcedIO){(new LogicalNetworkIO){new Probe }}
  val release      = (new ClientSourcedIO){(new LogicalNetworkIO){new Release }}
  val release_data = (new ClientSourcedDataIO){(new LogicalNetworkIO){new ReleaseData }}
  override def clone = { new TileLinkIO().asInstanceOf[this.type] }
}

object UncachedTileLinkIOArbiterShim {
  def apply[T <: HasClientTransactionId](in: ClientSourcedIO[LogicalNetworkIO[T]], id: Int, max: Int)(implicit lconf: LogicalNetworkConfiguration) = {
    val shim = (new UncachedTileLinkIOArbiterShim(id, max)){in.bits.payload.clone}
    shim.io.in <> in
    shim.io.out
  }
}
class UncachedTileLinkIOArbiterShim[T <: HasClientTransactionId](id: Int, max: Int)(data: => T)(implicit lconf: LogicalNetworkConfiguration) extends Component {
  val io = new Bundle {
    val in  = (new ClientSourcedIO){(new LogicalNetworkIO){ data }}.flip
    val out = (new ClientSourcedIO){(new LogicalNetworkIO){ data }}
  }
  io.out.bits := io.in.bits 
  io.out.bits.payload.client_xact_id := Cat(io.in.bits.payload.client_xact_id, UFix(id, log2Up(max)))
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
}


class UncachedTileLinkIOArbiter(n: Int)(implicit conf: LogicalNetworkConfiguration) extends Component {
  val io = new Bundle {
    val in = Vec(n) { new UncachedTileLinkIO }.flip
    val out = new UncachedTileLinkIO
  }

  val mem_cnt = Reg(resetVal = UFix(0, width = log2Up(REFILL_CYCLES)))
  val mem_cnt_next = mem_cnt + UFix(1)
  val locked = Reg(resetVal = Bool(false))
  val lock_idx = Reg(resetVal = UFix(n))

  when(io.out.acquire_data.valid && io.out.acquire_data.ready) {
    mem_cnt := mem_cnt_next
    when(!locked) {
      locked := Bool(true)
      lock_idx := Vec(io.in.map{ in => in.acquire_data.ready && in.acquire_data.valid}){Bool()}.indexWhere{i: Bool => i} 
    }
    when(mem_cnt_next === UFix(0)) {
      locked := Bool(false)
    }
  }

  val acqd_grant = ArbiterCtrl(io.in.map(_.acquire_data.valid))
  (0 until n).map(i => io.in(i).acquire_data.ready := Mux(locked, UFix(i) === lock_idx, acqd_grant(i)) && io.out.acquire_data.ready)
  var acqd_bits = io.in(n-1).acquire_data.bits
  for (i <- n-2 to 0 by -1) {
    acqd_bits = Mux(io.in(i).acquire_data.valid, io.in(i).acquire_data.bits, acqd_bits)
  }
  val locked_req = io.in(lock_idx).acquire_data
  io.out.acquire_data.bits := Mux(locked, locked_req.bits, acqd_bits)
  io.out.acquire_data.valid := Mux(locked, locked_req.valid, io.in.map(_.acquire_data.valid).reduce(_||_))

  val acq_arb = (new Arbiter(n)){ (new LogicalNetworkIO){new Acquire} }
  io.out.acquire <> acq_arb.io.out
  io.in.map(_.acquire).zipWithIndex.map{ case(acq, id) => UncachedTileLinkIOArbiterShim(acq, id, n) }.zip(acq_arb.io.in).map{ case (req, arb) => req <> arb}

  val grant_ack_arb = (new Arbiter(n)){ (new LogicalNetworkIO){new GrantAck} }
  io.out.grant_ack <> grant_ack_arb.io.out
  grant_ack_arb.io.in zip io.in map { case (arb, req) => arb <> req.grant_ack }

  io.out.grant.ready := Bool(false)
  for (i <- 0 until n) {
    val tag = io.out.grant.bits.payload.client_xact_id
    io.in(i).grant.valid := Bool(false)
    when (tag(log2Up(n)-1,0) === UFix(i)) {
      io.in(i).grant.valid := io.out.grant.valid
      io.out.grant.ready := io.in(i).grant.ready
    }
    io.in(i).grant.bits := io.out.grant.bits
    io.in(i).grant.bits.payload.client_xact_id := tag >> UFix(log2Up(n))
  }
}
