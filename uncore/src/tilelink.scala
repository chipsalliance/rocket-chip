package uncore

import Chisel._
import Constants._

class PhysicalAddress extends Bundle {
  val addr = UFix(width = PADDR_BITS - OFFSET_BITS)
}

class MemData extends Bundle {
  val data = Bits(width = MEM_DATA_BITS)
}

class MemReqCmd extends PhysicalAddress {
  val rw = Bool()
  val tag = Bits(width = MEM_TAG_BITS)
}

class MemResp extends MemData {
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

class Acquire extends PhysicalAddress {
  val a_type = Bits(width = ACQUIRE_TYPE_MAX_BITS)
  val client_xact_id = Bits(width = CLIENT_XACT_ID_BITS)
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
    acq
  }
  def apply(a_type: Bits, addr: UFix, client_xact_id: UFix, write_mask: Bits) = {
    val acq = new Acquire
    acq.a_type := a_type
    acq.addr := addr
    acq.client_xact_id := client_xact_id
    acq.write_mask := write_mask
    acq
  }
  def apply(a_type: Bits, addr: UFix, client_xact_id: UFix, subword_addr: UFix, atomic_opcode: UFix) = {
    val acq = new Acquire
    acq.a_type := a_type
    acq.addr := addr
    acq.client_xact_id := client_xact_id
    acq.subword_addr := subword_addr
    acq.atomic_opcode := atomic_opcode
    acq
  }
}

class AcquireData extends MemData

class Abort extends Bundle {
  val client_xact_id = Bits(width = CLIENT_XACT_ID_BITS)
}

class Probe extends PhysicalAddress {
  val p_type = Bits(width = PROBE_TYPE_MAX_BITS)
  val master_xact_id = Bits(width = MASTER_XACT_ID_BITS)
}

class Release extends Bundle {
  val r_type = Bits(width = RELEASE_TYPE_MAX_BITS)
  val master_xact_id = Bits(width = MASTER_XACT_ID_BITS)
}

class ReleaseData extends MemData

class Grant extends MemData {
  val g_type = Bits(width = GRANT_TYPE_MAX_BITS)
  val client_xact_id = Bits(width = CLIENT_XACT_ID_BITS)
  val master_xact_id = Bits(width = MASTER_XACT_ID_BITS)
  val require_ack = Bool()
}

class GrantAck extends Bundle {
  val master_xact_id = Bits(width = MASTER_XACT_ID_BITS)
}

abstract class DirectionalFIFOIO[T <: Data]()(data: => T) extends FIFOIO()(data)
class ClientSourcedIO[T <: Data]()(data: => T)  extends DirectionalFIFOIO()(data) 
class MasterSourcedIO[T <: Data]()(data: => T) extends DirectionalFIFOIO()(data) {flip()}

class TileLinkIO(implicit conf: LogicalNetworkConfiguration) extends Bundle { 
  val acquire      = (new ClientSourcedIO){(new LogicalNetworkIO){new Acquire }}
  val acquire_data = (new ClientSourcedIO){(new LogicalNetworkIO){new AcquireData }}
  val abort        = (new MasterSourcedIO){(new LogicalNetworkIO){new Abort }}
  val probe        = (new MasterSourcedIO){(new LogicalNetworkIO){new Probe }}
  val release      = (new ClientSourcedIO){(new LogicalNetworkIO){new Release }}
  val release_data = (new ClientSourcedIO){(new LogicalNetworkIO){new ReleaseData }}
  val grant        = (new MasterSourcedIO){(new LogicalNetworkIO){new Grant }}
  val grant_ack    = (new ClientSourcedIO){(new LogicalNetworkIO){new GrantAck }}
  override def clone = { new TileLinkIO().asInstanceOf[this.type] }
}
