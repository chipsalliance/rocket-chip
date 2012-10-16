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

class TransactionInit extends PhysicalAddress {
  val x_type = Bits(width = X_INIT_TYPE_MAX_BITS)
  val tile_xact_id = Bits(width = TILE_XACT_ID_BITS)
  val write_mask = Bits(width = X_INIT_WRITE_MASK_BITS)
  val subword_addr = Bits(width = X_INIT_SUBWORD_ADDR_BITS)
  val atomic_opcode = Bits(width = X_INIT_ATOMIC_OP_BITS)
}

object TransactionInit 
{
  def apply(x_type: Bits, addr: UFix, tile_xact_id: UFix) = {
    val init = new TransactionInit
    init.x_type := x_type
    init.addr := addr
    init.tile_xact_id := tile_xact_id
    init
  }
  def apply(x_type: Bits, addr: UFix, tile_xact_id: UFix, write_mask: Bits) = {
    val init = new TransactionInit
    init.x_type := x_type
    init.addr := addr
    init.tile_xact_id := tile_xact_id
    init.write_mask := write_mask
    init
  }
  def apply(x_type: Bits, addr: UFix, tile_xact_id: UFix, subword_addr: UFix, atomic_opcode: UFix) = {
    val init = new TransactionInit
    init.x_type := x_type
    init.addr := addr
    init.tile_xact_id := tile_xact_id
    init.subword_addr := subword_addr
    init.atomic_opcode := atomic_opcode
    init
  }
}

class TransactionInitData extends MemData

class TransactionAbort extends Bundle {
  val tile_xact_id = Bits(width = TILE_XACT_ID_BITS)
}

class ProbeRequest extends PhysicalAddress {
  val p_type = Bits(width = P_REQ_TYPE_MAX_BITS)
  val global_xact_id = Bits(width = GLOBAL_XACT_ID_BITS)
}

class ProbeReply extends Bundle {
  val p_type = Bits(width = P_REP_TYPE_MAX_BITS)
  val global_xact_id = Bits(width = GLOBAL_XACT_ID_BITS)
}

class ProbeReplyData extends MemData

class TransactionReply extends MemData {
  val x_type = Bits(width = X_REP_TYPE_MAX_BITS)
  val tile_xact_id = Bits(width = TILE_XACT_ID_BITS)
  val global_xact_id = Bits(width = GLOBAL_XACT_ID_BITS)
  val require_ack = Bool()
}

class TransactionFinish extends Bundle {
  val global_xact_id = Bits(width = GLOBAL_XACT_ID_BITS)
}

class ioTileLink extends Bundle { 
  val xact_init      = (new FIFOIO) { new TransactionInit }
  val xact_init_data = (new FIFOIO) { new TransactionInitData }
  val xact_abort     = (new FIFOIO) { new TransactionAbort }.flip
  val probe_req      = (new FIFOIO) { new ProbeRequest }.flip
  val probe_rep      = (new FIFOIO) { new ProbeReply }
  val probe_rep_data = (new FIFOIO) { new ProbeReplyData }
  val xact_rep       = (new FIFOIO) { new TransactionReply }.flip
  val xact_finish    = (new FIFOIO) { new TransactionFinish }
  val incoherent     = Bool(OUTPUT)
}
