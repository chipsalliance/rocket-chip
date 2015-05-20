// See LICENSE for license details.

package uncore
import Chisel._
import scala.math.max

case object NASTIDataBits extends Field[Int]
case object NASTIAddrBits extends Field[Int]
case object NASTIIdBits extends Field[Int]

trait NASTIParameters extends UsesParameters {
  val nastiXDataBits = params(NASTIDataBits)
  val nastiWStrobeBits = nastiXDataBits / 8
  val nastiXAddrBits = params(NASTIAddrBits)
  val nastiWIdBits = params(NASTIIdBits)
  val nastiRIdBits = params(NASTIIdBits)
  val nastiXIdBits = max(nastiWIdBits, nastiRIdBits)
  val nastiXUserBits = 1
  val nastiAWUserBits = nastiXUserBits
  val nastiWUserBits = nastiXUserBits
  val nastiBUserBits = nastiXUserBits
  val nastiARUserBits = nastiXUserBits
  val nastiRUserBits = nastiXUserBits
  val nastiXLenBits = 8
  val nastiXSizeBits = 3
  val nastiXBurstBits = 2
  val nastiXCacheBits = 4
  val nastiXProtBits = 3
  val nastiXQosBits = 4
  val nastiXRegionBits = 4
  val nastiXRespBits = 2

  def bytesToXSize(bytes: UInt) = MuxLookup(bytes, UInt("b111"), Array(
    UInt(1) -> UInt(0),
    UInt(2) -> UInt(1),
    UInt(4) -> UInt(2),
    UInt(8) -> UInt(3),
    UInt(16) -> UInt(4),
    UInt(32) -> UInt(5),
    UInt(64) -> UInt(6),
    UInt(128) -> UInt(7)))
}

abstract class NASTIBundle extends Bundle with NASTIParameters
abstract class NASTIModule extends Module with NASTIParameters

trait NASTIChannel extends NASTIBundle
trait NASTIMasterToSlaveChannel extends NASTIChannel
trait NASTISlaveToMasterChannel extends NASTIChannel

class NASTIMasterIO extends Bundle {
  val aw = Decoupled(new NASTIWriteAddressChannel)
  val w  = Decoupled(new NASTIWriteDataChannel)
  val b  = Decoupled(new NASTIWriteResponseChannel).flip
  val ar = Decoupled(new NASTIReadAddressChannel)
  val r  = Decoupled(new NASTIReadDataChannel).flip
}

class NASTISlaveIO extends NASTIMasterIO { flip() }

trait HasNASTIMetadata extends NASTIBundle {
  val addr   = UInt(width = nastiXAddrBits)
  val len    = UInt(width = nastiXLenBits)
  val size   = UInt(width = nastiXSizeBits)
  val burst  = UInt(width = nastiXBurstBits)
  val lock   = Bool()
  val cache  = UInt(width = nastiXCacheBits)
  val prot   = UInt(width = nastiXProtBits)
  val qos    = UInt(width = nastiXQosBits)
  val region = UInt(width = nastiXRegionBits)
}

trait HasNASTIData extends NASTIBundle {
  val data = UInt(width = nastiXDataBits)
  val last = Bool()
}

class NASTIAddressChannel extends NASTIMasterToSlaveChannel with HasNASTIMetadata

class NASTIResponseChannel extends NASTISlaveToMasterChannel {
  val resp = UInt(width = nastiXRespBits)
}

class NASTIWriteAddressChannel extends NASTIAddressChannel {
  val id   = UInt(width = nastiWIdBits)
  val user = UInt(width = nastiAWUserBits)
}

class NASTIWriteDataChannel extends NASTIMasterToSlaveChannel with HasNASTIData {
  val strb = UInt(width = nastiWStrobeBits)
  val user = UInt(width = nastiWUserBits)
}

class NASTIWriteResponseChannel extends NASTIResponseChannel {
  val id   = UInt(width = nastiWIdBits)
  val user = UInt(width = nastiBUserBits)
}

class NASTIReadAddressChannel extends NASTIAddressChannel {
  val id   = UInt(width = nastiRIdBits)
  val user = UInt(width = nastiARUserBits)
}

class NASTIReadDataChannel extends NASTIResponseChannel with HasNASTIData {
  val id   = UInt(width = nastiRIdBits)
  val user = UInt(width = nastiRUserBits)
}

class MemIONASTISlaveIOConverter extends MIFModule with NASTIParameters {
  val io = new Bundle {
    val nasti = new NASTISlaveIO
    val mem = new MemIO
  }

  require(mifDataBits == nastiXDataBits, "Data sizes between LLC and MC don't agree")
  val (mif_cnt_out, mif_wrap_out) = Counter(io.mem.resp.fire(), mifDataBeats)
  
  io.mem.req_cmd.bits.addr := Mux(io.nasti.aw.valid, io.nasti.aw.bits.addr, io.nasti.ar.bits.addr) >>
                                UInt(params(CacheBlockOffsetBits))
  io.mem.req_cmd.bits.tag := Mux(io.nasti.aw.valid, io.nasti.aw.bits.id, io.nasti.ar.bits.id)
  io.mem.req_cmd.bits.rw := io.nasti.aw.valid
  io.mem.req_cmd.valid := (io.nasti.aw.valid && io.nasti.b.ready) || io.nasti.ar.valid
  io.nasti.ar.ready := io.mem.req_cmd.ready && !io.nasti.aw.valid
  io.nasti.aw.ready := io.mem.req_cmd.ready && io.nasti.b.ready

  io.nasti.b.valid := io.nasti.aw.valid && io.mem.req_cmd.ready
  io.nasti.b.bits.id := io.nasti.aw.bits.id
  io.nasti.b.bits.resp := UInt(0)

  io.nasti.w.ready := io.mem.req_data.ready
  io.mem.req_data.valid := io.nasti.w.valid
  io.mem.req_data.bits.data := io.nasti.w.bits.data
  assert(!io.nasti.w.valid || io.nasti.w.bits.strb.andR, "MemIO must write full cache line")

  io.nasti.r.valid := io.mem.resp.valid
  io.nasti.r.bits.data := io.mem.resp.bits.data
  io.nasti.r.bits.last := mif_wrap_out
  io.nasti.r.bits.id := io.mem.resp.bits.tag
  io.nasti.r.bits.resp := UInt(0)
  io.mem.resp.ready := io.nasti.r.ready
}

class NASTIMasterIOTileLinkIOConverter extends TLModule with NASTIParameters {
  val io = new Bundle {
    val tl = new ManagerTileLinkIO
    val nasti = new NASTIMasterIO
  }

  val dataBits = tlDataBits*tlDataBeats 
  val dstIdBits = params(LNHeaderBits)
  require(tlDataBits == nastiXDataBits, "Data sizes between LLC and MC don't agree") // TODO: remove this restriction
  require(tlDataBeats < (1 << nastiXLenBits), "Can't have that many beats")
  require(dstIdBits + tlClientXactIdBits < nastiXIdBits, "NASTIMasterIO converter is going truncate tags: " + dstIdBits + " + " + tlClientXactIdBits + " >= " + nastiXIdBits)

  io.tl.acquire.ready := Bool(false)
  io.tl.probe.valid := Bool(false)
  io.tl.release.ready := Bool(false)
  io.tl.finish.ready := Bool(true)

  io.nasti.b.ready := Bool(false)
  io.nasti.r.ready := Bool(false)
  io.nasti.ar.valid := Bool(false)
  io.nasti.aw.valid := Bool(false)
  io.nasti.w.valid := Bool(false)

  val dst_off = dstIdBits + tlClientXactIdBits
  val acq_has_data = io.tl.acquire.bits.hasData()
  val rel_has_data = io.tl.release.bits.hasData()
  val is_write = io.tl.release.valid || (io.tl.acquire.valid && acq_has_data)

  // Decompose outgoing TL Acquires into NASTI address and data channels
  val active_out = Reg(init=Bool(false))
  val cmd_sent_out = Reg(init=Bool(false))
  val tag_out = Reg(UInt(width = nastiXIdBits))
  val addr_out = Reg(UInt(width = nastiXAddrBits))
  val has_data = Reg(init=Bool(false))
  val data_from_rel = Reg(init=Bool(false))
  val (tl_cnt_out, tl_wrap_out) =
    Counter((io.tl.acquire.fire() && acq_has_data) ||
              (io.tl.release.fire() && rel_has_data), tlDataBeats)
  val tl_done_out = Reg(init=Bool(false))

  io.nasti.ar.bits.id := tag_out
  io.nasti.ar.bits.addr := addr_out
  io.nasti.ar.bits.len := Mux(has_data, UInt(tlDataBeats-1), UInt(0)) 
  io.nasti.ar.bits.size := UInt(log2Ceil(tlDataBits))
  io.nasti.ar.bits.burst := UInt("b01")
  io.nasti.ar.bits.lock := Bool(false)
  io.nasti.ar.bits.cache := UInt("b0000")
  io.nasti.ar.bits.prot := UInt("b000")
  io.nasti.ar.bits.qos := UInt("b0000")
  io.nasti.ar.bits.region := UInt("b0000")
  io.nasti.ar.bits.user := UInt(0)
  io.nasti.aw.bits := io.nasti.ar.bits
  io.nasti.w.bits.strb := Mux(data_from_rel, SInt(-1), io.tl.acquire.bits.wmask())
  io.nasti.w.bits.data := Mux(data_from_rel, io.tl.release.bits.data, io.tl.acquire.bits.data)
  io.nasti.w.bits.last := tl_wrap_out

  when(!active_out){
    io.tl.release.ready := io.nasti.w.ready
    io.tl.acquire.ready := io.nasti.w.ready && !io.tl.release.valid
    io.nasti.w.valid := (io.tl.release.valid && rel_has_data) ||
                        (io.tl.acquire.valid && acq_has_data)
    when(io.nasti.w.ready && (io.tl.release.valid || io.tl.acquire.valid)) {
      active_out := (!is_write && !io.nasti.ar.ready) ||
                    (is_write && !(io.nasti.aw.ready && io.nasti.w.ready)) ||
                    (io.nasti.w.valid && Bool(tlDataBeats > 1))
      io.nasti.aw.valid := is_write
      io.nasti.ar.valid := !is_write
      cmd_sent_out := (!is_write && io.nasti.ar.ready) || (is_write && io.nasti.aw.ready)
      tl_done_out := tl_wrap_out
      when(io.tl.release.valid) {
        data_from_rel := Bool(true)
        io.nasti.w.bits.data := io.tl.release.bits.data
        io.nasti.w.bits.strb := SInt(-1)
        val tag =  Cat(io.tl.release.bits.client_id,
                       io.tl.release.bits.client_xact_id,
                       io.tl.release.bits.isVoluntary())
        val addr = io.tl.release.bits.full_addr()
        io.nasti.aw.bits.id := tag
        io.nasti.aw.bits.addr := addr
        io.nasti.aw.bits.len := UInt(tlDataBeats-1)
        io.nasti.aw.bits.size := MT_Q
        tag_out := tag
        addr_out := addr
        has_data := rel_has_data
      } .elsewhen(io.tl.acquire.valid) {
        data_from_rel := Bool(false)
        io.nasti.w.bits.data := io.tl.acquire.bits.data
        io.nasti.w.bits.strb := io.tl.acquire.bits.wmask()
        val tag = Cat(io.tl.acquire.bits.client_id,
                      io.tl.acquire.bits.client_xact_id,
                      io.tl.acquire.bits.isBuiltInType())
        val addr = io.tl.acquire.bits.full_addr()
        when(is_write) {
          io.nasti.aw.bits.id := tag
          io.nasti.aw.bits.addr := addr
          io.nasti.aw.bits.len := Mux(io.tl.acquire.bits.isBuiltInType(Acquire.putBlockType),
                                    UInt(tlDataBeats-1), UInt(0)) 
          io.nasti.aw.bits.size := bytesToXSize(PopCount(io.tl.acquire.bits.wmask()))
        } .otherwise {
          io.nasti.ar.bits.id := tag
          io.nasti.ar.bits.addr := addr
          io.nasti.ar.bits.len := Mux(io.tl.acquire.bits.isBuiltInType(Acquire.getBlockType),
                                    UInt(tlDataBeats-1), UInt(0)) 
          io.nasti.ar.bits.size := io.tl.acquire.bits.op_size()
        }
        tag_out := tag
        addr_out := addr
        has_data := acq_has_data
      }
    }
  }
  when(active_out) {
    io.nasti.ar.valid := !cmd_sent_out && !has_data
    io.nasti.aw.valid := !cmd_sent_out && has_data
    cmd_sent_out := cmd_sent_out || io.nasti.ar.fire() || io.nasti.aw.fire()
    when(has_data && !tl_done_out) {
      when(data_from_rel) {
        io.tl.release.ready := io.nasti.w.ready
        io.nasti.w.valid := io.tl.release.valid
      } .otherwise {
        io.tl.acquire.ready := io.nasti.w.ready
        io.nasti.w.valid := io.tl.acquire.valid
      }
    }
    when(tl_wrap_out) { tl_done_out := Bool(true) }
    when(cmd_sent_out && (!has_data || tl_done_out)) { active_out := Bool(false) }
  }

  // Aggregate incoming NASTI responses into TL Grants
  val (tl_cnt_in, tl_wrap_in) = Counter(io.tl.grant.fire() && io.tl.grant.bits.hasMultibeatData(), tlDataBeats)
  val gnt_arb = Module(new Arbiter(new GrantToDst, 2))
  io.tl.grant <> gnt_arb.io.out

  gnt_arb.io.in(0).valid := io.nasti.r.valid
  io.nasti.r.ready := gnt_arb.io.in(0).ready
  gnt_arb.io.in(0).bits := Grant(
    dst = (if(dstIdBits > 0) io.nasti.r.bits.id(dst_off, tlClientXactIdBits + 1) else UInt(0)),
    is_builtin_type = io.nasti.r.bits.id(0),
    g_type = Mux(io.nasti.r.bits.id(0), Grant.getDataBlockType, UInt(0)), // TODO: Assumes MI or MEI protocol
    client_xact_id = io.nasti.r.bits.id >> UInt(1),
    manager_xact_id = UInt(0),
    addr_beat = tl_cnt_in,
    data = io.nasti.r.bits.data)

  gnt_arb.io.in(1).valid := io.nasti.b.valid
  io.nasti.b.ready := gnt_arb.io.in(1).ready
  gnt_arb.io.in(1).bits := Grant(
    dst = (if(dstIdBits > 0) io.nasti.b.bits.id(dst_off, tlClientXactIdBits + 1) else UInt(0)),
    is_builtin_type = Bool(true),
    g_type = Mux(io.nasti.b.bits.id(0), Grant.voluntaryAckType, Grant.putAckType),
    client_xact_id = io.nasti.b.bits.id >> UInt(1),
    manager_xact_id = UInt(0))
}
