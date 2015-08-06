// See LICENSE for license details.

package junctions
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

class MemIONASTISlaveIOConverter(cacheBlockOffsetBits: Int) extends MIFModule with NASTIParameters {
  val io = new Bundle {
    val nasti = new NASTISlaveIO
    val mem = new MemIO
  }

  require(mifDataBits == nastiXDataBits, "Data sizes between LLC and MC don't agree")
  val (mif_cnt_out, mif_wrap_out) = Counter(io.mem.resp.fire(), mifDataBeats)

  // according to the spec, we can't send b until the last transfer on w
  val b_ok = Reg(init = Bool(true))
  when (io.nasti.aw.fire()) { b_ok := Bool(false) }
  when (io.nasti.w.fire() && io.nasti.w.bits.last) { b_ok := Bool(true) }

  val id_q = Module(new Queue(UInt(width = nastiWIdBits), 2))
  id_q.io.enq.valid := io.nasti.aw.valid
  id_q.io.enq.bits := io.nasti.aw.bits.id
  id_q.io.deq.ready := io.nasti.b.ready && b_ok

  io.mem.req_cmd.bits.addr := Mux(io.nasti.aw.valid, io.nasti.aw.bits.addr, io.nasti.ar.bits.addr) >>
                                UInt(cacheBlockOffsetBits)
  io.mem.req_cmd.bits.tag := Mux(io.nasti.aw.valid, io.nasti.aw.bits.id, io.nasti.ar.bits.id)
  io.mem.req_cmd.bits.rw := io.nasti.aw.valid
  io.mem.req_cmd.valid := (io.nasti.aw.valid && id_q.io.enq.ready) || io.nasti.ar.valid
  io.nasti.ar.ready := io.mem.req_cmd.ready && !io.nasti.aw.valid
  io.nasti.aw.ready := io.mem.req_cmd.ready && id_q.io.enq.ready

  io.nasti.b.valid := id_q.io.deq.valid && b_ok
  io.nasti.b.bits.id := id_q.io.deq.bits
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
