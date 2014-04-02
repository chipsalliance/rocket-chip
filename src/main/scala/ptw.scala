package rocket

import Chisel._
import uncore._
import Util._

class PTWResp()(implicit conf: AddressSpaceConfiguration) extends Bundle {
  val error = Bool()
  val ppn = UInt(width = conf.ppnBits)
  val perm = Bits(width = conf.permBits)

  override def clone = new PTWResp().asInstanceOf[this.type]
}

class TLBPTWIO()(implicit conf: AddressSpaceConfiguration) extends Bundle {
  val req = Decoupled(UInt(width = conf.vpnBits))
  val resp = Valid(new PTWResp).flip
  val status = new Status().asInput
  val invalidate = Bool(INPUT)
  val sret = Bool(INPUT)
}

class DatapathPTWIO()(implicit conf: AddressSpaceConfiguration) extends Bundle {
  val ptbr = UInt(INPUT, conf.paddrBits)
  val invalidate = Bool(INPUT)
  val sret = Bool(INPUT)
  val status = new Status().asInput
}

class PTW(n: Int)(implicit conf: RocketConfiguration) extends Module
{
  implicit val as = conf.as
  val io = new Bundle {
    val requestor = Vec.fill(n){new TLBPTWIO}.flip
    val mem = new HellaCacheIO()(conf.dcache)
    val dpath = new DatapathPTWIO
  }
  
  val levels = 3
  val bitsPerLevel = conf.as.vpnBits/levels
  require(conf.as.vpnBits == levels * bitsPerLevel)

  val s_ready :: s_req :: s_wait :: s_done :: s_error :: Nil = Enum(UInt(), 5)
  val state = Reg(init=s_ready)
  val count = Reg(UInt(width = log2Up(levels)))

  val r_req_vpn = Reg(Bits())
  val r_req_dest = Reg(Bits())
  val r_pte = Reg(Bits())
  
  val vpn_idx = AVec((0 until levels).map(i => (r_req_vpn >> (levels-i-1)*bitsPerLevel)(bitsPerLevel-1,0)))(count)

  val arb = Module(new RRArbiter(UInt(width = conf.as.vpnBits), n))
  arb.io.in <> io.requestor.map(_.req)
  arb.io.out.ready := state === s_ready

  when (arb.io.out.fire()) {
    r_req_vpn := arb.io.out.bits
    r_req_dest := arb.io.chosen
    r_pte := Cat(io.dpath.ptbr(conf.as.paddrBits-1,conf.as.pgIdxBits), io.mem.resp.bits.data(conf.as.pgIdxBits-1,0))
  }

  when (io.mem.resp.valid) {
    r_pte := io.mem.resp.bits.data
  }
  
  io.mem.req.valid     := state === s_req
  io.mem.req.bits.phys := Bool(true)
  io.mem.req.bits.cmd  := M_XRD
  io.mem.req.bits.typ  := MT_D
  io.mem.req.bits.addr := Cat(r_pte(conf.as.paddrBits-1,conf.as.pgIdxBits), vpn_idx).toUInt << log2Up(conf.xprlen/8)
  io.mem.req.bits.kill := Bool(false)
  
  val resp_val = state === s_done || state === s_error
  val resp_err = state === s_error || state === s_wait

  val r_resp_ppn = io.mem.req.bits.addr >> conf.as.pgIdxBits
  val resp_ppn = AVec((0 until levels-1).map(i => Cat(r_resp_ppn >> bitsPerLevel*(levels-i-1), r_req_vpn(bitsPerLevel*(levels-i-1)-1,0))) :+ r_resp_ppn)(count)

  for (i <- 0 until io.requestor.size) {
    val me = r_req_dest === UInt(i)
    io.requestor(i).resp.valid := resp_val && me
    io.requestor(i).resp.bits.error := resp_err
    io.requestor(i).resp.bits.perm := r_pte(8,3)
    io.requestor(i).resp.bits.ppn := resp_ppn.toUInt
    io.requestor(i).invalidate := io.dpath.invalidate
    io.requestor(i).sret := io.dpath.sret
    io.requestor(i).status := io.dpath.status
  }

  // control state machine
  switch (state) {
    is (s_ready) {
      when (arb.io.out.valid) {
        state := s_req
      }
      count := UInt(0)
    }
    is (s_req) {
      when (io.mem.req.ready) {
        state := s_wait
      }
    }
    is (s_wait) {
      when (io.mem.resp.bits.nack) {
        state := s_req
      }
      when (io.mem.resp.valid) {
        state := s_error
        when (io.mem.resp.bits.data(0)) {
          when (!io.mem.resp.bits.data(1)) {
            state := s_done
          }.elsewhen (count < levels-1) {
            state := s_req
            count := count + 1
          }
        }
      }
    }
    is (s_done) {
      state := s_ready
    }
    is (s_error) {
      state := s_ready
    }
  }
}
