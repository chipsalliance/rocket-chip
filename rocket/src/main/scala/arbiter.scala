package rocket

import Chisel._
import Node._
import Constants._
import uncore._

class ioHellaCacheArbiter(n: Int) extends Bundle
{
  val requestor = Vec(n) { new ioHellaCache() }.flip
  val mem = new ioHellaCache
}

class rocketHellaCacheArbiter(n: Int) extends Component
{
  val io = new ioHellaCacheArbiter(n)
  require(DCACHE_TAG_BITS >= log2Up(n) + CPU_TAG_BITS)

  var req_val = Bool(false)
  var req_rdy = io.mem.req.ready
  for (i <- 0 until n)
  {
    io.requestor(i).req.ready := req_rdy
    req_val = req_val || io.requestor(i).req.valid
    req_rdy = req_rdy && !io.requestor(i).req.valid
  }

  var req_cmd  = io.requestor(n-1).req.bits.cmd
  var req_type = io.requestor(n-1).req.bits.typ
  var req_idx  = io.requestor(n-1).req.bits.idx
  var req_ppn  = io.requestor(n-1).req.bits.ppn
  var req_data = io.requestor(n-1).req.bits.data
  var req_kill = io.requestor(n-1).req.bits.kill
  var req_tag  = io.requestor(n-1).req.bits.tag
  for (i <- n-1 to 0 by -1)
  {
    val r = io.requestor(i).req
    req_cmd  = Mux(r.valid, r.bits.cmd, req_cmd)
    req_type = Mux(r.valid, r.bits.typ, req_type)
    req_idx  = Mux(r.valid, r.bits.idx, req_idx)
    req_ppn  = Mux(Reg(r.valid), r.bits.ppn, req_ppn)
    req_data = Mux(Reg(r.valid), r.bits.data, req_data)
    req_kill = Mux(Reg(r.valid), r.bits.kill, req_kill)
    req_tag  = Mux(r.valid, Cat(r.bits.tag, UFix(i, log2Up(n))), req_tag)
  }

  io.mem.req.valid     := req_val
  io.mem.req.bits.cmd  := req_cmd
  io.mem.req.bits.typ  := req_type
  io.mem.req.bits.idx  := req_idx
  io.mem.req.bits.ppn  := req_ppn
  io.mem.req.bits.data := req_data
  io.mem.req.bits.kill := req_kill
  io.mem.req.bits.tag  := req_tag

  for (i <- 0 until n)
  {
    val r = io.requestor(i).resp
    val x = io.requestor(i).xcpt
    val tag_hit = io.mem.resp.bits.tag(log2Up(n)-1,0) === UFix(i)
    x.ma.ld := io.mem.xcpt.ma.ld && Reg(io.requestor(i).req.valid)
    x.ma.st := io.mem.xcpt.ma.st && Reg(io.requestor(i).req.valid)
    r.valid             := io.mem.resp.valid && tag_hit
    r.bits.miss         := io.mem.resp.bits.miss && tag_hit
    r.bits.nack         := io.mem.resp.bits.nack && Reg(io.requestor(i).req.valid)
    r.bits.replay       := io.mem.resp.bits.replay && tag_hit
    r.bits.data         := io.mem.resp.bits.data
    r.bits.data_subword := io.mem.resp.bits.data_subword
    r.bits.typ          := io.mem.resp.bits.typ
    r.bits.tag          := io.mem.resp.bits.tag >> UFix(log2Up(n))
  }
}

class ioUncachedRequestor extends Bundle {
  val xact_init      = (new FIFOIO) { new TransactionInit }
  val xact_abort     = (new FIFOIO) { new TransactionAbort }.flip
  val xact_rep       = (new FIFOIO)      { new TransactionReply }.flip
  val xact_finish    = (new FIFOIO) { new TransactionFinish }
}

class rocketMemArbiter(n: Int) extends Component {
  val io = new Bundle {
    val mem = new ioUncachedRequestor
    val requestor = Vec(n) { new ioUncachedRequestor }.flip
  }

  var xi_val = Bool(false)
  var xi_rdy = io.mem.xact_init.ready
  for (i <- 0 until n)
  {
    io.requestor(i).xact_init.ready := xi_rdy
    xi_val = xi_val || io.requestor(i).xact_init.valid
    xi_rdy = xi_rdy && !io.requestor(i).xact_init.valid
  }

  var xi_bits = new TransactionInit
  xi_bits := io.requestor(n-1).xact_init.bits
  xi_bits.tile_xact_id := Cat(io.requestor(n-1).xact_init.bits.tile_xact_id, UFix(n-1, log2Up(n)))
  for (i <- n-2 to 0 by -1)
  {
    var my_xi_bits = new TransactionInit
    my_xi_bits := io.requestor(i).xact_init.bits
    my_xi_bits.tile_xact_id := Cat(io.requestor(i).xact_init.bits.tile_xact_id, UFix(i, log2Up(n)))

    xi_bits = Mux(io.requestor(i).xact_init.valid, my_xi_bits, xi_bits)
  }

  io.mem.xact_init.valid := xi_val
  io.mem.xact_init.bits := xi_bits

  var xf_val = Bool(false)
  var xf_rdy = io.mem.xact_finish.ready
  for (i <- 0 until n)
  {
    io.requestor(i).xact_finish.ready := xf_rdy
    xf_val = xf_val || io.requestor(i).xact_finish.valid
    xf_rdy = xf_rdy && !io.requestor(i).xact_finish.valid
  }

  var xf_bits = io.requestor(n-1).xact_finish.bits
  for (i <- n-2 to 0 by -1)
    xf_bits = Mux(io.requestor(i).xact_finish.valid, io.requestor(i).xact_finish.bits, xf_bits)

  io.mem.xact_finish.valid := xf_val
  io.mem.xact_finish.bits := xf_bits

  for (i <- 0 until n)
  {
    val tag = io.mem.xact_rep.bits.tile_xact_id
    io.requestor(i).xact_rep.valid := io.mem.xact_rep.valid && tag(log2Up(n)-1,0) === UFix(i)
    io.requestor(i).xact_rep.bits := io.mem.xact_rep.bits
    io.requestor(i).xact_rep.bits.tile_xact_id := tag >> UFix(log2Up(n))
  }

  for (i <- 0 until n)
  {
    val tag = io.mem.xact_abort.bits.tile_xact_id
    io.requestor(i).xact_abort.valid := io.mem.xact_abort.valid && tag(log2Up(n)-1,0) === UFix(i)
    io.requestor(i).xact_abort.bits := io.mem.xact_abort.bits
    io.requestor(i).xact_abort.bits.tile_xact_id := tag >> UFix(log2Up(n))
  }

  io.mem.xact_abort.ready := Bool(true)
  io.mem.xact_rep.ready := Bool(true)
}
