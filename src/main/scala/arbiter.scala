package rocket

import Chisel._
import Node._
import Constants._
import uncore._

class HellaCacheArbiter(n: Int)(implicit conf: RocketConfiguration) extends Component
{
  val io = new Bundle {
    val requestor = Vec(n) { new HellaCacheIO()(conf.dcache) }.flip
    val mem = new HellaCacheIO()(conf.dcache)
  }

  val r_valid = io.requestor.map(r => Reg(r.req.valid))

  io.mem.req.valid := io.requestor.map(_.req.valid).reduce(_||_)
  io.requestor(0).req.ready := io.mem.req.ready
  for (i <- 1 until n)
    io.requestor(i).req.ready := io.requestor(i-1).req.ready && !io.requestor(i-1).req.valid

  io.mem.req.bits := io.requestor(n-1).req.bits
  io.mem.req.bits.tag := Cat(io.requestor(n-1).req.bits.tag, UFix(n-1, log2Up(n)))
  for (i <- n-2 to 0 by -1) {
    val req = io.requestor(i).req
    when (req.valid) {
      io.mem.req.bits.cmd := req.bits.cmd
      io.mem.req.bits.typ := req.bits.typ
      io.mem.req.bits.addr := req.bits.addr
      io.mem.req.bits.phys := req.bits.phys
      io.mem.req.bits.tag := Cat(req.bits.tag, UFix(i, log2Up(n)))
    }
    when (r_valid(i)) {
      io.mem.req.bits.kill := req.bits.kill
      io.mem.req.bits.data := req.bits.data
    }
  }

  for (i <- 0 until n) {
    val resp = io.requestor(i).resp
    val tag_hit = io.mem.resp.bits.tag(log2Up(n)-1,0) === UFix(i)
    resp.valid := io.mem.resp.valid && tag_hit
    io.requestor(i).xcpt := io.mem.xcpt
    resp.bits := io.mem.resp.bits
    resp.bits.tag := io.mem.resp.bits.tag >> UFix(log2Up(n))
    resp.bits.nack := io.mem.resp.bits.nack && tag_hit
    resp.bits.replay := io.mem.resp.bits.replay && tag_hit
  }
}

class UncachedRequestorIO(implicit conf: LogicalNetworkConfiguration) extends Bundle {
  val xact_init      = (new ClientSourcedIO){(new LogicalNetworkIO){new TransactionInit }}
  val xact_abort     = (new MasterSourcedIO) {(new LogicalNetworkIO){new TransactionAbort }}
  val xact_rep       = (new MasterSourcedIO) {(new LogicalNetworkIO){new TransactionReply }}
  val xact_finish    = (new ClientSourcedIO){(new LogicalNetworkIO){new TransactionFinish }}
}

class MemArbiter(n: Int)(implicit conf: LogicalNetworkConfiguration) extends Component {
  val io = new Bundle {
    val mem = new UncachedRequestorIO
    val requestor = Vec(n) { new UncachedRequestorIO }.flip
  }

  var xi_bits = new TransactionInit
  xi_bits := io.requestor(n-1).xact_init.bits.payload
  xi_bits.tile_xact_id := Cat(io.requestor(n-1).xact_init.bits.payload.tile_xact_id, UFix(n-1, log2Up(n)))
  for (i <- n-2 to 0 by -1)
  {
    var my_xi_bits = new TransactionInit
    my_xi_bits := io.requestor(i).xact_init.bits.payload
    my_xi_bits.tile_xact_id := Cat(io.requestor(i).xact_init.bits.payload.tile_xact_id, UFix(i, log2Up(n)))

    xi_bits = Mux(io.requestor(i).xact_init.valid, my_xi_bits, xi_bits)
  }

  io.mem.xact_init.bits.payload := xi_bits
  io.mem.xact_init.valid := io.requestor.map(_.xact_init.valid).reduce(_||_)
  io.requestor(0).xact_init.ready := io.mem.xact_init.ready
  for (i <- 1 until n)
    io.requestor(i).xact_init.ready := io.requestor(i-1).xact_init.ready && !io.requestor(i-1).xact_init.valid

  var xf_bits = io.requestor(n-1).xact_finish.bits
  for (i <- n-2 to 0 by -1)
    xf_bits = Mux(io.requestor(i).xact_finish.valid, io.requestor(i).xact_finish.bits, xf_bits)

  io.mem.xact_finish.bits := xf_bits
  io.mem.xact_finish.valid := io.requestor.map(_.xact_finish.valid).reduce(_||_)
  io.requestor(0).xact_finish.ready := io.mem.xact_finish.ready
  for (i <- 1 until n)
    io.requestor(i).xact_finish.ready := io.requestor(i-1).xact_finish.ready && !io.requestor(i-1).xact_finish.valid

  io.mem.xact_rep.ready := Bool(false)
  for (i <- 0 until n)
  {
    val tag = io.mem.xact_rep.bits.payload.tile_xact_id
    io.requestor(i).xact_rep.valid := Bool(false)
    when (tag(log2Up(n)-1,0) === UFix(i)) {
      io.requestor(i).xact_rep.valid := io.mem.xact_rep.valid
      io.mem.xact_rep.ready := io.requestor(i).xact_rep.ready
    }
    io.requestor(i).xact_rep.bits := io.mem.xact_rep.bits
    io.requestor(i).xact_rep.bits.payload.tile_xact_id := tag >> UFix(log2Up(n))
  }

  for (i <- 0 until n)
  {
    val tag = io.mem.xact_abort.bits.payload.tile_xact_id
    io.requestor(i).xact_abort.valid := io.mem.xact_abort.valid && tag(log2Up(n)-1,0) === UFix(i)
    io.requestor(i).xact_abort.bits := io.mem.xact_abort.bits
    io.requestor(i).xact_abort.bits.payload.tile_xact_id := tag >> UFix(log2Up(n))
  }

  io.mem.xact_abort.ready := Bool(true)
}
