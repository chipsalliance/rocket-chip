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
  val acquire      = (new ClientSourcedIO){(new LogicalNetworkIO){new Acquire }}
  val abort     = (new MasterSourcedIO) {(new LogicalNetworkIO){new Abort }}
  val grant       = (new MasterSourcedIO) {(new LogicalNetworkIO){new Grant }}
  val grant_ack    = (new ClientSourcedIO){(new LogicalNetworkIO){new GrantAck }}
}

class MemArbiter(n: Int)(implicit conf: LogicalNetworkConfiguration) extends Component {
  val io = new Bundle {
    val mem = new UncachedRequestorIO
    val requestor = Vec(n) { new UncachedRequestorIO }.flip
  }

  var xi_bits = new Acquire
  xi_bits := io.requestor(n-1).acquire.bits.payload
  xi_bits.client_xact_id := Cat(io.requestor(n-1).acquire.bits.payload.client_xact_id, UFix(n-1, log2Up(n)))
  for (i <- n-2 to 0 by -1)
  {
    var my_xi_bits = new Acquire
    my_xi_bits := io.requestor(i).acquire.bits.payload
    my_xi_bits.client_xact_id := Cat(io.requestor(i).acquire.bits.payload.client_xact_id, UFix(i, log2Up(n)))

    xi_bits = Mux(io.requestor(i).acquire.valid, my_xi_bits, xi_bits)
  }

  io.mem.acquire.bits.payload := xi_bits
  io.mem.acquire.valid := io.requestor.map(_.acquire.valid).reduce(_||_)
  io.requestor(0).acquire.ready := io.mem.acquire.ready
  for (i <- 1 until n)
    io.requestor(i).acquire.ready := io.requestor(i-1).acquire.ready && !io.requestor(i-1).acquire.valid

  var xf_bits = io.requestor(n-1).grant_ack.bits
  for (i <- n-2 to 0 by -1)
    xf_bits = Mux(io.requestor(i).grant_ack.valid, io.requestor(i).grant_ack.bits, xf_bits)

  io.mem.grant_ack.bits := xf_bits
  io.mem.grant_ack.valid := io.requestor.map(_.grant_ack.valid).reduce(_||_)
  io.requestor(0).grant_ack.ready := io.mem.grant_ack.ready
  for (i <- 1 until n)
    io.requestor(i).grant_ack.ready := io.requestor(i-1).grant_ack.ready && !io.requestor(i-1).grant_ack.valid

  io.mem.grant.ready := Bool(false)
  for (i <- 0 until n)
  {
    val tag = io.mem.grant.bits.payload.client_xact_id
    io.requestor(i).grant.valid := Bool(false)
    when (tag(log2Up(n)-1,0) === UFix(i)) {
      io.requestor(i).grant.valid := io.mem.grant.valid
      io.mem.grant.ready := io.requestor(i).grant.ready
    }
    io.requestor(i).grant.bits := io.mem.grant.bits
    io.requestor(i).grant.bits.payload.client_xact_id := tag >> UFix(log2Up(n))
  }

  for (i <- 0 until n)
  {
    val tag = io.mem.abort.bits.payload.client_xact_id
    io.requestor(i).abort.valid := io.mem.abort.valid && tag(log2Up(n)-1,0) === UFix(i)
    io.requestor(i).abort.bits := io.mem.abort.bits
    io.requestor(i).abort.bits.payload.client_xact_id := tag >> UFix(log2Up(n))
  }

  io.mem.abort.ready := Bool(true)
}
