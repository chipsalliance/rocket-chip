// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util.{Cat,log2Up}
import org.chipsalliance.cde.config.Parameters

class HellaCacheArbiter(n: Int)(implicit p: Parameters) extends Module
{
  val io = IO(new Bundle {
    val requestor = Flipped(Vec(n, new HellaCacheIO))
    val mem = new HellaCacheIO
  })

  if (n == 1) {
    io.mem <> io.requestor.head
  } else {
    val s1_id = Reg(UInt())
    val s2_id = RegNext(s1_id)

    io.mem.keep_clock_enabled := io.requestor.map(_.keep_clock_enabled).reduce(_||_)

    io.mem.req.valid := io.requestor.map(_.req.valid).reduce(_||_)
    io.requestor(0).req.ready := io.mem.req.ready
    for (i <- 1 until n)
      io.requestor(i).req.ready := io.requestor(i-1).req.ready && !io.requestor(i-1).req.valid

    for (i <- n-1 to 0 by -1) {
      val req = io.requestor(i).req
      def connect_s0() = {
        io.mem.req.bits := req.bits
        io.mem.req.bits.tag := Cat(req.bits.tag, i.U(log2Up(n).W))
        s1_id := i.U
      }
      def connect_s1() = {
        io.mem.s1_kill := io.requestor(i).s1_kill
        io.mem.s1_data := io.requestor(i).s1_data
      }
      def connect_s2() = {
        io.mem.s2_kill := io.requestor(i).s2_kill
      }

      if (i == n-1) {
        connect_s0()
        connect_s1()
        connect_s2()
      } else {
        when (req.valid) { connect_s0() }
        when (s1_id === i.U) { connect_s1() }
        when (s2_id === i.U) { connect_s2() }
      }
    }

    io.mem.uncached_resp.foreach(_.ready := false.B)

    for (i <- 0 until n) {
      val resp = io.requestor(i).resp
      val tag_hit = io.mem.resp.bits.tag(log2Up(n)-1,0) === i.U
      resp.valid := io.mem.resp.valid && tag_hit
      io.requestor(i).s2_xcpt := io.mem.s2_xcpt
      io.requestor(i).s2_gpa := io.mem.s2_gpa
      io.requestor(i).s2_gpa_is_pte := io.mem.s2_gpa_is_pte
      io.requestor(i).ordered := io.mem.ordered
      io.requestor(i).perf := io.mem.perf
      io.requestor(i).s2_nack := io.mem.s2_nack && s2_id === i.U
      io.requestor(i).s2_nack_cause_raw := io.mem.s2_nack_cause_raw
      io.requestor(i).s2_uncached := io.mem.s2_uncached
      io.requestor(i).s2_paddr := io.mem.s2_paddr
      io.requestor(i).clock_enabled := io.mem.clock_enabled
      resp.bits := io.mem.resp.bits
      resp.bits.tag := io.mem.resp.bits.tag >> log2Up(n)

      io.requestor(i).replay_next := io.mem.replay_next

      io.requestor(i).uncached_resp.map { uncached_resp =>
        val uncached_tag_hit = io.mem.uncached_resp.get.bits.tag(log2Up(n)-1,0) === i.U
        uncached_resp.valid := io.mem.uncached_resp.get.valid && uncached_tag_hit
        when (uncached_resp.ready && uncached_tag_hit) {
          io.mem.uncached_resp.get.ready := true.B
        }
        uncached_resp.bits := io.mem.uncached_resp.get.bits
        uncached_resp.bits.tag := io.mem.uncached_resp.get.bits.tag >> log2Up(n)
      }
    }
  }
}
