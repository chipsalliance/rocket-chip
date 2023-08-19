// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.groundtest

import chisel3._
import chisel3.util.{RRArbiter, Valid, log2Up, RegEnable}

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile.CoreModule
import freechips.rocketchip.util.ParameterizedBundle

class DummyPTW(n: Int)(implicit p: Parameters) extends CoreModule()(p) {
  val io = IO(new Bundle {
    val requestors = Flipped(Vec(n, new TLBPTWIO))
  })
  io.requestors := DontCare
  val req_arb = Module(new RRArbiter(Valid(new PTWReq), n))
  req_arb.io.in <> io.requestors.map(_.req)
  req_arb.io.out.ready := true.B

  def vpn_to_ppn(vpn: UInt): UInt = vpn(ppnBits - 1, 0)

  class QueueChannel extends ParameterizedBundle()(p) {
    val ppn = UInt(ppnBits.W)
    val chosen = UInt(log2Up(n).W)
  }

  val s1_ppn = vpn_to_ppn(req_arb.io.out.bits.bits.addr)
  val s2_ppn = RegEnable(s1_ppn, req_arb.io.out.valid)
  val s2_chosen = RegEnable(req_arb.io.chosen, req_arb.io.out.valid)
  val s2_valid = RegNext(req_arb.io.out.valid && req_arb.io.out.bits.valid)

  val s2_resp = WireDefault(0.U.asTypeOf(new PTWResp))
  s2_resp.pte.ppn := s2_ppn
  s2_resp.pte.reserved_for_software := 0.U
  s2_resp.level := (pgLevels-1).U
  s2_resp.pte.d := true.B
  s2_resp.pte.a := true.B
  s2_resp.pte.g := false.B
  s2_resp.pte.u := true.B
  s2_resp.pte.r := true.B
  s2_resp.pte.w := true.B
  s2_resp.pte.x := false.B
  s2_resp.pte.v := true.B

  io.requestors.zipWithIndex.foreach { case (requestor, i) =>
    requestor.resp.valid := s2_valid && s2_chosen === i.U
    requestor.resp.bits := s2_resp
    requestor.status := 0.U.asTypeOf(requestor.status)
    requestor.ptbr.mode := requestor.ptbr.pgLevelsToMode(pgLevels).U
    requestor.ptbr.asid := 0.U
    requestor.ptbr.ppn := 0.U
  }
}
