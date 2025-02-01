// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.trace

import chisel3._

class TraceCoreIngress(val params: TraceCoreParams) extends Module {
  val io = IO(new Bundle {
    val in = new Bundle {
      val valid = Input(Bool())
      val taken = Input(Bool())
      val is_branch = Input(Bool())
      val is_jal = Input(Bool())
      val is_jalr = Input(Bool())
      val is_compressed = Input(Bool())
      val insn = Input(UInt(params.xlen.W))
      val pc = Input(UInt(params.xlen.W))
      val interrupt = Input(Bool())
      val exception = Input(Bool())
      val trap_return = Input(Bool())
    }
    val out = Output(new TraceCoreGroup(params))
  })

  def gen_itype(insn: UInt, taken: Bool, is_branch: Bool, is_jal: Bool, is_jalr: Bool) = {
    val itype = Wire(TraceItype())
    when (io.in.exception) {
        itype := TraceItype.ITException
    }.elsewhen (io.in.interrupt) {
        itype := TraceItype.ITInterrupt
    }.elsewhen (io.in.trap_return) {
        itype := TraceItype.ITReturn
    }.elsewhen (is_branch && taken) {
        itype := TraceItype.ITBrTaken
    }.elsewhen (is_branch && !taken) {
        itype := TraceItype.ITBrNTaken
    }.elsewhen (is_jal) {
        itype := TraceItype.ITInJump
    }.elsewhen (is_jalr) {
        itype := TraceItype.ITUnJump
    }.otherwise {
        itype := TraceItype.ITNothing
    }
    itype
}
  
  io.out.iretire := io.in.valid
  io.out.iaddr := io.in.pc
  io.out.itype := gen_itype(io.in.insn, io.in.taken, io.in.is_branch, io.in.is_jal, io.in.is_jalr)
  io.out.ilastsize := io.in.valid && !io.in.is_compressed // 2^1 if non-compressed, 2^0 if compressed
}