// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util.{Decoupled,log2Ceil,Cat,UIntToOH,Fill}
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile._
import freechips.rocketchip.util._

class Instruction(implicit val p: Parameters) extends ParameterizedBundle with HasCoreParameters {
  val xcpt0 = new FrontendExceptions // exceptions on first half of instruction
  val xcpt1 = new FrontendExceptions // exceptions on second half of instruction
  val replay = Bool()
  val rvc = Bool()
  val inst = new ExpandedInstruction
  val raw = UInt(32.W)
  require(coreInstBits == (if (usingCompressed) 16 else 32))
}

class IBuf(implicit p: Parameters) extends CoreModule {
  val io = IO(new Bundle {
    val imem = Flipped(Decoupled(new FrontendResp))
    val kill = Input(Bool())
    val pc = Output(UInt(vaddrBitsExtended.W))
    val btb_resp = Output(new BTBResp())
    val inst = Vec(retireWidth, Decoupled(new Instruction))
  })

  // This module is meant to be more general, but it's not there yet
  require(decodeWidth == 1)

  val n = fetchWidth - 1
  val nBufValid = if (n == 0) 0.U else RegInit(init=0.U(log2Ceil(fetchWidth).W))
  val buf = Reg(chiselTypeOf(io.imem.bits))
  val ibufBTBResp = Reg(new BTBResp)
  val pcWordMask = (coreInstBytes*fetchWidth-1).U(vaddrBitsExtended.W)

  val pcWordBits = io.imem.bits.pc.extract(log2Ceil(fetchWidth*coreInstBytes)-1, log2Ceil(coreInstBytes))
  val nReady = WireDefault(0.U(log2Ceil(fetchWidth+1).W))
  val nIC = Mux(io.imem.bits.btb.taken, io.imem.bits.btb.bridx +& 1.U, fetchWidth.U) - pcWordBits
  val nICReady = nReady - nBufValid
  val nValid = Mux(io.imem.valid, nIC, 0.U) + nBufValid
  io.imem.ready := io.inst(0).ready && nReady >= nBufValid && (nICReady >= nIC || n.U >= nIC - nICReady)

  if (n > 0) {
    when (io.inst(0).ready) {
      nBufValid := Mux(nReady >== nBufValid, 0.U, nBufValid - nReady)
      if (n > 1) when (nReady > 0.U && nReady < nBufValid) {
        val shiftedBuf = shiftInsnRight(buf.data(n*coreInstBits-1, coreInstBits), (nReady-1.U)(log2Ceil(n-1)-1,0))
        buf.data := Cat(buf.data(n*coreInstBits-1, (n-1)*coreInstBits), shiftedBuf((n-1)*coreInstBits-1, 0))
        buf.pc := buf.pc & ~pcWordMask | (buf.pc + (nReady << log2Ceil(coreInstBytes))) & pcWordMask
      }
      when (io.imem.valid && nReady >= nBufValid && nICReady < nIC && n.U >= nIC - nICReady) {
        val shamt = pcWordBits + nICReady
        nBufValid := nIC - nICReady
        buf := io.imem.bits
        buf.data := shiftInsnRight(io.imem.bits.data, shamt)(n*coreInstBits-1,0)
        buf.pc := io.imem.bits.pc & ~pcWordMask | (io.imem.bits.pc + (nICReady << log2Ceil(coreInstBytes))) & pcWordMask
        ibufBTBResp := io.imem.bits.btb
      }
    }
    when (io.kill) {
      nBufValid := 0.U
    }
  }

  val icShiftAmt = (fetchWidth.U + nBufValid - pcWordBits)(log2Ceil(fetchWidth), 0)
  val icData = shiftInsnLeft(Cat(io.imem.bits.data, Fill(fetchWidth, io.imem.bits.data(coreInstBits-1, 0))), icShiftAmt)
    .extract(3*fetchWidth*coreInstBits-1, 2*fetchWidth*coreInstBits)
  val icMask = (~0.U((fetchWidth*coreInstBits).W) << (nBufValid << log2Ceil(coreInstBits)))(fetchWidth*coreInstBits-1,0)
  val inst = icData & icMask | buf.data & ~icMask

  val valid = (UIntToOH(nValid) - 1.U)(fetchWidth-1, 0)
  val bufMask = UIntToOH(nBufValid) - 1.U
  val xcpt = (0 until bufMask.getWidth).map(i => Mux(bufMask(i), buf.xcpt, io.imem.bits.xcpt))
  val buf_replay = Mux(buf.replay, bufMask, 0.U)
  val ic_replay = buf_replay | Mux(io.imem.bits.replay, valid & ~bufMask, 0.U)
  assert(!io.imem.valid || !io.imem.bits.btb.taken || io.imem.bits.btb.bridx >= pcWordBits)

  io.btb_resp := io.imem.bits.btb
  io.pc := Mux(nBufValid > 0.U, buf.pc, io.imem.bits.pc)
  expand(0, 0.U, inst)

  def expand(i: Int, j: UInt, curInst: UInt): Unit = if (i < retireWidth) {
    val exp = Module(new RVCExpander)
    exp.io.in := curInst
    io.inst(i).bits.inst := exp.io.out
    io.inst(i).bits.raw := curInst

    if (usingCompressed) {
      val replay = ic_replay(j) || (!exp.io.rvc && ic_replay(j+1.U))
      val full_insn = exp.io.rvc || valid(j+1.U) || buf_replay(j)
      io.inst(i).valid := valid(j) && full_insn
      io.inst(i).bits.xcpt0 := xcpt(j)
      io.inst(i).bits.xcpt1 := Mux(exp.io.rvc, 0.U, xcpt(j+1.U).asUInt).asTypeOf(new FrontendExceptions)
      io.inst(i).bits.replay := replay
      io.inst(i).bits.rvc := exp.io.rvc

      when ((bufMask(j) && exp.io.rvc) || bufMask(j+1.U)) { io.btb_resp := ibufBTBResp }

      when (full_insn && ((i == 0).B || io.inst(i).ready)) { nReady := Mux(exp.io.rvc, j+1.U, j+2.U) }

      expand(i+1, Mux(exp.io.rvc, j+1.U, j+2.U), Mux(exp.io.rvc, curInst >> 16, curInst >> 32))
    } else {
      when ((i == 0).B || io.inst(i).ready) { nReady := (i+1).U }
      io.inst(i).valid := valid(i)
      io.inst(i).bits.xcpt0 := xcpt(i)
      io.inst(i).bits.xcpt1 := 0.U.asTypeOf(new FrontendExceptions)
      io.inst(i).bits.replay := ic_replay(i)
      io.inst(i).bits.rvc := false.B

      expand(i+1, null, curInst >> 32)
    }
  }

  def shiftInsnLeft(in: UInt, dist: UInt) = {
    val r = in.getWidth/coreInstBits
    require(in.getWidth % coreInstBits == 0)
    val data = Cat(Fill((1 << (log2Ceil(r) + 1)) - r, in >> (r-1)*coreInstBits), in)
    data << (dist << log2Ceil(coreInstBits))
  }

  def shiftInsnRight(in: UInt, dist: UInt) = {
    val r = in.getWidth/coreInstBits
    require(in.getWidth % coreInstBits == 0)
    val data = Cat(Fill((1 << (log2Ceil(r) + 1)) - r, in >> (r-1)*coreInstBits), in)
    data >> (dist << log2Ceil(coreInstBits))
  }
}
