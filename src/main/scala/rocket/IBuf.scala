// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile._
import freechips.rocketchip.util._

class Instruction(implicit val p: Parameters) extends ParameterizedBundle with HasCoreParameters {
  val xcpt0 = new FrontendExceptions // exceptions on first half of instruction
  val xcpt1 = new FrontendExceptions // exceptions on second half of instruction
  val replay = Bool()
  val btb_hit = Bool()
  val rvc = Bool()
  val inst = new ExpandedInstruction
  val raw = UInt(width = 32)
  require(coreInstBits == (if (usingCompressed) 16 else 32))
}

class IBuf(implicit p: Parameters) extends CoreModule {
  val io = new Bundle {
    val imem = Decoupled(new FrontendResp).flip
    val kill = Bool(INPUT)
    val pc = UInt(OUTPUT, vaddrBitsExtended)
    val btb_resp = new BTBResp().asOutput
    val inst = Vec(retireWidth, Decoupled(new Instruction))
  }

  // This module is meant to be more general, but it's not there yet
  require(decodeWidth == 1)

  val n = fetchWidth - 1
  val nBufValid = if (n == 0) UInt(0) else Reg(init=UInt(0, log2Ceil(fetchWidth)))
  val buf = Reg(io.imem.bits)
  val ibufBTBHit = Reg(Bool())
  val ibufBTBResp = Reg(new BTBResp)
  val pcWordMask = UInt(coreInstBytes*fetchWidth-1, vaddrBitsExtended)

  val pcWordBits = io.imem.bits.pc.extract(log2Ceil(fetchWidth*coreInstBytes)-1, log2Ceil(coreInstBytes))
  val nReady = Wire(init = UInt(0, log2Ceil(fetchWidth+1)))
  val nIC = Mux(io.imem.bits.btb.valid && io.imem.bits.btb.bits.taken, io.imem.bits.btb.bits.bridx +& 1, UInt(fetchWidth)) - pcWordBits
  val nICReady = nReady - nBufValid
  val nValid = Mux(io.imem.valid, nIC, UInt(0)) + nBufValid
  io.imem.ready := io.inst(0).ready && nReady >= nBufValid && (nICReady >= nIC || n >= nIC - nICReady)

  if (n > 0) {
    when (io.inst(0).ready) {
      nBufValid := Mux(nReady >= nBufValid, UInt(0), nBufValid - nReady)
      if (n > 1) when (nReady > 0 && nReady < nBufValid) {
        val shiftedBuf = shiftInsnRight(buf.data(n*coreInstBits-1, coreInstBits), (nReady-1)(log2Ceil(n-1)-1,0))
        buf.data := Cat(buf.data(n*coreInstBits-1, (n-1)*coreInstBits), shiftedBuf((n-1)*coreInstBits-1, 0))
        buf.pc := buf.pc & ~pcWordMask | (buf.pc + (nReady << log2Ceil(coreInstBytes))) & pcWordMask
        ibufBTBResp.bridx := ibufBTBResp.bridx - nReady
      }
      when (io.imem.valid && nReady >= nBufValid && nICReady < nIC && n >= nIC - nICReady) {
        val shamt = pcWordBits + nICReady
        nBufValid := nIC - nICReady
        buf := io.imem.bits
        buf.data := shiftInsnRight(io.imem.bits.data, shamt)(n*coreInstBits-1,0)
        buf.pc := io.imem.bits.pc & ~pcWordMask | (io.imem.bits.pc + (nICReady << log2Ceil(coreInstBytes))) & pcWordMask
        ibufBTBHit := io.imem.bits.btb.valid
        ibufBTBResp := io.imem.bits.btb.bits
        ibufBTBResp.bridx := io.imem.bits.btb.bits.bridx + nICReady
      }
    }
    when (io.kill) {
      nBufValid := 0
    }
  }

  val icShiftAmt = (fetchWidth + nBufValid - pcWordBits)(log2Ceil(fetchWidth), 0)
  val icData = shiftInsnLeft(Cat(io.imem.bits.data, Fill(fetchWidth, io.imem.bits.data(coreInstBits-1, 0))), icShiftAmt)
    .extract(3*fetchWidth*coreInstBits-1, 2*fetchWidth*coreInstBits)
  val icMask = (~UInt(0, fetchWidth*coreInstBits) << (nBufValid << log2Ceil(coreInstBits)))(fetchWidth*coreInstBits-1,0)
  val inst = icData & icMask | buf.data & ~icMask

  val valid = (UIntToOH(nValid) - 1)(fetchWidth-1, 0)
  val bufMask = UIntToOH(nBufValid) - 1
  val xcpt = (0 until bufMask.getWidth).map(i => Mux(bufMask(i), buf.xcpt, io.imem.bits.xcpt))
  val buf_replay = Mux(buf.replay, bufMask, UInt(0))
  val ic_replay = buf_replay | Mux(io.imem.bits.replay, valid & ~bufMask, UInt(0))
  val ibufBTBHitMask = Mux(ibufBTBHit, UIntToOH(ibufBTBResp.bridx), UInt(0))
  assert(!io.imem.valid || !io.imem.bits.btb.valid || io.imem.bits.btb.bits.bridx >= pcWordBits)
  val icBTBHitMask = Mux(io.imem.bits.btb.valid, UIntToOH(io.imem.bits.btb.bits.bridx +& nBufValid - pcWordBits), UInt(0))
  val btbHitMask = ibufBTBHitMask & bufMask | icBTBHitMask & ~bufMask

  io.btb_resp := Mux((ibufBTBHitMask & bufMask).orR, ibufBTBResp, io.imem.bits.btb.bits)
  io.pc := Mux(nBufValid > 0, buf.pc, io.imem.bits.pc)
  expand(0, 0, inst)

  def expand(i: Int, j: UInt, curInst: UInt): Unit = if (i < retireWidth) {
    val exp = Module(new RVCExpander)
    exp.io.in := curInst
    io.inst(i).bits.inst := exp.io.out
    io.inst(i).bits.raw := curInst

    if (usingCompressed) {
      val replay = ic_replay(j) || (!exp.io.rvc && (btbHitMask(j) || ic_replay(j+1)))
      val full_insn = exp.io.rvc || valid(j+1) || buf_replay(j)
      io.inst(i).valid := valid(j) && full_insn
      io.inst(i).bits.xcpt0 := xcpt(j)
      io.inst(i).bits.xcpt1 := Mux(exp.io.rvc, 0.U, xcpt(j+1).asUInt).asTypeOf(new FrontendExceptions)
      io.inst(i).bits.replay := replay
      io.inst(i).bits.btb_hit := btbHitMask(j) || (!exp.io.rvc && btbHitMask(j+1))
      io.inst(i).bits.rvc := exp.io.rvc

      when (full_insn && (i == 0 || io.inst(i).ready)) { nReady := Mux(exp.io.rvc, j+1, j+2) }

      expand(i+1, Mux(exp.io.rvc, j+1, j+2), Mux(exp.io.rvc, curInst >> 16, curInst >> 32))
    } else {
      when (i == 0 || io.inst(i).ready) { nReady := i+1 }
      io.inst(i).valid := valid(i)
      io.inst(i).bits.xcpt0 := xcpt(i)
      io.inst(i).bits.xcpt1 := 0.U.asTypeOf(new FrontendExceptions)
      io.inst(i).bits.replay := ic_replay(i)
      io.inst(i).bits.rvc := false
      io.inst(i).bits.btb_hit := btbHitMask(i)

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
