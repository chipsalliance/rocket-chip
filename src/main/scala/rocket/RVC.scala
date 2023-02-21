// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile._
import freechips.rocketchip.util._

class ExpandedInstruction extends Bundle {
  val bits = UInt(32.W)
  val rd = UInt(5.W)
  val rs1 = UInt(5.W)
  val rs2 = UInt(5.W)
  val rs3 = UInt(5.W)
}

class RVCDecoder(x: UInt, xLen: Int, useAddiForMv: Boolean = false) {
  def inst(bits: UInt, rd: UInt = x(11,7), rs1: UInt = x(19,15), rs2: UInt = x(24,20), rs3: UInt = x(31,27)) = {
    val res = Wire(new ExpandedInstruction)
    res.bits := bits
    res.rd := rd
    res.rs1 := rs1
    res.rs2 := rs2
    res.rs3 := rs3
    res
  }

  def rs1p = Cat(1.U(2.W), x(9,7))
  def rs2p = Cat(1.U(2.W), x(4,2))
  def rs2 = x(6,2)
  def rd = x(11,7)
  def addi4spnImm = Cat(x(10,7), x(12,11), x(5), x(6), 0.U(2.W))
  def lwImm = Cat(x(5), x(12,10), x(6), 0.U(2.W))
  def ldImm = Cat(x(6,5), x(12,10), 0.U(3.W))
  def lwspImm = Cat(x(3,2), x(12), x(6,4), 0.U(2.W))
  def ldspImm = Cat(x(4,2), x(12), x(6,5), 0.U(3.W))
  def swspImm = Cat(x(8,7), x(12,9), 0.U(2.W))
  def sdspImm = Cat(x(9,7), x(12,10), 0.U(3.W))
  def luiImm = Cat(Fill(15, x(12)), x(6,2), 0.U(12.W))
  def addi16spImm = Cat(Fill(3, x(12)), x(4,3), x(5), x(2), x(6), 0.U(4.W))
  def addiImm = Cat(Fill(7, x(12)), x(6,2))
  def jImm = Cat(Fill(10, x(12)), x(8), x(10,9), x(6), x(7), x(2), x(11), x(5,3), 0.U(1.W))
  def bImm = Cat(Fill(5, x(12)), x(6,5), x(2), x(11,10), x(4,3), 0.U(1.W))
  def shamt = Cat(x(12), x(6,2))
  def x0 = 0.U(5.W)
  def ra = 1.U(5.W)
  def sp = 2.U(5.W)

  def q0 = {
    def addi4spn = {
      val opc = Mux(x(12,5).orR, 0x13.U(7.W), 0x1F.U(7.W))
      inst(Cat(addi4spnImm, sp, 0.U(3.W), rs2p, opc), rs2p, sp, rs2p)
    }
    def ld = inst(Cat(ldImm, rs1p, 3.U(3.W), rs2p, 0x03.U(7.W)), rs2p, rs1p, rs2p)
    def lw = inst(Cat(lwImm, rs1p, 2.U(3.W), rs2p, 0x03.U(7.W)), rs2p, rs1p, rs2p)
    def fld = inst(Cat(ldImm, rs1p, 3.U(3.W), rs2p, 0x07.U(7.W)), rs2p, rs1p, rs2p)
    def flw = {
      if (xLen == 32) inst(Cat(lwImm, rs1p, 2.U(3.W), rs2p, 0x07.U(7.W)), rs2p, rs1p, rs2p)
      else ld
    }
    def unimp = inst(Cat(lwImm >> 5, rs2p, rs1p, 2.U(3.W), lwImm(4,0), 0x3F.U(7.W)), rs2p, rs1p, rs2p)
    def sd = inst(Cat(ldImm >> 5, rs2p, rs1p, 3.U(3.W), ldImm(4,0), 0x23.U(7.W)), rs2p, rs1p, rs2p)
    def sw = inst(Cat(lwImm >> 5, rs2p, rs1p, 2.U(3.W), lwImm(4,0), 0x23.U(7.W)), rs2p, rs1p, rs2p)
    def fsd = inst(Cat(ldImm >> 5, rs2p, rs1p, 3.U(3.W), ldImm(4,0), 0x27.U(7.W)), rs2p, rs1p, rs2p)
    def fsw = {
      if (xLen == 32) inst(Cat(lwImm >> 5, rs2p, rs1p, 2.U(3.W), lwImm(4,0), 0x27.U(7.W)), rs2p, rs1p, rs2p)
      else sd
    }
    Seq(addi4spn, fld, lw, flw, unimp, fsd, sw, fsw)
  }

  def q1 = {
    def addi = inst(Cat(addiImm, rd, 0.U(3.W), rd, 0x13.U(7.W)), rd, rd, rs2p)
    def addiw = {
      val opc = Mux(rd.orR, 0x1B.U(7.W), 0x1F.U(7.W))
      inst(Cat(addiImm, rd, 0.U(3.W), rd, opc), rd, rd, rs2p)
    }
    def jal = {
      if (xLen == 32) inst(Cat(jImm(20), jImm(10,1), jImm(11), jImm(19,12), ra, 0x6F.U(7.W)), ra, rd, rs2p)
      else addiw
    }
    def li = inst(Cat(addiImm, x0, 0.U(3.W), rd, 0x13.U(7.W)), rd, x0, rs2p)
    def addi16sp = {
      val opc = Mux(addiImm.orR, 0x13.U(7.W), 0x1F.U(7.W))
      inst(Cat(addi16spImm, rd, 0.U(3.W), rd, opc), rd, rd, rs2p)
    }
    def lui = {
      val opc = Mux(addiImm.orR, 0x37.U(7.W), 0x3F.U(7.W))
      val me = inst(Cat(luiImm(31,12), rd, opc), rd, rd, rs2p)
      Mux(rd === x0 || rd === sp, addi16sp, me)
    }
    def j = inst(Cat(jImm(20), jImm(10,1), jImm(11), jImm(19,12), x0, 0x6F.U(7.W)), x0, rs1p, rs2p)
    def beqz = inst(Cat(bImm(12), bImm(10,5), x0, rs1p, 0.U(3.W), bImm(4,1), bImm(11), 0x63.U(7.W)), rs1p, rs1p, x0)
    def bnez = inst(Cat(bImm(12), bImm(10,5), x0, rs1p, 1.U(3.W), bImm(4,1), bImm(11), 0x63.U(7.W)), x0, rs1p, x0)
    def arith = {
      def srli = Cat(shamt, rs1p, 5.U(3.W), rs1p, 0x13.U(7.W))
      def srai = srli | (1 << 30).U
      def andi = Cat(addiImm, rs1p, 7.U(3.W), rs1p, 0x13.U(7.W))
      def rtype = {
        val funct = Seq(0.U, 4.U, 6.U, 7.U, 0.U, 0.U, 2.U, 3.U)(Cat(x(12), x(6,5)))
        val sub = Mux(x(6,5) === 0.U, (1 << 30).U, 0.U)
        val opc = Mux(x(12), 0x3B.U(7.W), 0x33.U(7.W))
        Cat(rs2p, rs1p, funct, rs1p, opc) | sub
      }
      inst(Seq(srli, srai, andi, rtype)(x(11,10)), rs1p, rs1p, rs2p)
    }
    Seq(addi, jal, li, lui, arith, j, beqz, bnez)
  }
  
  def q2 = {
    val load_opc = Mux(rd.orR, 0x03.U(7.W), 0x1F.U(7.W))
    def slli = inst(Cat(shamt, rd, 1.U(3.W), rd, 0x13.U(7.W)), rd, rd, rs2)
    def ldsp = inst(Cat(ldspImm, sp, 3.U(3.W), rd, load_opc), rd, sp, rs2)
    def lwsp = inst(Cat(lwspImm, sp, 2.U(3.W), rd, load_opc), rd, sp, rs2)
    def fldsp = inst(Cat(ldspImm, sp, 3.U(3.W), rd, 0x07.U(7.W)), rd, sp, rs2)
    def flwsp = {
      if (xLen == 32) inst(Cat(lwspImm, sp, 2.U(3.W), rd, 0x07.U(7.W)), rd, sp, rs2)
      else ldsp
    }
    def sdsp = inst(Cat(sdspImm >> 5, rs2, sp, 3.U(3.W), sdspImm(4,0), 0x23.U(7.W)), rd, sp, rs2)
    def swsp = inst(Cat(swspImm >> 5, rs2, sp, 2.U(3.W), swspImm(4,0), 0x23.U(7.W)), rd, sp, rs2)
    def fsdsp = inst(Cat(sdspImm >> 5, rs2, sp, 3.U(3.W), sdspImm(4,0), 0x27.U(7.W)), rd, sp, rs2)
    def fswsp = {
      if (xLen == 32) inst(Cat(swspImm >> 5, rs2, sp, 2.U(3.W), swspImm(4,0), 0x27.U(7.W)), rd, sp, rs2)
      else sdsp
    }
    def jalr = {
      val mv = {
        if (useAddiForMv) inst(Cat(rs2, 0.U(3.W), rd, 0x13.U(7.W)), rd, rs2, x0)
        else inst(Cat(rs2, x0, 0.U(3.W), rd, 0x33.U(7.W)), rd, x0, rs2)
      }
      val add = inst(Cat(rs2, rd, 0.U(3.W), rd, 0x33.U(7.W)), rd, rd, rs2)
      val jr = Cat(rs2, rd, 0.U(3.W), x0, 0x67.U(7.W))
      val reserved = Cat(jr >> 7, 0x1F.U(7.W))
      val jr_reserved = inst(Mux(rd.orR, jr, reserved), x0, rd, rs2)
      val jr_mv = Mux(rs2.orR, mv, jr_reserved)
      val jalr = Cat(rs2, rd, 0.U(3.W), ra, 0x67.U(7.W))
      val ebreak = Cat(jr >> 7, 0x73.U(7.W)) | (1 << 20).U
      val jalr_ebreak = inst(Mux(rd.orR, jalr, ebreak), ra, rd, rs2)
      val jalr_add = Mux(rs2.orR, add, jalr_ebreak)
      Mux(x(12), jalr_add, jr_mv)
    }
    Seq(slli, fldsp, lwsp, flwsp, jalr, fsdsp, swsp, fswsp)
  }

  def q3 = Seq.fill(8)(passthrough)

  def passthrough = inst(x)

  def decode = {
    val s = q0 ++ q1 ++ q2 ++ q3
    s(Cat(x(1,0), x(15,13)))
  }
}

class RVCExpander(useAddiForMv: Boolean = false)(implicit val p: Parameters) extends Module with HasCoreParameters {
  val io = IO(new Bundle {
    val in = Input(UInt(32.W))
    val out = Output(new ExpandedInstruction)
    val rvc = Output(Bool())
  })

  if (usingCompressed) {
    io.rvc := io.in(1,0) =/= 3.U
    io.out := new RVCDecoder(io.in, p(XLen), useAddiForMv).decode
  } else {
    io.rvc := false.B
    io.out := new RVCDecoder(io.in, p(XLen), useAddiForMv).passthrough
  }
}
