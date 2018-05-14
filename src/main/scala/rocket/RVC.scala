// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile._
import freechips.rocketchip.util._

class ExpandedInstruction extends Bundle {
  val bits = UInt(width = 32)
  val rd = UInt(width = 5)
  val rs1 = UInt(width = 5)
  val rs2 = UInt(width = 5)
  val rs3 = UInt(width = 5)
}

class RVCDecoder(x: UInt, xLen: Int) {
  def inst(bits: UInt, rd: UInt = x(11,7), rs1: UInt = x(19,15), rs2: UInt = x(24,20), rs3: UInt = x(31,27)) = {
    val res = Wire(new ExpandedInstruction)
    res.bits := bits
    res.rd := rd
    res.rs1 := rs1
    res.rs2 := rs2
    res.rs3 := rs3
    res
  }

  def rs1p = Cat(UInt(1,2), x(9,7))
  def rs2p = Cat(UInt(1,2), x(4,2))
  def rs2 = x(6,2)
  def rd = x(11,7)
  def addi4spnImm = Cat(x(10,7), x(12,11), x(5), x(6), UInt(0,2))
  def lwImm = Cat(x(5), x(12,10), x(6), UInt(0,2))
  def ldImm = Cat(x(6,5), x(12,10), UInt(0,3))
  def lwspImm = Cat(x(3,2), x(12), x(6,4), UInt(0,2))
  def ldspImm = Cat(x(4,2), x(12), x(6,5), UInt(0,3))
  def swspImm = Cat(x(8,7), x(12,9), UInt(0,2))
  def sdspImm = Cat(x(9,7), x(12,10), UInt(0,3))
  def luiImm = Cat(Fill(15, x(12)), x(6,2), UInt(0,12))
  def addi16spImm = Cat(Fill(3, x(12)), x(4,3), x(5), x(2), x(6), UInt(0,4))
  def addiImm = Cat(Fill(7, x(12)), x(6,2))
  def jImm = Cat(Fill(10, x(12)), x(8), x(10,9), x(6), x(7), x(2), x(11), x(5,3), UInt(0,1))
  def bImm = Cat(Fill(5, x(12)), x(6,5), x(2), x(11,10), x(4,3), UInt(0,1))
  def shamt = Cat(x(12), x(6,2))
  def x0 = UInt(0,5)
  def ra = UInt(1,5)
  def sp = UInt(2,5)

  def q0 = {
    def addi4spn = {
      val opc = Mux(x(12,5).orR, UInt(0x13,7), UInt(0x1F,7))
      inst(Cat(addi4spnImm, sp, UInt(0,3), rs2p, opc), rs2p, sp, rs2p)
    }
    def ld = inst(Cat(ldImm, rs1p, UInt(3,3), rs2p, UInt(0x03,7)), rs2p, rs1p, rs2p)
    def lw = inst(Cat(lwImm, rs1p, UInt(2,3), rs2p, UInt(0x03,7)), rs2p, rs1p, rs2p)
    def fld = inst(Cat(ldImm, rs1p, UInt(3,3), rs2p, UInt(0x07,7)), rs2p, rs1p, rs2p)
    def flw = {
      if (xLen == 32) inst(Cat(lwImm, rs1p, UInt(2,3), rs2p, UInt(0x07,7)), rs2p, rs1p, rs2p)
      else ld
    }
    def unimp = inst(Cat(lwImm >> 5, rs2p, rs1p, UInt(2,3), lwImm(4,0), UInt(0x3F,7)), rs2p, rs1p, rs2p)
    def sd = inst(Cat(ldImm >> 5, rs2p, rs1p, UInt(3,3), ldImm(4,0), UInt(0x23,7)), rs2p, rs1p, rs2p)
    def sw = inst(Cat(lwImm >> 5, rs2p, rs1p, UInt(2,3), lwImm(4,0), UInt(0x23,7)), rs2p, rs1p, rs2p)
    def fsd = inst(Cat(ldImm >> 5, rs2p, rs1p, UInt(3,3), ldImm(4,0), UInt(0x27,7)), rs2p, rs1p, rs2p)
    def fsw = {
      if (xLen == 32) inst(Cat(lwImm >> 5, rs2p, rs1p, UInt(2,3), lwImm(4,0), UInt(0x27,7)), rs2p, rs1p, rs2p)
      else sd
    }
    Seq(addi4spn, fld, lw, flw, unimp, fsd, sw, fsw)
  }

  def q1 = {
    def addi = inst(Cat(addiImm, rd, UInt(0,3), rd, UInt(0x13,7)), rd, rd, rs2p)
    def addiw = {
      val opc = Mux(rd.orR, UInt(0x1B,7), UInt(0x1F,7))
      inst(Cat(addiImm, rd, UInt(0,3), rd, opc), rd, rd, rs2p)
    }
    def jal = {
      if (xLen == 32) inst(Cat(jImm(20), jImm(10,1), jImm(11), jImm(19,12), ra, UInt(0x6F,7)), ra, rd, rs2p)
      else addiw
    }
    def li = inst(Cat(addiImm, x0, UInt(0,3), rd, UInt(0x13,7)), rd, x0, rs2p)
    def addi16sp = {
      val opc = Mux(addiImm.orR, UInt(0x13,7), UInt(0x1F,7))
      inst(Cat(addi16spImm, rd, UInt(0,3), rd, opc), rd, rd, rs2p)
    }
    def lui = {
      val opc = Mux(addiImm.orR, UInt(0x37,7), UInt(0x3F,7))
      val me = inst(Cat(luiImm(31,12), rd, opc), rd, rd, rs2p)
      Mux(rd === x0 || rd === sp, addi16sp, me)
    }
    def j = inst(Cat(jImm(20), jImm(10,1), jImm(11), jImm(19,12), x0, UInt(0x6F,7)), x0, rs1p, rs2p)
    def beqz = inst(Cat(bImm(12), bImm(10,5), x0, rs1p, UInt(0,3), bImm(4,1), bImm(11), UInt(0x63,7)), rs1p, rs1p, x0)
    def bnez = inst(Cat(bImm(12), bImm(10,5), x0, rs1p, UInt(1,3), bImm(4,1), bImm(11), UInt(0x63,7)), x0, rs1p, x0)
    def arith = {
      def srli = Cat(shamt, rs1p, UInt(5,3), rs1p, UInt(0x13,7))
      def srai = srli | UInt(1 << 30)
      def andi = Cat(addiImm, rs1p, UInt(7,3), rs1p, UInt(0x13,7))
      def rtype = {
        val funct = Seq(0.U, 4.U, 6.U, 7.U, 0.U, 0.U, 2.U, 3.U)(Cat(x(12), x(6,5)))
        val sub = Mux(x(6,5) === UInt(0), UInt(1 << 30), UInt(0))
        val opc = Mux(x(12), UInt(0x3B,7), UInt(0x33,7))
        Cat(rs2p, rs1p, funct, rs1p, opc) | sub
      }
      inst(Seq(srli, srai, andi, rtype)(x(11,10)), rs1p, rs1p, rs2p)
    }
    Seq(addi, jal, li, lui, arith, j, beqz, bnez)
  }
  
  def q2 = {
    val load_opc = Mux(rd.orR, UInt(0x03,7), UInt(0x1F,7))
    def slli = inst(Cat(shamt, rd, UInt(1,3), rd, UInt(0x13,7)), rd, rd, rs2)
    def ldsp = inst(Cat(ldspImm, sp, UInt(3,3), rd, load_opc), rd, sp, rs2)
    def lwsp = inst(Cat(lwspImm, sp, UInt(2,3), rd, load_opc), rd, sp, rs2)
    def fldsp = inst(Cat(ldspImm, sp, UInt(3,3), rd, UInt(0x07,7)), rd, sp, rs2)
    def flwsp = {
      if (xLen == 32) inst(Cat(lwspImm, sp, UInt(2,3), rd, UInt(0x07,7)), rd, sp, rs2)
      else ldsp
    }
    def sdsp = inst(Cat(sdspImm >> 5, rs2, sp, UInt(3,3), sdspImm(4,0), UInt(0x23,7)), rd, sp, rs2)
    def swsp = inst(Cat(swspImm >> 5, rs2, sp, UInt(2,3), swspImm(4,0), UInt(0x23,7)), rd, sp, rs2)
    def fsdsp = inst(Cat(sdspImm >> 5, rs2, sp, UInt(3,3), sdspImm(4,0), UInt(0x27,7)), rd, sp, rs2)
    def fswsp = {
      if (xLen == 32) inst(Cat(swspImm >> 5, rs2, sp, UInt(2,3), swspImm(4,0), UInt(0x27,7)), rd, sp, rs2)
      else sdsp
    }
    def jalr = {
      val mv = inst(Cat(rs2, x0, UInt(0,3), rd, UInt(0x33,7)), rd, x0, rs2)
      val add = inst(Cat(rs2, rd, UInt(0,3), rd, UInt(0x33,7)), rd, rd, rs2)
      val jr = Cat(rs2, rd, UInt(0,3), x0, UInt(0x67,7))
      val reserved = Cat(jr >> 7, UInt(0x1F,7))
      val jr_reserved = inst(Mux(rd.orR, jr, reserved), x0, rd, rs2)
      val jr_mv = Mux(rs2.orR, mv, jr_reserved)
      val jalr = Cat(rs2, rd, UInt(0,3), ra, UInt(0x67,7))
      val ebreak = Cat(jr >> 7, UInt(0x73,7)) | UInt(1 << 20)
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

class RVCExpander(implicit val p: Parameters) extends Module with HasCoreParameters {
  val io = new Bundle {
    val in = UInt(INPUT, 32)
    val out = new ExpandedInstruction
    val rvc = Bool(OUTPUT)
  }

  if (usingCompressed) {
    io.rvc := io.in(1,0) =/= UInt(3)
    io.out := new RVCDecoder(io.in, p(XLen)).decode
  } else {
    io.rvc := Bool(false)
    io.out := new RVCDecoder(io.in, p(XLen)).passthrough
  }
}
