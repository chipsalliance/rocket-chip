// See LICENSE for license details.

package rocket

import Chisel._
import Instructions._
import Util._
import FPConstants._
import uncore.constants.MemoryOpConstants._
import cde.{Parameters, Field}

case object SFMALatency
case object DFMALatency

object FPConstants
{
  val FCMD_ADD =    BitPat("b0??00")
  val FCMD_SUB =    BitPat("b0??01")
  val FCMD_MUL =    BitPat("b0??10")
  val FCMD_MADD =   BitPat("b1??00")
  val FCMD_MSUB =   BitPat("b1??01")
  val FCMD_NMSUB =  BitPat("b1??10")
  val FCMD_NMADD =  BitPat("b1??11")
  val FCMD_DIV =    BitPat("b?0011")
  val FCMD_SQRT =   BitPat("b?1011")
  val FCMD_SGNJ =   BitPat("b??1?0")
  val FCMD_MINMAX = BitPat("b?01?1")
  val FCMD_CVT_FF = BitPat("b??0??")
  val FCMD_CVT_IF = BitPat("b?10??")
  val FCMD_CMP =    BitPat("b?01??")
  val FCMD_MV_XF =  BitPat("b?11??")
  val FCMD_CVT_FI = BitPat("b??0??")
  val FCMD_MV_FX =  BitPat("b??1??")
  val FCMD_X =      BitPat("b?????")
  val FCMD_WIDTH = 5

  val RM_SZ = 3
  val FLAGS_SZ = 5
}

class FPUCtrlSigs extends Bundle
{
  val cmd = Bits(width = FCMD_WIDTH)
  val ldst = Bool()
  val wen = Bool()
  val ren1 = Bool()
  val ren2 = Bool()
  val ren3 = Bool()
  val swap12 = Bool()
  val swap23 = Bool()
  val single = Bool()
  val fromint = Bool()
  val toint = Bool()
  val fastpipe = Bool()
  val fma = Bool()
  val div = Bool()
  val sqrt = Bool()
  val round = Bool()
  val wflags = Bool()
}

class FPUDecoder extends Module
{
  val io = new Bundle {
    val inst = Bits(INPUT, 32)
    val sigs = new FPUCtrlSigs().asOutput
  }

  val decoder = DecodeLogic(io.inst,
    List                  (FCMD_X,      X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X),
    Array(FLW      -> List(FCMD_X,      Y,Y,N,N,N,X,X,Y,N,N,N,N,N,N,N,N),
          FLD      -> List(FCMD_X,      Y,Y,N,N,N,X,X,N,N,N,N,N,N,N,N,N),
          FSW      -> List(FCMD_MV_XF,  Y,N,N,Y,N,Y,X,Y,N,Y,N,N,N,N,N,N),
          FSD      -> List(FCMD_MV_XF,  Y,N,N,Y,N,Y,X,N,N,Y,N,N,N,N,N,N),
          FMV_S_X  -> List(FCMD_MV_FX,  N,Y,N,N,N,X,X,Y,Y,N,N,N,N,N,Y,N),
          FMV_D_X  -> List(FCMD_MV_FX,  N,Y,N,N,N,X,X,N,Y,N,N,N,N,N,Y,N),
          FCVT_S_W -> List(FCMD_CVT_FI, N,Y,N,N,N,X,X,Y,Y,N,N,N,N,N,Y,Y),
          FCVT_S_WU-> List(FCMD_CVT_FI, N,Y,N,N,N,X,X,Y,Y,N,N,N,N,N,Y,Y),
          FCVT_S_L -> List(FCMD_CVT_FI, N,Y,N,N,N,X,X,Y,Y,N,N,N,N,N,Y,Y),
          FCVT_S_LU-> List(FCMD_CVT_FI, N,Y,N,N,N,X,X,Y,Y,N,N,N,N,N,Y,Y),
          FCVT_D_W -> List(FCMD_CVT_FI, N,Y,N,N,N,X,X,N,Y,N,N,N,N,N,Y,Y),
          FCVT_D_WU-> List(FCMD_CVT_FI, N,Y,N,N,N,X,X,N,Y,N,N,N,N,N,Y,Y),
          FCVT_D_L -> List(FCMD_CVT_FI, N,Y,N,N,N,X,X,N,Y,N,N,N,N,N,Y,Y),
          FCVT_D_LU-> List(FCMD_CVT_FI, N,Y,N,N,N,X,X,N,Y,N,N,N,N,N,Y,Y),
          FMV_X_S  -> List(FCMD_MV_XF,  N,N,Y,N,N,N,X,Y,N,Y,N,N,N,N,Y,N),
          FMV_X_D  -> List(FCMD_MV_XF,  N,N,Y,N,N,N,X,N,N,Y,N,N,N,N,Y,N),
          FCLASS_S -> List(FCMD_MV_XF,  N,N,Y,N,N,N,X,Y,N,Y,N,N,N,N,Y,N),
          FCLASS_D -> List(FCMD_MV_XF,  N,N,Y,N,N,N,X,N,N,Y,N,N,N,N,Y,N),
          FCVT_W_S -> List(FCMD_CVT_IF, N,N,Y,N,N,N,X,Y,N,Y,N,N,N,N,Y,Y),
          FCVT_WU_S-> List(FCMD_CVT_IF, N,N,Y,N,N,N,X,Y,N,Y,N,N,N,N,Y,Y),
          FCVT_L_S -> List(FCMD_CVT_IF, N,N,Y,N,N,N,X,Y,N,Y,N,N,N,N,Y,Y),
          FCVT_LU_S-> List(FCMD_CVT_IF, N,N,Y,N,N,N,X,Y,N,Y,N,N,N,N,Y,Y),
          FCVT_W_D -> List(FCMD_CVT_IF, N,N,Y,N,N,N,X,N,N,Y,N,N,N,N,Y,Y),
          FCVT_WU_D-> List(FCMD_CVT_IF, N,N,Y,N,N,N,X,N,N,Y,N,N,N,N,Y,Y),
          FCVT_L_D -> List(FCMD_CVT_IF, N,N,Y,N,N,N,X,N,N,Y,N,N,N,N,Y,Y),
          FCVT_LU_D-> List(FCMD_CVT_IF, N,N,Y,N,N,N,X,N,N,Y,N,N,N,N,Y,Y),
          FCVT_S_D -> List(FCMD_CVT_FF, N,Y,Y,N,N,N,X,Y,N,N,Y,N,N,N,Y,Y),
          FCVT_D_S -> List(FCMD_CVT_FF, N,Y,Y,N,N,N,X,N,N,N,Y,N,N,N,Y,Y),
          FEQ_S    -> List(FCMD_CMP,    N,N,Y,Y,N,N,N,Y,N,Y,N,N,N,N,N,Y),
          FLT_S    -> List(FCMD_CMP,    N,N,Y,Y,N,N,N,Y,N,Y,N,N,N,N,N,Y),
          FLE_S    -> List(FCMD_CMP,    N,N,Y,Y,N,N,N,Y,N,Y,N,N,N,N,N,Y),
          FEQ_D    -> List(FCMD_CMP,    N,N,Y,Y,N,N,N,N,N,Y,N,N,N,N,N,Y),
          FLT_D    -> List(FCMD_CMP,    N,N,Y,Y,N,N,N,N,N,Y,N,N,N,N,N,Y),
          FLE_D    -> List(FCMD_CMP,    N,N,Y,Y,N,N,N,N,N,Y,N,N,N,N,N,Y),
          FSGNJ_S  -> List(FCMD_SGNJ,   N,Y,Y,Y,N,N,N,Y,N,N,Y,N,N,N,N,N),
          FSGNJN_S -> List(FCMD_SGNJ,   N,Y,Y,Y,N,N,N,Y,N,N,Y,N,N,N,N,N),
          FSGNJX_S -> List(FCMD_SGNJ,   N,Y,Y,Y,N,N,N,Y,N,N,Y,N,N,N,N,N),
          FSGNJ_D  -> List(FCMD_SGNJ,   N,Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,N),
          FSGNJN_D -> List(FCMD_SGNJ,   N,Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,N),
          FSGNJX_D -> List(FCMD_SGNJ,   N,Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,N),
          FMIN_S   -> List(FCMD_MINMAX, N,Y,Y,Y,N,N,N,Y,N,N,Y,N,N,N,N,Y),
          FMAX_S   -> List(FCMD_MINMAX, N,Y,Y,Y,N,N,N,Y,N,N,Y,N,N,N,N,Y),
          FMIN_D   -> List(FCMD_MINMAX, N,Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,Y),
          FMAX_D   -> List(FCMD_MINMAX, N,Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,Y),
          FADD_S   -> List(FCMD_ADD,    N,Y,Y,Y,N,N,Y,Y,N,N,N,Y,N,N,Y,Y),
          FSUB_S   -> List(FCMD_SUB,    N,Y,Y,Y,N,N,Y,Y,N,N,N,Y,N,N,Y,Y),
          FMUL_S   -> List(FCMD_MUL,    N,Y,Y,Y,N,N,N,Y,N,N,N,Y,N,N,Y,Y),
          FADD_D   -> List(FCMD_ADD,    N,Y,Y,Y,N,N,Y,N,N,N,N,Y,N,N,Y,Y),
          FSUB_D   -> List(FCMD_SUB,    N,Y,Y,Y,N,N,Y,N,N,N,N,Y,N,N,Y,Y),
          FMUL_D   -> List(FCMD_MUL,    N,Y,Y,Y,N,N,N,N,N,N,N,Y,N,N,Y,Y),
          FMADD_S  -> List(FCMD_MADD,   N,Y,Y,Y,Y,N,N,Y,N,N,N,Y,N,N,Y,Y),
          FMSUB_S  -> List(FCMD_MSUB,   N,Y,Y,Y,Y,N,N,Y,N,N,N,Y,N,N,Y,Y),
          FNMADD_S -> List(FCMD_NMADD,  N,Y,Y,Y,Y,N,N,Y,N,N,N,Y,N,N,Y,Y),
          FNMSUB_S -> List(FCMD_NMSUB,  N,Y,Y,Y,Y,N,N,Y,N,N,N,Y,N,N,Y,Y),
          FMADD_D  -> List(FCMD_MADD,   N,Y,Y,Y,Y,N,N,N,N,N,N,Y,N,N,Y,Y),
          FMSUB_D  -> List(FCMD_MSUB,   N,Y,Y,Y,Y,N,N,N,N,N,N,Y,N,N,Y,Y),
          FNMADD_D -> List(FCMD_NMADD,  N,Y,Y,Y,Y,N,N,N,N,N,N,Y,N,N,Y,Y),
          FNMSUB_D -> List(FCMD_NMSUB,  N,Y,Y,Y,Y,N,N,N,N,N,N,Y,N,N,Y,Y),
          FDIV_S   -> List(FCMD_DIV,    N,Y,Y,Y,N,N,N,Y,N,N,N,N,Y,N,Y,Y),
          FSQRT_S  -> List(FCMD_SQRT,   N,Y,Y,N,N,Y,X,Y,N,N,N,N,N,Y,Y,Y),
          FDIV_D   -> List(FCMD_DIV,    N,Y,Y,Y,N,N,N,N,N,N,N,N,Y,N,Y,Y),
          FSQRT_D  -> List(FCMD_SQRT,   N,Y,Y,N,N,Y,X,N,N,N,N,N,N,Y,Y,Y)
          ))
  val s = io.sigs
  val sigs = Seq(s.cmd, s.ldst, s.wen, s.ren1, s.ren2, s.ren3, s.swap12,
                 s.swap23, s.single, s.fromint, s.toint, s.fastpipe, s.fma,
                 s.div, s.sqrt, s.round, s.wflags)
  sigs zip decoder map {case(s,d) => s := d}
}

class FPUIO extends Bundle {
  val inst = Bits(INPUT, 32)
  val fromint_data = Bits(INPUT, 64)

  val fcsr_rm = Bits(INPUT, FPConstants.RM_SZ)
  val fcsr_flags = Valid(Bits(width = FPConstants.FLAGS_SZ))

  val store_data = Bits(OUTPUT, 64)
  val toint_data = Bits(OUTPUT, 64)

  val dmem_resp_val = Bool(INPUT)
  val dmem_resp_type = Bits(INPUT, 3)
  val dmem_resp_tag = UInt(INPUT, 5)
  val dmem_resp_data = Bits(INPUT, 64)

  val valid = Bool(INPUT)
  val fcsr_rdy = Bool(OUTPUT)
  val nack_mem = Bool(OUTPUT)
  val illegal_rm = Bool(OUTPUT)
  val killx = Bool(INPUT)
  val killm = Bool(INPUT)
  val dec = new FPUCtrlSigs().asOutput
  val sboard_set = Bool(OUTPUT)
  val sboard_clr = Bool(OUTPUT)
  val sboard_clra = UInt(OUTPUT, 5)

  val cp_req = Decoupled(new FPInput()).flip //cp doesn't pay attn to kill sigs
  val cp_resp = Decoupled(new FPResult())
}

class FPResult extends Bundle
{
  val data = Bits(width = 65)
  val exc = Bits(width = 5)
}

class FPInput extends FPUCtrlSigs {
  val rm = Bits(width = 3)
  val typ = Bits(width = 2)
  val in1 = Bits(width = 65)
  val in2 = Bits(width = 65)
  val in3 = Bits(width = 65)
}

object ClassifyRecFN {
  def apply(expWidth: Int, sigWidth: Int, in: UInt) = {
    val sign = in(sigWidth + expWidth)
    val exp = in(sigWidth + expWidth - 1, sigWidth - 1)
    val sig = in(sigWidth - 2, 0)

    val code        = exp(expWidth,expWidth-2)
    val codeHi      = code(2, 1)
    val isSpecial   = codeHi === UInt(3)

    val isHighSubnormalIn = exp(expWidth-2, 0) < UInt(2)
    val isSubnormal = code === UInt(1) || codeHi === UInt(1) && isHighSubnormalIn
    val isNormal = codeHi === UInt(1) && !isHighSubnormalIn || codeHi === UInt(2)
    val isZero = code === UInt(0)
    val isInf = isSpecial && !exp(expWidth-2)
    val isNaN = code.andR
    val isSNaN = isNaN && !sig(sigWidth-2)
    val isQNaN = isNaN && sig(sigWidth-2)

    Cat(isQNaN, isSNaN, isInf && !sign, isNormal && !sign,
        isSubnormal && !sign, isZero && !sign, isZero && sign,
        isSubnormal && sign, isNormal && sign, isInf && sign)
  }
}

class FPToInt extends Module
{
  val io = new Bundle {
    val in = Valid(new FPInput).flip
    val as_double = new FPInput().asOutput
    val out = Valid(new Bundle {
      val lt = Bool()
      val store = Bits(width = 64)
      val toint = Bits(width = 64)
      val exc = Bits(width = 5)
    })
  }

  val in = Reg(new FPInput)
  val valid = Reg(next=io.in.valid)

  def upconvert(x: UInt) = {
    val s2d = Module(new hardfloat.RecFNToRecFN(8, 24, 11, 53))
    s2d.io.in := x
    s2d.io.roundingMode := UInt(0)
    s2d.io.out
  }

  val in1_upconvert = upconvert(io.in.bits.in1)
  val in2_upconvert = upconvert(io.in.bits.in2)

  when (io.in.valid) {
    in := io.in.bits
    when (io.in.bits.single && !io.in.bits.ldst && io.in.bits.cmd =/= FCMD_MV_XF) {
      in.in1 := in1_upconvert
      in.in2 := in2_upconvert
    }
  }

  val unrec_s = hardfloat.fNFromRecFN(8, 24, in.in1)
  val unrec_d = hardfloat.fNFromRecFN(11, 53, in.in1)
  val unrec_out = Mux(in.single, Cat(Fill(32, unrec_s(31)), unrec_s), unrec_d)

  val classify_s = ClassifyRecFN(8, 24, in.in1)
  val classify_d = ClassifyRecFN(11, 53, in.in1)
  val classify_out = Mux(in.single, classify_s, classify_d)

  val dcmp = Module(new hardfloat.CompareRecFN(11, 53))
  dcmp.io.a := in.in1
  dcmp.io.b := in.in2
  dcmp.io.signaling := Bool(true)
  val dcmp_out = (~in.rm & Cat(dcmp.io.lt, dcmp.io.eq)).orR
  val dcmp_exc = dcmp.io.exceptionFlags

  val d2l = Module(new hardfloat.RecFNToIN(11, 53, 64))
  val d2w = Module(new hardfloat.RecFNToIN(11, 53, 32))
  d2l.io.in := in.in1
  d2l.io.roundingMode := in.rm
  d2l.io.signedOut := ~in.typ(0)
  d2w.io.in := in.in1
  d2w.io.roundingMode := in.rm
  d2w.io.signedOut := ~in.typ(0)

  io.out.bits.toint := Mux(in.rm(0), classify_out, unrec_out)
  io.out.bits.store := unrec_out
  io.out.bits.exc := Bits(0)

  when (in.cmd === FCMD_CMP) {
    io.out.bits.toint := dcmp_out
    io.out.bits.exc := dcmp_exc
  }
  when (in.cmd === FCMD_CVT_IF) {
    io.out.bits.toint := Mux(in.typ(1), d2l.io.out.toSInt, d2w.io.out.toSInt).toUInt
    val dflags = Mux(in.typ(1), d2l.io.intExceptionFlags, d2w.io.intExceptionFlags)
    io.out.bits.exc := Cat(dflags(2, 1).orR, UInt(0, 3), dflags(0))
  }

  io.out.valid := valid
  io.out.bits.lt := dcmp.io.lt
  io.as_double := in
}

class IntToFP(val latency: Int) extends Module
{
  val io = new Bundle {
    val in = Valid(new FPInput).flip
    val out = Valid(new FPResult)
  }

  val in = Pipe(io.in)

  val mux = Wire(new FPResult)
  mux.exc := Bits(0)
  mux.data := hardfloat.recFNFromFN(11, 53, in.bits.in1)
  when (in.bits.single) {
    mux.data := Cat(SInt(-1, 32), hardfloat.recFNFromFN(8, 24, in.bits.in1))
  }

  val longValue =
    Mux(in.bits.typ(1), in.bits.in1.toSInt,
    Mux(in.bits.typ(0), in.bits.in1(31,0).zext, in.bits.in1(31,0).toSInt))
  val l2s = Module(new hardfloat.INToRecFN(64, 8, 24))
  l2s.io.signedIn := ~in.bits.typ(0)
  l2s.io.in := longValue.toUInt
  l2s.io.roundingMode := in.bits.rm

  val l2d = Module(new hardfloat.INToRecFN(64, 11, 53))
  l2d.io.signedIn := ~in.bits.typ(0)
  l2d.io.in := longValue.toUInt
  l2d.io.roundingMode := in.bits.rm

  when (in.bits.cmd === FCMD_CVT_FI) {
    when (in.bits.single) {
      mux.data := Cat(SInt(-1, 32), l2s.io.out)
      mux.exc := l2s.io.exceptionFlags
    }.otherwise {
      mux.data := l2d.io.out
      mux.exc := l2d.io.exceptionFlags
    }
  }

  io.out <> Pipe(in.valid, mux, latency-1)
}

class FPToFP(val latency: Int) extends Module
{
  val io = new Bundle {
    val in = Valid(new FPInput).flip
    val out = Valid(new FPResult)
    val lt = Bool(INPUT) // from FPToInt
  }

  val in = Pipe(io.in)

  // fp->fp units
  val isSgnj = in.bits.cmd === FCMD_SGNJ
  def fsgnjSign(in1: Bits, in2: Bits, pos: Int, en: Bool, rm: Bits) =
    Mux(rm(1) || !en, in1(pos), rm(0)) ^ (en && in2(pos))
  val sign_s = fsgnjSign(in.bits.in1, in.bits.in2, 32, in.bits.single && isSgnj, in.bits.rm)
  val sign_d = fsgnjSign(in.bits.in1, in.bits.in2, 64, !in.bits.single && isSgnj, in.bits.rm)
  val fsgnj = Cat(sign_d, in.bits.in1(63,33), sign_s, in.bits.in1(31,0))

  val s2d = Module(new hardfloat.RecFNToRecFN(8, 24, 11, 53))
  val d2s = Module(new hardfloat.RecFNToRecFN(11, 53, 8, 24))
  s2d.io.in := in.bits.in1
  s2d.io.roundingMode := in.bits.rm
  d2s.io.in := in.bits.in1
  d2s.io.roundingMode := in.bits.rm

  val isnan1 = Mux(in.bits.single, in.bits.in1(31,29).andR, in.bits.in1(63,61).andR)
  val isnan2 = Mux(in.bits.single, in.bits.in2(31,29).andR, in.bits.in2(63,61).andR)
  val issnan1 = isnan1 && ~Mux(in.bits.single, in.bits.in1(22), in.bits.in1(51))
  val issnan2 = isnan2 && ~Mux(in.bits.single, in.bits.in2(22), in.bits.in2(51))
  val minmax_exc = Cat(issnan1 || issnan2, Bits(0,4))
  val isMax = in.bits.rm(0)
  val isLHS = isnan2 || isMax =/= io.lt && !isnan1

  val mux = Wire(new FPResult)
  mux.exc := minmax_exc
  mux.data := in.bits.in2

  when (isSgnj) { mux.exc := UInt(0) }
  when (isSgnj || isLHS) { mux.data := fsgnj }
  when (in.bits.cmd === FCMD_CVT_FF) {
    when (in.bits.single) {
      mux.data := Cat(SInt(-1, 32), d2s.io.out)
      mux.exc := d2s.io.exceptionFlags
    }.otherwise {
      mux.data := s2d.io.out
      mux.exc := s2d.io.exceptionFlags
    }
  }

  io.out <> Pipe(in.valid, mux, latency-1)
}

class FPUFMAPipe(val latency: Int, expWidth: Int, sigWidth: Int) extends Module
{
  val io = new Bundle {
    val in = Valid(new FPInput).flip
    val out = Valid(new FPResult)
  }

  val width = sigWidth + expWidth
  val one = UInt(1) << (width-1)
  val zero = (io.in.bits.in1(width) ^ io.in.bits.in2(width)) << width

  val valid = Reg(next=io.in.valid)
  val in = Reg(new FPInput)
  when (io.in.valid) {
    in := io.in.bits
    val cmd_fma = io.in.bits.ren3
    val cmd_addsub = io.in.bits.swap23
    in.cmd := Cat(io.in.bits.cmd(1) & (cmd_fma || cmd_addsub), io.in.bits.cmd(0))
    when (cmd_addsub) { in.in2 := one }
    unless (cmd_fma || cmd_addsub) { in.in3 := zero }
  }

  val fma = Module(new hardfloat.MulAddRecFN(expWidth, sigWidth))
  fma.io.op := in.cmd
  fma.io.roundingMode := in.rm
  fma.io.a := in.in1
  fma.io.b := in.in2
  fma.io.c := in.in3

  val res = Wire(new FPResult)
  res.data := Cat(SInt(-1, 32), fma.io.out)
  res.exc := fma.io.exceptionFlags
  io.out := Pipe(valid, res, latency-1)
}

class FPU(implicit p: Parameters) extends CoreModule()(p) {
  val io = new FPUIO

  val ex_reg_valid = Reg(next=io.valid, init=Bool(false))
  val req_valid = ex_reg_valid || io.cp_req.valid
  val ex_reg_inst = RegEnable(io.inst, io.valid)
  val ex_cp_valid = io.cp_req.valid && !ex_reg_valid
  val mem_reg_valid = Reg(next=ex_reg_valid && !io.killx || ex_cp_valid, init=Bool(false))
  val mem_reg_inst = RegEnable(ex_reg_inst, ex_reg_valid)
  val mem_cp_valid = Reg(next=ex_cp_valid, init=Bool(false))
  val killm = (io.killm || io.nack_mem) && !mem_cp_valid
  val wb_reg_valid = Reg(next=mem_reg_valid && (!killm || mem_cp_valid), init=Bool(false))
  val wb_cp_valid = Reg(next=mem_cp_valid, init=Bool(false))

  val fp_decoder = Module(new FPUDecoder)
  fp_decoder.io.inst := io.inst

  val cp_ctrl = Wire(new FPUCtrlSigs)
  cp_ctrl <> io.cp_req.bits
  io.cp_resp.valid := Bool(false)
  io.cp_resp.bits.data := UInt(0)

  val id_ctrl = fp_decoder.io.sigs
  val ex_ctrl = Mux(ex_reg_valid, RegEnable(id_ctrl, io.valid), cp_ctrl)
  val mem_ctrl = RegEnable(ex_ctrl, req_valid)
  val wb_ctrl = RegEnable(mem_ctrl, mem_reg_valid)

  // load response
  val load_wb = Reg(next=io.dmem_resp_val)
  val load_wb_single = RegEnable(io.dmem_resp_type === MT_W || io.dmem_resp_type === MT_WU, io.dmem_resp_val)
  val load_wb_data = RegEnable(io.dmem_resp_data, io.dmem_resp_val)
  val load_wb_tag = RegEnable(io.dmem_resp_tag, io.dmem_resp_val)
  val rec_s = hardfloat.recFNFromFN(8, 24, load_wb_data)
  val rec_d = hardfloat.recFNFromFN(11, 53, load_wb_data)
  val load_wb_data_recoded = Mux(load_wb_single, Cat(SInt(-1, 32), rec_s), rec_d)

  // regfile
  val regfile = Mem(32, Bits(width = 65))
  when (load_wb) {
    regfile(load_wb_tag) := load_wb_data_recoded
    if (enableCommitLog) {
      printf ("f%d p%d 0x%x\n", load_wb_tag, load_wb_tag + UInt(32),
        Mux(load_wb_single, load_wb_data(31,0), load_wb_data))
    }
  }

  val ex_ra1::ex_ra2::ex_ra3::Nil = List.fill(3)(Reg(UInt()))
  when (io.valid) {
    when (id_ctrl.ren1) {
      when (!id_ctrl.swap12) { ex_ra1 := io.inst(19,15) }
      when (id_ctrl.swap12) { ex_ra2 := io.inst(19,15) }
    }
    when (id_ctrl.ren2) {
      when (id_ctrl.swap12) { ex_ra1 := io.inst(24,20) }
      when (id_ctrl.swap23) { ex_ra3 := io.inst(24,20) }
      when (!id_ctrl.swap12 && !id_ctrl.swap23) { ex_ra2 := io.inst(24,20) }
    }
    when (id_ctrl.ren3) { ex_ra3 := io.inst(31,27) }
  }
  val ex_rs1::ex_rs2::ex_rs3::Nil = Seq(ex_ra1, ex_ra2, ex_ra3).map(regfile(_))
  val ex_rm = Mux(ex_reg_inst(14,12) === Bits(7), io.fcsr_rm, ex_reg_inst(14,12))

  val cp_rs1 = io.cp_req.bits.in1
  val cp_rs2 = Mux(io.cp_req.bits.swap23, io.cp_req.bits.in3, io.cp_req.bits.in2)
  val cp_rs3 = Mux(io.cp_req.bits.swap23, io.cp_req.bits.in2, io.cp_req.bits.in3)

  val req = Wire(new FPInput)
  req := ex_ctrl
  req.rm := Mux(ex_reg_valid, ex_rm, io.cp_req.bits.rm)
  req.in1 := Mux(ex_reg_valid, ex_rs1, cp_rs1)
  req.in2 := Mux(ex_reg_valid, ex_rs2, cp_rs2)
  req.in3 := Mux(ex_reg_valid, ex_rs3, cp_rs3)
  req.typ := Mux(ex_reg_valid, ex_reg_inst(21,20), io.cp_req.bits.typ)

  val sfma = Module(new FPUFMAPipe(p(SFMALatency), 8, 24))
  sfma.io.in.valid := req_valid && ex_ctrl.fma && ex_ctrl.single
  sfma.io.in.bits := req

  val dfma = Module(new FPUFMAPipe(p(DFMALatency), 11, 53))
  dfma.io.in.valid := req_valid && ex_ctrl.fma && !ex_ctrl.single
  dfma.io.in.bits := req

  val fpiu = Module(new FPToInt)
  fpiu.io.in.valid := req_valid && (ex_ctrl.toint || ex_ctrl.div || ex_ctrl.sqrt || ex_ctrl.cmd === FCMD_MINMAX)
  fpiu.io.in.bits := req
  io.store_data := fpiu.io.out.bits.store
  io.toint_data := fpiu.io.out.bits.toint
  when(fpiu.io.out.valid && mem_cp_valid && mem_ctrl.toint){
    io.cp_resp.bits.data := fpiu.io.out.bits.toint
    io.cp_resp.valid := Bool(true)
  }

  val ifpu = Module(new IntToFP(3))
  ifpu.io.in.valid := req_valid && ex_ctrl.fromint
  ifpu.io.in.bits := req
  ifpu.io.in.bits.in1 := Mux(ex_reg_valid, io.fromint_data, cp_rs1)

  val fpmu = Module(new FPToFP(2))
  fpmu.io.in.valid := req_valid && ex_ctrl.fastpipe
  fpmu.io.in.bits := req
  fpmu.io.lt := fpiu.io.out.bits.lt

  val divSqrt_wen = Reg(next=Bool(false))
  val divSqrt_inReady = Wire(init=Bool(false))
  val divSqrt_waddr = Reg(Bits())
  val divSqrt_wdata = Wire(Bits())
  val divSqrt_flags = Wire(Bits())
  val divSqrt_in_flight = Reg(init=Bool(false))
  val divSqrt_killed = Reg(Bool())

  // writeback arbitration
  case class Pipe(p: Module, lat: Int, cond: (FPUCtrlSigs) => Bool, res: FPResult)
  val pipes = List(
    Pipe(fpmu, fpmu.latency, (c: FPUCtrlSigs) => c.fastpipe, fpmu.io.out.bits),
    Pipe(ifpu, ifpu.latency, (c: FPUCtrlSigs) => c.fromint, ifpu.io.out.bits),
    Pipe(sfma, sfma.latency, (c: FPUCtrlSigs) => c.fma && c.single, sfma.io.out.bits),
    Pipe(dfma, dfma.latency, (c: FPUCtrlSigs) => c.fma && !c.single, dfma.io.out.bits))
  def latencyMask(c: FPUCtrlSigs, offset: Int) = {
    require(pipes.forall(_.lat >= offset))
    pipes.map(p => Mux(p.cond(c), UInt(1 << p.lat-offset), UInt(0))).reduce(_|_)
  }
  def pipeid(c: FPUCtrlSigs) = pipes.zipWithIndex.map(p => Mux(p._1.cond(c), UInt(p._2), UInt(0))).reduce(_|_)
  val maxLatency = pipes.map(_.lat).max
  val memLatencyMask = latencyMask(mem_ctrl, 2)

  val wen = Reg(init=Bits(0, maxLatency-1))
  val winfo = Reg(Vec(maxLatency-1, Bits()))
  val mem_wen = mem_reg_valid && (mem_ctrl.fma || mem_ctrl.fastpipe || mem_ctrl.fromint)
  val write_port_busy = RegEnable(mem_wen && (memLatencyMask & latencyMask(ex_ctrl, 1)).orR || (wen & latencyMask(ex_ctrl, 0)).orR, req_valid)
  val mem_winfo = Cat(mem_cp_valid, pipeid(mem_ctrl), mem_ctrl.single, mem_reg_inst(11,7)) //single only used for debugging

  for (i <- 0 until maxLatency-2) {
    when (wen(i+1)) { winfo(i) := winfo(i+1) }
  }
  wen := wen >> 1
  when (mem_wen) {
    when (!killm) {
      wen := wen >> 1 | memLatencyMask
    }
    for (i <- 0 until maxLatency-1) {
      when (!write_port_busy && memLatencyMask(i)) {
        winfo(i) := mem_winfo
      }
    }
  }

  val waddr = Mux(divSqrt_wen, divSqrt_waddr, winfo(0)(4,0).toUInt)
  val wsrc = (winfo(0) >> 6)
  val wcp = winfo(0)(6+log2Up(pipes.size))
  val wdata = Mux(divSqrt_wen, divSqrt_wdata, Vec(pipes.map(_.res.data))(wsrc))
  val wexc = Vec(pipes.map(_.res.exc))(wsrc)
  when ((!wcp && wen(0)) || divSqrt_wen) {
    regfile(waddr) := wdata
    if (enableCommitLog) {
      val wdata_unrec_s = hardfloat.fNFromRecFN(8, 24, wdata(64,0))
      val wdata_unrec_d = hardfloat.fNFromRecFN(11, 53, wdata(64,0))
      val wb_single = (winfo(0) >> 5)(0)
      printf ("f%d p%d 0x%x\n", waddr, waddr+ UInt(32),
        Mux(wb_single, Cat(UInt(0,32), wdata_unrec_s), wdata_unrec_d))
    }
  }
  when (wcp && wen(0)) {
    io.cp_resp.bits.data := wdata
    io.cp_resp.valid := Bool(true)
  }
  io.cp_req.ready := !ex_reg_valid

  val wb_toint_valid = wb_reg_valid && wb_ctrl.toint
  val wb_toint_exc = RegEnable(fpiu.io.out.bits.exc, mem_ctrl.toint)
  io.fcsr_flags.valid := wb_toint_valid || divSqrt_wen || wen(0)
  io.fcsr_flags.bits :=
    Mux(wb_toint_valid, wb_toint_exc, UInt(0)) |
    Mux(divSqrt_wen, divSqrt_flags, UInt(0)) |
    Mux(wen(0), wexc, UInt(0))

  val units_busy = mem_reg_valid && (mem_ctrl.div || mem_ctrl.sqrt) && (!divSqrt_inReady || wen.orR) // || mem_reg_valid && mem_ctrl.fma && Reg(next=Mux(ex_ctrl.single, io.sfma.valid, io.dfma.valid))
  io.fcsr_rdy := !(ex_reg_valid && ex_ctrl.wflags || mem_reg_valid && mem_ctrl.wflags || wb_reg_valid && wb_ctrl.toint || wen.orR || divSqrt_in_flight)
  io.nack_mem := units_busy || write_port_busy || divSqrt_in_flight
  io.dec <> fp_decoder.io.sigs
  def useScoreboard(f: ((Pipe, Int)) => Bool) = pipes.zipWithIndex.filter(_._1.lat > 3).map(x => f(x)).fold(Bool(false))(_||_)
  io.sboard_set := wb_reg_valid && !wb_cp_valid && Reg(next=useScoreboard(_._1.cond(mem_ctrl)) || mem_ctrl.div || mem_ctrl.sqrt)
  io.sboard_clr := !wb_cp_valid && (divSqrt_wen || (wen(0) && useScoreboard(x => wsrc === UInt(x._2))))
  io.sboard_clra := waddr
  // we don't currently support round-max-magnitude (rm=4)
  io.illegal_rm := ex_rm(2) && ex_ctrl.round

  divSqrt_wdata := 0
  divSqrt_flags := 0
  if (p(FDivSqrt)) {
    val divSqrt_single = Reg(Bool())
    val divSqrt_rm = Reg(Bits())
    val divSqrt_flags_double = Reg(Bits())
    val divSqrt_wdata_double = Reg(Bits())

    val divSqrt = Module(new hardfloat.DivSqrtRecF64)
    divSqrt_inReady := Mux(divSqrt.io.sqrtOp, divSqrt.io.inReady_sqrt, divSqrt.io.inReady_div)
    val divSqrt_outValid = divSqrt.io.outValid_div || divSqrt.io.outValid_sqrt
    divSqrt.io.inValid := mem_reg_valid && (mem_ctrl.div || mem_ctrl.sqrt) && !divSqrt_in_flight
    divSqrt.io.sqrtOp := mem_ctrl.sqrt
    divSqrt.io.a := fpiu.io.as_double.in1
    divSqrt.io.b := fpiu.io.as_double.in2
    divSqrt.io.roundingMode := fpiu.io.as_double.rm

    when (divSqrt.io.inValid && divSqrt_inReady) {
      divSqrt_in_flight := true
      divSqrt_killed := killm
      divSqrt_single := mem_ctrl.single
      divSqrt_waddr := mem_reg_inst(11,7)
      divSqrt_rm := divSqrt.io.roundingMode
    }

    when (divSqrt_outValid) {
      divSqrt_wen := !divSqrt_killed
      divSqrt_wdata_double := divSqrt.io.out
      divSqrt_in_flight := false
      divSqrt_flags_double := divSqrt.io.exceptionFlags
    }

    val divSqrt_toSingle = Module(new hardfloat.RecFNToRecFN(11, 53, 8, 24))
    divSqrt_toSingle.io.in := divSqrt_wdata_double
    divSqrt_toSingle.io.roundingMode := divSqrt_rm
    divSqrt_wdata := Mux(divSqrt_single, divSqrt_toSingle.io.out, divSqrt_wdata_double)
    divSqrt_flags := divSqrt_flags_double | Mux(divSqrt_single, divSqrt_toSingle.io.exceptionFlags, Bits(0))
  }
}
