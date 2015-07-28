// See LICENSE for license details.

package rocket

import Chisel._
import Instructions._
import Util._
import FPConstants._
import uncore.constants.MemoryOpConstants._

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
  Vec(s.cmd, s.ldst, s.wen, s.ren1, s.ren2, s.ren3, s.swap12, s.swap23, s.single, s.fromint,
      s.toint, s.fastpipe, s.fma, s.div, s.sqrt, s.round, s.wflags) := decoder
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
}

class CtrlFPUIO extends Bundle {
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
  when (io.in.valid) {
    def upconvert(x: UInt) = hardfloat.recodedFloatNToRecodedFloatM(x, Bits(0), 23, 9, 52, 12)._1
    in := io.in.bits
    when (io.in.bits.single && !io.in.bits.ldst && io.in.bits.cmd != FCMD_MV_XF) {
      in.in1 := upconvert(io.in.bits.in1)
      in.in2 := upconvert(io.in.bits.in2)
    }
  }

  val unrec_s = hardfloat.recodedFloatNToFloatN(in.in1, 23, 9)
  val unrec_d = hardfloat.recodedFloatNToFloatN(in.in1, 52, 12)
  val unrec_out = Mux(in.single, Cat(Fill(32, unrec_s(31)), unrec_s), unrec_d)

  val classify_s = hardfloat.recodedFloatNClassify(in.in1, 23, 9)
  val classify_d = hardfloat.recodedFloatNClassify(in.in1, 52, 12)
  val classify_out = Mux(in.single, classify_s, classify_d)

  val dcmp = Module(new hardfloat.recodedFloatNCompare(52, 12))
  dcmp.io.a := in.in1
  dcmp.io.b := in.in2
  val dcmp_out = (~in.rm & Cat(dcmp.io.a_lt_b, dcmp.io.a_eq_b)).orR
  val dcmp_exc = (~in.rm & Cat(dcmp.io.a_lt_b_invalid, dcmp.io.a_eq_b_invalid)).orR << UInt(4)

  val d2i = hardfloat.recodedFloatNToAny(in.in1, in.rm, in.typ ^ 1, 52, 12, 64)

  io.out.bits.toint := Mux(in.rm(0), classify_out, unrec_out)
  io.out.bits.store := unrec_out
  io.out.bits.exc := Bits(0)

  when (in.cmd === FCMD_CMP) {
    io.out.bits.toint := dcmp_out
    io.out.bits.exc := dcmp_exc
  }
  when (in.cmd === FCMD_CVT_IF) {
    io.out.bits.toint := Mux(in.typ(1), d2i._1, d2i._1(31,0).toSInt)
    io.out.bits.exc := d2i._2
  }

  io.out.valid := valid
  io.out.bits.lt := dcmp.io.a_lt_b
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
  mux.data := hardfloat.floatNToRecodedFloatN(in.bits.in1, 52, 12)
  when (in.bits.single) {
    mux.data := Cat(SInt(-1, 32), hardfloat.floatNToRecodedFloatN(in.bits.in1, 23, 9))
  }

  when (in.bits.cmd === FCMD_CVT_FI) {
    when (in.bits.single) {
      val u = hardfloat.anyToRecodedFloatN(in.bits.in1(63,0), in.bits.rm, in.bits.typ ^ 1, 23, 9, 64)
      mux.data := Cat(SInt(-1, 32), u._1)
      mux.exc := u._2
    }.otherwise {
      val u = hardfloat.anyToRecodedFloatN(in.bits.in1(63,0), in.bits.rm, in.bits.typ ^ 1, 52, 12, 64)
      mux.data := u._1
      mux.exc := u._2
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

  val s2d = hardfloat.recodedFloatNToRecodedFloatM(in.bits.in1, in.bits.rm, 23, 9, 52, 12)
  val d2s = hardfloat.recodedFloatNToRecodedFloatM(in.bits.in1, in.bits.rm, 52, 12, 23, 9)

  val isnan1 = Mux(in.bits.single, in.bits.in1(31,29).andR, in.bits.in1(63,61).andR)
  val isnan2 = Mux(in.bits.single, in.bits.in2(31,29).andR, in.bits.in2(63,61).andR)
  val issnan1 = isnan1 && ~Mux(in.bits.single, in.bits.in1(22), in.bits.in1(51))
  val issnan2 = isnan2 && ~Mux(in.bits.single, in.bits.in2(22), in.bits.in2(51))
  val minmax_exc = Cat(issnan1 || issnan2, Bits(0,4))
  val isMax = in.bits.rm(0)
  val isLHS = isnan2 || isMax != io.lt && !isnan1

  val mux = Wire(new FPResult)
  mux.exc := minmax_exc
  mux.data := in.bits.in2

  when (isSgnj) { mux.exc := UInt(0) }
  when (isSgnj || isLHS) { mux.data := fsgnj }
  when (in.bits.cmd === FCMD_CVT_FF) {
    when (in.bits.single) {
      mux.data := Cat(SInt(-1, 32), d2s._1)
      mux.exc := d2s._2
    }.otherwise {
      mux.data := s2d._1
      mux.exc := s2d._2
    }
  }

  io.out <> Pipe(in.valid, mux, latency-1)
}

class FPUFMAPipe(val latency: Int, sigWidth: Int, expWidth: Int) extends Module
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

  val fma = Module(new hardfloat.mulAddSubRecodedFloatN(sigWidth, expWidth))
  fma.io.op := in.cmd
  fma.io.roundingMode := in.rm
  fma.io.a := in.in1
  fma.io.b := in.in2
  fma.io.c := in.in3

  val res = Wire(new FPResult)
  res.data := fma.io.out
  res.exc := fma.io.exceptionFlags
  io.out := Pipe(valid, res, latency-1)
}

class FPU extends Module
{
  val io = new FPUIO

  val ex_reg_valid = Reg(next=io.valid, init=Bool(false))
  val ex_reg_inst = RegEnable(io.inst, io.valid)
  val mem_reg_valid = Reg(next=ex_reg_valid && !io.killx, init=Bool(false))
  val mem_reg_inst = RegEnable(ex_reg_inst, ex_reg_valid)
  val killm = io.killm || io.nack_mem
  val wb_reg_valid = Reg(next=mem_reg_valid && !killm, init=Bool(false))

  val fp_decoder = Module(new FPUDecoder)
  fp_decoder.io.inst := io.inst

  val id_ctrl = fp_decoder.io.sigs
  val ex_ctrl = RegEnable(id_ctrl, io.valid)
  val mem_ctrl = RegEnable(ex_ctrl, ex_reg_valid)
  val wb_ctrl = RegEnable(mem_ctrl, mem_reg_valid)

  // load response
  val load_wb = Reg(next=io.dmem_resp_val)
  val load_wb_single = RegEnable(io.dmem_resp_type === MT_W || io.dmem_resp_type === MT_WU, io.dmem_resp_val)
  val load_wb_data = RegEnable(io.dmem_resp_data, io.dmem_resp_val)
  val load_wb_tag = RegEnable(io.dmem_resp_tag, io.dmem_resp_val)
  val rec_s = hardfloat.floatNToRecodedFloatN(load_wb_data, 23, 9)
  val rec_d = hardfloat.floatNToRecodedFloatN(load_wb_data, 52, 12)
  val load_wb_data_recoded = Mux(load_wb_single, Cat(SInt(-1, 32), rec_s), rec_d)

  // regfile
  val regfile = Mem(Bits(width = 65), 32)
  when (load_wb) { regfile(load_wb_tag) := load_wb_data_recoded }

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

  val req = Wire(new FPInput)
  req := ex_ctrl
  req.rm := ex_rm
  req.in1 := ex_rs1
  req.in2 := ex_rs2
  req.in3 := ex_rs3
  req.typ := ex_reg_inst(21,20)

  val sfma = Module(new FPUFMAPipe(params(SFMALatency), 23, 9))
  sfma.io.in.valid := ex_reg_valid && ex_ctrl.fma && ex_ctrl.single
  sfma.io.in.bits := req

  val dfma = Module(new FPUFMAPipe(params(DFMALatency), 52, 12))
  dfma.io.in.valid := ex_reg_valid && ex_ctrl.fma && !ex_ctrl.single
  dfma.io.in.bits := req

  val fpiu = Module(new FPToInt)
  fpiu.io.in.valid := ex_reg_valid && (ex_ctrl.toint || ex_ctrl.div || ex_ctrl.sqrt || ex_ctrl.cmd === FCMD_MINMAX)
  fpiu.io.in.bits := req
  io.store_data := fpiu.io.out.bits.store
  io.toint_data := fpiu.io.out.bits.toint

  val ifpu = Module(new IntToFP(3))
  ifpu.io.in.valid := ex_reg_valid && ex_ctrl.fromint
  ifpu.io.in.bits := req
  ifpu.io.in.bits.in1 := io.fromint_data

  val fpmu = Module(new FPToFP(2))
  fpmu.io.in.valid := ex_reg_valid && ex_ctrl.fastpipe
  fpmu.io.in.bits := req
  fpmu.io.lt := fpiu.io.out.bits.lt

  val divSqrt_wen = Reg(next=Bool(false))
  val divSqrt_inReady = Wire(init=Bool(false))
  val divSqrt_waddr = Reg(Bits())
  val divSqrt_wdata = Wire(Bits())
  val divSqrt_flags = Wire(Bits())
  val divSqrt_in_flight = Reg(init=Bool(false))

  // writeback arbitration
  case class Pipe(p: Module, lat: Int, cond: (FPUCtrlSigs) => Bool, wdata: Bits, wexc: Bits)
  val pipes = List(
    Pipe(fpmu, fpmu.latency, (c: FPUCtrlSigs) => c.fastpipe, fpmu.io.out.bits.data, fpmu.io.out.bits.exc),
    Pipe(ifpu, ifpu.latency, (c: FPUCtrlSigs) => c.fromint, ifpu.io.out.bits.data, ifpu.io.out.bits.exc),
    Pipe(sfma, sfma.latency, (c: FPUCtrlSigs) => c.fma && c.single, Cat(SInt(-1, 32), sfma.io.out.bits.data), sfma.io.out.bits.exc),
    Pipe(dfma, dfma.latency, (c: FPUCtrlSigs) => c.fma && !c.single, dfma.io.out.bits.data, dfma.io.out.bits.exc))
  def latencyMask(c: FPUCtrlSigs, offset: Int) = {
    require(pipes.forall(_.lat >= offset))
    pipes.map(p => Mux(p.cond(c), UInt(1 << p.lat-offset), UInt(0))).reduce(_|_)
  }
  def pipeid(c: FPUCtrlSigs) = pipes.zipWithIndex.map(p => Mux(p._1.cond(c), UInt(p._2), UInt(0))).reduce(_|_)
  val maxLatency = pipes.map(_.lat).max
  val memLatencyMask = latencyMask(mem_ctrl, 2)

  val wen = Reg(init=Bits(0, maxLatency-1))
  val winfo = Reg(Vec.fill(maxLatency-1){Bits()})
  val mem_wen = mem_reg_valid && (mem_ctrl.fma || mem_ctrl.fastpipe || mem_ctrl.fromint)
  val write_port_busy = RegEnable(mem_wen && (memLatencyMask & latencyMask(ex_ctrl, 1)).orR || (wen & latencyMask(ex_ctrl, 0)).orR, ex_reg_valid)
  val mem_winfo = Cat(pipeid(mem_ctrl), mem_reg_inst(11,7))

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
  val wsrc = winfo(0) >> 5
  val wdata = Mux(divSqrt_wen, divSqrt_wdata, Vec(pipes.map(_.wdata))(wsrc))
  val wexc = Vec(pipes.map(_.wexc))(wsrc)
  when (wen(0) || divSqrt_wen) { regfile(waddr) := wdata }

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
  io.sboard_set := wb_reg_valid && Reg(next=useScoreboard(_._1.cond(mem_ctrl)) || mem_ctrl.div || mem_ctrl.sqrt)
  io.sboard_clr := divSqrt_wen || (wen(0) && useScoreboard(x => wsrc === UInt(x._2)))
  io.sboard_clra := waddr
  // we don't currently support round-max-magnitude (rm=4)
  io.illegal_rm := ex_rm(2) && ex_ctrl.round

  divSqrt_wdata := 0
  divSqrt_flags := 0
  if (params(FDivSqrt)) {
    val divSqrt_single = Reg(Bool())
    val divSqrt_rm = Reg(Bits())
    val divSqrt_flags_double = Reg(Bits())
    val divSqrt_wdata_double = Reg(Bits())

    val divSqrt = Module(new hardfloat.divSqrtRecodedFloat64)
    divSqrt_inReady := Mux(divSqrt.io.sqrtOp, divSqrt.io.inReady_sqrt, divSqrt.io.inReady_div)
    val divSqrt_outValid = divSqrt.io.outValid_div || divSqrt.io.outValid_sqrt
    val divSqrt_wb_hazard = wen.orR
    divSqrt.io.inValid := mem_reg_valid && !divSqrt_wb_hazard && !divSqrt_in_flight && !io.killm && (mem_ctrl.div || mem_ctrl.sqrt)
    divSqrt.io.sqrtOp := mem_ctrl.sqrt
    divSqrt.io.a := fpiu.io.as_double.in1
    divSqrt.io.b := fpiu.io.as_double.in2
    divSqrt.io.roundingMode := fpiu.io.as_double.rm

    when (divSqrt.io.inValid && divSqrt_inReady) {
      divSqrt_in_flight := true
      divSqrt_single := mem_ctrl.single
      divSqrt_waddr := mem_reg_inst(11,7)
      divSqrt_rm := divSqrt.io.roundingMode
    }

    when (divSqrt_outValid) {
      divSqrt_wen := true
      divSqrt_wdata_double := divSqrt.io.out
      divSqrt_in_flight := false
      divSqrt_flags_double := divSqrt.io.exceptionFlags
    }

    val divSqrt_toSingle = hardfloat.recodedFloatNToRecodedFloatM(divSqrt_wdata_double, ex_rm, 52, 12, 23, 9)
    divSqrt_wdata := Mux(divSqrt_single, divSqrt_toSingle._1, divSqrt_wdata_double)
    divSqrt_flags := divSqrt_flags_double | Mux(divSqrt_single, divSqrt_toSingle._2, Bits(0))
  }
}
