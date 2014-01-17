package rocket

import Chisel._
import Instructions._
import Util._
import FPConstants._
import uncore.constants.MemoryOpConstants._

object FPConstants
{
  val FCMD_ADD =        Bits("b000000")
  val FCMD_SUB =        Bits("b000001")
  val FCMD_MUL =        Bits("b000010")
  val FCMD_DIV =        Bits("b000011")
  val FCMD_SQRT =       Bits("b000100")
  val FCMD_SGNJ =       Bits("b000101")
  val FCMD_SGNJN =      Bits("b000110")
  val FCMD_SGNJX =      Bits("b000111")
  val FCMD_CVT_L_FMT =  Bits("b001000")
  val FCMD_CVT_LU_FMT = Bits("b001001")
  val FCMD_CVT_W_FMT =  Bits("b001010")
  val FCMD_CVT_WU_FMT = Bits("b001011")
  val FCMD_CVT_FMT_L =  Bits("b001100")
  val FCMD_CVT_FMT_LU = Bits("b001101")
  val FCMD_CVT_FMT_W =  Bits("b001110")
  val FCMD_CVT_FMT_WU = Bits("b001111")
  val FCMD_CVT_FMT_S =  Bits("b010000")
  val FCMD_CVT_FMT_D =  Bits("b010001")
  val FCMD_EQ =         Bits("b010101")
  val FCMD_LT =         Bits("b010110")
  val FCMD_LE =         Bits("b010111")
  val FCMD_MIN =        Bits("b011000")
  val FCMD_MAX =        Bits("b011001")
  val FCMD_MFTX =       Bits("b011100")
  val FCMD_MXTF =       Bits("b011110")
  val FCMD_MADD =       Bits("b100100")
  val FCMD_MSUB =       Bits("b100101")
  val FCMD_NMSUB =      Bits("b100110")
  val FCMD_NMADD =      Bits("b100111")
  val FCMD_LOAD =       Bits("b111000")
  val FCMD_STORE =      Bits("b111001")
  val FCMD_X =          Bits("b??????")
  val FCMD_WIDTH = 6

  val RM_SZ = 3
  val FLAGS_SZ = 5
}

class FPUCtrlSigs extends Bundle
{
  val cmd = Bits(width = FCMD_WIDTH)
  val wen = Bool()
  val ren1 = Bool()
  val ren2 = Bool()
  val ren3 = Bool()
  val single = Bool()
  val fromint = Bool()
  val toint = Bool()
  val fastpipe = Bool()
  val fma = Bool()
  val round = Bool()
}

class FPUDecoder extends Module
{
  val io = new Bundle {
    val inst = Bits(INPUT, 32)
    val sigs = new FPUCtrlSigs().asOutput
  }

  val N = Bool(false)
  val Y = Bool(true)
  val X = Bool(false)
  val decoder = DecodeLogic(io.inst,
    List                  (FCMD_X,         X,X,X,X,X,X,X,X,X,X),
    Array(FLW      -> List(FCMD_LOAD,      Y,N,N,N,Y,N,N,N,N,N),
          FLD      -> List(FCMD_LOAD,      Y,N,N,N,N,N,N,N,N,N),
          FSW      -> List(FCMD_STORE,     N,N,Y,N,Y,N,Y,N,N,N),
          FSD      -> List(FCMD_STORE,     N,N,Y,N,N,N,Y,N,N,N),
          FMV_S_X  -> List(FCMD_MXTF,      Y,N,N,N,Y,Y,N,N,N,Y),
          FMV_D_X  -> List(FCMD_MXTF,      Y,N,N,N,N,Y,N,N,N,Y),
          FCVT_S_W -> List(FCMD_CVT_FMT_W, Y,N,N,N,Y,Y,N,N,N,Y),
          FCVT_S_WU-> List(FCMD_CVT_FMT_WU,Y,N,N,N,Y,Y,N,N,N,Y),
          FCVT_S_L -> List(FCMD_CVT_FMT_L, Y,N,N,N,Y,Y,N,N,N,Y),
          FCVT_S_LU-> List(FCMD_CVT_FMT_LU,Y,N,N,N,Y,Y,N,N,N,Y),
          FCVT_D_W -> List(FCMD_CVT_FMT_W, Y,N,N,N,N,Y,N,N,N,Y),
          FCVT_D_WU-> List(FCMD_CVT_FMT_WU,Y,N,N,N,N,Y,N,N,N,Y),
          FCVT_D_L -> List(FCMD_CVT_FMT_L, Y,N,N,N,N,Y,N,N,N,Y),
          FCVT_D_LU-> List(FCMD_CVT_FMT_LU,Y,N,N,N,N,Y,N,N,N,Y),
          FMV_X_S  -> List(FCMD_MFTX,      N,Y,N,N,Y,N,Y,N,N,Y),
          FMV_X_D  -> List(FCMD_MFTX,      N,Y,N,N,N,N,Y,N,N,Y),
          FCVT_W_S -> List(FCMD_CVT_W_FMT, N,Y,N,N,Y,N,Y,N,N,Y),
          FCVT_WU_S-> List(FCMD_CVT_WU_FMT,N,Y,N,N,Y,N,Y,N,N,Y),
          FCVT_L_S -> List(FCMD_CVT_L_FMT, N,Y,N,N,Y,N,Y,N,N,Y),
          FCVT_LU_S-> List(FCMD_CVT_LU_FMT,N,Y,N,N,Y,N,Y,N,N,Y),
          FCVT_W_D -> List(FCMD_CVT_W_FMT, N,Y,N,N,N,N,Y,N,N,Y),
          FCVT_WU_D-> List(FCMD_CVT_WU_FMT,N,Y,N,N,N,N,Y,N,N,Y),
          FCVT_L_D -> List(FCMD_CVT_L_FMT, N,Y,N,N,N,N,Y,N,N,Y),
          FCVT_LU_D-> List(FCMD_CVT_LU_FMT,N,Y,N,N,N,N,Y,N,N,Y),
          FCVT_S_D -> List(FCMD_CVT_FMT_D, Y,Y,N,N,Y,N,N,Y,N,Y),
          FCVT_D_S -> List(FCMD_CVT_FMT_S, Y,Y,N,N,N,N,N,Y,N,Y),
          FEQ_S    -> List(FCMD_EQ,        N,Y,Y,N,Y,N,Y,N,N,Y),
          FLT_S    -> List(FCMD_LT,        N,Y,Y,N,Y,N,Y,N,N,Y),
          FLE_S    -> List(FCMD_LE,        N,Y,Y,N,Y,N,Y,N,N,Y),
          FEQ_D    -> List(FCMD_EQ,        N,Y,Y,N,N,N,Y,N,N,Y),
          FLT_D    -> List(FCMD_LT,        N,Y,Y,N,N,N,Y,N,N,Y),
          FLE_D    -> List(FCMD_LE,        N,Y,Y,N,N,N,Y,N,N,Y),
          FSGNJ_S  -> List(FCMD_SGNJ,      Y,Y,Y,N,Y,N,N,Y,N,Y),
          FSGNJN_S -> List(FCMD_SGNJN,     Y,Y,Y,N,Y,N,N,Y,N,Y),
          FSGNJX_S -> List(FCMD_SGNJX,     Y,Y,Y,N,Y,N,N,Y,N,Y),
          FSGNJ_D  -> List(FCMD_SGNJ,      Y,Y,Y,N,N,N,N,Y,N,Y),
          FSGNJN_D -> List(FCMD_SGNJN,     Y,Y,Y,N,N,N,N,Y,N,Y),
          FSGNJX_D -> List(FCMD_SGNJX,     Y,Y,Y,N,N,N,N,Y,N,Y),
          FMIN_S   -> List(FCMD_MIN,       Y,Y,Y,N,Y,N,Y,Y,N,Y),
          FMAX_S   -> List(FCMD_MAX,       Y,Y,Y,N,Y,N,Y,Y,N,Y),
          FMIN_D   -> List(FCMD_MIN,       Y,Y,Y,N,N,N,Y,Y,N,Y),
          FMAX_D   -> List(FCMD_MAX,       Y,Y,Y,N,N,N,Y,Y,N,Y),
          FADD_S   -> List(FCMD_ADD,       Y,Y,Y,N,Y,N,N,N,Y,Y),
          FSUB_S   -> List(FCMD_SUB,       Y,Y,Y,N,Y,N,N,N,Y,Y),
          FMUL_S   -> List(FCMD_MUL,       Y,Y,Y,N,Y,N,N,N,Y,Y),
          FADD_D   -> List(FCMD_ADD,       Y,Y,Y,N,N,N,N,N,Y,Y),
          FSUB_D   -> List(FCMD_SUB,       Y,Y,Y,N,N,N,N,N,Y,Y),
          FMUL_D   -> List(FCMD_MUL,       Y,Y,Y,N,N,N,N,N,Y,Y),
          FMADD_S  -> List(FCMD_MADD,      Y,Y,Y,Y,Y,N,N,N,Y,Y),
          FMSUB_S  -> List(FCMD_MSUB,      Y,Y,Y,Y,Y,N,N,N,Y,Y),
          FNMADD_S -> List(FCMD_NMADD,     Y,Y,Y,Y,Y,N,N,N,Y,Y),
          FNMSUB_S -> List(FCMD_NMSUB,     Y,Y,Y,Y,Y,N,N,N,Y,Y),
          FMADD_D  -> List(FCMD_MADD,      Y,Y,Y,Y,N,N,N,N,Y,Y),
          FMSUB_D  -> List(FCMD_MSUB,      Y,Y,Y,Y,N,N,N,N,Y,Y),
          FNMADD_D -> List(FCMD_NMADD,     Y,Y,Y,Y,N,N,N,N,Y,Y),
          FNMSUB_D -> List(FCMD_NMSUB,     Y,Y,Y,Y,N,N,N,N,Y,Y)
          ))
  val cmd :: wen :: ren1 :: ren2 :: ren3 :: single :: fromint :: toint :: fastpipe :: fma :: round :: Nil = decoder

  io.sigs.cmd := cmd
  io.sigs.wen := wen.toBool
  io.sigs.ren1 := ren1.toBool
  io.sigs.ren2 := ren2.toBool
  io.sigs.ren3 := ren3.toBool
  io.sigs.single := single.toBool
  io.sigs.fromint := fromint.toBool
  io.sigs.toint := toint.toBool
  io.sigs.fastpipe := fastpipe.toBool
  io.sigs.fma := fma.toBool
  io.sigs.round := round.toBool
}

class DpathFPUIO extends Bundle {
  val inst = Bits(OUTPUT, 32)
  val fromint_data = Bits(OUTPUT, 64)

  val fcsr_rm = Bits(OUTPUT, FPConstants.RM_SZ)
  val fcsr_flags = Valid(Bits(width = FPConstants.FLAGS_SZ)).flip

  val store_data = Bits(INPUT, 64)
  val toint_data = Bits(INPUT, 64)

  val dmem_resp_val = Bool(OUTPUT)
  val dmem_resp_type = Bits(OUTPUT, 3)
  val dmem_resp_tag = UInt(OUTPUT, 5)
  val dmem_resp_data = Bits(OUTPUT, 64)
}

class CtrlFPUIO extends Bundle {
  val valid = Bool(OUTPUT)
  val fcsr_rdy = Bool(INPUT)
  val nack_mem = Bool(INPUT)
  val illegal_rm = Bool(INPUT)
  val killx = Bool(OUTPUT)
  val killm = Bool(OUTPUT)
  val dec = new FPUCtrlSigs().asInput
  val sboard_set = Bool(INPUT)
  val sboard_clr = Bool(INPUT)
  val sboard_clra = UInt(INPUT, 5)
}

class FPToInt extends Module
{
  class Input extends Bundle {
    val single = Bool()
    val cmd = Bits(width = FCMD_WIDTH)
    val rm = Bits(width = 3)
    val in1 = Bits(width = 65)
    val in2 = Bits(width = 65)
    override def clone = new Input().asInstanceOf[this.type]
  }
  val io = new Bundle {
    val in = Valid(new Input).flip
    val out = Valid(new Bundle {
      val lt = Bool()
      val store = Bits(width = 64)
      val toint = Bits(width = 64)
      val exc = Bits(width = 5)
    })
  }

  val in = Reg(new Input)
  val valid = Reg(next=io.in.valid)
  when (io.in.valid) {
    def upconvert(x: UInt) = hardfloat.recodedFloatNToRecodedFloatM(x, Bits(0), 23, 9, 52, 12)._1
    when (io.in.bits.cmd === FCMD_STORE) {
      in.in1 := io.in.bits.in2
    }.otherwise {
      val doUpconvert = io.in.bits.single && io.in.bits.cmd != FCMD_MFTX
      in.in1 := Mux(doUpconvert, upconvert(io.in.bits.in1), io.in.bits.in1)
      in.in2 := Mux(doUpconvert, upconvert(io.in.bits.in2), io.in.bits.in2)
    }
    in.single := io.in.bits.single
    in.cmd := io.in.bits.cmd
    in.rm := io.in.bits.rm
  }

  val unrec_s = hardfloat.recodedFloatNToFloatN(in.in1, 23, 9)
  val unrec_d = hardfloat.recodedFloatNToFloatN(in.in1, 52, 12)

  val dcmp = Module(new hardfloat.recodedFloatNCompare(52, 12))
  dcmp.io.a := in.in1
  dcmp.io.b := in.in2
  val dcmp_out = (in.cmd & Cat(dcmp.io.a_lt_b, dcmp.io.a_eq_b)).orR
  val dcmp_exc = (in.cmd & Cat(dcmp.io.a_lt_b_invalid, dcmp.io.a_eq_b_invalid)).orR << UInt(4)

  val d2i = hardfloat.recodedFloatNToAny(in.in1, in.rm, ~in.cmd(1,0), 52, 12, 64)

  io.out.bits.toint := Mux(in.single, Cat(Fill(32, unrec_s(31)), unrec_s), unrec_d)
  io.out.bits.exc := Bits(0)

  when (in.cmd === FCMD_CVT_W_FMT || in.cmd === FCMD_CVT_WU_FMT) {
    io.out.bits.toint := Cat(Fill(32, d2i._1(31)), d2i._1(31,0))
    io.out.bits.exc := d2i._2
  }
  when (in.cmd === FCMD_CVT_L_FMT || in.cmd === FCMD_CVT_LU_FMT) {
    io.out.bits.toint := d2i._1
    io.out.bits.exc := d2i._2
  }
  when (in.cmd === FCMD_EQ || in.cmd === FCMD_LT || in.cmd === FCMD_LE) {
    io.out.bits.toint := dcmp_out
    io.out.bits.exc := dcmp_exc
  }

  io.out.valid := valid
  io.out.bits.store := Mux(in.single, Cat(unrec_d(63,32), unrec_s), unrec_d)
  io.out.bits.lt := dcmp.io.a_lt_b
}

class FPResult extends Bundle
{
  val data = Bits(width = 65)
  val exc = Bits(width = 5)
}

class IntToFP(val latency: Int) extends Module
{
  class Input extends Bundle {
    val single = Bool()
    val cmd = Bits(width = FCMD_WIDTH)
    val rm = Bits(width = 3)
    val data = Bits(width = 64)
    override def clone = new Input().asInstanceOf[this.type]
  }
  val io = new Bundle {
    val in = Valid(new Input).flip
    val out = Valid(new FPResult)
  }

  val in = Pipe(io.in)

  val mux = new FPResult
  mux.exc := Bits(0)
  mux.data := hardfloat.floatNToRecodedFloatN(in.bits.data, 52, 12)
  when (in.bits.single) {
    mux.data := hardfloat.floatNToRecodedFloatN(in.bits.data, 23, 9)
  }

  when (in.bits.cmd === FCMD_CVT_FMT_W || in.bits.cmd === FCMD_CVT_FMT_WU ||
        in.bits.cmd === FCMD_CVT_FMT_L || in.bits.cmd === FCMD_CVT_FMT_LU) {
    when (in.bits.single) {
      val u = hardfloat.anyToRecodedFloatN(in.bits.data, in.bits.rm, ~in.bits.cmd(1,0), 23, 9, 64)
      mux.data := Cat(SInt(-1, 32), u._1)
      mux.exc := u._2
    }.otherwise {
      val u = hardfloat.anyToRecodedFloatN(in.bits.data, in.bits.rm, ~in.bits.cmd(1,0), 52, 12, 64)
      mux.data := u._1
      mux.exc := u._2
    }
  }

  io.out <> Pipe(in.valid, mux, latency-1)
}

class FPToFP(val latency: Int) extends Module
{
  class Input extends Bundle {
    val single = Bool()
    val cmd = Bits(width = FCMD_WIDTH)
    val rm = Bits(width = 3)
    val in1 = Bits(width = 65)
    val in2 = Bits(width = 65)
    override def clone = new Input().asInstanceOf[this.type]
  }
  val io = new Bundle {
    val in = Valid(new Input).flip
    val out = Valid(new FPResult)
    val lt = Bool(INPUT) // from FPToInt
  }

  val in = Pipe(io.in)

  // fp->fp units
  val sign_s = Mux(in.bits.cmd === FCMD_SGNJ, in.bits.in2(32),
               Mux(in.bits.cmd === FCMD_SGNJN, ~in.bits.in2(32),
                   in.bits.in1(32) ^ in.bits.in2(32))) // FCMD_SGNJX
  val sign_d = Mux(in.bits.cmd === FCMD_SGNJ, in.bits.in2(64),
               Mux(in.bits.cmd === FCMD_SGNJN, ~in.bits.in2(64),
                   in.bits.in1(64) ^ in.bits.in2(64))) // FCMD_SGNJX
  val fsgnj = Cat(Mux(in.bits.single, in.bits.in1(64), sign_d), in.bits.in1(63,33),
                  Mux(in.bits.single, sign_s, in.bits.in1(32)), in.bits.in1(31,0))

  val s2d = hardfloat.recodedFloatNToRecodedFloatM(in.bits.in1, in.bits.rm, 23, 9, 52, 12)
  val d2s = hardfloat.recodedFloatNToRecodedFloatM(in.bits.in1, in.bits.rm, 52, 12, 23, 9)

  val isnan1 = Mux(in.bits.single, in.bits.in1(31,29) === Bits("b111"), in.bits.in1(63,61) === Bits("b111"))
  val isnan2 = Mux(in.bits.single, in.bits.in2(31,29) === Bits("b111"), in.bits.in2(63,61) === Bits("b111"))
  val issnan1 = isnan1 && ~Mux(in.bits.single, in.bits.in1(22), in.bits.in1(51))
  val issnan2 = isnan2 && ~Mux(in.bits.single, in.bits.in2(22), in.bits.in2(51))
  val minmax_exc = Cat(issnan1 || issnan2, Bits(0,4))
  val min = in.bits.cmd === FCMD_MIN
  val minmax = Mux(isnan2 || !isnan1 && (min === io.lt), in.bits.in1, in.bits.in2)

  val mux = new FPResult
  mux.data := fsgnj
  mux.exc := Bits(0)

  when (in.bits.cmd === FCMD_MIN || in.bits.cmd === FCMD_MAX) {
    mux.data := minmax
  }
  when (in.bits.cmd === FCMD_CVT_FMT_S || in.bits.cmd === FCMD_CVT_FMT_D) {
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

class ioFMA(width: Int) extends Bundle {
  val valid = Bool(INPUT)
  val cmd = Bits(INPUT, FCMD_WIDTH)
  val rm = Bits(INPUT, 3)
  val in1 = Bits(INPUT, width)
  val in2 = Bits(INPUT, width)
  val in3 = Bits(INPUT, width)
  val out = Bits(OUTPUT, width)
  val exc = Bits(OUTPUT, 5)
}

class FPUSFMAPipe(val latency: Int) extends Module
{
  val io = new ioFMA(33)
  
  val cmd = Reg(Bits())
  val rm = Reg(Bits())
  val in1 = Reg(Bits())
  val in2 = Reg(Bits())
  val in3 = Reg(Bits())

  val cmd_fma = io.cmd === FCMD_MADD  || io.cmd === FCMD_MSUB ||
                io.cmd === FCMD_NMADD || io.cmd === FCMD_NMSUB
  val cmd_addsub = io.cmd === FCMD_ADD || io.cmd === FCMD_SUB

  val one = Bits("h80000000")
  val zero = Cat(io.in1(32) ^ io.in2(32), Bits(0, 32))

  val valid = Reg(next=io.valid)
  when (io.valid) {
    cmd := Cat(io.cmd(1) & (cmd_fma || cmd_addsub), io.cmd(0))
    rm := io.rm
    in1 := io.in1
    in2 := Mux(cmd_addsub, one, io.in2)
    in3 := Mux(cmd_fma, io.in3, Mux(cmd_addsub, io.in2, zero))
  }

  val fma = Module(new hardfloat.mulAddSubRecodedFloatN(23, 9))
  fma.io.op := cmd
  fma.io.roundingMode := rm
  fma.io.a := in1
  fma.io.b := in2
  fma.io.c := in3

  io.out := Pipe(valid, fma.io.out, latency-1).bits
  io.exc := Pipe(valid, fma.io.exceptionFlags, latency-1).bits
}

class FPUDFMAPipe(val latency: Int) extends Module
{
  val io = new ioFMA(65)
  
  val cmd = Reg(Bits())
  val rm = Reg(Bits())
  val in1 = Reg(Bits())
  val in2 = Reg(Bits())
  val in3 = Reg(Bits())

  val cmd_fma = io.cmd === FCMD_MADD  || io.cmd === FCMD_MSUB ||
                io.cmd === FCMD_NMADD || io.cmd === FCMD_NMSUB
  val cmd_addsub = io.cmd === FCMD_ADD || io.cmd === FCMD_SUB

  val one = Bits("h8000000000000000")
  val zero = Cat(io.in1(64) ^ io.in2(64), Bits(0, 64))

  val valid = Reg(next=io.valid)
  when (io.valid) {
    cmd := Cat(io.cmd(1) & (cmd_fma || cmd_addsub), io.cmd(0))
    rm := io.rm
    in1 := io.in1
    in2 := Mux(cmd_addsub, one, io.in2)
    in3 := Mux(cmd_fma, io.in3, Mux(cmd_addsub, io.in2, zero))
  }

  val fma = Module(new hardfloat.mulAddSubRecodedFloatN(52, 12))
  fma.io.op := cmd
  fma.io.roundingMode := rm
  fma.io.a := in1
  fma.io.b := in2
  fma.io.c := in3

  io.out := Pipe(valid, fma.io.out, latency-1).bits
  io.exc := Pipe(valid, fma.io.exceptionFlags, latency-1).bits
}

class FPU(sfma_latency: Int, dfma_latency: Int) extends Module
{
  val io = new Bundle {
    val ctrl = (new CtrlFPUIO).flip
    val dpath = (new DpathFPUIO).flip
    val sfma = new ioFMA(33)
    val dfma = new ioFMA(65)
  }

  val ex_reg_inst = Reg(Bits())
  when (io.ctrl.valid) {
    ex_reg_inst := io.dpath.inst
  }
  val ex_reg_valid = Reg(next=io.ctrl.valid, init=Bool(false))
  val mem_reg_valid = Reg(next=ex_reg_valid && !io.ctrl.killx, init=Bool(false))
  val killm = io.ctrl.killm || io.ctrl.nack_mem
  val wb_reg_valid = Reg(next=mem_reg_valid && !killm, init=Bool(false))

  val fp_decoder = Module(new FPUDecoder)
  fp_decoder.io.inst := io.dpath.inst

  val ctrl = RegEnable(fp_decoder.io.sigs, io.ctrl.valid)
  val mem_ctrl = RegEnable(ctrl, ex_reg_valid)
  val wb_ctrl = RegEnable(mem_ctrl, mem_reg_valid)

  // load response
  val load_wb = Reg(next=io.dpath.dmem_resp_val)
  val load_wb_single = RegEnable(io.dpath.dmem_resp_type === MT_W || io.dpath.dmem_resp_type === MT_WU, io.dpath.dmem_resp_val)
  val load_wb_data = RegEnable(io.dpath.dmem_resp_data, io.dpath.dmem_resp_val)
  val load_wb_tag = RegEnable(io.dpath.dmem_resp_tag, io.dpath.dmem_resp_val)
  val rec_s = hardfloat.floatNToRecodedFloatN(load_wb_data, 23, 9)
  val rec_d = hardfloat.floatNToRecodedFloatN(load_wb_data, 52, 12)
  val load_wb_data_recoded = Mux(load_wb_single, Cat(SInt(-1), rec_s), rec_d)

  // regfile
  val regfile = Mem(Bits(width = 65), 32)
  when (load_wb) { regfile(load_wb_tag) := load_wb_data_recoded }

  val ex_rs1 = regfile(ex_reg_inst(19,15))
  val ex_rs2 = regfile(ex_reg_inst(24,20))
  val ex_rs3 = regfile(ex_reg_inst(31,27))
  val ex_rm = Mux(ex_reg_inst(14,12) === Bits(7), io.dpath.fcsr_rm, ex_reg_inst(14,12))

  val fpiu = Module(new FPToInt)
  fpiu.io.in.valid := ex_reg_valid && ctrl.toint
  fpiu.io.in.bits := ctrl
  fpiu.io.in.bits.rm := ex_rm
  fpiu.io.in.bits.in1 := ex_rs1
  fpiu.io.in.bits.in2 := ex_rs2

  io.dpath.store_data := fpiu.io.out.bits.store
  io.dpath.toint_data := fpiu.io.out.bits.toint

  val ifpu = Module(new IntToFP(3))
  ifpu.io.in.valid := ex_reg_valid && ctrl.fromint
  ifpu.io.in.bits := ctrl
  ifpu.io.in.bits.rm := ex_rm
  ifpu.io.in.bits.data := io.dpath.fromint_data
  val fpmu = Module(new FPToFP(2))
  fpmu.io.in.valid := ex_reg_valid && ctrl.fastpipe
  fpmu.io.in.bits := ctrl
  fpmu.io.in.bits.rm := ex_rm
  fpmu.io.in.bits.in1 := ex_rs1
  fpmu.io.in.bits.in2 := ex_rs2
  fpmu.io.lt := fpiu.io.out.bits.lt

  val cmd_fma = mem_ctrl.cmd === FCMD_MADD  || mem_ctrl.cmd === FCMD_MSUB ||
                mem_ctrl.cmd === FCMD_NMADD || mem_ctrl.cmd === FCMD_NMSUB
  val cmd_addsub = mem_ctrl.cmd === FCMD_ADD || mem_ctrl.cmd === FCMD_SUB
  val sfma = Module(new FPUSFMAPipe(sfma_latency))
  sfma.io.valid := io.sfma.valid || ex_reg_valid && ctrl.fma && ctrl.single
  sfma.io.in1 := Mux(io.sfma.valid, io.sfma.in1, ex_rs1)
  sfma.io.in2 := Mux(io.sfma.valid, io.sfma.in2, ex_rs2)
  sfma.io.in3 := Mux(io.sfma.valid, io.sfma.in3, ex_rs3)
  sfma.io.cmd := Mux(io.sfma.valid, io.sfma.cmd, ctrl.cmd)
  sfma.io.rm := Mux(io.sfma.valid, io.sfma.rm, ex_rm)
  io.sfma.out := sfma.io.out
  io.sfma.exc := sfma.io.exc

  val dfma = Module(new FPUDFMAPipe(dfma_latency))
  dfma.io.valid := io.dfma.valid || ex_reg_valid && ctrl.fma && !ctrl.single
  dfma.io.in1 := Mux(io.dfma.valid, io.dfma.in1, ex_rs1)
  dfma.io.in2 := Mux(io.dfma.valid, io.dfma.in2, ex_rs2)
  dfma.io.in3 := Mux(io.dfma.valid, io.dfma.in3, ex_rs3)
  dfma.io.cmd := Mux(io.dfma.valid, io.dfma.cmd, ctrl.cmd)
  dfma.io.rm := Mux(io.dfma.valid, io.dfma.rm, ex_rm)
  io.dfma.out := dfma.io.out
  io.dfma.exc := dfma.io.exc

  // writeback arbitration
  case class Pipe(p: Module, lat: Int, cond: (FPUCtrlSigs) => Bool, wdata: Bits, wexc: Bits)
  val pipes = List(
    Pipe(fpmu, fpmu.latency, (c: FPUCtrlSigs) => c.fastpipe, fpmu.io.out.bits.data, fpmu.io.out.bits.exc),
    Pipe(ifpu, ifpu.latency, (c: FPUCtrlSigs) => c.fromint, ifpu.io.out.bits.data, ifpu.io.out.bits.exc),
    Pipe(sfma, sfma.latency, (c: FPUCtrlSigs) => c.fma && c.single, sfma.io.out, sfma.io.exc),
    Pipe(dfma, dfma.latency, (c: FPUCtrlSigs) => c.fma && !c.single, dfma.io.out, dfma.io.exc))
  def latencyMask(c: FPUCtrlSigs, offset: Int) = {
    require(pipes.forall(_.lat >= offset))
    pipes.map(p => Mux(p.cond(c), UInt(1 << p.lat-offset), UInt(0))).reduce(_|_)
  }
  def pipeid(c: FPUCtrlSigs) = pipes.zipWithIndex.map(p => Mux(p._1.cond(c), UInt(p._2), UInt(0))).reduce(_|_)
  val maxLatency = pipes.map(_.lat).max
  val memLatencyMask = latencyMask(mem_ctrl, 2)

  val wen = Reg(init=Bits(0, maxLatency-1))
  val winfo = Vec.fill(maxLatency-1){Reg(Bits())}
  val mem_wen = mem_reg_valid && (mem_ctrl.fma || mem_ctrl.fastpipe || mem_ctrl.fromint)
  val (write_port_busy, mem_winfo) = (Reg(Bool()), Reg(Bits()))
  when (ex_reg_valid) {
    write_port_busy := mem_wen && (memLatencyMask & latencyMask(ctrl, 1)).orR || (wen & latencyMask(ctrl, 0)).orR
    mem_winfo := Cat(pipeid(ctrl), ex_reg_inst(11,7))
  }

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

  val waddr = winfo(0)(4,0).toUInt
  val wsrc = winfo(0) >> waddr.getWidth
  val wdata = Vec(pipes.map(_.wdata))(wsrc)
  val wexc = Vec(pipes.map(_.wexc))(wsrc)
  when (wen(0)) { regfile(waddr(4,0)) := wdata }

  val wb_toint_valid = wb_reg_valid && wb_ctrl.toint
  val wb_toint_exc = RegEnable(fpiu.io.out.bits.exc, mem_ctrl.toint)
  io.dpath.fcsr_flags.valid := wb_toint_valid || wen(0)
  io.dpath.fcsr_flags.bits :=
    Mux(wb_toint_valid, wb_toint_exc, UInt(0)) |
    Mux(wen(0), wexc, UInt(0))

  val fp_inflight = wb_reg_valid && wb_ctrl.toint || wen.orR
  val units_busy = mem_reg_valid && mem_ctrl.fma && Reg(next=Mux(ctrl.single, io.sfma.valid, io.dfma.valid))
  io.ctrl.fcsr_rdy := !fp_inflight
  io.ctrl.nack_mem := units_busy || write_port_busy
  io.ctrl.dec <> fp_decoder.io.sigs
  def useScoreboard(f: ((Pipe, Int)) => Bool) = pipes.zipWithIndex.filter(_._1.lat > 3).map(x => f(x)).fold(Bool(false))(_||_)
  io.ctrl.sboard_set := wb_reg_valid && Reg(next=useScoreboard(_._1.cond(mem_ctrl)))
  io.ctrl.sboard_clr := wen(0) && useScoreboard(x => wsrc === UInt(x._2))
  io.ctrl.sboard_clra := waddr
  // we don't currently support round-max-magnitude (rm=4)
  io.ctrl.illegal_rm := ex_rm(2) && ctrl.round
}
