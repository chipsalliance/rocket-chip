package rocket

import Chisel._
import Node._
import Constants._
import Instructions._
import Util._
import FPConstants._

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
  val FCMD_MFFSR =      Bits("b011101")
  val FCMD_MXTF =       Bits("b011110")
  val FCMD_MTFSR =      Bits("b011111")
  val FCMD_MADD =       Bits("b100100")
  val FCMD_MSUB =       Bits("b100101")
  val FCMD_NMSUB =      Bits("b100110")
  val FCMD_NMADD =      Bits("b100111")
  val FCMD_LOAD =       Bits("b111000")
  val FCMD_STORE =      Bits("b111001")
  val FCMD_X =          Bits("b??????")
  val FCMD_WIDTH = 6
  val FSR_WIDTH = 8
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
  val rdfsr = Bool()
  val wrfsr = Bool()
}

class FPUDecoder extends Component
{
  val io = new Bundle {
    val inst = Bits(INPUT, 32)
    val sigs = new FPUCtrlSigs().asOutput
  }

  val N = Bool(false)
  val Y = Bool(true)
  val X = Bool(false)
  val decoder = DecodeLogic(io.inst,
    List                  (FCMD_X,         X,X,X,X,X,X,X,X,X,X,X,X),
    Array(FLW      -> List(FCMD_LOAD,      Y,N,N,N,Y,N,N,N,N,N,N,N),
          FLD      -> List(FCMD_LOAD,      Y,N,N,N,N,N,N,N,N,N,N,N),
          FSW      -> List(FCMD_STORE,     N,N,Y,N,Y,N,Y,N,N,N,N,N),
          FSD      -> List(FCMD_STORE,     N,N,Y,N,N,N,Y,N,N,N,N,N),
          MXTF_S   -> List(FCMD_MXTF,      Y,N,N,N,Y,Y,N,N,N,Y,N,N),
          MXTF_D   -> List(FCMD_MXTF,      Y,N,N,N,N,Y,N,N,N,Y,N,N),
          FCVT_S_W -> List(FCMD_CVT_FMT_W, Y,N,N,N,Y,Y,N,N,N,Y,N,N),
          FCVT_S_WU-> List(FCMD_CVT_FMT_WU,Y,N,N,N,Y,Y,N,N,N,Y,N,N),
          FCVT_S_L -> List(FCMD_CVT_FMT_L, Y,N,N,N,Y,Y,N,N,N,Y,N,N),
          FCVT_S_LU-> List(FCMD_CVT_FMT_LU,Y,N,N,N,Y,Y,N,N,N,Y,N,N),
          FCVT_D_W -> List(FCMD_CVT_FMT_W, Y,N,N,N,N,Y,N,N,N,Y,N,N),
          FCVT_D_WU-> List(FCMD_CVT_FMT_WU,Y,N,N,N,N,Y,N,N,N,Y,N,N),
          FCVT_D_L -> List(FCMD_CVT_FMT_L, Y,N,N,N,N,Y,N,N,N,Y,N,N),
          FCVT_D_LU-> List(FCMD_CVT_FMT_LU,Y,N,N,N,N,Y,N,N,N,Y,N,N),
          MFTX_S   -> List(FCMD_MFTX,      N,Y,N,N,Y,N,Y,N,N,Y,N,N),
          MFTX_D   -> List(FCMD_MFTX,      N,Y,N,N,N,N,Y,N,N,Y,N,N),
          FCVT_W_S -> List(FCMD_CVT_W_FMT, N,Y,N,N,Y,N,Y,N,N,Y,N,N),
          FCVT_WU_S-> List(FCMD_CVT_WU_FMT,N,Y,N,N,Y,N,Y,N,N,Y,N,N),
          FCVT_L_S -> List(FCMD_CVT_L_FMT, N,Y,N,N,Y,N,Y,N,N,Y,N,N),
          FCVT_LU_S-> List(FCMD_CVT_LU_FMT,N,Y,N,N,Y,N,Y,N,N,Y,N,N),
          FCVT_W_D -> List(FCMD_CVT_W_FMT, N,Y,N,N,N,N,Y,N,N,Y,N,N),
          FCVT_WU_D-> List(FCMD_CVT_WU_FMT,N,Y,N,N,N,N,Y,N,N,Y,N,N),
          FCVT_L_D -> List(FCMD_CVT_L_FMT, N,Y,N,N,N,N,Y,N,N,Y,N,N),
          FCVT_LU_D-> List(FCMD_CVT_LU_FMT,N,Y,N,N,N,N,Y,N,N,Y,N,N),
          FCVT_S_D -> List(FCMD_CVT_FMT_D, Y,Y,N,N,Y,N,N,Y,N,Y,N,N),
          FCVT_D_S -> List(FCMD_CVT_FMT_S, Y,Y,N,N,N,N,N,Y,N,Y,N,N),
          FEQ_S    -> List(FCMD_EQ,        N,Y,Y,N,Y,N,Y,N,N,Y,N,N),
          FLT_S    -> List(FCMD_LT,        N,Y,Y,N,Y,N,Y,N,N,Y,N,N),
          FLE_S    -> List(FCMD_LE,        N,Y,Y,N,Y,N,Y,N,N,Y,N,N),
          FEQ_D    -> List(FCMD_EQ,        N,Y,Y,N,N,N,Y,N,N,Y,N,N),
          FLT_D    -> List(FCMD_LT,        N,Y,Y,N,N,N,Y,N,N,Y,N,N),
          FLE_D    -> List(FCMD_LE,        N,Y,Y,N,N,N,Y,N,N,Y,N,N),
          MTFSR    -> List(FCMD_MTFSR,     N,N,N,N,Y,N,Y,N,N,Y,Y,Y),
          MFFSR    -> List(FCMD_MFFSR,     N,N,N,N,Y,N,Y,N,N,Y,Y,N),
          FSGNJ_S  -> List(FCMD_SGNJ,      Y,Y,Y,N,Y,N,N,Y,N,Y,N,N),
          FSGNJN_S -> List(FCMD_SGNJN,     Y,Y,Y,N,Y,N,N,Y,N,Y,N,N),
          FSGNJX_S -> List(FCMD_SGNJX,     Y,Y,Y,N,Y,N,N,Y,N,Y,N,N),
          FSGNJ_D  -> List(FCMD_SGNJ,      Y,Y,Y,N,N,N,N,Y,N,Y,N,N),
          FSGNJN_D -> List(FCMD_SGNJN,     Y,Y,Y,N,N,N,N,Y,N,Y,N,N),
          FSGNJX_D -> List(FCMD_SGNJX,     Y,Y,Y,N,N,N,N,Y,N,Y,N,N),
          FMIN_S   -> List(FCMD_MIN,       Y,Y,Y,N,Y,N,Y,Y,N,Y,N,N),
          FMAX_S   -> List(FCMD_MAX,       Y,Y,Y,N,Y,N,Y,Y,N,Y,N,N),
          FMIN_D   -> List(FCMD_MIN,       Y,Y,Y,N,N,N,Y,Y,N,Y,N,N),
          FMAX_D   -> List(FCMD_MAX,       Y,Y,Y,N,N,N,Y,Y,N,Y,N,N),
          FADD_S   -> List(FCMD_ADD,       Y,Y,Y,N,Y,N,N,N,Y,Y,N,N),
          FSUB_S   -> List(FCMD_SUB,       Y,Y,Y,N,Y,N,N,N,Y,Y,N,N),
          FMUL_S   -> List(FCMD_MUL,       Y,Y,Y,N,Y,N,N,N,Y,Y,N,N),
          FADD_D   -> List(FCMD_ADD,       Y,Y,Y,N,N,N,N,N,Y,Y,N,N),
          FSUB_D   -> List(FCMD_SUB,       Y,Y,Y,N,N,N,N,N,Y,Y,N,N),
          FMUL_D   -> List(FCMD_MUL,       Y,Y,Y,N,N,N,N,N,Y,Y,N,N),
          FMADD_S  -> List(FCMD_MADD,      Y,Y,Y,Y,Y,N,N,N,Y,Y,N,N),
          FMSUB_S  -> List(FCMD_MSUB,      Y,Y,Y,Y,Y,N,N,N,Y,Y,N,N),
          FNMADD_S -> List(FCMD_NMADD,     Y,Y,Y,Y,Y,N,N,N,Y,Y,N,N),
          FNMSUB_S -> List(FCMD_NMSUB,     Y,Y,Y,Y,Y,N,N,N,Y,Y,N,N),
          FMADD_D  -> List(FCMD_MADD,      Y,Y,Y,Y,N,N,N,N,Y,Y,N,N),
          FMSUB_D  -> List(FCMD_MSUB,      Y,Y,Y,Y,N,N,N,N,Y,Y,N,N),
          FNMADD_D -> List(FCMD_NMADD,     Y,Y,Y,Y,N,N,N,N,Y,Y,N,N),
          FNMSUB_D -> List(FCMD_NMSUB,     Y,Y,Y,Y,N,N,N,N,Y,Y,N,N)
          ))
  val cmd :: wen :: ren1 :: ren2 :: ren3 :: single :: fromint :: toint :: fastpipe :: fma :: round :: rdfsr :: wrfsr :: Nil = decoder

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
  io.sigs.rdfsr := rdfsr.toBool
  io.sigs.wrfsr := wrfsr.toBool
}

class DpathFPUIO extends Bundle {
  val inst = Bits(OUTPUT, 32)
  val fromint_data = Bits(OUTPUT, 64)

  val store_data = Bits(INPUT, 64)
  val toint_data = Bits(INPUT, 64)

  val dmem_resp_val = Bool(OUTPUT)
  val dmem_resp_type = Bits(OUTPUT, 3)
  val dmem_resp_tag = UFix(OUTPUT, 5)
  val dmem_resp_data = Bits(OUTPUT, 64)
}

class CtrlFPUIO extends Bundle {
  val valid = Bool(OUTPUT)
  val nack_mem = Bool(INPUT)
  val illegal_rm = Bool(INPUT)
  val killx = Bool(OUTPUT)
  val killm = Bool(OUTPUT)
  val dec = new FPUCtrlSigs().asInput
  val sboard_set = Bool(INPUT)
  val sboard_clr = Bool(INPUT)
  val sboard_clra = UFix(INPUT, 5)
}

object RegEn
{
  def apply[T <: Data](data: T, en: Bool) = {
    val r = Reg() { data.clone }
    when (en) { r := data }
    r
  }
  def apply[T <: Bits](data: T, en: Bool, resetVal: T) = {
    val r = Reg(resetVal = resetVal) { data.clone }
    when (en) { r := data }
    r
  }
}

class FPToInt extends Component
{
  class Input extends Bundle {
    val single = Bool()
    val cmd = Bits(width = FCMD_WIDTH)
    val rm = Bits(width = 3)
    val fsr = Bits(width = FSR_WIDTH)
    val in1 = Bits(width = 65)
    val in2 = Bits(width = 65)
    override def clone = new Input().asInstanceOf[this.type]
  }
  val io = new Bundle {
    val in = new PipeIO()(new Input).flip
    val out = new PipeIO()(new Bundle {
      val lt = Bool()
      val store = Bits(width = 64)
      val toint = Bits(width = 64)
      val exc = Bits(width = 5)
    })
  }

  val in = Reg() { new Input }
  val valid = Reg(io.in.valid)
  when (io.in.valid) {
    def upconvert(x: Bits) = hardfloat.recodedFloatNToRecodedFloatM(x, Bits(0), 23, 9, 52, 12)._1
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
    in.fsr := io.in.bits.fsr
  }

  val unrec_s = hardfloat.recodedFloatNToFloatN(in.in1, 23, 9)
  val unrec_d = hardfloat.recodedFloatNToFloatN(in.in1, 52, 12)

  val dcmp = new hardfloat.recodedFloatNCompare(52, 12)
  dcmp.io.a := in.in1
  dcmp.io.b := in.in2
  val dcmp_out = (in.cmd & Cat(dcmp.io.a_lt_b, dcmp.io.a_eq_b)).orR
  val dcmp_exc = (in.cmd & Cat(dcmp.io.a_lt_b_invalid, dcmp.io.a_eq_b_invalid)).orR << UFix(4)

  val d2i = hardfloat.recodedFloatNToAny(in.in1, in.rm, ~in.cmd(1,0), 52, 12, 64)

  io.out.bits.toint := Mux(in.single, Cat(Fill(32, unrec_s(31)), unrec_s), unrec_d)
  io.out.bits.exc := Bits(0)

  when (in.cmd === FCMD_MTFSR || in.cmd === FCMD_MFFSR) {
    io.out.bits.toint := io.in.bits.fsr
  }
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
  io.out.bits.store := Mux(in.single, Cat(unrec_s, unrec_s), unrec_d)
  io.out.bits.lt := dcmp.io.a_lt_b
}

class FPResult extends Bundle
{
  val data = Bits(width = 65)
  val exc = Bits(width = 5)
}

class IntToFP(val latency: Int) extends Component
{
  class Input extends Bundle {
    val single = Bool()
    val cmd = Bits(width = FCMD_WIDTH)
    val rm = Bits(width = 3)
    val data = Bits(width = 64)
    override def clone = new Input().asInstanceOf[this.type]
  }
  val io = new Bundle {
    val in = new PipeIO()(new Input).flip
    val out = new PipeIO()(new FPResult)
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
      mux.data := Cat(Fix(-1, 32), u._1)
      mux.exc := u._2
    }.otherwise {
      val u = hardfloat.anyToRecodedFloatN(in.bits.data, in.bits.rm, ~in.bits.cmd(1,0), 52, 12, 64)
      mux.data := u._1
      mux.exc := u._2
    }
  }

  io.out <> Pipe(in.valid, mux, latency-1)
}

class FPToFP(val latency: Int) extends Component
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
    val in = new PipeIO()(new Input).flip
    val out = new PipeIO()(new FPResult)
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
      mux.data := Cat(Fix(-1, 32), d2s._1)
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
  val cmd = Bits(INPUT, 2)
  val rm = Bits(INPUT, 2)
  val in1 = Bits(INPUT, width)
  val in2 = Bits(INPUT, width)
  val in3 = Bits(INPUT, width)
  val out = Bits(OUTPUT, width)
  val exc = Bits(OUTPUT, 5)
}

class FPUSFMAPipe(val latency: Int) extends Component
{
  val io = new ioFMA(33)

  val r_cmd = Reg() { Bits() }
  val r_rm = Reg() { Bits() }
  val r_in1 = Reg() { Bits() }
  val r_in2 = Reg() { Bits() }
  val r_in3 = Reg() { Bits() }
  
  val out_reg = Reg() { Bits() }
  val exc_reg = Reg() { Bits() }
  
  val valid_pipe_regs = Vec(latency) { Reg() { Bool() } }
  val dout_pipe_regs  = Vec(latency-2) { Reg() { Bits() } }
  val exc_pipe_regs   = Vec(latency-2) { Reg() { Bits() } }
  
  valid_pipe_regs(0) := io.valid
  for (i <- 1 until latency) {
  	valid_pipe_regs(i) := valid_pipe_regs(i-1)
  }
  
  when (io.valid) {
    r_cmd := io.cmd
    r_rm  := io.rm
    r_in1 := io.in1
    r_in2 := io.in2
    r_in3 := io.in3
  }

  val fma = new hardfloat.mulAddSubRecodedFloatN(23, 9)
  
  fma.io.op := r_cmd
  fma.io.roundingMode := r_rm
  fma.io.a := r_in1
  fma.io.b := r_in2
  fma.io.c := r_in3

  when (valid_pipe_regs(0)) {
  	dout_pipe_regs(0) := fma.io.out
  	exc_pipe_regs(0)  := fma.io.exceptionFlags
  }
  
  for (i <- 1 until latency-2) {
  	when (valid_pipe_regs(i)) {
  		dout_pipe_regs(i) := dout_pipe_regs(i-1)
  		exc_pipe_regs(i)  := exc_pipe_regs(i-1)
  	}
  }

  when (valid_pipe_regs(latency-2)) {
	out_reg := dout_pipe_regs(latency-3)
	exc_reg := exc_pipe_regs(latency-3)
  }

  io.out := out_reg
  io.exc := exc_reg
}

class FPUDFMAPipe(val latency: Int) extends Component
{
  val io = new ioFMA(65)
  
  val r_cmd = Reg() { Bits() }
  val r_rm = Reg() { Bits() }
  val r_in1 = Reg() { Bits() }
  val r_in2 = Reg() { Bits() }
  val r_in3 = Reg() { Bits() }
  
  val out_reg = Reg() { Bits() }
  val exc_reg = Reg() { Bits() }
  
  val valid_pipe_regs = Vec(latency) { Reg() { Bool() } }
  val dout_pipe_regs  = Vec(latency-2) { Reg() { Bits() } }
  val exc_pipe_regs   = Vec(latency-2) { Reg() { Bits() } }
  
  valid_pipe_regs(0) := io.valid
  for (i <- 1 until latency) {
  	valid_pipe_regs(i) := valid_pipe_regs(i-1)
  }
  
  when (io.valid) {
    r_cmd := io.cmd
    r_rm  := io.rm
    r_in1 := io.in1
    r_in2 := io.in2
    r_in3 := io.in3
  }
  
  val fma = new hardfloat.mulAddSubRecodedFloatN(52, 12)
  
  fma.io.op := r_cmd
  fma.io.roundingMode := r_rm
  fma.io.a := r_in1
  fma.io.b := r_in2
  fma.io.c := r_in3

  when (valid_pipe_regs(0)) {
  	dout_pipe_regs(0) := fma.io.out
  	exc_pipe_regs(0)  := fma.io.exceptionFlags
  }
  
  for (i <- 1 until latency-2) {
  	when (valid_pipe_regs(i)) {
  		dout_pipe_regs(i) := dout_pipe_regs(i-1)
  		exc_pipe_regs(i)  := exc_pipe_regs(i-1)
  	}
  }

  when (valid_pipe_regs(latency-2)) {
	out_reg := dout_pipe_regs(latency-3)
	exc_reg := exc_pipe_regs(latency-3)
  }

  io.out := out_reg
  io.exc := exc_reg
}

class FPU(sfma_latency: Int, dfma_latency: Int) extends Component
{
  val io = new Bundle {
    val ctrl = new CtrlFPUIO().flip
    val dpath = new DpathFPUIO().flip
    val sfma = new ioFMA(33)
    val dfma = new ioFMA(65)
  }

  val ex_reg_inst = Reg() { Bits() }
  when (io.ctrl.valid) {
    ex_reg_inst := io.dpath.inst
  }
  val ex_reg_valid = Reg(io.ctrl.valid, Bool(false))
  val mem_reg_valid = Reg(ex_reg_valid && !io.ctrl.killx, resetVal = Bool(false))
  val killm = io.ctrl.killm || io.ctrl.nack_mem
  val wb_reg_valid = Reg(mem_reg_valid && !killm, resetVal = Bool(false))

  val fp_decoder = new FPUDecoder
  fp_decoder.io.inst := io.dpath.inst

  val ctrl = RegEn(fp_decoder.io.sigs, io.ctrl.valid)
  val mem_ctrl = RegEn(ctrl, ex_reg_valid)
  val wb_ctrl = RegEn(mem_ctrl, mem_reg_valid)

  // load response
  val load_wb = Reg(io.dpath.dmem_resp_val)
  val load_wb_single = RegEn(io.dpath.dmem_resp_type === MT_W || io.dpath.dmem_resp_type === MT_WU, io.dpath.dmem_resp_val)
  val load_wb_data = RegEn(io.dpath.dmem_resp_data, io.dpath.dmem_resp_val)
  val load_wb_tag = RegEn(io.dpath.dmem_resp_tag, io.dpath.dmem_resp_val)
  val rec_s = hardfloat.floatNToRecodedFloatN(load_wb_data, 23, 9)
  val rec_d = hardfloat.floatNToRecodedFloatN(load_wb_data, 52, 12)
  val load_wb_data_recoded = Mux(load_wb_single, Cat(Fix(-1), rec_s), rec_d)

  val fsr_rm = Reg() { Bits(width = 3) }
  val fsr_exc = Reg() { Bits(width = 5) }

  // regfile
  val regfile = Mem(32) { Bits(width = 65) }
  when (load_wb) { regfile(load_wb_tag) := load_wb_data_recoded }

  val ex_rs1 = regfile(ex_reg_inst(26,22))
  val ex_rs2 = regfile(ex_reg_inst(21,17))
  val ex_rs3 = regfile(ex_reg_inst(16,12))
  val ex_rm = Mux(ex_reg_inst(11,9) === Bits(7), fsr_rm, ex_reg_inst(11,9))

  val fpiu = new FPToInt
  fpiu.io.in.valid := ex_reg_valid && ctrl.toint
  fpiu.io.in.bits := ctrl
  fpiu.io.in.bits.rm := ex_rm
  fpiu.io.in.bits.fsr := Cat(fsr_rm, fsr_exc)
  fpiu.io.in.bits.in1 := ex_rs1
  fpiu.io.in.bits.in2 := ex_rs2

  io.dpath.store_data := fpiu.io.out.bits.store
  io.dpath.toint_data := fpiu.io.out.bits.toint

  val ifpu = new IntToFP(3)
  ifpu.io.in.valid := ex_reg_valid && ctrl.fromint
  ifpu.io.in.bits := ctrl
  ifpu.io.in.bits.rm := ex_rm
  ifpu.io.in.bits.data := io.dpath.fromint_data
  val fpmu = new FPToFP(2)
  fpmu.io.in.valid := ex_reg_valid && ctrl.fastpipe
  fpmu.io.in.bits := ctrl
  fpmu.io.in.bits.rm := ex_rm
  fpmu.io.in.bits.in1 := ex_rs1
  fpmu.io.in.bits.in2 := ex_rs2
  fpmu.io.lt := fpiu.io.out.bits.lt

  val cmd_fma = mem_ctrl.cmd === FCMD_MADD  || mem_ctrl.cmd === FCMD_MSUB ||
                mem_ctrl.cmd === FCMD_NMADD || mem_ctrl.cmd === FCMD_NMSUB
  val cmd_addsub = mem_ctrl.cmd === FCMD_ADD || mem_ctrl.cmd === FCMD_SUB
  
  // RIMAS: refactoring for retiming
  // moved recoding of cmd -> op outside of DFMA/SFMA blocks
  // also moved muxing of operands based on command bits out of module
  
  // Single precision FMA
  val sfma = new FPUSFMAPipe(sfma_latency)
  val sfma_cmd = Mux(io.sfma.valid, io.sfma.cmd, ctrl.cmd)
  val sfma_cmd_fma = sfma_cmd === FCMD_MADD  || sfma_cmd === FCMD_MSUB ||
                	 sfma_cmd === FCMD_NMADD || sfma_cmd === FCMD_NMSUB
  val sfma_cmd_addsub = sfma_cmd === FCMD_ADD || sfma_cmd === FCMD_SUB
  
  val sfma_in1 = Mux(io.sfma.valid, io.sfma.in1, ex_rs1)
  val sfma_in2 = Mux(io.sfma.valid, io.sfma.in2, ex_rs2)
  val sfma_in3 = Mux(io.sfma.valid, io.sfma.in3, ex_rs3)
  val sfma_one = Bits("h80000000")
  val sfma_zero = Cat(sfma_in1(32) ^ sfma_in2(32), Bits(0, 32))

  sfma.io.valid := io.sfma.valid || ex_reg_valid && ctrl.fma && ctrl.single
  sfma.io.in1 := sfma_in1
  sfma.io.in2 := Mux(sfma_cmd_addsub, sfma_one, sfma_in2)
  sfma.io.in3 := Mux(sfma_cmd_fma, sfma_in3, Mux(sfma_cmd_addsub, sfma_in2, sfma_zero))
  sfma.io.cmd := Cat(sfma_cmd(1) & (sfma_cmd_fma || sfma_cmd_addsub), sfma_cmd(0))
  sfma.io.rm  := Mux(io.sfma.valid, io.sfma.rm(1,0), ex_rm(1,0))
  io.sfma.out := sfma.io.out
  io.sfma.exc := sfma.io.exc
  
  // Double precision FMA
  val dfma = new FPUDFMAPipe(dfma_latency)
  val dfma_cmd = Mux(io.dfma.valid, io.dfma.cmd, ctrl.cmd)
  val dfma_cmd_fma = dfma_cmd === FCMD_MADD  || dfma_cmd === FCMD_MSUB ||
                	 dfma_cmd === FCMD_NMADD || dfma_cmd === FCMD_NMSUB
  val dfma_cmd_addsub = dfma_cmd === FCMD_ADD || dfma_cmd === FCMD_SUB
  
  val dfma_in1 = Mux(io.dfma.valid, io.dfma.in1, ex_rs1)
  val dfma_in2 = Mux(io.dfma.valid, io.dfma.in2, ex_rs2)
  val dfma_in3 = Mux(io.dfma.valid, io.dfma.in3, ex_rs3)
  val dfma_one = Bits("h8000000000000000")
  val dfma_zero = Cat(dfma_in1(64) ^ dfma_in2(64), Bits(0, 64))
  
  dfma.io.valid := io.dfma.valid || ex_reg_valid && ctrl.fma && !ctrl.single
  dfma.io.in1 := dfma_in1
  dfma.io.in2 := Mux(dfma_cmd_addsub, dfma_one, dfma_in2)
  dfma.io.in3 := Mux(dfma_cmd_fma, dfma_in3, Mux(dfma_cmd_addsub, dfma_in2, dfma_zero))
  dfma.io.cmd := Cat(dfma_cmd(1) & (dfma_cmd_fma || dfma_cmd_addsub), dfma_cmd(0))
  dfma.io.rm  := Mux(io.dfma.valid, io.dfma.rm(1,0), ex_rm(1,0))  
  io.dfma.out := dfma.io.out
  io.dfma.exc := dfma.io.exc

  // writeback arbitration
  case class Pipe(p: Component, lat: Int, cond: (FPUCtrlSigs) => Bool, wdata: Bits, wexc: Bits)
  val pipes = List(
    Pipe(fpmu, fpmu.latency, (c: FPUCtrlSigs) => c.fastpipe, fpmu.io.out.bits.data, fpmu.io.out.bits.exc),
    Pipe(ifpu, ifpu.latency, (c: FPUCtrlSigs) => c.fromint, ifpu.io.out.bits.data, ifpu.io.out.bits.exc),
    Pipe(sfma, sfma.latency, (c: FPUCtrlSigs) => c.fma && c.single, sfma.io.out, sfma.io.exc),
    Pipe(dfma, dfma.latency, (c: FPUCtrlSigs) => c.fma && !c.single, dfma.io.out, dfma.io.exc))
  def latencyMask(c: FPUCtrlSigs, offset: Int) = {
    require(pipes.forall(_.lat >= offset))
    pipes.map(p => Mux(p.cond(c), UFix(1 << p.lat-offset), UFix(0))).reduce(_|_)
  }
  def pipeid(c: FPUCtrlSigs) = pipes.zipWithIndex.map(p => Mux(p._1.cond(c), UFix(p._2), UFix(0))).reduce(_|_)
  val maxLatency = pipes.map(_.lat).max
  val memLatencyMask = latencyMask(mem_ctrl, 2)

  val wen = Reg(resetVal = Bits(0, maxLatency-1))
  val winfo = Vec(maxLatency-1) { Reg() { Bits() } }
  val mem_wen = mem_reg_valid && (mem_ctrl.fma || mem_ctrl.fastpipe || mem_ctrl.fromint)
  val (write_port_busy, mem_winfo) = (Reg{Bool()}, Reg{Bits()})
  when (ex_reg_valid) {
    write_port_busy := mem_wen && (memLatencyMask & latencyMask(ctrl, 1)).orR || (wen & latencyMask(ctrl, 0)).orR
    mem_winfo := Cat(pipeid(ctrl), ex_reg_inst(31,27))
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

  val waddr = winfo(0)(4,0).toUFix
  val wsrc = winfo(0) >> waddr.getWidth
  val wdata = (Vec(pipes.map(_.wdata)){Bits()})(wsrc)
  val wexc = (Vec(pipes.map(_.wexc)){Bits()})(wsrc)
  when (wen(0)) { regfile(waddr(4,0)) := wdata }

  val wb_toint_exc = RegEn(fpiu.io.out.bits.exc, mem_ctrl.toint)
  when (wb_reg_valid && wb_ctrl.toint || wen(0)) {
    fsr_exc := fsr_exc |
      Fill(fsr_exc.getWidth, wb_reg_valid && wb_ctrl.toint) & wb_toint_exc |
      Fill(fsr_exc.getWidth, wen(0)) & wexc
  }

  val mem_fsr_wdata = RegEn(io.dpath.fromint_data(FSR_WIDTH-1,0), ex_reg_valid && ctrl.wrfsr)
  val wb_fsr_wdata = RegEn(mem_fsr_wdata, mem_reg_valid && mem_ctrl.wrfsr)
  when (wb_reg_valid && wb_ctrl.wrfsr) {
    fsr_exc := wb_fsr_wdata
    fsr_rm := wb_fsr_wdata >> fsr_exc.getWidth
  }

  val fp_inflight = wb_reg_valid && wb_ctrl.toint || wen.orR
  val fsr_busy = mem_ctrl.rdfsr && fp_inflight || wb_reg_valid && wb_ctrl.wrfsr
  val units_busy = mem_reg_valid && mem_ctrl.fma && Reg(Mux(ctrl.single, io.sfma.valid, io.dfma.valid))
  io.ctrl.nack_mem := fsr_busy || units_busy || write_port_busy
  io.ctrl.dec <> fp_decoder.io.sigs
  def useScoreboard(f: ((Pipe, Int)) => Bool) = pipes.zipWithIndex.filter(_._1.lat > 3).map(x => f(x)).fold(Bool(false))(_||_)
  io.ctrl.sboard_set := wb_reg_valid && Reg(useScoreboard(_._1.cond(mem_ctrl)))
  io.ctrl.sboard_clr := wen(0) && useScoreboard(x => wsrc === UFix(x._2))
  io.ctrl.sboard_clra := waddr
  // we don't currently support round-max-magnitude (rm=4)
  io.ctrl.illegal_rm := ex_rm(2) && ctrl.round
}
