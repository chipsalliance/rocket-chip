package rocket

import Chisel._
import Node._
import Constants._
import Instructions._

object rocketFPConstants
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
  val FCMD_WIDTH = 6
  val FSR_WIDTH = 8
}
import rocketFPConstants._

class rocketFPUCtrlSigs extends Bundle
{
  val cmd = Bits(width = FCMD_WIDTH)
  val valid = Bool()
  val wen = Bool()
  val sboard = Bool()
  val ren1 = Bool()
  val ren2 = Bool()
  val ren3 = Bool()
  val single = Bool()
  val fromint = Bool()
  val toint = Bool()
  val fastpipe = Bool()
  val fma = Bool()
  val store = Bool()
  val rdfsr = Bool()
  val wrfsr = Bool()
}

class rocketFPUDecoder extends Component
{
  val io = new Bundle {
    val inst = Bits(32, INPUT)
    val sigs = new rocketFPUCtrlSigs().asOutput
  }

  val N = Bool(false)
  val Y = Bool(true)
  val X = Bool(false)
  val FCMD_X = FCMD_ADD
  val decoder = ListLookup(io.inst,
    List                  (N,FCMD_X,         X,X,X,X,X,X,X,X,X,X,X,X,X),
    Array(FLW      -> List(Y,FCMD_LOAD,      Y,N,N,N,N,Y,N,N,N,N,N,N,N),
          FLD      -> List(Y,FCMD_LOAD,      Y,N,N,N,N,N,N,N,N,N,N,N,N),
          FSW      -> List(Y,FCMD_STORE,     N,N,N,Y,N,Y,N,N,N,N,Y,N,N),
          FSD      -> List(Y,FCMD_STORE,     N,N,N,Y,N,N,N,N,N,N,Y,N,N),
          MXTF_S   -> List(Y,FCMD_MXTF,      Y,N,N,N,N,Y,Y,N,Y,N,N,N,N),
          MXTF_D   -> List(Y,FCMD_MXTF,      Y,N,N,N,N,N,Y,N,Y,N,N,N,N),
          FCVT_S_W -> List(Y,FCMD_CVT_FMT_W, Y,N,N,N,N,Y,Y,N,Y,N,N,N,N),
          FCVT_S_WU-> List(Y,FCMD_CVT_FMT_WU,Y,N,N,N,N,Y,Y,N,Y,N,N,N,N),
          FCVT_S_L -> List(Y,FCMD_CVT_FMT_L, Y,N,N,N,N,Y,Y,N,Y,N,N,N,N),
          FCVT_S_LU-> List(Y,FCMD_CVT_FMT_LU,Y,N,N,N,N,Y,Y,N,Y,N,N,N,N),
          FCVT_D_W -> List(Y,FCMD_CVT_FMT_W, Y,N,N,N,N,N,Y,N,Y,N,N,N,N),
          FCVT_D_WU-> List(Y,FCMD_CVT_FMT_WU,Y,N,N,N,N,N,Y,N,Y,N,N,N,N),
          FCVT_D_L -> List(Y,FCMD_CVT_FMT_L, Y,N,N,N,N,N,Y,N,Y,N,N,N,N),
          FCVT_D_LU-> List(Y,FCMD_CVT_FMT_LU,Y,N,N,N,N,N,Y,N,Y,N,N,N,N),
          MFTX_S   -> List(Y,FCMD_MFTX,      N,N,Y,N,N,Y,N,Y,N,N,N,N,N),
          MFTX_D   -> List(Y,FCMD_MFTX,      N,N,Y,N,N,N,N,Y,N,N,N,N,N),
          FCVT_W_S -> List(Y,FCMD_CVT_W_FMT, N,N,Y,N,N,Y,N,Y,N,N,N,N,N),
          FCVT_WU_S-> List(Y,FCMD_CVT_WU_FMT,N,N,Y,N,N,Y,N,Y,N,N,N,N,N),
          FCVT_L_S -> List(Y,FCMD_CVT_L_FMT, N,N,Y,N,N,Y,N,Y,N,N,N,N,N),
          FCVT_LU_S-> List(Y,FCMD_CVT_LU_FMT,N,N,Y,N,N,Y,N,Y,N,N,N,N,N),
          FCVT_W_D -> List(Y,FCMD_CVT_W_FMT, N,N,Y,N,N,N,N,Y,N,N,N,N,N),
          FCVT_WU_D-> List(Y,FCMD_CVT_WU_FMT,N,N,Y,N,N,N,N,Y,N,N,N,N,N),
          FCVT_L_D -> List(Y,FCMD_CVT_L_FMT, N,N,Y,N,N,N,N,Y,N,N,N,N,N),
          FCVT_LU_D-> List(Y,FCMD_CVT_LU_FMT,N,N,Y,N,N,N,N,Y,N,N,N,N,N),
          FCVT_S_D -> List(Y,FCMD_CVT_FMT_D, Y,N,Y,N,N,Y,N,N,Y,N,N,N,N),
          FCVT_D_S -> List(Y,FCMD_CVT_FMT_S, Y,N,Y,N,N,N,N,N,Y,N,N,N,N),
          FEQ_S    -> List(Y,FCMD_EQ,        N,N,Y,Y,N,Y,N,Y,N,N,N,N,N),
          FLT_S    -> List(Y,FCMD_LT,        N,N,Y,Y,N,Y,N,Y,N,N,N,N,N),
          FLE_S    -> List(Y,FCMD_LE,        N,N,Y,Y,N,Y,N,Y,N,N,N,N,N),
          FEQ_D    -> List(Y,FCMD_EQ,        N,N,Y,Y,N,N,N,Y,N,N,N,N,N),
          FLT_D    -> List(Y,FCMD_LT,        N,N,Y,Y,N,N,N,Y,N,N,N,N,N),
          FLE_D    -> List(Y,FCMD_LE,        N,N,Y,Y,N,N,N,Y,N,N,N,N,N),
          MTFSR    -> List(Y,FCMD_MTFSR,     N,N,N,N,N,Y,N,Y,N,N,N,Y,Y),
          MFFSR    -> List(Y,FCMD_MFFSR,     N,N,N,N,N,Y,N,Y,N,N,N,Y,N),
          FSGNJ_S  -> List(Y,FCMD_SGNJ,      Y,N,Y,Y,N,Y,N,N,Y,N,N,N,N),
          FSGNJN_S -> List(Y,FCMD_SGNJN,     Y,N,Y,Y,N,Y,N,N,Y,N,N,N,N),
          FSGNJX_S -> List(Y,FCMD_SGNJX,     Y,N,Y,Y,N,Y,N,N,Y,N,N,N,N),
          FSGNJ_D  -> List(Y,FCMD_SGNJ,      Y,N,Y,Y,N,N,N,N,Y,N,N,N,N),
          FSGNJN_D -> List(Y,FCMD_SGNJN,     Y,N,Y,Y,N,N,N,N,Y,N,N,N,N),
          FSGNJX_D -> List(Y,FCMD_SGNJX,     Y,N,Y,Y,N,N,N,N,Y,N,N,N,N),
          FMIN_S   -> List(Y,FCMD_MIN,       Y,N,Y,Y,N,Y,N,N,Y,N,N,N,N),
          FMAX_S   -> List(Y,FCMD_MAX,       Y,N,Y,Y,N,Y,N,N,Y,N,N,N,N),
          FMIN_D   -> List(Y,FCMD_MIN,       Y,N,Y,Y,N,N,N,N,Y,N,N,N,N),
          FMAX_D   -> List(Y,FCMD_MAX,       Y,N,Y,Y,N,N,N,N,Y,N,N,N,N),
          FADD_S   -> List(Y,FCMD_ADD,       Y,Y,Y,Y,N,Y,N,N,N,Y,N,N,N),
          FSUB_S   -> List(Y,FCMD_SUB,       Y,Y,Y,Y,N,Y,N,N,N,Y,N,N,N),
          FMUL_S   -> List(Y,FCMD_MUL,       Y,Y,Y,Y,N,Y,N,N,N,Y,N,N,N),
          FADD_D   -> List(Y,FCMD_ADD,       Y,Y,Y,Y,N,N,N,N,N,Y,N,N,N),
          FSUB_D   -> List(Y,FCMD_SUB,       Y,Y,Y,Y,N,N,N,N,N,Y,N,N,N),
          FMUL_D   -> List(Y,FCMD_MUL,       Y,Y,Y,Y,N,N,N,N,N,Y,N,N,N),
          FMADD_S  -> List(Y,FCMD_MADD,      Y,Y,Y,Y,Y,Y,N,N,N,Y,N,N,N),
          FMSUB_S  -> List(Y,FCMD_MSUB,      Y,Y,Y,Y,Y,Y,N,N,N,Y,N,N,N),
          FNMADD_S -> List(Y,FCMD_NMADD,     Y,Y,Y,Y,Y,Y,N,N,N,Y,N,N,N),
          FNMSUB_S -> List(Y,FCMD_NMSUB,     Y,Y,Y,Y,Y,Y,N,N,N,Y,N,N,N),
          FMADD_D  -> List(Y,FCMD_MADD,      Y,Y,Y,Y,Y,N,N,N,N,Y,N,N,N),
          FMSUB_D  -> List(Y,FCMD_MSUB,      Y,Y,Y,Y,Y,N,N,N,N,Y,N,N,N),
          FNMADD_D -> List(Y,FCMD_NMADD,     Y,Y,Y,Y,Y,N,N,N,N,Y,N,N,N),
          FNMSUB_D -> List(Y,FCMD_NMSUB,     Y,Y,Y,Y,Y,N,N,N,N,Y,N,N,N)
          ))
  val valid :: cmd :: wen :: sboard :: ren1 :: ren2 :: ren3 :: single :: fromint :: toint :: fastpipe :: fma :: store :: rdfsr :: wrfsr :: Nil = decoder

  io.sigs.valid := valid.toBool
  io.sigs.cmd := cmd
  io.sigs.wen := wen.toBool
  io.sigs.sboard := sboard.toBool
  io.sigs.ren1 := ren1.toBool
  io.sigs.ren2 := ren2.toBool
  io.sigs.ren3 := ren3.toBool
  io.sigs.single := single.toBool
  io.sigs.fromint := fromint.toBool
  io.sigs.toint := toint.toBool
  io.sigs.fastpipe := fastpipe.toBool
  io.sigs.fma := fma.toBool
  io.sigs.store := store.toBool
  io.sigs.rdfsr := rdfsr.toBool
  io.sigs.wrfsr := wrfsr.toBool
}

class ioDpathFPU extends Bundle {
  val inst = Bits(32, OUTPUT)
  val fromint_data = Bits(64, OUTPUT)

  val store_data = Bits(64, INPUT)
  val toint_data = Bits(64, INPUT)

  val dmem_resp_val = Bool(OUTPUT)
  val dmem_resp_type = Bits(3, OUTPUT)
  val dmem_resp_tag = UFix(5, OUTPUT)
  val dmem_resp_data = Bits(64, OUTPUT)
}

class ioCtrlFPU extends Bundle {
  val valid = Bool(OUTPUT)
  val nack_mem = Bool(INPUT)
  val illegal_rm = Bool(INPUT)
  val killx = Bool(OUTPUT)
  val killm = Bool(OUTPUT)
  val dec = new rocketFPUCtrlSigs().asInput
  val sboard_clr = Bool(INPUT)
  val sboard_clra = UFix(5, INPUT)
}

class rocketFPIntUnit extends Component
{
  val io = new Bundle {
    val single = Bool(INPUT)
    val cmd = Bits(FCMD_WIDTH, INPUT)
    val rm = Bits(3, INPUT)
    val fsr = Bits(FSR_WIDTH, INPUT)
    val in1 = Bits(65, INPUT)
    val in2 = Bits(65, INPUT)
    val lt_s = Bool(OUTPUT)
    val lt_d = Bool(OUTPUT)
    val store_data = Bits(64, OUTPUT)
    val toint_data = Bits(64, OUTPUT)
    val exc = Bits(5, OUTPUT)
  }

  val unrec_s = new hardfloat.recodedFloat32ToFloat32
  val unrec_d = new hardfloat.recodedFloat64ToFloat64
  unrec_s.io.in := io.in1
  unrec_d.io.in := io.in1

  io.store_data := Mux(io.single, Cat(unrec_s.io.out, unrec_s.io.out), unrec_d.io.out)

  val scmp = new hardfloat.recodedFloat32Compare
  scmp.io.a := io.in1
  scmp.io.b := io.in2
  val scmp_out = (io.cmd & Cat(scmp.io.a_lt_b, scmp.io.a_eq_b)).orR
  val scmp_exc = (io.cmd & Cat(scmp.io.a_lt_b_invalid, scmp.io.a_eq_b_invalid)).orR << UFix(4)

  val s2i = new hardfloat.recodedFloat32ToAny
  s2i.io.in := io.in1
  s2i.io.roundingMode := io.rm
  s2i.io.typeOp := ~io.cmd(1,0)

  val dcmp = new hardfloat.recodedFloat64Compare
  dcmp.io.a := io.in1
  dcmp.io.b := io.in2
  val dcmp_out = (io.cmd & Cat(dcmp.io.a_lt_b, dcmp.io.a_eq_b)).orR
  val dcmp_exc = (io.cmd & Cat(dcmp.io.a_lt_b_invalid, dcmp.io.a_eq_b_invalid)).orR << UFix(4)

  val d2i = new hardfloat.recodedFloat64ToAny
  d2i.io.in := io.in1
  d2i.io.roundingMode := io.rm
  d2i.io.typeOp := ~io.cmd(1,0)

  // output muxing
  val (out_s, exc_s) = (Wire() { Bits() }, Wire() { Bits() })
  out_s := Cat(Fill(32, unrec_s.io.out(31)), unrec_s.io.out)
  exc_s := Bits(0)
  val (out_d, exc_d) = (Wire() { Bits() }, Wire() { Bits() })
  out_d := unrec_d.io.out
  exc_d := Bits(0)

  when (io.cmd === FCMD_MTFSR || io.cmd === FCMD_MFFSR) {
    out_s := io.fsr
  }
  when (io.cmd === FCMD_CVT_W_FMT || io.cmd === FCMD_CVT_WU_FMT) {
    out_s := Cat(Fill(32, s2i.io.out(31)), s2i.io.out(31,0))
    exc_s := s2i.io.exceptionFlags
    out_d := Cat(Fill(32, d2i.io.out(31)), d2i.io.out(31,0))
    exc_d := d2i.io.exceptionFlags
  }
  when (io.cmd === FCMD_CVT_L_FMT || io.cmd === FCMD_CVT_LU_FMT) {
    out_s := s2i.io.out
    exc_s := s2i.io.exceptionFlags
    out_d := d2i.io.out
    exc_d := d2i.io.exceptionFlags
  }
  when (io.cmd === FCMD_EQ || io.cmd === FCMD_LT || io.cmd === FCMD_LE) {
    out_s := scmp_out
    exc_s := scmp_exc
    out_d := dcmp_out
    exc_d := dcmp_exc
  }

  io.toint_data := Mux(io.single, out_s, out_d)
  io.exc := Mux(io.single, exc_s, exc_d)
  io.lt_s := scmp.io.a_lt_b
  io.lt_d := dcmp.io.a_lt_b
}

class rocketFPUFastPipe extends Component
{
  val io = new Bundle {
    val single = Bool(INPUT)
    val cmd = Bits(FCMD_WIDTH, INPUT)
    val rm = Bits(3, INPUT)
    val fromint = Bits(64, INPUT)
    val in1 = Bits(65, INPUT)
    val in2 = Bits(65, INPUT)
    val lt_s = Bool(INPUT)
    val lt_d = Bool(INPUT)
    val out_s = Bits(33, OUTPUT)
    val exc_s = Bits(5, OUTPUT)
    val out_d = Bits(65, OUTPUT)
    val exc_d = Bits(5, OUTPUT)
  }

  // int->fp units
  val rec_s = new hardfloat.float32ToRecodedFloat32
  val rec_d = new hardfloat.float64ToRecodedFloat64
  rec_s.io.in := io.fromint
  rec_d.io.in := io.fromint

  val i2s = new hardfloat.anyToRecodedFloat32
  i2s.io.in := io.fromint
  i2s.io.roundingMode := io.rm
  i2s.io.typeOp := ~io.cmd(1,0)

  val i2d = new hardfloat.anyToRecodedFloat64
  i2d.io.in := io.fromint
  i2d.io.roundingMode := io.rm
  i2d.io.typeOp := ~io.cmd(1,0)

  // fp->fp units
  val sign_s = Mux(io.cmd === FCMD_SGNJ, io.in2(32),
               Mux(io.cmd === FCMD_SGNJN, ~io.in2(32),
                   io.in1(32) ^ io.in2(32))) // FCMD_SGNJX
  val sign_d = Mux(io.cmd === FCMD_SGNJ, io.in2(64),
               Mux(io.cmd === FCMD_SGNJN, ~io.in2(64),
                   io.in1(64) ^ io.in2(64))) // FCMD_SGNJX
  val fsgnj = Cat(Mux(io.single, io.in1(64), sign_d), io.in1(63,33),
                  Mux(io.single, sign_s, io.in1(32)), io.in1(31,0))

  val s2d = new hardfloat.recodedFloat32ToRecodedFloat64
  s2d.io.in := io.in1

  val d2s = new hardfloat.recodedFloat64ToRecodedFloat32
  d2s.io.in := io.in1
  d2s.io.roundingMode := io.rm

  val isnan1 = Mux(io.single, io.in1(31,29) === Bits("b111"), io.in1(63,61) === Bits("b111"))
  val isnan2 = Mux(io.single, io.in2(31,29) === Bits("b111"), io.in2(63,61) === Bits("b111"))
  val issnan1 = isnan1 && ~Mux(io.single, io.in1(22), io.in1(51))
  val issnan2 = isnan2 && ~Mux(io.single, io.in2(22), io.in2(51))
  val minmax_exc = Cat(issnan1 || issnan2, Bits(0,4))
  val min = io.cmd === FCMD_MIN
  val lt = Mux(io.single, io.lt_s, io.lt_d)
  val minmax = Mux(isnan2 || !isnan1 && (min === lt), io.in1, io.in2)

  // output muxing
  val (out_s, exc_s) = (Wire() { Bits() }, Wire() { Bits() })
  out_s := Reg(rec_s.io.out)
  exc_s := Bits(0)
  val (out_d, exc_d) = (Wire() { Bits() }, Wire() { Bits() })
  out_d := Reg(rec_d.io.out)
  exc_d := Bits(0)

  val r_cmd = Reg(io.cmd)

  when (r_cmd === FCMD_MTFSR || r_cmd === FCMD_MFFSR) {
    out_s := Reg(io.fromint(FSR_WIDTH-1,0))
  }
  when (r_cmd === FCMD_SGNJ || r_cmd === FCMD_SGNJN || r_cmd === FCMD_SGNJX) {
    val r_fsgnj = Reg(fsgnj)
    out_s := r_fsgnj(32,0)
    out_d := r_fsgnj
  }
  when (r_cmd === FCMD_MIN || r_cmd === FCMD_MAX) {
    val r_minmax = Reg(minmax)
    val r_minmax_exc = Reg(minmax_exc)
    out_s := r_minmax(32,0)
    out_d := r_minmax
    exc_s := r_minmax_exc
    exc_d := r_minmax_exc
  }
  when (r_cmd === FCMD_CVT_FMT_S || r_cmd === FCMD_CVT_FMT_D) {
    out_s := Reg(d2s.io.out)
    exc_s := Reg(d2s.io.exceptionFlags)
    out_d := Reg(s2d.io.out)
    exc_d := Reg(s2d.io.exceptionFlags)
  }
  when (r_cmd === FCMD_CVT_FMT_W || r_cmd === FCMD_CVT_FMT_WU ||
        r_cmd === FCMD_CVT_FMT_L || r_cmd === FCMD_CVT_FMT_LU) {
    out_s := Reg(i2s.io.out)
    exc_s := Reg(i2s.io.exceptionFlags)
    out_d := Reg(i2d.io.out)
    exc_d := Reg(i2d.io.exceptionFlags)
  }

  io.out_s := out_s
  io.exc_s := exc_s
  io.out_d := out_d
  io.exc_d := exc_d
}

class ioFMA(width: Int) extends Bundle {
  val valid = Bool(INPUT)
  val cmd = Bits(FCMD_WIDTH, INPUT)
  val rm = Bits(3, INPUT)
  val in1 = Bits(width, INPUT)
  val in2 = Bits(width, INPUT)
  val in3 = Bits(width, INPUT)
  val out = Bits(width, OUTPUT)
  val exc = Bits(5, OUTPUT)
}

class rocketFPUSFMAPipe(latency: Int) extends Component
{
  val io = new ioFMA(33)
  
  val cmd = Reg() { Bits() }
  val rm = Reg() { Bits() }
  val in1 = Reg() { Bits() }
  val in2 = Reg() { Bits() }
  val in3 = Reg() { Bits() }

  val cmd_fma = io.cmd === FCMD_MADD  || io.cmd === FCMD_MSUB ||
                io.cmd === FCMD_NMADD || io.cmd === FCMD_NMSUB
  val cmd_addsub = io.cmd === FCMD_ADD || io.cmd === FCMD_SUB

  val one = Bits("h80000000")
  val zero = Cat(io.in1(32) ^ io.in2(32), Bits(0, 32))

  when (io.valid) {
    cmd := Cat(io.cmd(1) & (cmd_fma || cmd_addsub), io.cmd(0))
    rm := io.rm
    in1 := io.in1
    in2 := Mux(cmd_addsub, one, io.in2)
    in3 := Mux(cmd_fma, io.in3, Mux(cmd_addsub, io.in2, zero))
  }

  val fma = new hardfloat.mulAddSubRecodedFloat32_1
  fma.io.op := cmd
  fma.io.roundingMode := rm
  fma.io.a := in1
  fma.io.b := in2
  fma.io.c := in3

  io.out := ShiftRegister(latency-1, fma.io.out)
  io.exc := ShiftRegister(latency-1, fma.io.exceptionFlags)
}

class rocketFPUDFMAPipe(latency: Int) extends Component
{
  val io = new ioFMA(65)
  
  val cmd = Reg() { Bits() }
  val rm = Reg() { Bits() }
  val in1 = Reg() { Bits() }
  val in2 = Reg() { Bits() }
  val in3 = Reg() { Bits() }

  val cmd_fma = io.cmd === FCMD_MADD  || io.cmd === FCMD_MSUB ||
                io.cmd === FCMD_NMADD || io.cmd === FCMD_NMSUB
  val cmd_addsub = io.cmd === FCMD_ADD || io.cmd === FCMD_SUB

  val one = Bits("h8000000000000000")
  val zero = Cat(io.in1(64) ^ io.in2(64), Bits(0, 64))

  when (io.valid) {
    cmd := Cat(io.cmd(1) & (cmd_fma || cmd_addsub), io.cmd(0))
    rm := io.rm
    in1 := io.in1
    in2 := Mux(cmd_addsub, one, io.in2)
    in3 := Mux(cmd_fma, io.in3, Mux(cmd_addsub, io.in2, zero))
  }

  val fma = new hardfloat.mulAddSubRecodedFloat64_1
  fma.io.op := cmd
  fma.io.roundingMode := rm
  fma.io.a := in1
  fma.io.b := in2
  fma.io.c := in3

  io.out := ShiftRegister(latency-1, fma.io.out)
  io.exc := ShiftRegister(latency-1, fma.io.exceptionFlags)
}

class rocketFPU(sfma_latency: Int, dfma_latency: Int) extends Component
{
  val io = new Bundle {
    val ctrl = new ioCtrlFPU().flip
    val dpath = new ioDpathFPU().flip
    val sfma = new ioFMA(33)
    val dfma = new ioFMA(65)
  }

  val ex_reg_inst = Reg() { Bits() }
  when (io.ctrl.valid) {
    ex_reg_inst := io.dpath.inst
  }
  val ex_reg_valid = Reg(io.ctrl.valid, Bool(false))

  val fp_decoder = new rocketFPUDecoder
  fp_decoder.io.inst := io.dpath.inst

  val ctrl = Reg() { new rocketFPUCtrlSigs }
  when (io.ctrl.valid) {
    ctrl := fp_decoder.io.sigs
  }
  val mem_ctrl = Reg(ctrl)
  val wb_ctrl = Reg(mem_ctrl)

  // load response
  val load_wb = Reg(io.dpath.dmem_resp_val, resetVal = Bool(false))
  val load_wb_single = Reg() { Bool() }
  val load_wb_data = Reg() { Bits(width = 64) } // XXX WTF why doesn't bit width inference work for the regfile?!
  val load_wb_tag = Reg() { UFix() }
  when (io.dpath.dmem_resp_val) {
    load_wb_single := io.dpath.dmem_resp_type === MT_W || io.dpath.dmem_resp_type === MT_WU
    load_wb_data := io.dpath.dmem_resp_data
    load_wb_tag := io.dpath.dmem_resp_tag
  }
  val rec_s = new hardfloat.float32ToRecodedFloat32
  val rec_d = new hardfloat.float64ToRecodedFloat64
  rec_s.io.in := load_wb_data
  rec_d.io.in := load_wb_data
  val sp_msbs = Fill(32, UFix(1,1))
  val load_wb_data_recoded = Mux(load_wb_single, Cat(sp_msbs, rec_s.io.out), rec_d.io.out)

  val fsr_rm = Reg() { Bits(width = 3) }
  val fsr_exc = Reg() { Bits(width = 5) }

  // regfile
  val regfile = Mem(32, load_wb, load_wb_tag, load_wb_data_recoded);
  regfile.setReadLatency(0);
  regfile.setTarget('inst);

  val ex_rs1 = regfile.read(ex_reg_inst(26,22))
  val ex_rs2 = regfile.read(ex_reg_inst(21,17))
  val ex_rs3 = regfile.read(ex_reg_inst(16,12))
  val ex_rm = Mux(ex_reg_inst(11,9) === Bits(7), fsr_rm, ex_reg_inst(11,9))

  val mem_reg_valid = Reg(ex_reg_valid && !io.ctrl.killx, resetVal = Bool(false))
  val mem_fromint_data = Reg() { Bits() }
  val mem_rs1 = Reg() { Bits() }
  val mem_rs2 = Reg() { Bits() }
  val mem_rs3 = Reg() { Bits() }
  val mem_rm = Reg() { Bits() }

  when (ex_reg_valid) {
    mem_rm := ex_rm
    when (ctrl.fromint || ctrl.wrfsr) {
      mem_fromint_data := io.dpath.fromint_data
    }
    when (ctrl.ren1) {
      mem_rs1 := ex_rs1
    }
    when (ctrl.store) {
      mem_rs1 := ex_rs2
    }
    when (ctrl.ren2) {
      mem_rs2 := ex_rs2
    }
    when (ctrl.ren3) {
      mem_rs3 := ex_rs3
    }
  }

  // currently we assume FP stores and FP->int ops take 1 cycle (MEM)
  val fpiu = new rocketFPIntUnit
  fpiu.io.single := mem_ctrl.single
  fpiu.io.cmd := mem_ctrl.cmd
  fpiu.io.rm := mem_rm
  fpiu.io.fsr := Cat(fsr_rm, fsr_exc)
  fpiu.io.in1 := mem_rs1
  fpiu.io.in2 := mem_rs2

  io.dpath.store_data := fpiu.io.store_data
  io.dpath.toint_data := fpiu.io.toint_data

  // 2-cycle pipe for int->FP and non-FMA FP->FP ops
  val fastpipe = new rocketFPUFastPipe
  fastpipe.io.single := mem_ctrl.single
  fastpipe.io.cmd := mem_ctrl.cmd
  fastpipe.io.rm := mem_rm
  fastpipe.io.fromint := mem_fromint_data
  fastpipe.io.in1 := mem_rs1
  fastpipe.io.in2 := mem_rs2
  fastpipe.io.lt_s := fpiu.io.lt_s
  fastpipe.io.lt_d := fpiu.io.lt_d

  val cmd_fma = mem_ctrl.cmd === FCMD_MADD  || mem_ctrl.cmd === FCMD_MSUB ||
                mem_ctrl.cmd === FCMD_NMADD || mem_ctrl.cmd === FCMD_NMSUB
  val cmd_addsub = mem_ctrl.cmd === FCMD_ADD || mem_ctrl.cmd === FCMD_SUB
  val sfma = new rocketFPUSFMAPipe(sfma_latency-1)
  sfma.io.valid := io.sfma.valid || mem_reg_valid && mem_ctrl.fma && mem_ctrl.single
  sfma.io.in1 := Mux(io.sfma.valid, io.sfma.in1, mem_rs1)
  sfma.io.in2 := Mux(io.sfma.valid, io.sfma.in2, mem_rs2)
  sfma.io.in3 := Mux(io.sfma.valid, io.sfma.in3, mem_rs3)
  sfma.io.cmd := Mux(io.sfma.valid, io.sfma.cmd, mem_ctrl.cmd)
  sfma.io.rm := Mux(io.sfma.valid, io.sfma.rm, mem_rm)
  io.sfma.out := sfma.io.out
  io.sfma.exc := sfma.io.exc

  val dfma = new rocketFPUDFMAPipe(dfma_latency-1)
  dfma.io.valid := io.dfma.valid || mem_reg_valid && mem_ctrl.fma && !mem_ctrl.single
  dfma.io.in1 := Mux(io.dfma.valid, io.dfma.in1, mem_rs1)
  dfma.io.in2 := Mux(io.dfma.valid, io.dfma.in2, mem_rs2)
  dfma.io.in3 := Mux(io.dfma.valid, io.dfma.in3, mem_rs3)
  dfma.io.cmd := Mux(io.dfma.valid, io.dfma.cmd, mem_ctrl.cmd)
  dfma.io.rm := Mux(io.dfma.valid, io.dfma.rm, mem_rm)
  io.dfma.out := dfma.io.out
  io.dfma.exc := dfma.io.exc

  val wb_reg_valid = Reg(mem_reg_valid && !io.ctrl.killm, resetVal = Bool(false))
  val wb_toint_exc = Reg(fpiu.io.exc)

  // writeback arbitration
  val wen = Reg(resetVal = Bits(0, dfma_latency))
  val winfo = Vec(dfma_latency-1) { Reg() { Bits() } }
  val mem_wen = Reg(resetVal = Bool(false))

  val fastpipe_latency = 2
  require(fastpipe_latency < sfma_latency && sfma_latency <= dfma_latency)
  val ex_stage_fu_latency = Mux(ctrl.fastpipe, UFix(fastpipe_latency-1),
                            Mux(ctrl.single, UFix(sfma_latency-1),
                                UFix(dfma_latency-1)))
  val mem_fu_latency = Reg(ex_stage_fu_latency - UFix(1))
  val write_port_busy = Reg(ctrl.fastpipe && wen(fastpipe_latency) ||
    Bool(sfma_latency < dfma_latency) && ctrl.fma && ctrl.single && wen(sfma_latency) ||
    mem_wen && mem_fu_latency === ex_stage_fu_latency)
  mem_wen := ex_reg_valid && !io.ctrl.killx && (ctrl.fma || ctrl.fastpipe)
  val ex_stage_wsrc = Cat(ctrl.fastpipe, ctrl.single)
  val mem_winfo = Reg(Cat(ex_reg_inst(31,27), ex_stage_wsrc))

  for (i <- 0 until dfma_latency-2) {
    winfo(i) := winfo(i+1)
  }
  wen := wen >> UFix(1)
  when (mem_wen) {
    when (!io.ctrl.killm) {
      wen := (wen >> UFix(1)) | (UFix(1) << mem_fu_latency)
    }
    for (i <- 0 until dfma_latency-1) {
      when (!write_port_busy && UFix(i) === mem_fu_latency) {
        winfo(i) := mem_winfo
      }
    }
  }

  val wsrc = winfo(0)(1,0)
  val wdata = Mux(wsrc === UFix(0), dfma.io.out, // DFMA
              Mux(wsrc === UFix(1), Cat(sp_msbs, sfma.io.out), // SFMA
              Mux(wsrc === UFix(2), fastpipe.io.out_d,
              Cat(sp_msbs, fastpipe.io.out_s))))
  val wexc = Mux(wsrc === UFix(0), dfma.io.exc, // DFMA
             Mux(wsrc === UFix(1), sfma.io.exc, // SFMA
             Mux(wsrc === UFix(2), fastpipe.io.exc_d,
             fastpipe.io.exc_s)))
  val waddr = winfo(0).toUFix >> UFix(2)
  regfile.write(waddr(4,0), wdata, wen(0))

  when (wb_reg_valid && wb_ctrl.toint || wen(0)) {
    fsr_exc := fsr_exc |
      Fill(fsr_exc.getWidth, wb_reg_valid && wb_ctrl.toint) & wb_toint_exc |
      Fill(fsr_exc.getWidth, wen(0)) & wexc
  }
  when (wb_reg_valid && wb_ctrl.wrfsr) {
    fsr_exc := fastpipe.io.out_s(4,0)
    fsr_rm := fastpipe.io.out_s(7,5)
  }

  val fp_inflight = wb_reg_valid && wb_ctrl.toint || wen.orR
  val fsr_busy = mem_ctrl.rdfsr && fp_inflight || wb_reg_valid && wb_ctrl.wrfsr
  val units_busy = mem_reg_valid && mem_ctrl.fma && (io.sfma.valid && mem_ctrl.single || io.dfma.valid && !mem_ctrl.single)
  io.ctrl.nack_mem := fsr_busy || units_busy || write_port_busy
  io.ctrl.dec <> fp_decoder.io.sigs
  // we don't currently support round-max-magnitude (rm=4)
  io.ctrl.illegal_rm := ex_rm(2)
  io.ctrl.sboard_clr := wen(0) && !wsrc(1).toBool // only for FMA pipes
  io.ctrl.sboard_clra := waddr
}
