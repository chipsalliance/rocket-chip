// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import Chisel._
import Chisel.ImplicitConversions._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.experimental._

case class FPUParams(
  fLen: Int = 64,
  divSqrt: Boolean = true,
  sfmaLatency: Int = 3,
  dfmaLatency: Int = 4
)

object FPConstants
{
  val RM_SZ = 3
  val FLAGS_SZ = 5
}
import FPConstants._

trait HasFPUCtrlSigs {
  val ldst = Bool()
  val wen = Bool()
  val ren1 = Bool()
  val ren2 = Bool()
  val ren3 = Bool()
  val swap12 = Bool()
  val swap23 = Bool()
  val singleIn = Bool()
  val singleOut = Bool()
  val fromint = Bool()
  val toint = Bool()
  val fastpipe = Bool()
  val fma = Bool()
  val div = Bool()
  val sqrt = Bool()
  val wflags = Bool()
}

class FPUCtrlSigs extends Bundle with HasFPUCtrlSigs

class FPUDecoder(implicit p: Parameters) extends FPUModule()(p) {
  val io = new Bundle {
    val inst = Bits(INPUT, 32)
    val sigs = new FPUCtrlSigs().asOutput
  }

  val default =       List(X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X)
  val f =
    Array(FLW      -> List(Y,Y,N,N,N,X,X,X,X,N,N,N,N,N,N,N),
          FSW      -> List(Y,N,N,Y,N,Y,X,N,Y,N,Y,N,N,N,N,N),
          FMV_S_X  -> List(N,Y,N,N,N,X,X,Y,N,Y,N,N,N,N,N,N),
          FCVT_S_W -> List(N,Y,N,N,N,X,X,Y,Y,Y,N,N,N,N,N,Y),
          FCVT_S_WU-> List(N,Y,N,N,N,X,X,Y,Y,Y,N,N,N,N,N,Y),
          FCVT_S_L -> List(N,Y,N,N,N,X,X,Y,Y,Y,N,N,N,N,N,Y),
          FCVT_S_LU-> List(N,Y,N,N,N,X,X,Y,Y,Y,N,N,N,N,N,Y),
          FMV_X_S  -> List(N,N,Y,N,N,N,X,N,Y,N,Y,N,N,N,N,N),
          FCLASS_S -> List(N,N,Y,N,N,N,X,Y,Y,N,Y,N,N,N,N,N),
          FCVT_W_S -> List(N,N,Y,N,N,N,X,Y,Y,N,Y,N,N,N,N,Y),
          FCVT_WU_S-> List(N,N,Y,N,N,N,X,Y,Y,N,Y,N,N,N,N,Y),
          FCVT_L_S -> List(N,N,Y,N,N,N,X,Y,Y,N,Y,N,N,N,N,Y),
          FCVT_LU_S-> List(N,N,Y,N,N,N,X,Y,Y,N,Y,N,N,N,N,Y),
          FEQ_S    -> List(N,N,Y,Y,N,N,N,Y,Y,N,Y,N,N,N,N,Y),
          FLT_S    -> List(N,N,Y,Y,N,N,N,Y,Y,N,Y,N,N,N,N,Y),
          FLE_S    -> List(N,N,Y,Y,N,N,N,Y,Y,N,Y,N,N,N,N,Y),
          FSGNJ_S  -> List(N,Y,Y,Y,N,N,N,Y,Y,N,N,Y,N,N,N,N),
          FSGNJN_S -> List(N,Y,Y,Y,N,N,N,Y,Y,N,N,Y,N,N,N,N),
          FSGNJX_S -> List(N,Y,Y,Y,N,N,N,Y,Y,N,N,Y,N,N,N,N),
          FMIN_S   -> List(N,Y,Y,Y,N,N,N,Y,Y,N,N,Y,N,N,N,Y),
          FMAX_S   -> List(N,Y,Y,Y,N,N,N,Y,Y,N,N,Y,N,N,N,Y),
          FADD_S   -> List(N,Y,Y,Y,N,N,Y,Y,Y,N,N,N,Y,N,N,Y),
          FSUB_S   -> List(N,Y,Y,Y,N,N,Y,Y,Y,N,N,N,Y,N,N,Y),
          FMUL_S   -> List(N,Y,Y,Y,N,N,N,Y,Y,N,N,N,Y,N,N,Y),
          FMADD_S  -> List(N,Y,Y,Y,Y,N,N,Y,Y,N,N,N,Y,N,N,Y),
          FMSUB_S  -> List(N,Y,Y,Y,Y,N,N,Y,Y,N,N,N,Y,N,N,Y),
          FNMADD_S -> List(N,Y,Y,Y,Y,N,N,Y,Y,N,N,N,Y,N,N,Y),
          FNMSUB_S -> List(N,Y,Y,Y,Y,N,N,Y,Y,N,N,N,Y,N,N,Y),
          FDIV_S   -> List(N,Y,Y,Y,N,N,N,Y,Y,N,N,N,N,Y,N,Y),
          FSQRT_S  -> List(N,Y,Y,N,N,N,X,Y,Y,N,N,N,N,N,Y,Y))
  val d =
    Array(FLD      -> List(Y,Y,N,N,N,X,X,X,N,N,N,N,N,N,N,N),
          FSD      -> List(Y,N,N,Y,N,Y,X,N,N,N,Y,N,N,N,N,N),
          FMV_D_X  -> List(N,Y,N,N,N,X,X,X,N,Y,N,N,N,N,N,N),
          FCVT_D_W -> List(N,Y,N,N,N,X,X,N,N,Y,N,N,N,N,N,Y),
          FCVT_D_WU-> List(N,Y,N,N,N,X,X,N,N,Y,N,N,N,N,N,Y),
          FCVT_D_L -> List(N,Y,N,N,N,X,X,N,N,Y,N,N,N,N,N,Y),
          FCVT_D_LU-> List(N,Y,N,N,N,X,X,N,N,Y,N,N,N,N,N,Y),
          FMV_X_D  -> List(N,N,Y,N,N,N,X,N,N,N,Y,N,N,N,N,N),
          FCLASS_D -> List(N,N,Y,N,N,N,X,N,N,N,Y,N,N,N,N,N),
          FCVT_W_D -> List(N,N,Y,N,N,N,X,N,N,N,Y,N,N,N,N,Y),
          FCVT_WU_D-> List(N,N,Y,N,N,N,X,N,N,N,Y,N,N,N,N,Y),
          FCVT_L_D -> List(N,N,Y,N,N,N,X,N,N,N,Y,N,N,N,N,Y),
          FCVT_LU_D-> List(N,N,Y,N,N,N,X,N,N,N,Y,N,N,N,N,Y),
          FCVT_S_D -> List(N,Y,Y,N,N,N,X,N,Y,N,N,Y,N,N,N,Y),
          FCVT_D_S -> List(N,Y,Y,N,N,N,X,Y,N,N,N,Y,N,N,N,Y),
          FEQ_D    -> List(N,N,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,Y),
          FLT_D    -> List(N,N,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,Y),
          FLE_D    -> List(N,N,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,Y),
          FSGNJ_D  -> List(N,Y,Y,Y,N,N,N,N,N,N,N,Y,N,N,N,N),
          FSGNJN_D -> List(N,Y,Y,Y,N,N,N,N,N,N,N,Y,N,N,N,N),
          FSGNJX_D -> List(N,Y,Y,Y,N,N,N,N,N,N,N,Y,N,N,N,N),
          FMIN_D   -> List(N,Y,Y,Y,N,N,N,N,N,N,N,Y,N,N,N,Y),
          FMAX_D   -> List(N,Y,Y,Y,N,N,N,N,N,N,N,Y,N,N,N,Y),
          FADD_D   -> List(N,Y,Y,Y,N,N,Y,N,N,N,N,N,Y,N,N,Y),
          FSUB_D   -> List(N,Y,Y,Y,N,N,Y,N,N,N,N,N,Y,N,N,Y),
          FMUL_D   -> List(N,Y,Y,Y,N,N,N,N,N,N,N,N,Y,N,N,Y),
          FMADD_D  -> List(N,Y,Y,Y,Y,N,N,N,N,N,N,N,Y,N,N,Y),
          FMSUB_D  -> List(N,Y,Y,Y,Y,N,N,N,N,N,N,N,Y,N,N,Y),
          FNMADD_D -> List(N,Y,Y,Y,Y,N,N,N,N,N,N,N,Y,N,N,Y),
          FNMSUB_D -> List(N,Y,Y,Y,Y,N,N,N,N,N,N,N,Y,N,N,Y),
          FDIV_D   -> List(N,Y,Y,Y,N,N,N,N,N,N,N,N,N,Y,N,Y),
          FSQRT_D  -> List(N,Y,Y,N,N,N,X,N,N,N,N,N,N,N,Y,Y))

  val insns = fLen match {
    case 32 => f
    case 64 => f ++ d
  }
  val decoder = DecodeLogic(io.inst, default, insns)
  val s = io.sigs
  val sigs = Seq(s.ldst, s.wen, s.ren1, s.ren2, s.ren3, s.swap12,
                 s.swap23, s.singleIn, s.singleOut, s.fromint, s.toint,
                 s.fastpipe, s.fma, s.div, s.sqrt, s.wflags)
  sigs zip decoder map {case(s,d) => s := d}
}

class FPUCoreIO(implicit p: Parameters) extends CoreBundle()(p) {
  val inst = Bits(INPUT, 32)
  val fromint_data = Bits(INPUT, xLen)

  val fcsr_rm = Bits(INPUT, FPConstants.RM_SZ)
  val fcsr_flags = Valid(Bits(width = FPConstants.FLAGS_SZ))

  val store_data = Bits(OUTPUT, fLen)
  val toint_data = Bits(OUTPUT, xLen)

  val dmem_resp_val = Bool(INPUT)
  val dmem_resp_type = Bits(INPUT, 3)
  val dmem_resp_tag = UInt(INPUT, 5)
  val dmem_resp_data = Bits(INPUT, fLen)

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

  val keep_clock_enabled = Bool(INPUT)
}

class FPUIO(implicit p: Parameters) extends FPUCoreIO ()(p) {
  val cp_req = Decoupled(new FPInput()).flip //cp doesn't pay attn to kill sigs
  val cp_resp = Decoupled(new FPResult())
}

class FPResult(implicit p: Parameters) extends CoreBundle()(p) {
  val data = Bits(width = fLen+1)
  val exc = Bits(width = FPConstants.FLAGS_SZ)
}

class IntToFPInput(implicit p: Parameters) extends CoreBundle()(p) with HasFPUCtrlSigs {
  val rm = Bits(width = FPConstants.RM_SZ)
  val typ = Bits(width = 2)
  val in1 = Bits(width = xLen)
}

class FPInput(implicit p: Parameters) extends CoreBundle()(p) with HasFPUCtrlSigs {
  val rm = Bits(width = FPConstants.RM_SZ)
  val fmaCmd = Bits(width = 2)
  val typ = Bits(width = 2)
  val in1 = Bits(width = fLen+1)
  val in2 = Bits(width = fLen+1)
  val in3 = Bits(width = fLen+1)

  override def cloneType = new FPInput().asInstanceOf[this.type]
}

case class FType(exp: Int, sig: Int) {
  def ieeeWidth = exp + sig
  def recodedWidth = ieeeWidth + 1

  def qNaN = UInt((BigInt(7) << (exp + sig - 3)) + (BigInt(1) << (sig - 2)), exp + sig + 1)
  def isNaN(x: UInt) = x(sig + exp - 1, sig + exp - 3).andR
  def isSNaN(x: UInt) = isNaN(x) && !x(sig - 2)

  def classify(x: UInt) = {
    val sign = x(sig + exp)
    val code = x(exp + sig - 1, exp + sig - 3)
    val codeHi = code(2, 1)
    val isSpecial = codeHi === UInt(3)

    val isHighSubnormalIn = x(exp + sig - 3, sig - 1) < UInt(2)
    val isSubnormal = code === UInt(1) || codeHi === UInt(1) && isHighSubnormalIn
    val isNormal = codeHi === UInt(1) && !isHighSubnormalIn || codeHi === UInt(2)
    val isZero = code === UInt(0)
    val isInf = isSpecial && !code(0)
    val isNaN = code.andR
    val isSNaN = isNaN && !x(sig-2)
    val isQNaN = isNaN && x(sig-2)

    Cat(isQNaN, isSNaN, isInf && !sign, isNormal && !sign,
        isSubnormal && !sign, isZero && !sign, isZero && sign,
        isSubnormal && sign, isNormal && sign, isInf && sign)
  }

  // convert between formats, ignoring rounding, range, NaN
  def unsafeConvert(x: UInt, to: FType) = if (this == to) x else {
    val sign = x(sig + exp)
    val fractIn = x(sig - 2, 0)
    val expIn = x(sig + exp - 1, sig - 1)
    val fractOut = fractIn << to.sig >> sig
    val expOut = {
      val expCode = expIn(exp, exp - 2)
      val commonCase = (expIn + (1 << to.exp)) - (1 << exp)
      Mux(expCode === 0 || expCode >= 6, Cat(expCode, commonCase(to.exp - 3, 0)), commonCase(to.exp, 0))
    }
    Cat(sign, expOut, fractOut)
  }

  def recode(x: UInt) = hardfloat.recFNFromFN(exp, sig, x)
  def ieee(x: UInt) = hardfloat.fNFromRecFN(exp, sig, x)
}

object FType {
  val S = new FType(8, 24)
  val D = new FType(11, 53)

  val all = List(S, D)
}

trait HasFPUParameters {
  require(fLen == 32 || fLen == 64)
  val fLen: Int
  def xLen: Int
  val minXLen = 32
  val nIntTypes = log2Ceil(xLen/minXLen) + 1
  val floatTypes = FType.all.filter(_.ieeeWidth <= fLen)
  val minType = floatTypes.head
  val maxType = floatTypes.last
  def prevType(t: FType) = floatTypes(typeTag(t) - 1)
  val maxExpWidth = maxType.exp
  val maxSigWidth = maxType.sig
  def typeTag(t: FType) = floatTypes.indexOf(t)

  private def isBox(x: UInt, t: FType): Bool = x(t.sig + t.exp, t.sig + t.exp - 4).andR

  private def box(x: UInt, xt: FType, y: UInt, yt: FType): UInt = {
    require(xt.ieeeWidth == 2 * yt.ieeeWidth)
    val swizzledNaN = Cat(
      x(xt.sig + xt.exp, xt.sig + xt.exp - 3),
      x(xt.sig - 2, yt.recodedWidth - 1).andR,
      x(xt.sig + xt.exp - 5, xt.sig),
      y(yt.recodedWidth - 2),
      x(xt.sig - 2, yt.recodedWidth - 1),
      y(yt.recodedWidth - 1),
      y(yt.recodedWidth - 3, 0))
    Mux(xt.isNaN(x), swizzledNaN, x)
  }

  // implement NaN unboxing for FU inputs
  def unbox(x: UInt, tag: UInt, exactType: Option[FType]): UInt = {
    val outType = exactType.getOrElse(maxType)
    def helper(x: UInt, t: FType): Seq[(Bool, UInt)] = {
      val prev =
        if (t == minType) {
          Seq()
        } else {
          val prevT = prevType(t)
          val unswizzled = Cat(
            x(prevT.sig + prevT.exp - 1),
            x(t.sig - 1),
            x(prevT.sig + prevT.exp - 2, 0))
          val prev = helper(unswizzled, prevT)
          val isbox = isBox(x, t)
          prev.map(p => (isbox && p._1, p._2))
        }
      prev :+ (true.B, t.unsafeConvert(x, outType))
    }

    val (oks, floats) = helper(x, maxType).unzip
    if (exactType.isEmpty || floatTypes.size == 1) {
      Mux(oks(tag), floats(tag), maxType.qNaN)
    } else {
      val t = exactType.get
      floats(typeTag(t)) | Mux(oks(typeTag(t)), 0.U, t.qNaN)
    }
  }

  // make sure that the redundant bits in the NaN-boxed encoding are consistent
  def consistent(x: UInt): Bool = {
    def helper(x: UInt, t: FType): Bool = if (typeTag(t) == 0) true.B else {
      val prevT = prevType(t)
      val unswizzled = Cat(
        x(prevT.sig + prevT.exp - 1),
        x(t.sig - 1),
        x(prevT.sig + prevT.exp - 2, 0))
      val prevOK = !isBox(x, t) || helper(unswizzled, prevT)
      val curOK = !t.isNaN(x) || x(t.sig + t.exp - 4) === x(t.sig - 2, prevT.recodedWidth - 1).andR
      prevOK && curOK
    }
    helper(x, maxType)
  }

  // generate a NaN box from an FU result
  def box(x: UInt, t: FType): UInt = {
    if (t == maxType) {
      x
    } else {
      val nt = floatTypes(typeTag(t) + 1)
      val bigger = box(UInt((BigInt(1) << nt.recodedWidth)-1), nt, x, t)
      bigger | UInt((BigInt(1) << maxType.recodedWidth) - (BigInt(1) << nt.recodedWidth))
    }
  }

  // generate a NaN box from an FU result
  def box(x: UInt, tag: UInt): UInt = {
    val opts = floatTypes.map(t => box(x, t))
    opts(tag)
  }

  // zap bits that hardfloat thinks are don't-cares, but we do care about
  def sanitizeNaN(x: UInt, t: FType): UInt = {
    if (typeTag(t) == 0) {
      x
    } else {
      val maskedNaN = x & ~UInt((BigInt(1) << (t.sig-1)) | (BigInt(1) << (t.sig+t.exp-4)), t.recodedWidth)
      Mux(t.isNaN(x), maskedNaN, x)
    }
  }

  // implement NaN boxing and recoding for FL*/fmv.*.x
  def recode(x: UInt, tag: UInt): UInt = {
    def helper(x: UInt, t: FType): UInt = {
      if (typeTag(t) == 0) {
        t.recode(x)
      } else {
        val prevT = prevType(t)
        box(t.recode(x), t, helper(x, prevT), prevT)
      }
    }

    // fill MSBs of subword loads to emulate a wider load of a NaN-boxed value
    val boxes = floatTypes.map(t => UInt((BigInt(1) << maxType.ieeeWidth) - (BigInt(1) << t.ieeeWidth)))
    helper(boxes(tag) | x, maxType)
  }

  // implement NaN unboxing and un-recoding for FS*/fmv.x.*
  def ieee(x: UInt, t: FType = maxType): UInt = {
    if (typeTag(t) == 0) {
      t.ieee(x)
    } else {
      val unrecoded = t.ieee(x)
      val prevT = prevType(t)
      val prevRecoded = Cat(
        x(prevT.recodedWidth-2),
        x(t.sig-1),
        x(prevT.recodedWidth-3, 0))
      val prevUnrecoded = ieee(prevRecoded, prevT)
      Cat(unrecoded >> prevT.ieeeWidth, Mux(t.isNaN(x), prevUnrecoded, unrecoded(prevT.ieeeWidth-1, 0)))
    }
  }
}

abstract class FPUModule(implicit p: Parameters) extends CoreModule()(p) with HasFPUParameters

class FPToInt(implicit p: Parameters) extends FPUModule()(p) with ShouldBeRetimed {
  class Output extends Bundle {
    val in = new FPInput
    val lt = Bool()
    val store = Bits(width = fLen)
    val toint = Bits(width = xLen)
    val exc = Bits(width = FPConstants.FLAGS_SZ)
    override def cloneType = new Output().asInstanceOf[this.type]
  }
  val io = new Bundle {
    val in = Valid(new FPInput).flip
    val out = Valid(new Output)
  }

  val in = RegEnable(io.in.bits, io.in.valid)
  val valid = Reg(next=io.in.valid)

  val dcmp = Module(new hardfloat.CompareRecFN(maxExpWidth, maxSigWidth))
  dcmp.io.a := in.in1
  dcmp.io.b := in.in2
  dcmp.io.signaling := !in.rm(1)

  val tag = !in.singleOut // TODO typeTag
  val store = ieee(in.in1)
  val toint = Wire(init = store)
  val intType = Wire(init = tag)
  io.out.bits.store := (floatTypes.map(t => Fill(maxType.ieeeWidth / t.ieeeWidth, store(t.ieeeWidth - 1, 0))): Seq[UInt])(tag)
  io.out.bits.toint := ((0 until nIntTypes).map(i => toint((minXLen << i) - 1, 0).sextTo(xLen)): Seq[UInt])(intType)
  io.out.bits.exc := Bits(0)

  when (in.rm(0)) {
    val classify_out = (floatTypes.map(t => t.classify(maxType.unsafeConvert(in.in1, t))): Seq[UInt])(tag)
    toint := classify_out | (store >> minXLen << minXLen)
    intType := 0
  }

  when (in.wflags) { // feq/flt/fle, fcvt
    toint := (~in.rm & Cat(dcmp.io.lt, dcmp.io.eq)).orR | (store >> minXLen << minXLen)
    io.out.bits.exc := dcmp.io.exceptionFlags
    intType := 0

    when (!in.ren2) { // fcvt
      val cvtType = in.typ.extract(log2Ceil(nIntTypes), 1)
      intType := cvtType

      val conv = Module(new hardfloat.RecFNToIN(maxExpWidth, maxSigWidth, xLen))
      conv.io.in := in.in1
      conv.io.roundingMode := in.rm
      conv.io.signedOut := ~in.typ(0)
      toint := conv.io.out
      io.out.bits.exc := Cat(conv.io.intExceptionFlags(2, 1).orR, UInt(0, 3), conv.io.intExceptionFlags(0))

      for (i <- 0 until nIntTypes-1) {
        val w = minXLen << i
        when (cvtType === i) {
          val narrow = Module(new hardfloat.RecFNToIN(maxExpWidth, maxSigWidth, w))
          narrow.io.in := in.in1
          narrow.io.roundingMode := in.rm
          narrow.io.signedOut := ~in.typ(0)

          val excSign = in.in1(maxExpWidth + maxSigWidth) && !maxType.isNaN(in.in1)
          val excOut = Cat(conv.io.signedOut === excSign, Fill(w-1, !excSign))
          val invalid = conv.io.intExceptionFlags(2) || narrow.io.intExceptionFlags(1)
          when (invalid) { toint := Cat(conv.io.out >> w, excOut) }
          io.out.bits.exc := Cat(invalid, UInt(0, 3), !invalid && conv.io.intExceptionFlags(0))
        }
      }
    }
  }

  io.out.valid := valid
  io.out.bits.lt := dcmp.io.lt || (dcmp.io.a.asSInt < 0.S && dcmp.io.b.asSInt >= 0.S)
  io.out.bits.in := in
}

class IntToFP(val latency: Int)(implicit p: Parameters) extends FPUModule()(p) with ShouldBeRetimed {
  val io = new Bundle {
    val in = Valid(new IntToFPInput).flip
    val out = Valid(new FPResult)
  }

  val in = Pipe(io.in)
  val tag = !in.bits.singleIn // TODO typeTag

  val mux = Wire(new FPResult)
  mux.exc := Bits(0)
  mux.data := recode(in.bits.in1, !in.bits.singleIn)

  val intValue = {
    val res = Wire(init = in.bits.in1.asSInt)
    for (i <- 0 until nIntTypes-1) {
      val smallInt = in.bits.in1((minXLen << i) - 1, 0)
      when (in.bits.typ.extract(log2Ceil(nIntTypes), 1) === i) {
        res := Mux(in.bits.typ(0), smallInt.zext, smallInt.asSInt)
      }
    }
    res.asUInt
  }

  when (in.bits.wflags) { // fcvt
    // could be improved for RVD/RVQ with a single variable-position rounding
    // unit, rather than N fixed-position ones
    val i2fResults = for (t <- floatTypes) yield {
      val i2f = Module(new hardfloat.INToRecFN(xLen, t.exp, t.sig))
      i2f.io.signedIn := ~in.bits.typ(0)
      i2f.io.in := intValue
      i2f.io.roundingMode := in.bits.rm
      i2f.io.detectTininess := hardfloat.consts.tininess_afterRounding
      (sanitizeNaN(i2f.io.out, t), i2f.io.exceptionFlags)
    }

    val (data, exc) = i2fResults.unzip
    val dataPadded = data.init.map(d => Cat(data.last >> d.getWidth, d)) :+ data.last
    mux.data := dataPadded(tag)
    mux.exc := exc(tag)
  }

  io.out <> Pipe(in.valid, mux, latency-1)
}

class FPToFP(val latency: Int)(implicit p: Parameters) extends FPUModule()(p) with ShouldBeRetimed {
  val io = new Bundle {
    val in = Valid(new FPInput).flip
    val out = Valid(new FPResult)
    val lt = Bool(INPUT) // from FPToInt
  }

  val in = Pipe(io.in)

  val signNum = Mux(in.bits.rm(1), in.bits.in1 ^ in.bits.in2, Mux(in.bits.rm(0), ~in.bits.in2, in.bits.in2))
  val fsgnj = Cat(signNum(fLen), in.bits.in1(fLen-1, 0))

  val fsgnjMux = Wire(new FPResult)
  fsgnjMux.exc := UInt(0)
  fsgnjMux.data := fsgnj

  when (in.bits.wflags) { // fmin/fmax
    val isnan1 = maxType.isNaN(in.bits.in1)
    val isnan2 = maxType.isNaN(in.bits.in2)
    val isInvalid = maxType.isSNaN(in.bits.in1) || maxType.isSNaN(in.bits.in2)
    val isNaNOut = isnan1 && isnan2
    val isLHS = isnan2 || in.bits.rm(0) =/= io.lt && !isnan1
    fsgnjMux.exc := isInvalid << 4
    fsgnjMux.data := Mux(isNaNOut, maxType.qNaN, Mux(isLHS, in.bits.in1, in.bits.in2))
  }

  val inTag = !in.bits.singleIn // TODO typeTag
  val outTag = !in.bits.singleOut // TODO typeTag
  val mux = Wire(init = fsgnjMux)
  for (t <- floatTypes.init) {
    when (outTag === typeTag(t)) {
      mux.data := Cat(fsgnjMux.data >> t.recodedWidth, maxType.unsafeConvert(fsgnjMux.data, t))
    }
  }

  when (in.bits.wflags && !in.bits.ren2) { // fcvt
    if (floatTypes.size > 1) {
      // widening conversions simply canonicalize NaN operands
      val widened = Mux(maxType.isNaN(in.bits.in1), maxType.qNaN, in.bits.in1)
      fsgnjMux.data := widened
      fsgnjMux.exc := maxType.isSNaN(in.bits.in1) << 4

      // narrowing conversions require rounding (for RVQ, this could be
      // optimized to use a single variable-position rounding unit, rather
      // than two fixed-position ones)
      for (outType <- floatTypes.init) when (outTag === typeTag(outType) && (typeTag(outType) == 0 || outTag < inTag)) {
        val narrower = Module(new hardfloat.RecFNToRecFN(maxType.exp, maxType.sig, outType.exp, outType.sig))
        narrower.io.in := in.bits.in1
        narrower.io.roundingMode := in.bits.rm
        narrower.io.detectTininess := hardfloat.consts.tininess_afterRounding
        val narrowed = sanitizeNaN(narrower.io.out, outType)
        mux.data := Cat(fsgnjMux.data >> narrowed.getWidth, narrowed)
        mux.exc := narrower.io.exceptionFlags
      }
    }
  }

  io.out <> Pipe(in.valid, mux, latency-1)
}

class MulAddRecFNPipe(latency: Int, expWidth: Int, sigWidth: Int) extends Module
{
    require(latency<=2) 

    val io = new Bundle {
        val validin = Bool(INPUT)
        val op = Bits(INPUT, 2)
        val a = Bits(INPUT, expWidth + sigWidth + 1)
        val b = Bits(INPUT, expWidth + sigWidth + 1)
        val c = Bits(INPUT, expWidth + sigWidth + 1)
        val roundingMode   = UInt(INPUT, 3)
        val detectTininess = UInt(INPUT, 1)
        val out = Bits(OUTPUT, expWidth + sigWidth + 1)
        val exceptionFlags = Bits(OUTPUT, 5)
        val validout = Bool(OUTPUT)
    }

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------
    val mulAddRecFNToRaw_preMul =
        Module(new hardfloat.MulAddRecFNToRaw_preMul(expWidth, sigWidth))
    val mulAddRecFNToRaw_postMul =
        Module(new hardfloat.MulAddRecFNToRaw_postMul(expWidth, sigWidth))

    mulAddRecFNToRaw_preMul.io.op := io.op
    mulAddRecFNToRaw_preMul.io.a  := io.a
    mulAddRecFNToRaw_preMul.io.b  := io.b
    mulAddRecFNToRaw_preMul.io.c  := io.c

    val mulAddResult =
        (mulAddRecFNToRaw_preMul.io.mulAddA *
             mulAddRecFNToRaw_preMul.io.mulAddB) +&
            mulAddRecFNToRaw_preMul.io.mulAddC

    val valid_stage0 = Wire(Bool())
    val roundingMode_stage0 = Wire(UInt(width=3))
    val detectTininess_stage0 = Wire(UInt(width=1))
  
    val postmul_regs = if(latency>0) 1 else 0
    mulAddRecFNToRaw_postMul.io.fromPreMul   := Pipe(io.validin, mulAddRecFNToRaw_preMul.io.toPostMul, postmul_regs).bits
    mulAddRecFNToRaw_postMul.io.mulAddResult := Pipe(io.validin, mulAddResult, postmul_regs).bits
    mulAddRecFNToRaw_postMul.io.roundingMode := Pipe(io.validin, io.roundingMode, postmul_regs).bits
    roundingMode_stage0                      := Pipe(io.validin, io.roundingMode, postmul_regs).bits
    detectTininess_stage0                    := Pipe(io.validin, io.detectTininess, postmul_regs).bits
    valid_stage0                             := Pipe(io.validin, false.B, postmul_regs).valid
    
    //------------------------------------------------------------------------
    //------------------------------------------------------------------------
    val roundRawFNToRecFN = Module(new hardfloat.RoundRawFNToRecFN(expWidth, sigWidth, 0))

    val round_regs = if(latency==2) 1 else 0
    roundRawFNToRecFN.io.invalidExc         := Pipe(valid_stage0, mulAddRecFNToRaw_postMul.io.invalidExc, round_regs).bits
    roundRawFNToRecFN.io.in                 := Pipe(valid_stage0, mulAddRecFNToRaw_postMul.io.rawOut, round_regs).bits
    roundRawFNToRecFN.io.roundingMode       := Pipe(valid_stage0, roundingMode_stage0, round_regs).bits
    roundRawFNToRecFN.io.detectTininess     := Pipe(valid_stage0, detectTininess_stage0, round_regs).bits
    io.validout                             := Pipe(valid_stage0, false.B, round_regs).valid

    roundRawFNToRecFN.io.infiniteExc := Bool(false)

    io.out            := roundRawFNToRecFN.io.out
    io.exceptionFlags := roundRawFNToRecFN.io.exceptionFlags
}

class FPUFMAPipe(val latency: Int, val t: FType)
                (implicit p: Parameters) extends FPUModule()(p) with ShouldBeRetimed {
  require(latency>0)

  val io = new Bundle {
    val in = Valid(new FPInput).flip
    val out = Valid(new FPResult)
  }

  val valid = Reg(next=io.in.valid)
  val in = Reg(new FPInput)
  when (io.in.valid) {
    val one = UInt(1) << (t.sig + t.exp - 1)
    val zero = (io.in.bits.in1 ^ io.in.bits.in2) & (UInt(1) << (t.sig + t.exp))
    val cmd_fma = io.in.bits.ren3
    val cmd_addsub = io.in.bits.swap23
    in := io.in.bits
    when (cmd_addsub) { in.in2 := one }
    when (!(cmd_fma || cmd_addsub)) { in.in3 := zero }
  }

  val fma = Module(new MulAddRecFNPipe((latency-1) min 2, t.exp, t.sig))
  fma.io.validin := valid
  fma.io.op := in.fmaCmd
  fma.io.roundingMode := in.rm
  fma.io.detectTininess := hardfloat.consts.tininess_afterRounding
  fma.io.a := in.in1
  fma.io.b := in.in2
  fma.io.c := in.in3

  val res = Wire(new FPResult)
  res.data := sanitizeNaN(fma.io.out, t)
  res.exc := fma.io.exceptionFlags

  io.out := Pipe(fma.io.validout, res, (latency-3) max 0)
}

@chiselName
class FPU(cfg: FPUParams)(implicit p: Parameters) extends FPUModule()(p) {
  val io = new FPUIO

  val useClockGating = coreParams match {
    case r: RocketCoreParams => r.clockGate
    case _ => false
  }
  val clock_en_reg = Reg(Bool())
  val clock_en = clock_en_reg || io.cp_req.valid
  val gated_clock =
    if (!useClockGating) clock
    else ClockGate(clock, clock_en, "fpu_clock_gate")

  val fp_decoder = Module(new FPUDecoder)
  fp_decoder.io.inst := io.inst
  val id_ctrl = fp_decoder.io.sigs

  val ex_reg_valid = Reg(next=io.valid, init=Bool(false))
  val ex_reg_inst = RegEnable(io.inst, io.valid)
  val ex_reg_ctrl = RegEnable(id_ctrl, io.valid)
  val ex_ra = List.fill(3)(Reg(UInt()))

  // load response
  val load_wb = Reg(next=io.dmem_resp_val)
  val load_wb_double = RegEnable(io.dmem_resp_type(0), io.dmem_resp_val)
  val load_wb_data = RegEnable(io.dmem_resp_data, io.dmem_resp_val)
  val load_wb_tag = RegEnable(io.dmem_resp_tag, io.dmem_resp_val)

  @chiselName class FPUImpl { // entering gated-clock domain

  val req_valid = ex_reg_valid || io.cp_req.valid
  val ex_cp_valid = io.cp_req.fire()
  val mem_cp_valid = Reg(next=ex_cp_valid, init=Bool(false))
  val wb_cp_valid = Reg(next=mem_cp_valid, init=Bool(false))
  val mem_reg_valid = RegInit(false.B)
  val killm = (io.killm || io.nack_mem) && !mem_cp_valid
  // Kill X-stage instruction if M-stage is killed.  This prevents it from
  // speculatively being sent to the div-sqrt unit, which can cause priority
  // inversion for two back-to-back divides, the first of which is killed.
  val killx = io.killx || mem_reg_valid && killm
  mem_reg_valid := ex_reg_valid && !killx || ex_cp_valid
  val mem_reg_inst = RegEnable(ex_reg_inst, ex_reg_valid)
  val wb_reg_valid = Reg(next=mem_reg_valid && (!killm || mem_cp_valid), init=Bool(false))

  val cp_ctrl = Wire(new FPUCtrlSigs)
  cp_ctrl <> io.cp_req.bits
  io.cp_resp.valid := Bool(false)
  io.cp_resp.bits.data := UInt(0)

  val ex_ctrl = Mux(ex_cp_valid, cp_ctrl, ex_reg_ctrl)
  val mem_ctrl = RegEnable(ex_ctrl, req_valid)
  val wb_ctrl = RegEnable(mem_ctrl, mem_reg_valid)

  // regfile
  val regfile = Mem(32, Bits(width = fLen+1))
  when (load_wb) {
    val wdata = recode(load_wb_data, load_wb_double)
    regfile(load_wb_tag) := wdata
    assert(consistent(wdata))
    if (enableCommitLog)
      printf("f%d p%d 0x%x\n", load_wb_tag, load_wb_tag + 32, load_wb_data)
  }

  val ex_rs = ex_ra.map(a => regfile(a))
  when (io.valid) {
    when (id_ctrl.ren1) {
      when (!id_ctrl.swap12) { ex_ra(0) := io.inst(19,15) }
      when (id_ctrl.swap12) { ex_ra(1) := io.inst(19,15) }
    }
    when (id_ctrl.ren2) {
      when (id_ctrl.swap12) { ex_ra(0) := io.inst(24,20) }
      when (id_ctrl.swap23) { ex_ra(2) := io.inst(24,20) }
      when (!id_ctrl.swap12 && !id_ctrl.swap23) { ex_ra(1) := io.inst(24,20) }
    }
    when (id_ctrl.ren3) { ex_ra(2) := io.inst(31,27) }
  }
  val ex_rm = Mux(ex_reg_inst(14,12) === Bits(7), io.fcsr_rm, ex_reg_inst(14,12))

  def fuInput(minT: Option[FType]): FPInput = {
    val req = Wire(new FPInput)
    val tag = !ex_ctrl.singleIn // TODO typeTag
    req := ex_ctrl
    req.rm := ex_rm
    req.in1 := unbox(ex_rs(0), tag, minT)
    req.in2 := unbox(ex_rs(1), tag, minT)
    req.in3 := unbox(ex_rs(2), tag, minT)
    req.typ := ex_reg_inst(21,20)
    req.fmaCmd := ex_reg_inst(3,2) | (!ex_ctrl.ren3 && ex_reg_inst(27))
    when (ex_cp_valid) {
      req := io.cp_req.bits
      when (io.cp_req.bits.swap23) {
        req.in2 := io.cp_req.bits.in3
        req.in3 := io.cp_req.bits.in2
      }
    }
    req
  }

  val sfma = Module(new FPUFMAPipe(cfg.sfmaLatency, FType.S))
  sfma.io.in.valid := req_valid && ex_ctrl.fma && ex_ctrl.singleOut
  sfma.io.in.bits := fuInput(Some(sfma.t))

  val fpiu = Module(new FPToInt)
  fpiu.io.in.valid := req_valid && (ex_ctrl.toint || ex_ctrl.div || ex_ctrl.sqrt || (ex_ctrl.fastpipe && ex_ctrl.wflags))
  fpiu.io.in.bits := fuInput(None)
  io.store_data := fpiu.io.out.bits.store
  io.toint_data := fpiu.io.out.bits.toint
  when(fpiu.io.out.valid && mem_cp_valid && mem_ctrl.toint){
    io.cp_resp.bits.data := fpiu.io.out.bits.toint
    io.cp_resp.valid := Bool(true)
  }

  val ifpu = Module(new IntToFP(2))
  ifpu.io.in.valid := req_valid && ex_ctrl.fromint
  ifpu.io.in.bits := fpiu.io.in.bits
  ifpu.io.in.bits.in1 := Mux(ex_cp_valid, io.cp_req.bits.in1, io.fromint_data)

  val fpmu = Module(new FPToFP(2))
  fpmu.io.in.valid := req_valid && ex_ctrl.fastpipe
  fpmu.io.in.bits := fpiu.io.in.bits
  fpmu.io.lt := fpiu.io.out.bits.lt

  val divSqrt_wen = Wire(init = false.B)
  val divSqrt_inFlight = Wire(init = false.B)
  val divSqrt_waddr = Reg(UInt(width = 5))
  val divSqrt_typeTag = Wire(UInt(width = log2Up(floatTypes.size)))
  val divSqrt_wdata = Wire(UInt(width = fLen+1))
  val divSqrt_flags = Wire(UInt(width = FPConstants.FLAGS_SZ))

  // writeback arbitration
  case class Pipe(p: Module, lat: Int, cond: (FPUCtrlSigs) => Bool, res: FPResult)
  val pipes = List(
    Pipe(fpmu, fpmu.latency, (c: FPUCtrlSigs) => c.fastpipe, fpmu.io.out.bits),
    Pipe(ifpu, ifpu.latency, (c: FPUCtrlSigs) => c.fromint, ifpu.io.out.bits),
    Pipe(sfma, sfma.latency, (c: FPUCtrlSigs) => c.fma && c.singleOut, sfma.io.out.bits)) ++
    (fLen > 32).option({
          val dfma = Module(new FPUFMAPipe(cfg.dfmaLatency, FType.D))
          dfma.io.in.valid := req_valid && ex_ctrl.fma && !ex_ctrl.singleOut
          dfma.io.in.bits := fuInput(Some(dfma.t))
          Pipe(dfma, dfma.latency, (c: FPUCtrlSigs) => c.fma && !c.singleOut, dfma.io.out.bits)
        })
  def latencyMask(c: FPUCtrlSigs, offset: Int) = {
    require(pipes.forall(_.lat >= offset))
    pipes.map(p => Mux(p.cond(c), UInt(1 << p.lat-offset), UInt(0))).reduce(_|_)
  }
  def pipeid(c: FPUCtrlSigs) = pipes.zipWithIndex.map(p => Mux(p._1.cond(c), UInt(p._2), UInt(0))).reduce(_|_)
  val maxLatency = pipes.map(_.lat).max
  val memLatencyMask = latencyMask(mem_ctrl, 2)

  class WBInfo extends Bundle {
    val rd = UInt(width = 5)
    val single = Bool()
    val cp = Bool()
    val pipeid = UInt(width = log2Ceil(pipes.size))
    override def cloneType: this.type = new WBInfo().asInstanceOf[this.type]
  }

  val wen = Reg(init=Bits(0, maxLatency-1))
  val wbInfo = Reg(Vec(maxLatency-1, new WBInfo))
  val mem_wen = mem_reg_valid && (mem_ctrl.fma || mem_ctrl.fastpipe || mem_ctrl.fromint)
  val write_port_busy = RegEnable(mem_wen && (memLatencyMask & latencyMask(ex_ctrl, 1)).orR || (wen & latencyMask(ex_ctrl, 0)).orR, req_valid)
  ccover(mem_reg_valid && write_port_busy, "WB_STRUCTURAL", "structural hazard on writeback")

  for (i <- 0 until maxLatency-2) {
    when (wen(i+1)) { wbInfo(i) := wbInfo(i+1) }
  }
  wen := wen >> 1
  when (mem_wen) {
    when (!killm) {
      wen := wen >> 1 | memLatencyMask
    }
    for (i <- 0 until maxLatency-1) {
      when (!write_port_busy && memLatencyMask(i)) {
        wbInfo(i).cp := mem_cp_valid
        wbInfo(i).single := mem_ctrl.singleOut
        wbInfo(i).pipeid := pipeid(mem_ctrl)
        wbInfo(i).rd := mem_reg_inst(11,7)
      }
    }
  }

  val waddr = Mux(divSqrt_wen, divSqrt_waddr, wbInfo(0).rd)
  val wdouble = Mux(divSqrt_wen, divSqrt_typeTag, !wbInfo(0).single)
  val wdata = box(Mux(divSqrt_wen, divSqrt_wdata, (pipes.map(_.res.data): Seq[UInt])(wbInfo(0).pipeid)), wdouble)
  val wexc = (pipes.map(_.res.exc): Seq[UInt])(wbInfo(0).pipeid)
  when ((!wbInfo(0).cp && wen(0)) || divSqrt_wen) {
    assert(consistent(wdata))
    regfile(waddr) := wdata
    if (enableCommitLog) {
      printf("f%d p%d 0x%x\n", waddr, waddr + 32, ieee(wdata))
    }
  }
  when (wbInfo(0).cp && wen(0)) {
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

  val divSqrt_write_port_busy = (mem_ctrl.div || mem_ctrl.sqrt) && wen.orR
  io.fcsr_rdy := !(ex_reg_valid && ex_ctrl.wflags || mem_reg_valid && mem_ctrl.wflags || wb_reg_valid && wb_ctrl.toint || wen.orR || divSqrt_inFlight)
  io.nack_mem := write_port_busy || divSqrt_write_port_busy || divSqrt_inFlight
  io.dec <> fp_decoder.io.sigs
  def useScoreboard(f: ((Pipe, Int)) => Bool) = pipes.zipWithIndex.filter(_._1.lat > 3).map(x => f(x)).fold(Bool(false))(_||_)
  io.sboard_set := wb_reg_valid && !wb_cp_valid && Reg(next=useScoreboard(_._1.cond(mem_ctrl)) || mem_ctrl.div || mem_ctrl.sqrt)
  io.sboard_clr := !wb_cp_valid && (divSqrt_wen || (wen(0) && useScoreboard(x => wbInfo(0).pipeid === UInt(x._2))))
  io.sboard_clra := waddr
  ccover(io.sboard_clr && load_wb, "DUAL_WRITEBACK", "load and FMA writeback on same cycle")
  // we don't currently support round-max-magnitude (rm=4)
  io.illegal_rm := io.inst(14,12).isOneOf(5, 6) || io.inst(14,12) === 7 && io.fcsr_rm >= 5

  if (cfg.divSqrt) {
    val divSqrt_killed = Reg(Bool())
    ccover(divSqrt_inFlight && divSqrt_killed, "DIV_KILLED", "divide killed after issued to divider")
    ccover(divSqrt_inFlight && mem_reg_valid && (mem_ctrl.div || mem_ctrl.sqrt), "DIV_BUSY", "divider structural hazard")
    ccover(mem_reg_valid && divSqrt_write_port_busy, "DIV_WB_STRUCTURAL", "structural hazard on division writeback")

    for (t <- floatTypes) {
      val tag = !mem_ctrl.singleOut // TODO typeTag
      val divSqrt = Module(new hardfloat.DivSqrtRecFN_small(t.exp, t.sig, 0))
      divSqrt.io.inValid := mem_reg_valid && tag === typeTag(t) && (mem_ctrl.div || mem_ctrl.sqrt) && !divSqrt_inFlight
      divSqrt.io.sqrtOp := mem_ctrl.sqrt
      divSqrt.io.a := maxType.unsafeConvert(fpiu.io.out.bits.in.in1, t)
      divSqrt.io.b := maxType.unsafeConvert(fpiu.io.out.bits.in.in2, t)
      divSqrt.io.roundingMode := fpiu.io.out.bits.in.rm
      divSqrt.io.detectTininess := hardfloat.consts.tininess_afterRounding

      when (!divSqrt.io.inReady) { divSqrt_inFlight := true } // only 1 in flight

      when (divSqrt.io.inValid && divSqrt.io.inReady) {
        divSqrt_killed := killm
        divSqrt_waddr := mem_reg_inst(11,7)
      }

      when (divSqrt.io.outValid_div || divSqrt.io.outValid_sqrt) {
        divSqrt_wen := !divSqrt_killed
        divSqrt_wdata := sanitizeNaN(divSqrt.io.out, t)
        divSqrt_flags := divSqrt.io.exceptionFlags
        divSqrt_typeTag := typeTag(t)
      }
    }
  } else {
    when (id_ctrl.div || id_ctrl.sqrt) { io.illegal_rm := true }
  }

  // gate the clock
  clock_en_reg := !useClockGating ||
    io.keep_clock_enabled || // chicken bit
    io.valid || // ID stage
    req_valid || // EX stage
    mem_reg_valid || mem_cp_valid || // MEM stage
    wb_reg_valid || wb_cp_valid || // WB stage
    wen.orR || divSqrt_inFlight || // post-WB stage
    io.dmem_resp_val // load writeback

  } // leaving gated-clock domain
  val fpuImpl = withClock (gated_clock) { new FPUImpl }

  def ccover(cond: Bool, label: String, desc: String)(implicit sourceInfo: SourceInfo) =
    cover(cond, s"FPU_$label", "Core;;" + desc)
}
