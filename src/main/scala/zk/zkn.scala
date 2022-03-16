// See LICENSE.SiFive for license details.

package freechips.rocketchip.zk

import chisel3._
import chisel3.util.{BitPat, HasBlackBoxInline}

// utils
object barrel {

  /** A Barrel Shifter implementation for Vec type.
    *
    * @param inputs           input signal to be shifted, should be a [[Vec]] type.
    * @param shiftInput       input signal to indicate the shift number, encoded in UInt.
    * @param shiftType        [[ShiftType]] to indicate the type of shifter.
    * @param shiftGranularity how many bits will be resolved in each layer.
    *                         For a smaller `shiftGranularity`, latency will be high, but area is smaller.
    *                         For a large `shiftGranularity`, latency will be low, but area is higher.
    */
  def apply[T <: Data](inputs: Vec[T], shiftInput: UInt, shiftType: ShiftType, shiftGranularity: Int = 1): Vec[T] = {
    val elementType: T = chiselTypeOf(inputs.head)
    shiftInput
      .asBools()
      .grouped(shiftGranularity)
      .map(VecInit(_).asUInt())
      .zipWithIndex
      .foldLeft(inputs) {
        case (prev, (shiftBits, layer)) =>
          Mux1H(
            UIntToOH(shiftBits),
            Seq.tabulate(1 << shiftBits.getWidth)(i => {
              // shift no more than inputs length
              // prev.drop will not warn about overflow!
              val layerShift: Int = (i * (1 << (layer * shiftGranularity))).min(prev.length)
              VecInit(shiftType match {
                case LeftRotate =>
                  prev.drop(layerShift) ++ prev.take(layerShift)
                case LeftShift =>
                  prev.drop(layerShift) ++ Seq.fill(layerShift)(0.U.asTypeOf(elementType))
                case RightRotate =>
                  prev.takeRight(layerShift) ++ prev.dropRight(layerShift)
                case RightShift =>
                  Seq.fill(layerShift)(0.U.asTypeOf(elementType)) ++ prev.dropRight(layerShift)
              })
            })
          )
      }
  }

  def leftShift[T <: Data](inputs: Vec[T], shift: UInt, layerSize: Int = 1): Vec[T] =
    apply(inputs, shift, LeftShift, layerSize)

  def rightShift[T <: Data](inputs: Vec[T], shift: UInt, layerSize: Int = 1): Vec[T] =
    apply(inputs, shift, RightShift, layerSize)

  def leftRotate[T <: Data](inputs: Vec[T], shift: UInt, layerSize: Int = 1): Vec[T] =
    apply(inputs, shift, LeftRotate, layerSize)

  def rightRotate[T <: Data](inputs: Vec[T], shift: UInt, layerSize: Int = 1): Vec[T] =
    apply(inputs, shift, RightRotate, layerSize)
}

object ZKN {
  val AES32DSI    = BitPat("b??10101??????????000?????0110011")
  val AES32DSMI   = BitPat("b??10111??????????000?????0110011")
  val AES32ESI    = BitPat("b??10001??????????000?????0110011")
  val AES32ESMI   = BitPat("b??10011??????????000?????0110011")
  val AES64DS     = BitPat("b0011101??????????000?????0110011")
  val AES64DSM    = BitPat("b0011111??????????000?????0110011")
  val AES64ES     = BitPat("b0011001??????????000?????0110011")
  val AES64ESM    = BitPat("b0011011??????????000?????0110011")
  val AES64IM     = BitPat("b001100000000?????001?????0010011")
  val AES64KS1I   = BitPat("b00110001?????????001?????0010011")
  val AES64KS2    = BitPat("b0111111??????????000?????0110011")
  val SHA256SUM0  = BitPat("b000100000000?????001?????0010011")
  val SHA256SUM1  = BitPat("b000100000001?????001?????0010011")
  val SHA256SIG0  = BitPat("b000100000010?????001?????0010011")
  val SHA256SIG1  = BitPat("b000100000011?????001?????0010011")
  val SHA512SUM0  = BitPat("b000100000100?????001?????0010011")
  val SHA512SUM1  = BitPat("b000100000101?????001?????0010011")
  val SHA512SIG0  = BitPat("b000100000110?????001?????0010011")
  val SHA512SIG1  = BitPat("b000100000111?????001?????0010011")
  val SHA512SUM0R = BitPat("b0101000??????????000?????0110011")
  val SHA512SUM1R = BitPat("b0101001??????????000?????0110011")
  val SHA512SIG0L = BitPat("b0101010??????????000?????0110011")
  val SHA512SIG1L = BitPat("b0101011??????????000?????0110011")
  val SHA512SIG0H = BitPat("b0101110??????????000?????0110011")
  val SHA512SIG1H = BitPat("b0101111??????????000?????0110011")

  val FN_Len         = 4
  def FN_AES_DS      =  0.U(FN_Len.W)
  def FN_AES_DSM     =  1.U(FN_Len.W)
  def FN_AES_ES      =  2.U(FN_Len.W)
  def FN_AES_ESM     =  3.U(FN_Len.W)
  def FN_AES_IM      =  4.U(FN_Len.W)
  def FN_AES_KS1     =  5.U(FN_Len.W)
  def FN_AES_KS2     =  6.U(FN_Len.W)
  def FN_SHA256_SIG0 =  7.U(FN_Len.W)
  def FN_SHA256_SIG1 =  8.U(FN_Len.W)
  def FN_SHA256_SUM0 =  9.U(FN_Len.W)
  def FN_SHA256_SUM1 = 10.U(FN_Len.W)
  def FN_SHA512_SIG0 = 11.U(FN_Len.W)
  def FN_SHA512_SIG1 = 12.U(FN_Len.W)
  def FN_SHA512_SUM0 = 13.U(FN_Len.W)
  def FN_SHA512_SUM1 = 14.U(FN_Len.W)
}

class ZKNInterface(xLen: Int) extends Bundle {
  val zkn_fn = Input(UInt(ZKN.FN_Len.W))
  val valid  = Input(Bool())
  val hl     = Input(Bool())
  val bs     = Input(UInt(2.W))
  val rcon   = Input(UInt(4.W))
  val rs1    = Input(UInt(xLen.W))
  val rs2    = Input(UInt(xLen.W))
  val rd     = Output(UInt(xLen.W))
}

object AES {
  val enc: Seq[Int] = Seq(
    0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
    0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
    0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
    0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
    0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
    0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
    0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
    0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
    0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
    0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
    0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
    0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
    0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
    0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
    0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
    0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16
  )

  val dec: Seq[Int] = Seq(
    0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38, 0xbf, 0x40, 0xa3, 0x9e, 0x81, 0xf3, 0xd7, 0xfb,
    0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87, 0x34, 0x8e, 0x43, 0x44, 0xc4, 0xde, 0xe9, 0xcb,
    0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 0x3d, 0xee, 0x4c, 0x95, 0x0b, 0x42, 0xfa, 0xc3, 0x4e,
    0x08, 0x2e, 0xa1, 0x66, 0x28, 0xd9, 0x24, 0xb2, 0x76, 0x5b, 0xa2, 0x49, 0x6d, 0x8b, 0xd1, 0x25,
    0x72, 0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xd4, 0xa4, 0x5c, 0xcc, 0x5d, 0x65, 0xb6, 0x92,
    0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda, 0x5e, 0x15, 0x46, 0x57, 0xa7, 0x8d, 0x9d, 0x84,
    0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a, 0xf7, 0xe4, 0x58, 0x05, 0xb8, 0xb3, 0x45, 0x06,
    0xd0, 0x2c, 0x1e, 0x8f, 0xca, 0x3f, 0x0f, 0x02, 0xc1, 0xaf, 0xbd, 0x03, 0x01, 0x13, 0x8a, 0x6b,
    0x3a, 0x91, 0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea, 0x97, 0xf2, 0xcf, 0xce, 0xf0, 0xb4, 0xe6, 0x73,
    0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85, 0xe2, 0xf9, 0x37, 0xe8, 0x1c, 0x75, 0xdf, 0x6e,
    0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89, 0x6f, 0xb7, 0x62, 0x0e, 0xaa, 0x18, 0xbe, 0x1b,
    0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 0x79, 0x20, 0x9a, 0xdb, 0xc0, 0xfe, 0x78, 0xcd, 0x5a, 0xf4,
    0x1f, 0xdd, 0xa8, 0x33, 0x88, 0x07, 0xc7, 0x31, 0xb1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xec, 0x5f,
    0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d, 0x2d, 0xe5, 0x7a, 0x9f, 0x93, 0xc9, 0x9c, 0xef,
    0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0, 0xc8, 0xeb, 0xbb, 0x3c, 0x83, 0x53, 0x99, 0x61,
    0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 0x26, 0xe1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0c, 0x7d
  )

  val rcon: Seq[Int] = Seq(
    0x01, 0x02, 0x04, 0x08,
    0x10, 0x20, 0x40, 0x80,
    0x1b, 0x36, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00
  )
}

class SBox(table: Seq[Int]) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(8.W))
    val out = Output(UInt(8.W))
  })

  // TODO some note on optimization: constant time look up!
  io.out := VecInit(table.map(_.U(8.W)).toSeq)(io.in)
}

class AESSBox extends Module {
  val io = IO(new Bundle {
    val zkn_fn = Input(UInt(ZKN.FN_Len.W))
    val in = Input(UInt(8.W))
    val out = Output(UInt(8.W))
  })

  val enc = Module(new SBox(AES.enc))
  val dec = Module(new SBox(AES.dec))
  enc.io.in := io.in
  dec.io.in := io.in
  io.out := Mux(io.zkn_fn === ZKN.FN_AES_ES ||
    io.zkn_fn === ZKN.FN_AES_ESM ||
    io.zkn_fn === ZKN.FN_AES_KS1,
    enc.io.out, dec.io.out)
}

class GFMul(y: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(8.W))
    val out = Input(UInt(8.W))
  })

  // x*f(x) = 2*in in GF
  def xt(in: UInt): UInt = (in << 1)(7,0) ^ Mux(in(7), 0x1b.U(8.W), 0x00.U(8.W))
  // 4*in in GF
  def xt2(in: UInt): UInt = xt(xt(in))
  // 8*in in GF
  def xt3(in: UInt): UInt = xt(xt2(in))

  require(y != 0)
  io.out := VecInit(
    (if ((y & 0x1) != 0) Seq(   (io.in)) else Nil) ++
    (if ((y & 0x2) != 0) Seq( xt(io.in)) else Nil) ++
    (if ((y & 0x4) != 0) Seq(xt2(io.in)) else Nil) ++
    (if ((y & 0x8) != 0) Seq(xt3(io.in)) else Nil) ++
  ).reduce(_ ^ _)
}

class ShiftRows(enc: Boolean) extends Module {
  val io = IO(new Bundle {
    val in1 = Input(UInt(64.W))
    val in2 = Input(UInt(64.W))
    val out = Input(UInt(64.W))
  })

  val stride = if (enc) 5 else 13
  val indexes = Seq.tabulate(4)(x => (x * stride) % 16) ++
    Seq.tabulate(4)(x => (x * stride + 4) % 16)

  def asBytes(in: UInt): Vec[UInt] = VecInit(in.asBools.grouped(8).map(VecInit(_).asUInt).toSeq)
  val bytes = asBytes(io.in1) ++ asBytes(io.in2)

  io.out := VecInit(indexes.map(bytes(_)).toSeq).toUInt
}

class MixColumn8(enc: Boolean) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(8.W))
    val out = Input(UInt(32.W))
  })

  def m(x: UInt, y: Int): UInt = {
    val m = Module(new GFMul(y))
    m.io.in := x
    m.io.out
  }

  io.out := if (enc) Cat(m(io.in, 3), io.in, io.in, m(io.in, 2))
    else Cat(m(io.in, 0xb), m(io.in, 0xd), m(io.in, 9), m(io.in, 0xe))
}

class MixColumn32(enc: Boolean) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(32.W))
    val out = Input(UInt(32.W))
  })

  def asBytes(in: UInt): Vec[UInt] = VecInit(in.asBools.grouped(8).map(VecInit(_).asUInt).toSeq)
  io.out := asBytes(io.in).zipWithIndex.map({
    case (b, i) => {
      val m = Module(new MixColumn8(enc))
      m.in := b
      m.out.rotateLeft(i * 8)
    }
  }).reduce(_ ^ _)
}

class MixColumn64(enc: Boolean) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(64.W))
    val out = Input(UInt(64.W))
  })

  io.out := VecInit(io.in.asBools.grouped(32).map(VecInit(_).asUInt).map({
    x => {
      val m = Module(new MixColumn32(enc))
      m.in := x
      m.out
    }
  }).toSeq).toUInt
}

class ZKNImp(xLen:Int) extends Module {
  val io = IO(new ZKNInterface(xLen))

  // helper
  def asBytes(in: UInt): Vec[UInt] = VecInit(in.asBools.grouped(8).map(VecInit(_).asUInt).toSeq)

  // aes
  val aes = if (xLen == 32) {
    val si = asBytes(io.rs2)(bs)
    val so = {
      val m = Module(new AESSBox)
      m.io.in := si
      m.io.zkn_fn := io.zkn_fn
      m.io.out
    }
    val mixed_so = Mux(io.zkn_fn === ZKN.FN_AES_ES || io.zkn_fn == ZKN.FN_AES_DS, Cat(0.U(24.W), so), {
        val mc_enc = Module(new MixColumn8(true))
        val mc_dec = Module(new MixColumn8(false))
        mc_enc.io.in := so
        mc_dec.io.in := so
        Mux(io.zkn_fn === ZKN.FN_AES_ESM, mc_enc.io.out, mc_dec.io.out)
      })
    io.rs1 ^ barrel.leftRotate(asBytes(mixed_so), bs)
  } else {
    require(xLen == 64)
    // var name from rvk spec enc/dec
    val sr = {
      val sr_enc = Module(new ShiftRows(true))
      val sr_dec = Module(new ShiftRows(false))
      sr_enc.io.in1 := io.rs1
      sr_enc.io.in2 := io.rs2
      sr_dec.io.in1 := io.rs1
      sr_dec.io.in2 := io.rs2
      Mux(io.zkn_fn === ZKN.FN_AES_ES || io.zkn_fn === ZKN.FN_AES_ESM, sr_enc.io.out, sr_dec.io.out)
    }
    // TODO: for ks: how to handle illegal rcon
    // var name from rvk spec ks1
    val tmp1 = io.rs1(63,32)
    val tmp2 = Mux(io.rcon === 0xA.U, tmp1, tmp1.rotateRight(8))
    // reuse 8 Sbox here
    val si = Mux(io.zkn_fn === FN_AES_KS1, Cat(0.U(32.W), tmp2), sr)
    val so = VecInit(asBytes(sr).map(x => {
      val m = Module(new AESSBox)
      m.io.in := x
      m.io.zkn_fn := io.zkn_fn
      m.io.out
    }).toSeq).asUInt
    // var name from rvk spec ks1
    val rc = VecInit(AES.rcon.map(_.U(8.W)).toSeq)(io.rcon)
    val tmp4 = so(31,0) ^ rc
    val ks1 = Cat(tmp4, tmp4)
    // var name from rvk spec ks2
    val w0 = tmp1 ^ io.rs2(31,0)
    val w1 = w0 ^ io.rs2(63,32)
    val ks2 = Cat(w1, w0)
    // TODO: rewrite into Mux1H
    Mux(io.zkn_fn === ZKN.FN_AES_ES ||
      io.zkn_fn == ZKN.FN_AES_DS ||
      io.zkn_fn == ZKN.FN_AES_KS1 ||
      io.zkn_fn == ZKN.FN_AES_KS2,
        Mux(io.zkn_fn === ZKN.FN_AES_ES || io.zkn_fn === ZKN.FN_AES_DS, so,
          Mux(io.zkn_fn === ZKN.FN_AES_KS1, ks1, ks2)), {
        val mc_in = Mux(io.zkn_fn === ZKN.FN_AES_IM, io.rs1, so)
        val mc_enc = Module(new MixColumn64(true))
        val mc_dec = Module(new MixColumn64(false))
        mc_enc.io.in := mc_in
        mc_dec.io.in := mc_in
        // note the case io.zkn_fn === ZKN.FN_AES_IM! it is also dec
        Mux(io.zkn_fn === ZKN.FN_AES_ESM, mc_enc.io.out, mc_dec.io.out)
      })
  }

  // sha
  def sext(in: UInt): UInt = if (xLen == 32) in
    else {
      require(xLen == 64)
      val in_hi_32 = Fill(32, in(31))
      Cat(in_hi_32, in)
    }
  val inb = io.rs1(31,0)
  val sha256sig0 = sext(inb.rotateRight(7)  ^ inb.rotateRight(18) ^ (inb >>  3))
  val sha256sig1 = sext(inb.rotateRight(17) ^ inb.rotateRight(19) ^ (inb >> 10))
  val sha256sum0 = sext(inb.rotateRight(2)  ^ inb.rotateRight(13) ^ inb.rotateRight(22))
  val sha256sum1 = sext(inb.rotateRight(6)  ^ inb.rotateRight(11) ^ inb.rotateRight(25))

  val sha512sig0 = if (xLen == 32) {
    val sha512sig0_rs1  = (io.rs1 >> 1 )  ^ (io.rs1 >> 7) ^ (io.rs1 >>  8)
    val sha512sig0_rs2h = (io.rs2 << 31)  ^                 (io.rs1 << 24)
    val sha512sig0_rs2l = sha512sig0_rs2h ^ (io.rs2 << 25)
    sha512sig0_rs1 ^ Mux(io.lh, sha512sig0_rs2h, sha512sig0_rs2l)
  } else {
    require(xLen == 64)
    io.rs1.rotateRight(1) ^ io.rs1.rotateRight(8) ^ io.rs1 >> 7
  }

  val sha512sig1 = if (xLen == 32) {
    val sha512sig1_rs1  = (io.rs1 >> 3 )  ^ (io.rs1 >> 6) ^ (io.rs1 >> 19)
    val sha512sig1_rs2h = (io.rs2 << 29)  ^                 (io.rs1 << 13)
    val sha512sig1_rs2l = sha512sig1_rs2h ^ (io.rs2 << 16)
    sha512sig1_rs1 ^ Mux(io.lh, sha512sig1_rs2h, sha512sig1_rs2l)
  } else {
    require(xLen == 64)
    io.rs1.rotateRight(19) ^ io.rs1.rotateRight(61) ^ io.rs1 >> 6
  }

  val sha512sum0 = if (xLen == 32) {
    val sha512sum0_rs1  = (io.rs1 << 25) ^ (io.rs1 << 30) ^ (io.rs1 >> 28)
    val sha512sum0_rs2  = (io.rs2 >>  7) ^ (io.rs2 >>  2) ^ (io.rs2 <<  4)
    sha512sum0_rs1 ^ sha512sum0_rs2
  } else {
    require(xLen == 64)
    io.rs1.rotateRight(28) ^ io.rs1.rotateRight(34) ^ io.rs1.rotateRight(39)
  }

  val sha512sum1 = if (xLen == 32) {
    val sha512sum1_rs1  = (io.rs1 << 23) ^ (io.rs1 >> 14) ^ (io.rs1 >> 18)
    val sha512sum1_rs2  = (io.rs2 >>  9) ^ (io.rs2 << 18) ^ (io.rs2 << 14)
    sha512sum1_rs1 ^ sha512sum1_rs2
  } else {
    require(xLen == 64)
    io.rs1.rotateRight(14) ^ io.rs1.rotateRight(18) ^ io.rs1.rotateRight(41)
  }
}
