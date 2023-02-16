// SPDX-License-Identifier: Apache-2.0

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import freechips.rocketchip.util._

class CryptoNISTInterface(xLen: Int) extends Bundle {
  val fn   = Input(UInt(ABLUFN().SZ_ZKN_FN.W))
  val hl   = Input(Bool())
  val bs   = Input(UInt(2.W))
  val rs1  = Input(UInt(xLen.W))
  val rs2  = Input(UInt(xLen.W))
  val rd   = Output(UInt(xLen.W))
}

object AES {
  val rcon: Seq[Int] = Seq(
    0x01, 0x02, 0x04, 0x08,
    0x10, 0x20, 0x40, 0x80,
    0x1b, 0x36, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00
  )
}

class AESSBox extends Module {
  val io = IO(new Bundle {
    val isEnc = Input(Bool())
    val in = Input(UInt(8.W))
    val out = Output(UInt(8.W))
  })

  val enc = SBoxAESEncIn(io.in)
  val dec = SBoxAESDecIn(io.in)
  val mid = SBoxMid(Mux(io.isEnc, enc, dec))
  io.out := Mux(io.isEnc, SBoxAESEncOut(mid), SBoxAESDecOut(mid))
}

class GFMul(y: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(8.W))
    val out = Output(UInt(8.W))
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
    (if ((y & 0x8) != 0) Seq(xt3(io.in)) else Nil)
  ).reduce(_ ^ _)
}

class ShiftRows(enc: Boolean) extends Module {
  val io = IO(new Bundle {
    val in1 = Input(UInt(64.W))
    val in2 = Input(UInt(64.W))
    val out = Output(UInt(64.W))
  })

  val stride = if (enc) 5 else 13
  val indexes = Seq.tabulate(4)(x => (x * stride) % 16) ++
    Seq.tabulate(4)(x => (x * stride + 4) % 16)

  def asBytes(in: UInt): Vec[UInt] = VecInit(in.asBools.grouped(8).map(VecInit(_).asUInt).toSeq)
  val bytes = asBytes(io.in1) ++ asBytes(io.in2)

  io.out := VecInit(indexes.map(bytes(_)).toSeq).asUInt
}

class MixColumn8(enc: Boolean) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(8.W))
    val out = Output(UInt(32.W))
  })

  def m(x: UInt, y: Int): UInt = {
    val m = Module(new GFMul(y))
    m.io.in := x
    m.io.out
  }

  val out = if (enc) Cat(m(io.in, 3), io.in, io.in, m(io.in, 2))
    else Cat(m(io.in, 0xb), m(io.in, 0xd), m(io.in, 9), m(io.in, 0xe))
  io.out := out
}

class MixColumn32(enc: Boolean) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(32.W))
    val out = Output(UInt(32.W))
  })

  def asBytes(in: UInt): Vec[UInt] = VecInit(in.asBools.grouped(8).map(VecInit(_).asUInt).toSeq)
  io.out := asBytes(io.in).zipWithIndex.map({
    case (b, i) => {
      val m = Module(new MixColumn8(enc))
      m.io.in := b
      m.io.out.rotateLeft(i * 8)
    }
  }).reduce(_ ^ _)
}

class MixColumn64(enc: Boolean) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(64.W))
    val out = Output(UInt(64.W))
  })

  io.out := VecInit(io.in.asBools.grouped(32).map(VecInit(_).asUInt).map({
    x => {
      val m = Module(new MixColumn32(enc))
      m.io.in := x
      m.io.out
    }
  }).toSeq).asUInt
}

class CryptoNIST(xLen:Int) extends Module {
  val fn = ABLUFN()
  val io = IO(new CryptoNISTInterface(xLen))

  val isEnc = fn.isEnc(io.fn)
  val isNotMix = fn.isNotMix(io.fn)
  val isKs1 = fn.isKs1(io.fn)
  val isIm = fn.isIm(io.fn)
  val aes1H = fn.aes1H(io.fn)
  val out1H = fn.zknOut1H(io.fn)

  // helper
  def asBytes(in: UInt): Vec[UInt] = VecInit(in.asBools.grouped(8).map(VecInit(_).asUInt).toSeq)

  // aes
  val aes = if (xLen == 32) {
    val si = asBytes(io.rs2)(io.bs)
    val so = {
      val m = Module(new AESSBox)
      m.io.in := si
      m.io.isEnc := isEnc
      m.io.out
    }
    val mixed_so = Mux(isNotMix, Cat(0.U(24.W), so), {
        val mc_enc = Module(new MixColumn8(true))
        val mc_dec = Module(new MixColumn8(false))
        mc_enc.io.in := so
        mc_dec.io.in := so
        Mux(isEnc, mc_enc.io.out, mc_dec.io.out)
      })
    // Vec rightRotate = UInt rotateLeft as Vec is big endian while UInt is little endian
    // FIXME: use chisel3.stdlib.BarrelShifter after chisel3 3.6.0
    io.rs1 ^ BarrelShifter.rightRotate(asBytes(mixed_so), io.bs).asUInt
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
      Mux(isEnc, sr_enc.io.out, sr_dec.io.out)
    }
    // var name from rvk spec ks1
    val rnum = io.rs2(3,0)
    val tmp1 = io.rs1(63,32)
    val tmp2 = Mux(rnum === 0xA.U, tmp1, tmp1.rotateRight(8))
    // reuse 8 Sbox here
    val si = Mux(isKs1, Cat(0.U(32.W), tmp2), sr)
    val so = VecInit(asBytes(si).map(x => {
      val m = Module(new AESSBox)
      m.io.in := x
      m.io.isEnc := isEnc
      m.io.out
    }).toSeq).asUInt
    val mixed = {
      val mc_in = Mux(isIm, io.rs1, so)
      val mc_enc = Module(new MixColumn64(true))
      val mc_dec = Module(new MixColumn64(false))
      mc_enc.io.in := mc_in
      mc_dec.io.in := mc_in
      // for isIm, it is also dec
      Mux(isEnc, mc_enc.io.out, mc_dec.io.out)
    }
    // var name from rvk spec ks1
    val rc = VecInit(AES.rcon.map(_.U(8.W)).toSeq)(rnum)
    val tmp4 = so(31,0) ^ rc
    val ks1 = Cat(tmp4, tmp4)
    // var name from rvk spec ks2
    val w0 = tmp1 ^ io.rs2(31,0)
    val w1 = w0 ^ io.rs2(63,32)
    val ks2 = Cat(w1, w0)
    Mux1H(aes1H, Seq(so, mixed, ks1, ks2))
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
    val sha512sig0_rs2h = (io.rs2 << 31)  ^                 (io.rs2 << 24)
    val sha512sig0_rs2l = sha512sig0_rs2h ^ (io.rs2 << 25)
    sha512sig0_rs1 ^ Mux(io.hl, sha512sig0_rs2h, sha512sig0_rs2l)
  } else {
    require(xLen == 64)
    io.rs1.rotateRight(1) ^ io.rs1.rotateRight(8) ^ io.rs1 >> 7
  }

  val sha512sig1 = if (xLen == 32) {
    val sha512sig1_rs1  = (io.rs1 << 3 )  ^ (io.rs1 >> 6) ^ (io.rs1 >> 19)
    val sha512sig1_rs2h = (io.rs2 >> 29)  ^                 (io.rs2 << 13)
    val sha512sig1_rs2l = sha512sig1_rs2h ^ (io.rs2 << 26)
    sha512sig1_rs1 ^ Mux(io.hl, sha512sig1_rs2h, sha512sig1_rs2l)
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

  io.rd := Mux1H(out1H, Seq(
    aes,
    sha256sig0, sha256sig1,
    sha256sum0, sha256sum1,
    sha512sig0, sha512sig1,
    sha512sum0, sha512sum1))
}
