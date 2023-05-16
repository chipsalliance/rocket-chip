// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{RRTest0, RRTest1}
import freechips.rocketchip.unittest._

class TLRRTest0(address: BigInt)(implicit p: Parameters)
  extends RRTest0(address)
  with HasTLControlRegMap

class TLRRTest1(address: BigInt)(implicit p: Parameters)
  extends RRTest1(address, concurrency = 6, undefZero = false)
  with HasTLControlRegMap

class FuzzRRTest0(txns: Int)(implicit p: Parameters) extends LazyModule {
  val fuzz = LazyModule(new TLFuzzer(txns))
  val rrtr = LazyModule(new TLRRTest0(0x400))

  rrtr.node := TLFragmenter(4, 32) := TLDelayer(0.1) := fuzz.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class TLRR0Test(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new FuzzRRTest0(txns)).module)
  io.finished := dut.io.finished
  dut.io.start := io.start
}

class FuzzRRTest1(txns: Int)(implicit p: Parameters) extends LazyModule {
  val fuzz = LazyModule(new TLFuzzer(txns))
  val rrtr = LazyModule(new TLRRTest1(0x400))

  rrtr.node := TLFragmenter(4, 32) := TLDelayer(0.1) := fuzz.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class TLRR1Test(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new FuzzRRTest1(txns)).module)
  io.finished := dut.io.finished
  dut.io.start := io.start
}

