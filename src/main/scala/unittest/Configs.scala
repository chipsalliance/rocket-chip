// See LICENSE.SiFive for license details.

package unittest

import Chisel._
import config._
import rocketchip.{BaseConfig, BasePlatformConfig}

case object TestDurationMultiplier extends Field[Int]

class WithTestDuration(x: Int) extends Config((site, here, up) => {
  case TestDurationMultiplier => x
})

class WithUncoreUnitTests extends Config((site, here, up) => {
  case uncore.tilelink.TLId => "L1toL2"
  case UnitTests => (q: Parameters) => {
    implicit val p = q
    val txns = 100 * site(TestDurationMultiplier)
    val timeout = 50000 * site(TestDurationMultiplier)
    Seq(
      Module(new uncore.tilelink2.TLFuzzRAMTest(  txns=5*txns, timeout=timeout)),
      Module(new uncore.ahb.AHBBridgeTest(true,   txns=5*txns, timeout=timeout)),
      Module(new uncore.ahb.AHBNativeTest(true,   txns=5*txns, timeout=timeout)),
      Module(new uncore.ahb.AHBNativeTest(false,  txns=5*txns, timeout=timeout)),
      Module(new uncore.apb.APBBridgeTest(true,   txns=5*txns, timeout=timeout)),
      Module(new uncore.apb.APBBridgeTest(false,  txns=5*txns, timeout=timeout)),
      Module(new uncore.axi4.AXI4LiteFuzzRAMTest( txns=5*txns, timeout=timeout)),
      Module(new uncore.axi4.AXI4FullFuzzRAMTest( txns=5*txns, timeout=timeout)),
      Module(new uncore.axi4.AXI4BridgeTest(      txns=5*txns, timeout=timeout))) }
})

class WithTLSimpleUnitTests extends Config((site, here, up) => {
  case UnitTests => (q: Parameters) => {
    implicit val p = q
    val txns = 100 * site(TestDurationMultiplier)
    val timeout = 50000 * site(TestDurationMultiplier)
    Seq(
      Module(new uncore.tilelink2.TLRAMSimpleTest(1,        txns=5*txns, timeout=timeout)),
      Module(new uncore.tilelink2.TLRAMSimpleTest(4,        txns=5*txns, timeout=timeout)),
      Module(new uncore.tilelink2.TLRAMSimpleTest(16,       txns=5*txns, timeout=timeout)),
      Module(new uncore.tilelink2.TLRAMZeroDelayTest(4,     txns=5*txns, timeout=timeout)),
      Module(new uncore.tilelink2.TLRR0Test(                txns=5*txns, timeout=timeout)),
      Module(new uncore.tilelink2.TLRR1Test(                txns=5*txns, timeout=timeout)),
      Module(new uncore.tilelink2.TLRAMRationalCrossingTest(txns=5*txns, timeout=timeout)),
      Module(new uncore.tilelink2.TLRAMAsyncCrossingTest(   txns=5*txns, timeout=timeout)),
      Module(new uncore.tilelink2.TLRAMAtomicAutomataTest(  txns=5*txns, timeout=timeout)) ) }
})

class WithTLWidthUnitTests extends Config((site, here, up) => {
  case UnitTests => (q: Parameters) => {
    implicit val p = q
    val txns = 100 * site(TestDurationMultiplier)
    val timeout = 50000 * site(TestDurationMultiplier)
    Seq(
      Module(new uncore.tilelink2.TLRAMFragmenterTest( 4, 256, txns=5*txns, timeout=timeout)),
      Module(new uncore.tilelink2.TLRAMFragmenterTest(16,  64, txns=5*txns, timeout=timeout)),
      Module(new uncore.tilelink2.TLRAMFragmenterTest( 4,  16, txns=5*txns, timeout=timeout)),
      Module(new uncore.tilelink2.TLRAMWidthWidgetTest( 1,  1, txns=5*txns, timeout=timeout)),
      Module(new uncore.tilelink2.TLRAMWidthWidgetTest( 4, 64, txns=5*txns, timeout=timeout)),
      Module(new uncore.tilelink2.TLRAMWidthWidgetTest(64,  4, txns=5*txns, timeout=timeout)) ) }
})

class WithTLXbarUnitTests extends Config((site, here, up) => {
  case UnitTests => (q: Parameters) => {
    implicit val p = q
    val txns = 100 * site(TestDurationMultiplier)
    val timeout = 50000 * site(TestDurationMultiplier)
    Seq(
      Module(new uncore.tilelink2.TLRAMXbarTest(1,           txns=5*txns, timeout=timeout)),
      Module(new uncore.tilelink2.TLRAMXbarTest(2,           txns=5*txns, timeout=timeout)),
      Module(new uncore.tilelink2.TLRAMXbarTest(8,           txns=5*txns, timeout=timeout)),
      Module(new uncore.tilelink2.TLMulticlientXbarTest(4,4, txns=5*txns, timeout=timeout)) ) }
})

class UncoreUnitTestConfig extends Config(new WithUncoreUnitTests ++ new WithTestDuration(10) ++ new BasePlatformConfig)
class TLSimpleUnitTestConfig extends Config(new WithTLSimpleUnitTests ++ new WithTestDuration(10) ++ new BasePlatformConfig)
class TLWidthUnitTestConfig extends Config(new WithTLWidthUnitTests ++ new WithTestDuration(10) ++ new BasePlatformConfig)
class TLXbarUnitTestConfig extends Config(new WithTLXbarUnitTests ++ new WithTestDuration(10) ++ new BasePlatformConfig)
