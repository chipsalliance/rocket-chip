// See LICENSE.SiFive for license details.

package freechips.rocketchip.unittest

import Chisel._
import freechips.rocketchip.amba.ahb._
import freechips.rocketchip.amba.apb._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config._
import freechips.rocketchip.coreplex.{BaseCoreplexConfig}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.tilelink._

case object TestDurationMultiplier extends Field[Int]

class WithTestDuration(x: Int) extends Config((site, here, up) => {
  case TestDurationMultiplier => x
})

class WithAMBAUnitTests extends Config((site, here, up) => {
  case UnitTests => (q: Parameters) => {
    implicit val p = q
    val txns = 100 * site(TestDurationMultiplier)
    val timeout = 50000 * site(TestDurationMultiplier)
    Seq(
      Module(new AHBBridgeTest(true, txns=8*txns, timeout=timeout)),
      Module(new AHBNativeTest(true, txns=6*txns, timeout=timeout)),
      Module(new AHBNativeTest(false,txns=6*txns, timeout=timeout)),
      Module(new APBBridgeTest(true, txns=6*txns, timeout=timeout)),
      Module(new APBBridgeTest(false,txns=6*txns, timeout=timeout)),
      Module(new AXI4LiteFuzzRAMTest(txns=6*txns, timeout=timeout)),
      Module(new AXI4FullFuzzRAMTest(txns=3*txns, timeout=timeout)),
      Module(new AXI4BridgeTest(     txns=3*txns, timeout=timeout))) }
})

class WithTLSimpleUnitTests extends Config((site, here, up) => {
  case UnitTests => (q: Parameters) => {
    implicit val p = q
    val txns = 100 * site(TestDurationMultiplier)
    val timeout = 50000 * site(TestDurationMultiplier)
    Seq(
      Module(new TLRAMSimpleTest(1,        txns=15*txns, timeout=timeout)),
      Module(new TLRAMSimpleTest(4,        txns=15*txns, timeout=timeout)),
      Module(new TLRAMSimpleTest(16,       txns=15*txns, timeout=timeout)),
      Module(new TLRAMZeroDelayTest(4,     txns=15*txns, timeout=timeout)),
      Module(new TLFuzzRAMTest(            txns= 3*txns, timeout=timeout)),
      Module(new TLRR0Test(                txns= 3*txns, timeout=timeout)),
      Module(new TLRR1Test(                txns= 3*txns, timeout=timeout)),
      Module(new TLRAMRationalCrossingTest(txns= 3*txns, timeout=timeout)),
      Module(new TLRAMAsyncCrossingTest(   txns= 5*txns, timeout=timeout)),
      Module(new TLRAMAtomicAutomataTest(  txns=10*txns, timeout=timeout)) ) }
})

class WithTLWidthUnitTests extends Config((site, here, up) => {
  case UnitTests => (q: Parameters) => {
    implicit val p = q
    val txns = 100 * site(TestDurationMultiplier)
    val timeout = 50000 * site(TestDurationMultiplier)
    Seq(
      Module(new TLRAMFragmenterTest( 4, 256, txns= 5*txns, timeout=timeout)),
      Module(new TLRAMFragmenterTest(16,  64, txns=15*txns, timeout=timeout)),
      Module(new TLRAMFragmenterTest( 4,  16, txns=15*txns, timeout=timeout)),
      Module(new TLRAMWidthWidgetTest( 1,  1, txns= 1*txns, timeout=timeout)),
      Module(new TLRAMWidthWidgetTest( 4, 64, txns= 4*txns, timeout=timeout)),
      Module(new TLRAMWidthWidgetTest(64,  4, txns= 5*txns, timeout=timeout)) ) }
})

class WithTLXbarUnitTests extends Config((site, here, up) => {
  case UnitTests => (q: Parameters) => {
    implicit val p = q
    val txns = 100 * site(TestDurationMultiplier)
    val timeout = 50000 * site(TestDurationMultiplier)
    Seq(
      Module(new TLRAMXbarTest(1,           txns=5*txns, timeout=timeout)),
      Module(new TLRAMXbarTest(2,           txns=5*txns, timeout=timeout)),
      Module(new TLRAMXbarTest(8,           txns=5*txns, timeout=timeout)),
      Module(new TLMulticlientXbarTest(4,4, txns=2*txns, timeout=timeout)) ) }
})

class AMBAUnitTestConfig extends Config(new WithAMBAUnitTests ++ new WithTestDuration(10) ++ new BaseCoreplexConfig)
class TLSimpleUnitTestConfig extends Config(new WithTLSimpleUnitTests ++ new WithTestDuration(10) ++ new BaseCoreplexConfig)
class TLWidthUnitTestConfig extends Config(new WithTLWidthUnitTests ++ new WithTestDuration(10) ++ new BaseCoreplexConfig)
class TLXbarUnitTestConfig extends Config(new WithTLXbarUnitTests ++ new WithTestDuration(10) ++ new BaseCoreplexConfig)
