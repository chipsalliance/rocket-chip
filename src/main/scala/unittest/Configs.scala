// See LICENSE.SiFive for license details.

package freechips.rocketchip.unittest

import Chisel._
import freechips.rocketchip.amba.ahb._
import freechips.rocketchip.amba.apb._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config._
import freechips.rocketchip.subsystem.{BaseSubsystemConfig}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

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
      Module(new AXI4BridgeTest(     txns=3*txns, timeout=timeout)),
      Module(new AXI4XbarTest(       txns=1*txns, timeout=timeout)),
      Module(new AXI4RAMAsyncCrossingTest(txns=3*txns, timeout=timeout))) }
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
      Module(new TLRAMHintHandlerTest(     txns=15*txns, timeout=timeout)),
      Module(new TLFuzzRAMTest(            txns= 3*txns, timeout=timeout)),
      Module(new TLRR0Test(                txns= 3*txns, timeout=timeout)),
      Module(new TLRR1Test(                txns= 3*txns, timeout=timeout)),
      Module(new TLRAMRationalCrossingTest(txns= 3*txns, timeout=timeout)),
      Module(new TLRAMAsyncCrossingTest(   txns= 5*txns, timeout=timeout)),
      Module(new TLRAMAtomicAutomataTest(  txns=10*txns, timeout=timeout)),
      Module(new TLRAMECCTest(8, 4,        txns=15*txns, timeout=timeout)),
      Module(new TLRAMECCTest(4, 1,        txns=15*txns, timeout=timeout)),
      Module(new TLRAMECCTest(1, 1,        txns=15*txns, timeout=timeout)) ) }
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
      Module(new TLMulticlientXbarTest(4,4, txns=2*txns, timeout=timeout)),
      Module(new TLMasterMuxTest(           txns=5*txns, timeout=timeout)) ) }
})

class WithECCTests extends Config((site, here, up) => {
  case UnitTests => (q: Parameters) => {
    Seq(
      // try some perfect codes
      Module(new ECCTest(1)),  // n=3
      Module(new ECCTest(4)),  // n=7
      Module(new ECCTest(11)), // n=15
      // try +1 perfect
      Module(new ECCTest(2)),  // n=5
      Module(new ECCTest(5)),  // n=9
      Module(new ECCTest(12)), // n=17
      // try -1 perfect
      Module(new ECCTest(3)),  // n=6
      Module(new ECCTest(10)), // n=14
      // try a useful size
      Module(new ECCTest(8)) ) }
})

class WithScatterGatherTests extends Config((site, here, up) => {
  case UnitTests => (q: Parameters) => {
    Seq(
      Module(new GatherTest(1)),
      Module(new GatherTest(2)),
      Module(new GatherTest(3)),
      Module(new GatherTest(7)),
      Module(new GatherTest(8)),
      Module(new GatherTest(9)),
      Module(new ScatterTest(1)),
      Module(new ScatterTest(2)),
      Module(new ScatterTest(3)),
      Module(new ScatterTest(7)),
      Module(new ScatterTest(8)),
      Module(new ScatterTest(9)))}})

class WithPowerQueueTests extends Config((site, here, up) => {
  case UnitTests => (q: Parameters) => {
    Seq(
      Module(new PositionedQueueTest(FloppedLanePositionedQueue,                   1,  2, 10000)),
      Module(new PositionedQueueTest(FloppedLanePositionedQueue,                   2,  6, 10000)),
      Module(new PositionedQueueTest(FloppedLanePositionedQueue,                   3, 10, 10000)),
      Module(new PositionedQueueTest(OnePortLanePositionedQueue(new IdentityCode), 4, 12, 10000)),
      Module(new PositionedQueueTest(OnePortLanePositionedQueue(new IdentityCode), 4, 16, 10000)),
      Module(new PositionedQueueTest(OnePortLanePositionedQueue(new IdentityCode), 4, 20, 10000)),
      Module(new PositionedQueueTest(OnePortLanePositionedQueue(new IdentityCode), 1, 12, 10000)),
      Module(new PositionedQueueTest(OnePortLanePositionedQueue(new IdentityCode), 3, 16, 10000)),
      Module(new PositionedQueueTest(OnePortLanePositionedQueue(new IdentityCode), 5, 20, 10000)),
      Module(new MultiPortQueueTest(1, 1, 2, 10000)),
      Module(new MultiPortQueueTest(3, 3, 2, 10000)),
      Module(new MultiPortQueueTest(5, 5, 6, 10000)),
      Module(new MultiPortQueueTest(4, 3, 6, 10000)),
      Module(new MultiPortQueueTest(4, 5, 2, 10000)),
      Module(new MultiLaneQueueTest(1, 2, 10000)),
      Module(new MultiLaneQueueTest(3, 2, 10000)),
      Module(new MultiLaneQueueTest(5, 6, 10000))
      )}})

class AMBAUnitTestConfig extends Config(new WithAMBAUnitTests ++ new WithTestDuration(10) ++ new BaseSubsystemConfig)
class TLSimpleUnitTestConfig extends Config(new WithTLSimpleUnitTests ++ new WithTestDuration(10) ++ new BaseSubsystemConfig)
class TLWidthUnitTestConfig extends Config(new WithTLWidthUnitTests ++ new WithTestDuration(10) ++ new BaseSubsystemConfig)
class TLXbarUnitTestConfig extends Config(new WithTLXbarUnitTests ++ new WithTestDuration(10) ++ new BaseSubsystemConfig)
class ECCUnitTestConfig extends Config(new WithECCTests)
class ScatterGatherTestConfig extends Config(new WithScatterGatherTests)
class PowerQueueTestConfig extends Config(new WithPowerQueueTests)
