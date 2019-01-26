// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import freechips.rocketchip.diplomacy.{LazyModuleImp, DTSTimebase}
import freechips.rocketchip.devices.tilelink.CanHavePeripheryCLINT

trait HasRTCModuleImp extends LazyModuleImp {
  val outer: BaseSubsystem with CanHavePeripheryCLINT
  private val pbusFreq = outer.p(PeripheryBusKey).frequency
  private val rtcFreq = outer.p(DTSTimebase)
  private val internalPeriod: BigInt = pbusFreq / rtcFreq

  // check whether pbusFreq >= rtcFreq
  require(internalPeriod > 0)
  // check wehther the integer division is within 5% of the real division
  require((pbusFreq - rtcFreq * internalPeriod) * 100 / pbusFreq <= 5)

  // Use the static period to toggle the RTC
  val (_, int_rtc_tick) = Counter(true.B, internalPeriod.toInt)

  outer.clintOpt.foreach { clint =>
    clint.module.io.rtcTick := int_rtc_tick
  }
}
