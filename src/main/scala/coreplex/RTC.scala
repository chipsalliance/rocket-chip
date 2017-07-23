// See LICENSE.SiFive for license details.

package freechips.rocketchip.coreplex

import Chisel._
import freechips.rocketchip.config.Field
import freechips.rocketchip.diplomacy.LazyMultiIOModuleImp
import freechips.rocketchip.devices.tilelink.HasPeripheryClint

/** Real-time clock is based on RTCPeriod relative to system clock.
  */
case object RTCPeriod extends Field[Option[Int]]

trait HasRTCModuleImp extends LazyMultiIOModuleImp {
  val outer: HasPeripheryClint
  private val internalPeriod: Option[Int] = outer.p(RTCPeriod)
  require(internalPeriod.isDefined, "RTCPeriod is not defined")

  // Use the static period to toggle the RTC
  val (rtc_counter, _) = Counter(true.B, internalPeriod.get)
  val tick = rtc_counter(log2Up(internalPeriod.get)-1)

  outer.clint.module.io.rtcTick := tick
}
