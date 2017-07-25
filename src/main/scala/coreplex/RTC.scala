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
  val (_, int_rtc_tick) = Counter(true.B, internalPeriod.get)

  outer.clint.module.io.rtcTick := int_rtc_tick
}
