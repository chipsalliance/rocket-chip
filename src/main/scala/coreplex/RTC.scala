// See LICENSE.SiFive for license details.

package freechips.rocketchip.coreplex

import Chisel._
import freechips.rocketchip.config.Field
import freechips.rocketchip.diplomacy.LazyMultiIOModuleImp
import freechips.rocketchip.devices.tilelink.HasPeripheryClint

/** If defined, Real-Time Clock is driven internally **/
case object RTCPeriod extends Field[Option[Int]]

/** Real-time clock is based on RTCPeriod relative to system clock.
  * Note: nothing about this is diplomatic, all the work is done in the ModuleImp
  */
trait HasRTC extends HasPeripheryClint

trait CanHaveRTCBundle {
  val rtc_toggle: Option[Bool]

  def driveSimpleRTC(dummy: Int = 1) {
    rtc_toggle.foreach { t =>
      val toggle = RegInit(UInt(0, width = 5))
      toggle := toggle + UInt(1)
      t := toggle === UInt(0)
    }
  }
}

trait HasRTCModuleImp extends LazyMultiIOModuleImp with CanHaveRTCBundle {
  val outer: HasRTC
  private val internalPeriod: Option[Int] = p(RTCPeriod)

  // RTC is driven exernally if static period is not provided
  val rtc_toggle = if(internalPeriod.isDefined) None else Some(IO(Bool(INPUT)))

  val tick = internalPeriod.map { period =>
    // Use the static period to toggle the RTC
    val (rtc_counter, _) = Counter(true.B, period)
    rtc_counter(log2Up(period)-1)
  } getOrElse {
    // Synchronize the external toggle into the clint
    val rtc_sync = ShiftRegister(rtc_toggle.get, 3)
    val rtc_last = Reg(init = Bool(false), next=rtc_sync)
    Reg(init = Bool(false), next=(rtc_sync & (~rtc_last)))
  }

  outer.clint.module.io.rtcTick := tick
}
