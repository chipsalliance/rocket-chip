// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import chisel3._
import chisel3.util.Counter
import freechips.rocketchip.config.Field
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.devices.tilelink.CanHavePeripheryCLINT

case object RTCPeriodCycles extends Field[Int](1 << 5)

@deprecated("HasRTCModuleImp does nothing, use CLINTAttachParams.driveRTCTickWithPeriod as done in BaseSubsystemConfig", "rocket-chip 1.3")
trait HasRTCModuleImp extends LazyModuleImp {
  val outer: BaseSubsystem with CanHavePeripheryCLINT
}

object RTC {
  /** Prvoide a Real-Time Clock source at a specified period of clock cyles.
    *
    * Returns a tuple of the toggled state and a tick notification of when it toggles.
    */
  def source(c: Clock, r: Reset, periodCycles: Int): (UInt, UInt) = {
    withClockAndReset(c, r) {
      val toggle_reg = RegInit(false.B)
      val (_, tick) = Counter(true.B, periodCycles)
      when (tick) { toggle_reg := ~toggle_reg }
      (toggle_reg, tick)
    }
  }

  /** Provide toggle for a real-time clock based on a period */
  def toggle(c: Clock, r: Reset, periodCycles: Int): UInt = source(c, r, periodCycles)._1

  /** Provide ticks for a real-time clock based on a period */
  def tick(c: Clock, r: Reset, periodCycles: Int): UInt = source(c, r, periodCycles)._2

  /** Drive an RTC toggle wire at the specified period of clock cycles. */
  def drive(rtc_toggle: Bool, c: Clock, r: Reset, periodCycles: Int = 1 << 5): Unit = {
    rtc_toggle := toggle(c, r, periodCycles)
  }
}
