// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import freechips.rocketchip.diplomacy.{LazyModule, DTSTimebase, InModuleBody}
import freechips.rocketchip.devices.tilelink.{CanHavePeripheryCLINT, CLINTAttachKey}

trait HasRTC extends LazyModule { this: BaseSubsystem with CanHavePeripheryCLINT =>
  private val pbusFreq = p(PeripheryBusKey).dtsFrequency.get
  private val rtcFreq = p(DTSTimebase)
  private val internalPeriod: BigInt = pbusFreq / rtcFreq
  private val tlbus = locateTLBusWrapper(p(CLINTAttachKey).slaveWhere)

  // check whether pbusFreq >= rtcFreq
  require(internalPeriod > 0)
  // check whether the integer division is within 5% of the real division
  require((pbusFreq - rtcFreq * internalPeriod) * 100 / pbusFreq <= 5)

  clintOpt.foreach { clint =>
    tlbus { InModuleBody {
      // Use the static period to toggle the RTC
      val (_, int_rtc_tick) = Counter(true.B, internalPeriod.toInt)
      clint.module.io.rtcTick := int_rtc_tick
    } }
  }
}
