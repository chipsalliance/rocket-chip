// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import chisel3._
import chisel3.util.Counter
import freechips.rocketchip.diplomacy.{LazyRawModuleImp, DTSTimebase}
import freechips.rocketchip.devices.tilelink.{CLINTAttachKey, CanHavePeripheryCLINT}

trait HasRTCModuleImp extends LazyRawModuleImp {
  val outer: BaseSubsystem with CanHavePeripheryCLINT

  // Use the static period to toggle the RTC
  outer.clintDomainOpt.map { domain => {
    val bus = outer.locateTLBusWrapper(p(CLINTAttachKey).slaveWhere)
    val busFreq = bus.dtsFrequency.get
    val rtcFreq = outer.p(DTSTimebase)
    val internalPeriod: BigInt = busFreq / rtcFreq


    // check whether pbusFreq >= rtcFreq
    require(internalPeriod > 0)
    // check wehther the integer division is within 5% of the real division
    require((busFreq - rtcFreq * internalPeriod) * 100 / busFreq <= 5)

    withClockAndReset (domain.module.clock, domain.module.reset) {
      val (_, int_rtc_tick) = Counter(true.B, internalPeriod.toInt)
      outer.clintTickOpt.foreach { _ := int_rtc_tick }
    }
  }}
}
