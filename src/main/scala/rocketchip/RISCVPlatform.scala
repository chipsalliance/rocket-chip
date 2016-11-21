// See LICENSE for license details.

package rocketchip

import Chisel._
import config._
import diplomacy._
import uncore.tilelink2._
import uncore.devices._
import util._
import junctions.JTAGIO
import coreplex._

trait PeripheryJTAG extends TopNetwork {
  val module: PeripheryJTAGModule
  val coreplex: CoreplexRISCVPlatform
}

trait PeripheryJTAGBundle extends TopNetworkBundle {
  val outer: PeripheryJTAG

  val jtag = new JTAGIO(true).flip
}

trait PeripheryJTAGModule extends TopNetworkModule {
  val outer: PeripheryJTAG
  val io: PeripheryJTAGBundle

  val dtm = Module (new JtagDTMWithSync)
  dtm.io.jtag <> io.jtag
  outer.coreplex.module.io.debug <> dtm.io.debug

  dtm.clock := io.jtag.TCK
  dtm.reset := io.jtag.TRST
}

trait PeripheryDTM extends TopNetwork {
  val module: PeripheryDTMModule
  val coreplex: CoreplexRISCVPlatform
}

trait PeripheryDTMBundle extends TopNetworkBundle {
  val outer: PeripheryDTM

  val debug = new DebugBusIO().flip
}

trait PeripheryDTMModule extends TopNetworkModule {
  val outer: PeripheryDTM
  val io: PeripheryDTMBundle

  outer.coreplex.module.io.debug <> ToAsyncDebugBus(io.debug)
}

trait PeripheryCounter extends TopNetwork {
  val module: PeripheryCounterModule
  val coreplex: CoreplexRISCVPlatform
}

trait PeripheryCounterBundle extends TopNetworkBundle {
  val outer: PeripheryCounter
}

trait PeripheryCounterModule extends TopNetworkModule {
  val outer: PeripheryCounter
  val io: PeripheryCounterBundle
  
  {
    val period = p(rocketchip.RTCPeriod)
    val rtcCounter = RegInit(UInt(0, width = log2Up(period)))
    val rtcWrap = rtcCounter === UInt(period-1)
    rtcCounter := Mux(rtcWrap, UInt(0), rtcCounter + UInt(1))

    outer.coreplex.module.io.rtcToggle := rtcCounter(log2Up(period)-1)
  }
}

trait HardwiredResetVector extends TopNetwork {
  val module: HardwiredResetVectorModule
  val coreplex: CoreplexRISCVPlatform
}

trait HardwiredResetVectorBundle extends TopNetworkBundle {
  val outer: HardwiredResetVector
}

trait HardwiredResetVectorModule extends TopNetworkModule {
  val outer: HardwiredResetVector
  val io: HardwiredResetVectorBundle

  outer.coreplex.module.io.resetVector := UInt(0x1000) // boot ROM
}
