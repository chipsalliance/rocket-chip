// See LICENSE.SiFive for license details.

package rocketchip

import Chisel._
import config._
import diplomacy._
import uncore.tilelink2._
import uncore.devices._
import util._
import jtag.JTAGIO
import coreplex._

// System with JTAG DTM Instantiated inside. JTAG interface is
// exported outside.

trait PeripheryJTAGDTM extends HasTopLevelNetworks {
  val module: PeripheryJTAGDTMModule
  val coreplex: CoreplexRISCVPlatform
}

trait PeripheryJTAGDTMBundle extends HasTopLevelNetworksBundle {
  val outer: PeripheryJTAGDTM

  val jtag = new JTAGIO(hasTRSTn = false).flip
  val jtag_reset = Bool(INPUT)

}

trait PeripheryJTAGDTMModule extends HasTopLevelNetworksModule {
  val outer: PeripheryJTAGDTM
  val io: PeripheryJTAGDTMBundle

  val dtm = Module (new DebugTransportModuleJTAG(p(DMKey).nDMIAddrSize, p(JtagDTMKey)))
  dtm.io.jtag <> io.jtag
  
  dtm.clock             := io.jtag.TCK
  dtm.io.jtag_reset     := io.jtag_reset
  dtm.reset             := dtm.io.fsmReset

  outer.coreplex.module.io.debug.dmi <> dtm.io.dmi
  outer.coreplex.module.io.debug.dmiClock := io.jtag.TCK
  outer.coreplex.module.io.debug.dmiReset := ResetCatchAndSync(io.jtag.TCK, io.jtag_reset, "dmiResetCatch")

}

// System with Debug Module Interface Only. Any sort of DTM
// can be connected outside. DMI Clock and Reset must be provided.

trait PeripheryDMI extends HasTopLevelNetworks {
  val module: PeripheryDMIModule
  val coreplex: CoreplexRISCVPlatform
}

trait PeripheryDMIBundle extends HasTopLevelNetworksBundle {
  val outer: PeripheryDMI

  val debug = new ClockedDMIIO().flip
}

trait PeripheryDMIModule extends HasTopLevelNetworksModule {
  val outer: PeripheryDMI
  val io: PeripheryDMIBundle

  outer.coreplex.module.io.debug <> io.debug
}

// System with DMI or JTAG interface based on a parameter

trait PeripheryDebug extends HasTopLevelNetworks {
  val module: PeripheryDebugModule
  val coreplex: CoreplexRISCVPlatform
}

trait PeripheryDebugBundle extends HasTopLevelNetworksBundle {
  val outer: PeripheryDebug

  val debug = (!p(IncludeJtagDTM)).option(new ClockedDMIIO().flip)

  val jtag        = (p(IncludeJtagDTM)).option(new JTAGIO(hasTRSTn = false).flip)
  val jtag_reset  = (p(IncludeJtagDTM)).option(Bool(INPUT))

  val ndreset = Bool(OUTPUT)
  val dmactive = Bool(OUTPUT)
}

trait PeripheryDebugModule extends HasTopLevelNetworksModule {
  val outer: PeripheryDebug
  val io: PeripheryDebugBundle

  io.debug.foreach { dbg => outer.coreplex.module.io.debug <> dbg }

  val dtm = if (io.jtag.isDefined) Some[DebugTransportModuleJTAG](Module (new DebugTransportModuleJTAG(p(DMKey).nDMIAddrSize, p(JtagDTMKey)))) else None
  dtm.foreach { dtm =>
    dtm.io.jtag <> io.jtag.get

    dtm.clock          := io.jtag.get.TCK
    dtm.io.jtag_reset  := io.jtag_reset.get
    dtm.reset          := dtm.io.fsmReset

    outer.coreplex.module.io.debug.dmi <> dtm.io.dmi
    outer.coreplex.module.io.debug.dmiClock := io.jtag.get.TCK
    outer.coreplex.module.io.debug.dmiReset := ResetCatchAndSync(io.jtag.get.TCK, io.jtag_reset.get, "dmiResetCatch")
  }

  io.ndreset  := outer.coreplex.module.io.ndreset
  io.dmactive := outer.coreplex.module.io.dmactive

}

/// Real-time clock is based on RTCPeriod relative to Top clock

trait PeripheryCounter extends HasTopLevelNetworks {
  val module: PeripheryCounterModule
  val coreplex: CoreplexRISCVPlatform
}

trait PeripheryCounterBundle extends HasTopLevelNetworksBundle {
  val outer: PeripheryCounter
}

trait PeripheryCounterModule extends HasTopLevelNetworksModule {
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

/// Coreplex will power-on running at 0x1000 (BootROM)

trait HardwiredResetVector extends HasTopLevelNetworks {
  val module: HardwiredResetVectorModule
  val coreplex: CoreplexRISCVPlatform
}

trait HardwiredResetVectorBundle extends HasTopLevelNetworksBundle {
  val outer: HardwiredResetVector
}

trait HardwiredResetVectorModule extends HasTopLevelNetworksModule {
  val outer: HardwiredResetVector
  val io: HardwiredResetVectorBundle

  outer.coreplex.module.io.resetVector := UInt(0x10040) // boot ROM: hang
}
