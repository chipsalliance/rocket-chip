package org.chipsalliance.rocketchip.internal.tests

import freechips.rocketchip.devices.tilelink.BootROMLocated
import freechips.rocketchip.subsystem.{WithCoherentBusTopology, WithNBigCores}
import freechips.rocketchip.system.BaseConfig
import org.chipsalliance.cde.config.Config

class WithDPITestConfig
    extends Config((site, here, up) => {
      case BootROMLocated(_) => None
    })

class DefaultConfig
    extends Config(
      new WithDPITestConfig ++
        new WithNBigCores(1) ++
        new WithCoherentBusTopology ++
        new BaseConfig
    )
