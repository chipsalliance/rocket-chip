package sanitytests

import chipsalliance.rocketchip.config.Config
import freechips.rocketchip.devices.tilelink.BootROMLocated
import freechips.rocketchip.util.ClockGateModelFile

class TestConfig
    extends Config((site, here, up) => {
      case ClockGateModelFile => Some("/vsrc/EICG_wrapper.v")
      case BootROMLocated(x) =>
        up(BootROMLocated(x), site).map(_.copy(contentFileName = {
          resource("VerilatorTest/bootrom.img").toString()
        }))
    })
