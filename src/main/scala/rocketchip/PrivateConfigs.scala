// See LICENSE for license details.

package rocketchip

import coreplex._
import rocket._
import uncore.agents._
import hwacha._
import config._

class HwachaConfig extends Config(new DefaultHwachaConfig ++ new DefaultL2Config)
class HwachaFPGAConfig extends Config(new DefaultHwachaConfig ++ new DefaultL2FPGAConfig)

class EOS24Config extends Config(new WithNBanksPerMemChannel(4) ++ new WithL2Capacity(256) ++ new HwachaConfig)
class EOS24FPGAConfig extends Config(new FPGAConfig ++ new EOS24Config)

class WithNL2AcquireXacts(n: Int) extends Config((site, here, up) => {
  case NAcquireTransactors => n
})

class WithNLanes(n: Int) extends Config((site, here, up) => {
  case HwachaNLanes => n
})

class With32BtbEntires extends Config((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site) map { r =>
    r.copy(btb = r.btb.map(_.copy(nEntries = 32)))
  }
})

class Process28nmConfig extends Config((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site) map { r =>
    r.copy(core = r.core.copy(fpu = r.core.fpu.map(_.copy(sfmaLatency = 3, dfmaLatency = 4))))
  }
})

class WithoutConfPrec extends Config((site, here, up) => {
    case HwachaConfPrec => false
})

class WithSmallPredRF extends Config((site, here, up) => {
    case HwachaNPredRFEntries => 128
})

class ISCA2016Config extends Config(
  new Process28nmConfig ++
  new WithNBanksPerMemChannel(4) ++
  new WithNL2AcquireXacts(4) ++ new WithL2Capacity(256) ++ new With32BtbEntires ++ new HwachaConfig)

class ISCA2016L2Config extends Config(new WithNLanes(2) ++ new ISCA2016Config)
class ISCA2016L4Config extends Config(new WithNLanes(4) ++ new ISCA2016Config)

class ISCA2016HOVB4Config extends Config(new WithNL2AcquireXacts(9) ++ new WithNBanksPerMemChannel(2) ++ new ISCA2016Config)
class ISCA2016HOVB8Config extends Config(new ISCA2016Config)
class ISCA2016LOVB4Config extends Config(new WithoutConfPrec ++ new ISCA2016HOVB4Config)
class ISCA2016LOVB8Config extends Config(new WithoutConfPrec ++ new ISCA2016HOVB8Config)

class ISCA2016HOVL2B4Config extends Config(new WithNLanes(2) ++ new ISCA2016HOVB4Config)
class ISCA2016HOVL2B8Config extends Config(new WithNLanes(2) ++ new ISCA2016HOVB8Config)
class ISCA2016LOVL2B4Config extends Config(new WithNLanes(2) ++ new ISCA2016LOVB4Config)
class ISCA2016LOVL2B8Config extends Config(new WithNLanes(2) ++ new ISCA2016LOVB8Config)

class ISCA2016HOVL4B4Config extends Config(new WithNLanes(4) ++ new ISCA2016HOVB4Config)
class ISCA2016HOVL4B8Config extends Config(new WithNLanes(4) ++ new ISCA2016HOVB8Config)
class ISCA2016LOVL4B4Config extends Config(new WithNLanes(4) ++ new ISCA2016LOVB4Config)
class ISCA2016LOVL4B8Config extends Config(new WithNLanes(4) ++ new ISCA2016LOVB8Config)

class DualCoreISCA2016L2Config extends Config(new WithNBigCores(2) ++ new WithNLanes(2) ++ new ISCA2016Config)

class HurricaneSimilarConfig extends Config(new WithNLanes(2) ++ new WithL2Capacity(512) ++ new WithNMemoryChannels(8) ++ new WithNBanksPerMemChannel(1) ++ new WithNL2AcquireXacts(9) ++ new ISCA2016Config)
