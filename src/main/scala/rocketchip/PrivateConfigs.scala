// See LICENSE for license details.

package rocketchip

import coreplex._
import rocket._
import uncore.agents._
import hwacha._
import cde._
import cde.Implicits._

class HwachaConfig extends Config(new DefaultHwachaConfig ++ new DefaultL2Config)
class HwachaFPGAConfig extends Config(new DefaultHwachaConfig ++ new DefaultL2FPGAConfig)

class EOS24Config extends Config(new WithNBanksPerMemChannel(4) ++ new WithL2Capacity(256) ++ new HwachaConfig)
class EOS24FPGAConfig extends Config(new FPGAConfig ++ new EOS24Config)

class WithNL2AcquireXacts(n: Int) extends Config(
  (pname,site,here) => pname match {
    case NAcquireTransactors => n
    case _ => throw new CDEMatchError
  }
)

class WithNLanes(n: Int) extends Config(
  (pname,site,here) => pname match {
    case HwachaNLanes => n
    case _ => throw new CDEMatchError
  }
)

class With32BtbEntires extends Config(
  (pname,site,here) => pname match {
    case BtbKey => BtbParameters(nEntries = 32)
    case _ => throw new CDEMatchError
  }
)

class Process28nmConfig extends Config(
  (pname,site,here) => pname match {
    case FPUKey => Some(FPUConfig(sfmaLatency = 3, dfmaLatency = 4))
    case _ => throw new CDEMatchError
  }
)

class WithoutConfPrec extends Config(
  (pname,site,here) => pname match {
    case HwachaConfPrec => false
    case _ => throw new CDEMatchError
  }
)

class WithSmallPredRF extends Config(
  (pname,site,here) => pname match {
    case HwachaNPredRFEntries => 128
    case _ => throw new CDEMatchError
  }
)

class ISCA2016Config extends Config(
  new Process28nmConfig ++
  new WithNMemoryChannels(2) ++ new WithNBanksPerMemChannel(4) ++
  new WithNL2AcquireXacts(4) ++ new WithL2Capacity(256) ++ new With32BtbEntires ++ new HwachaConfig)
{
  override val knobValues:Any=>Any = {
    case "HWACHA_NSRAMRF_ENTRIES" => 256
    case "HWACHA_BUILD_VRU" => true
    case x => (new Config(new WithNMemoryChannels(2) ++ new WithNBanksPerMemChannel(4) ++ new WithL2Capacity(256) ++ new HwachaConfig)).knobValues(x)
  }

  override val topConstraints:List[ViewSym=>Ex[Boolean]] = {
    List(
      {ex => (ex(HwachaNSRAMRFEntries) === 256)},
      {ex => (ex(HwachaBuildVRU) === true || ex(HwachaBuildVRU) === false)}
    )
  }
}

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
