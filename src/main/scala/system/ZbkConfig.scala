// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.system

import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.groundtest.WithTraceGen

class WithZBK extends Config((site, here, up) => {
	case RocketTilesKey => up(RocketTilesKey, site) map { r =>
		r.copy(core = r.core.copy(useZBK = true))
	}
})

class RocketWithZbkConf32 extends Config(
	  new WithZBK ++
		new DefaultRV32Config ++
		new BaseConfig)

class RocketWithZbkConf64 extends Config(
  	new WithZBK ++
		new WithNBigCores(1) ++
		new WithCoherentBusTopology ++
		new BaseConfig)



