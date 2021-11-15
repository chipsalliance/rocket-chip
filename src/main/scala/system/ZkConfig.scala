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

class WithZKN extends Config((site, here, up) => {
	case RocketTilesKey => up(RocketTilesKey, site) map { r =>
		r.copy(core = r.core.copy(useZKN = true))
	}
})

class WithZKR extends Config((site, here, up) => {
	case RocketTilesKey => up(RocketTilesKey, site) map { r =>
		r.copy(core = r.core.copy(useZKR = true))
	}
})

class WithZKS extends Config((site, here, up) => {
	case RocketTilesKey => up(RocketTilesKey, site) map { r =>
		r.copy(core = r.core.copy(useZKS = true))
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

class RocketWithZknConf32 extends Config(
	  new WithZKN ++
		new DefaultRV32Config ++
		new BaseConfig)

class RocketWithZknConf64 extends Config(
	  new WithZKN ++
		new WithNBigCores(1) ++
		new WithCoherentBusTopology ++
		new BaseConfig)

class RocketWithZksConf32 extends Config(
	  new WithZKS ++
		new DefaultRV32Config ++
		new BaseConfig)

class RocketWithZksConf64 extends Config(
		new WithZKS ++
		new WithNBigCores(1) ++
		new WithCoherentBusTopology ++
		new BaseConfig)

class RocketWithZkConf32 extends Config(
		new WithZKN ++
		new WithZKR ++
		new DefaultRV32Config ++
		new BaseConfig)

class RocketWithZkConf64 extends Config(
		new WithZKN ++
		new WithZKR ++
		new WithNBigCores(1) ++
		new WithCoherentBusTopology ++
		new BaseConfig)