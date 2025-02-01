// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.system

import org.chipsalliance.cde.config.Config
import freechips.rocketchip.subsystem._
import freechips.rocketchip.rocket.{WithNBigCores, WithNMedCores, WithNSmallCores, WithRV32, WithFP16, WithHypervisor, With1TinyCore, WithScratchpadsOnly, WithCloneRocketTiles, WithB}

class BaseLitexConfig extends Config(
  new WithLitexMemPort() ++
  new WithLitexMMIOPort() ++
  new WithLitexSlavePort ++
  new WithNExtTopInterrupts(8) ++
  new WithCoherentBusTopology ++
  new BaseConfig
)

class LitexConfigSmall1x1 extends Config(
  new WithNSmallCores(1) ++
  new WithNBitMemoryBus(64) ++
  new BaseLitexConfig
)

class LitexConfigSmall1x2 extends Config(
  new WithNSmallCores(1) ++
  new WithNBitMemoryBus(128) ++
  new BaseLitexConfig
)

class LitexConfigSmall1x4 extends Config(
  new WithNSmallCores(1) ++
  new WithNBitMemoryBus(256) ++
  new BaseLitexConfig
)

class LitexConfigSmall1x8 extends Config(
  new WithNSmallCores(1) ++
  new WithNBitMemoryBus(512) ++
  new BaseLitexConfig
)

class LitexConfigSmall2x1 extends Config(
  new WithNSmallCores(2) ++
  new WithNBitMemoryBus(64) ++
  new BaseLitexConfig
)

class LitexConfigSmall2x2 extends Config(
  new WithNSmallCores(2) ++
  new WithNBitMemoryBus(128) ++
  new BaseLitexConfig
)

class LitexConfigSmall2x4 extends Config(
  new WithNSmallCores(2) ++
  new WithNBitMemoryBus(256) ++
  new BaseLitexConfig
)

class LitexConfigSmall2x8 extends Config(
  new WithNSmallCores(2) ++
  new WithNBitMemoryBus(512) ++
  new BaseLitexConfig
)

class LitexConfigSmall4x1 extends Config(
  new WithNSmallCores(4) ++
  new WithNBitMemoryBus(64) ++
  new BaseLitexConfig
)

class LitexConfigSmall4x2 extends Config(
  new WithNSmallCores(4) ++
  new WithNBitMemoryBus(128) ++
  new BaseLitexConfig
)

class LitexConfigSmall4x4 extends Config(
  new WithNSmallCores(4) ++
  new WithNBitMemoryBus(256) ++
  new BaseLitexConfig
)

class LitexConfigSmall4x8 extends Config(
  new WithNSmallCores(4) ++
  new WithNBitMemoryBus(512) ++
  new BaseLitexConfig
)

class LitexConfigSmall8x1 extends Config(
  new WithNSmallCores(8) ++
  new WithNBitMemoryBus(64) ++
  new BaseLitexConfig
)

class LitexConfigSmall8x2 extends Config(
  new WithNSmallCores(8) ++
  new WithNBitMemoryBus(128) ++
  new BaseLitexConfig
)

class LitexConfigSmall8x4 extends Config(
  new WithNSmallCores(8) ++
  new WithNBitMemoryBus(256) ++
  new BaseLitexConfig
)

class LitexConfigSmall8x8 extends Config(
  new WithNSmallCores(8) ++
  new WithNBitMemoryBus(512) ++
  new BaseLitexConfig
)

class LitexConfigBig1x1 extends Config(
  new WithNBigCores(1) ++
  new WithNBitMemoryBus(64) ++
  new BaseLitexConfig
)

class LitexConfigBig1x2 extends Config(
  new WithNBigCores(1) ++
  new WithNBitMemoryBus(128) ++
  new BaseLitexConfig
)

class LitexConfigBig1x4 extends Config(
  new WithNBigCores(1) ++
  new WithNBitMemoryBus(256) ++
  new BaseLitexConfig
)

class LitexConfigBig1x8 extends Config(
  new WithNBigCores(1) ++
  new WithNBitMemoryBus(512) ++
  new BaseLitexConfig
)

class LitexConfigBig2x1 extends Config(
  new WithNBigCores(2) ++
  new WithNBitMemoryBus(64) ++
  new BaseLitexConfig
)

class LitexConfigBig2x2 extends Config(
  new WithNBigCores(2) ++
  new WithNBitMemoryBus(128) ++
  new BaseLitexConfig
)

class LitexConfigBig2x4 extends Config(
  new WithNBigCores(2) ++
  new WithNBitMemoryBus(256) ++
  new BaseLitexConfig
)

class LitexConfigBig2x8 extends Config(
  new WithNBigCores(2) ++
  new WithNBitMemoryBus(512) ++
  new BaseLitexConfig
)

class LitexConfigBig4x1 extends Config(
  new WithNBigCores(4) ++
  new WithNBitMemoryBus(64) ++
  new BaseLitexConfig
)

class LitexConfigBig4x2 extends Config(
  new WithNBigCores(4) ++
  new WithNBitMemoryBus(128) ++
  new BaseLitexConfig
)

class LitexConfigBig4x4 extends Config(
  new WithNBigCores(4) ++
  new WithNBitMemoryBus(256) ++
  new BaseLitexConfig
)

class LitexConfigBig4x8 extends Config(
  new WithNBigCores(4) ++
  new WithNBitMemoryBus(512) ++
  new BaseLitexConfig
)

class LitexConfigBig8x1 extends Config(
  new WithNBigCores(8) ++
  new WithNBitMemoryBus(64) ++
  new BaseLitexConfig
)

class LitexConfigBig8x2 extends Config(
  new WithNBigCores(8) ++
  new WithNBitMemoryBus(128) ++
  new BaseLitexConfig
)

class LitexConfigBig8x4 extends Config(
  new WithNBigCores(8) ++
  new WithNBitMemoryBus(256) ++
  new BaseLitexConfig
)

class LitexConfigBig8x8 extends Config(
  new WithNBigCores(8) ++
  new WithNBitMemoryBus(512) ++
  new BaseLitexConfig
)
