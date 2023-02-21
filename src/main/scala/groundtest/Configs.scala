// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.groundtest

import org.chipsalliance.cde.config.Config
import freechips.rocketchip.devices.tilelink.{CLINTKey, PLICKey}
import freechips.rocketchip.devices.debug.{DebugModuleKey}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.system.BaseConfig
import freechips.rocketchip.rocket.{DCacheParams}
import freechips.rocketchip.tile.{XLen}

/** Actual testing target Configs */

class TraceGenConfig extends Config(
  new WithTraceGen(2)() ++
  new GroundTestBaseConfig
)

class TraceGenBufferlessConfig extends Config(
  new WithBufferlessBroadcastHub ++
  new TraceGenConfig
)

/* Composable Configs to set individual parameters */

class GroundTestBaseConfig extends Config(
  new BaseConfig().alter((site,here,up) => {
    case DebugModuleKey => None
    case CLINTKey => None
    case PLICKey => None
    case SubsystemExternalResetVectorKey => true
  })
)

class WithTraceGen(
  n: Int = 2,
  overrideIdOffset: Option[Int] = None,
  overrideMemOffset: Option[BigInt] = None)(
  params: Seq[DCacheParams] = List.fill(n){ DCacheParams(nSets = 16, nWays = 1) },
  nReqs: Int = 8192
) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => {
    val prev = up(TilesLocated(InSubsystem), site)
    val idOffset = overrideIdOffset.getOrElse(prev.size)
    val memOffset: BigInt = overrideMemOffset.orElse(site(ExtMem).map(_.master.base)).getOrElse(0x0L)
    params.zipWithIndex.map { case (dcp, i) =>
      TraceGenTileAttachParams(
        tileParams = TraceGenParams(
          hartId = i + idOffset,
          dcache = Some(dcp),
          wordBits = site(XLen),
          addrBits = 32,
          addrBag = {
            val nSets = dcp.nSets
            val nWays = dcp.nWays
            val blockOffset = site(SystemBusKey).blockOffset
            val nBeats = site(SystemBusKey).blockBeats
            List.tabulate(nWays) { i =>
              Seq.tabulate(nBeats) { j => BigInt((j * 8) + ((i * nSets) << blockOffset)) }
            }.flatten
          },
          maxRequests = nReqs,
          memStart = memOffset,
          numGens = params.size),
        crossingParams = RocketCrossingParams()
      )
    } ++ prev
  }
})
