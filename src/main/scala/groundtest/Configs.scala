// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.groundtest

import org.chipsalliance.cde.config._

import freechips.rocketchip.devices.tilelink.{CLINTKey, PLICKey}
import freechips.rocketchip.devices.debug.DebugModuleKey
import freechips.rocketchip.subsystem._
import freechips.rocketchip.system.BaseConfig
import freechips.rocketchip.rocket.DCacheParams

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
    case HasTilesExternalResetVectorKey => true
  })
)

class WithTraceGen(
  n: Int = 2,
  overrideMemOffset: Option[BigInt] = None)(
  params: Seq[DCacheParams] = List.fill(n){ DCacheParams(nSets = 16, nWays = 1) },
  nReqs: Int = 8192,
  wordBits: Int = 32
) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => {
    val prev = up(TilesLocated(InSubsystem), site)
    val idOffset = up(NumTiles)
    val memOffset: BigInt = overrideMemOffset.orElse(site(ExtMem).map(_.client.base)).getOrElse(0x0L)
    params.zipWithIndex.map { case (dcp, i) =>
      TraceGenTileAttachParams(
        tileParams = TraceGenParams(
          tileId = i + idOffset,
          dcache = Some(dcp),
          wordBits = wordBits,
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
  case NumTiles => up(NumTiles) + n
})
