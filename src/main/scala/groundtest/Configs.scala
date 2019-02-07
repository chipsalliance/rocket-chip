// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.groundtest

import Chisel._
import freechips.rocketchip.config.Config
import freechips.rocketchip.subsystem._
import freechips.rocketchip.rocket.{DCacheParams}
import freechips.rocketchip.tile.{MaxHartIdBits, XLen}

/** Actual testing target Configs */

class TraceGenConfig extends Config(new WithTraceGen(List.fill(2){ DCacheParams(nSets = 16, nWays = 1) }) ++ new BaseSubsystemConfig)

class TraceGenBufferlessConfig extends Config(new WithBufferlessBroadcastHub ++ new TraceGenConfig)

/* Composable Configs to set individual parameters */

class WithTraceGen(params: Seq[DCacheParams], nReqs: Int = 8192) extends Config((site, here, up) => {
  case GroundTestTilesKey => params.map { dcp => TraceGenParams(
    dcache = Some(dcp),
    wordBits = site(XLen),
    addrBits = 32,
    addrBag = {
      val nSets = 2
      val nWays = 1
      val blockOffset = site(SystemBusKey).blockOffset
      val nBeats = site(SystemBusKey).blockBeats
      List.tabulate(4 * nWays) { i =>
        Seq.tabulate(nBeats) { j => BigInt((j * 8) + ((i * nSets) << blockOffset)) }
      }.flatten
    },
    maxRequests = nReqs,
    memStart = site(ExtMem).get.master.base,
    numGens = params.size)
  }   
  case MaxHartIdBits => log2Up(params.size)
})
