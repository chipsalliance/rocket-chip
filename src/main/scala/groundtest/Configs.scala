// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.groundtest

import Chisel._
import freechips.rocketchip.chip._
import freechips.rocketchip.coreplex._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

/** Actual testing target Configs */

class TraceGenConfig extends Config(new WithTraceGen(2) ++ new BaseConfig)

class TraceGenBufferlessConfig extends Config(
  new WithBufferlessBroadcastHub ++ new TraceGenConfig)

/* Composable Configs to set individual parameters */

class WithTraceGen(n: Int) extends Config((site, here, up) => {
  case GroundTestTilesKey => Seq.fill(n) { TraceGenParams(
    dcache = Some(DCacheParams(nSets = 16, nWays = 1)),
    wordBits = site(XLen),
    addrBits = site(PAddrBits),
    addrBag = {
      val nSets = 2
      val nWays = 1
      val blockOffset = log2Up(site(CacheBlockBytes))
      val nBeats = site(CacheBlockBytes)/site(L1toL2Config).beatBytes
      List.tabulate(4 * nWays) { i =>
        Seq.tabulate(nBeats) { j => BigInt((j * 8) + ((i * nSets) << blockOffset)) }
      }.flatten
    },
    maxRequests = 8192,
    memStart = site(ExtMem).base,
    numGens = n)
  }   
})
