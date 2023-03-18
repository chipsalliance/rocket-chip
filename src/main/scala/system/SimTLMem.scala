// See LICENSE.SiFive for license details.

package freechips.rocketchip.system // TODO this should really be in a testharness package

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

/** Memory with TL port for use in elaboratable test harnesses.
 * 
 * Topology: TLRAM <-< TLBuffer <-< TLFragmenter <-< TLXbar <-< TLMasterNode
*/
class SimTLMem(edge: TLEdgeParameters, size: BigInt, base: BigInt = 0)(implicit p: Parameters) extends SimpleLazyModule {
  val node = TLClientNode(List(edge.master))
  val srams = AddressSet.misaligned(base, size).map { aSet =>
    LazyModule(new TLRAM(
      address = aSet,
      beatBytes = edge.bundle.dataBits/8))
  }
  val xbar = TLXbar()
// srams.foreach{ s => s.node := TLBuffer() := TLFragmenter() := xbar }
  srams.foreach{ s => s.node := TLBuffer() := xbar }
  xbar := node
  val io_tl = InModuleBody { node.makeIOs() }
}
