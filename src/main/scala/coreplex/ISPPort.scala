// See LICENSE.SiFive for license details.

package freechips.rocketchip.coreplex

import Chisel._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.tilelink._

trait HasISPPort extends CoreplexNetwork {
  val module: HasISPPortModule

  // TODO: use ChipLink instead of AsyncTileLink
  val isp_in = TLAsyncInputNode()
  val isp_out = TLAsyncOutputNode()

  private val out_xbar = LazyModule(new TLXbar)
  private val out_nums = LazyModule(new TLNodeNumberer)
  private val out_async = LazyModule(new TLAsyncCrossingSource)
  out_xbar.node :=* tile_splitter.node
  out_nums.node :*= out_xbar.node
  out_async.node :*= out_nums.node
  isp_out :*= out_async.node

  private val in_async = LazyModule(new TLAsyncCrossingSink)
  in_async.node :=* isp_in
  sbus.node :=* in_async.node
}

trait HasISPPortBundle extends CoreplexNetworkBundle {
  val outer: HasISPPort

  // TODO: move to IO(...) in Module?
  val isp_in = outer.isp_in.bundleIn
  val isp_out = outer.isp_out.bundleOut
}

trait HasISPPortModule extends CoreplexNetworkModule {
  val outer: HasISPPort
  val io: HasISPPortBundle
}
