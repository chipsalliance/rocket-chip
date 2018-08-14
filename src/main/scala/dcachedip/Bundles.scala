// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket.hellacache

import chisel3._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._

// Signal directions are from the master's point-of-view
class HellaCacheDiplomaticBundle()(implicit val p: Parameters) extends HellaCacheIO(p) 

object HellaCacheDiplomaticBundle {
	def apply(implicit p: Parameters) = new HellaCacheDiplomaticBundle
}
