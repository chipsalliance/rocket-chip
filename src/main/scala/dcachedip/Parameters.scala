// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket.hellacache

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import scala.math.max

case class HellaCacheSinkParameters()

case class HellaCacheSourceParameters(
	dcacheReqBits: Int, //corresponding with dcacheReqTagBits
	addrBits: Int, //corresponding with coreMaxAddrBits
	dataBits: Int //corresponding with coreDataBits
)

case class HellaCacheBundleParameters(
	dcacheReqBits: Int,
	addrBits: Int,
	dataBits: Int
)

case class HellaCacheNullParameters()
