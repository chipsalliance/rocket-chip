// See LICENSE.SiFive for license details.

package freechips.rocketchip.NAMESAPCE

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile
import scala.math.max


case class NAMESPACESinkParameters(
	fLen: Int,
	divSqrt: Boolean
)

//case class NAMESPACESinkPortParameters(
//  slaves:    Seq[NAMESPACESinkParameters],
//  beatBytes: Int)
//{
//  require (!slaves.isEmpty)
//  require (isPow2(beatBytes))
//
//  val maxTransfer = slaves.map(_.maxTransfer).max
//  val maxAddress = slaves.map(_.maxAddress).max
//
//  // Check the link is not pointlessly wide
//  require (maxTransfer >= beatBytes)
//  // Check that the link can be implemented in NAMESPACE
//  require (maxTransfer <= beatBytes * NAMESPACEParameters.maxTransfer)
//
//  // Require disjoint ranges for addresses
//  slaves.combinations(2).foreach { case Seq(x,y) =>
//    x.address.foreach { a => y.address.foreach { b =>
//      require (!a.overlaps(b))
//    } }
//  }
//}

case class NAMESPACENullParameters()

//case class NAMESPACESourcePortParameters(
//  masters: Seq[NAMESPACESourceParameters])

//case class NAMESPACEBundleParameters()

//object NAMESPACEBundleParameters

//  val emptyBundleParams = NAMESPACEBundleParameters(addrBits = 1, dataBits = 8)
//  def union(x: Seq[NAMESPACEBundleParameters]) = x.foldLeft(emptyBundleParams)((x,y) => x.union(y))
//
//  def apply(master: NAMESPACESourcePortParameters, slave: NAMESPACESinkPortParameters) =
//    new NAMESPACEBundleParameters(
//      addrBits = log2Up(slave.maxAddress+1),
//      dataBits = slave.beatBytes * 8)


