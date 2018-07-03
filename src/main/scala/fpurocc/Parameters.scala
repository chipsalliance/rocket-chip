// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile.NAMESAPCE

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile
import scala.math.max

case class NAMESPACESinkParameters(
	fLen: Int
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

case class NAMESPACESourceParameters(
  name:     String,
  nodePath: Seq[BaseNode] = Seq())

case class NAMESPACESourcePortParameters(
  masters: Seq[NAMESPACESourceParameters])

case class NAMESPACEBundleParameters(
  addrBits: Int,
  dataBits: Int)
{
  require (dataBits >= 8)
  require (addrBits >= 1)
  require (isPow2(dataBits))

  // Bring the globals into scope
  val transBits = NAMESPACEParameters.transBits
  val burstBits = NAMESPACEParameters.burstBits
  val protBits  = NAMESPACEParameters.protBits
  val sizeBits  = NAMESPACEParameters.sizeBits

  def union(x: NAMESPACEBundleParameters) =
    NAMESPACEBundleParameters(
      max(addrBits, x.addrBits),
      max(dataBits, x.dataBits))
}

object NAMESPACEBundleParameters
{
  val emptyBundleParams = NAMESPACEBundleParameters(addrBits = 1, dataBits = 8)
  def union(x: Seq[NAMESPACEBundleParameters]) = x.foldLeft(emptyBundleParams)((x,y) => x.union(y))

  def apply(master: NAMESPACESourcePortParameters, slave: NAMESPACESinkPortParameters) =
    new NAMESPACEBundleParameters(
      addrBits = log2Up(slave.maxAddress+1),
      dataBits = slave.beatBytes * 8)
}

case class NAMESPACEEdgeParameters(
  master: NAMESPACESourcePortParameters,
  slave:  NAMESPACESinkPortParameters,
  params: Parameters,
  sourceInfo: SourceInfo)
{
  val bundle = NAMESPACEBundleParameters(master, slave)
}
