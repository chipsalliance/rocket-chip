// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import Chisel._

import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.tilelink.ClientMetadata
import freechips.rocketchip.util._

trait L1CacheParams {
  def nSets:         Int
  def nWays:         Int
  def rowBits:       Int
  def nTLBEntries:   Int
  def blockBytes:    Int // TODO this is ignored in favor of p(CacheBlockBytes) in BaseTile
}

trait HasL1CacheParameters extends HasTileParameters {
  val cacheParams: L1CacheParams
  private val bundleParams = p(SharedMemoryTLEdge).bundle

  def nSets = cacheParams.nSets
  def blockOffBits = lgCacheBlockBytes
  def idxBits = log2Up(cacheParams.nSets)
  def untagBits = blockOffBits + idxBits
  def tagBits = bundleParams.addressBits - (if (usingVM) untagBits min pgIdxBits else untagBits)
  def nWays = cacheParams.nWays
  def wayBits = log2Up(nWays)
  def isDM = nWays == 1
  def rowBits = cacheParams.rowBits
  def rowBytes = rowBits/8
  def rowOffBits = log2Up(rowBytes)
  def nTLBEntries = cacheParams.nTLBEntries

  def cacheDataBits = bundleParams.dataBits
  def cacheDataBeats = (cacheBlockBytes * 8) / cacheDataBits
  def refillCycles = cacheDataBeats
}

abstract class L1CacheModule(implicit val p: Parameters) extends Module
  with HasL1CacheParameters

abstract class L1CacheBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasL1CacheParameters
