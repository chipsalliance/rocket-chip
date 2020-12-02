// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import Chisel._

import freechips.rocketchip.config.Parameters

trait L1CacheParams {
  def nSets:         Int
  def nWays:         Int
  def rowBits:       Int
  def nTLBSets:      Int
  def nTLBWays:      Int
  def blockBytes:    Int // TODO this is ignored in favor of p(CacheBlockBytes) in BaseTile
}

/**
  *                        ┌──idxBits──┐
  *                        ↓           ↓
  * │          tag         │    set    │offset│
  *                        ↑           ↑
  *                   untagBits   blockOffBits
  */
trait HasL1CacheParameters extends HasTileParameters {
  val cacheParams: L1CacheParams

  /** Set Size. */
  def nSets = cacheParams.nSets
  /** Block offset Bits. */
  def blockOffBits = lgCacheBlockBytes
  /** Set bits. */
  def idxBits = log2Up(cacheParams.nSets)
  /** Untag Bits */
  def untagBits = blockOffBits + idxBits
  /** minimal of [[pgIdxBits]] or [[untagBits]] when using paging.
    * used for way sizes greater than 4 KiB when using paging.
    */
  def pgUntagBits = if (usingVM) untagBits min pgIdxBits else untagBits
  def tagBits = tlBundleParams.addressBits - pgUntagBits
  def nWays = cacheParams.nWays
  def wayBits = log2Up(nWays)
  def isDM = nWays == 1
  def rowBits = cacheParams.rowBits
  def rowBytes = rowBits/8
  def rowOffBits = log2Up(rowBytes)
  def nTLBSets = cacheParams.nTLBSets
  def nTLBWays = cacheParams.nTLBWays

  def cacheDataBits = tlBundleParams.dataBits
  def cacheDataBytes = cacheDataBits / 8
  def cacheDataBeats = (cacheBlockBytes * 8) / cacheDataBits
  /** How many cycles will be consumed for a refilling. */
  def refillCycles = cacheDataBeats
}

abstract class L1CacheModule(implicit val p: Parameters) extends Module
  with HasL1CacheParameters

abstract class L1CacheBundle(implicit val p: Parameters) extends Bundle
  with HasL1CacheParameters
