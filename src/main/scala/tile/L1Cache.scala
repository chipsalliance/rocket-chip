// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import chisel3._
import chisel3.util.log2Up

import org.chipsalliance.cde.config.Parameters

trait L1CacheParams {
  def nSets:         Int
  def nWays:         Int
  def rowBits:       Int
  def nTLBSets:      Int
  def nTLBWays:      Int
  def blockBytes:    Int // TODO this is ignored in favor of p(CacheBlockBytes) in BaseTile
}

trait HasL1CacheParameters extends HasTileParameters {
  val cacheParams: L1CacheParams

  def nSets = cacheParams.nSets
  def blockOffBits = lgCacheBlockBytes
  def idxBits = log2Up(cacheParams.nSets)
  def untagBits = blockOffBits + idxBits
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

  def cacheDataBits = cacheParams.rowBits
  def cacheDataBytes = cacheDataBits / 8
  def cacheDataBeats = (cacheBlockBytes * 8) / cacheDataBits
  def refillCycles = cacheDataBeats
}

abstract class L1CacheModule(implicit val p: Parameters) extends Module
  with HasL1CacheParameters

abstract class L1CacheBundle(implicit val p: Parameters) extends Bundle
  with HasL1CacheParameters
