// See LICENSE for license details.

package junctions

import Chisel._
import cde.{Parameters, Field}
import scala.collection.mutable.HashMap

case object PAddrBits extends Field[Int]
case object VAddrBits extends Field[Int]
case object PgIdxBits extends Field[Int]
case object PgLevels extends Field[Int]
case object PgLevelBits extends Field[Int]
case object ASIdBits extends Field[Int]
case object PPNBits extends Field[Int]
case object VPNBits extends Field[Int]

case object GlobalAddrMap extends Field[AddrMap]

trait HasAddrMapParameters {
  implicit val p: Parameters

  val paddrBits = p(PAddrBits)
  val vaddrBits = p(VAddrBits)
  val pgIdxBits = p(PgIdxBits)
  val ppnBits = p(PPNBits)
  val vpnBits = p(VPNBits)
  val pgLevels = p(PgLevels)
  val pgLevelBits = p(PgLevelBits)
  val asIdBits = p(ASIdBits)

  val addrMap = p(GlobalAddrMap)
}

case class MemAttr(prot: Int, cacheable: Boolean = false)

sealed abstract class MemRegion {
  def start: BigInt
  def size: BigInt
  def numSlaves: Int
  def attr: MemAttr

  def containsAddress(x: UInt) = UInt(start) <= x && x < UInt(start + size)
}

case class MemSize(size: BigInt, attr: MemAttr) extends MemRegion {
  def start = 0
  def numSlaves = 1
}

case class MemRange(start: BigInt, size: BigInt, attr: MemAttr) extends MemRegion {
  def numSlaves = 1
}

object AddrMapProt {
  val R = 0x1
  val W = 0x2
  val X = 0x4
  val RW = R | W
  val RX = R | X
  val RWX = R | W | X
  val SZ = 3
}

class AddrMapProt extends Bundle {
  val x = Bool()
  val w = Bool()
  val r = Bool()
}

case class AddrMapEntry(name: String, region: MemRegion)

object AddrMap {
  def apply(elems: AddrMapEntry*): AddrMap = new AddrMap(elems)
}

class AddrMap(entriesIn: Seq[AddrMapEntry], val start: BigInt = BigInt(0)) extends MemRegion {
  private val slavePorts = HashMap[String, Int]()
  private val mapping = HashMap[String, MemRegion]()

  def isEmpty = entries.isEmpty
  def length = entries.size
  def numSlaves = slavePorts.size

  val (size: BigInt, entries: Seq[AddrMapEntry], attr: MemAttr) = {
    var ind = 0
    var base = start
    var rebasedEntries = collection.mutable.ArrayBuffer[AddrMapEntry]()
    var prot = 0
    var cacheable = true
    for (AddrMapEntry(name, r) <- entriesIn) {
      if (r.start != 0) {
        val align = BigInt(1) << log2Ceil(r.size)
        require(r.start >= base, s"region $name base address 0x${r.start.toString(16)} overlaps previous base 0x${base.toString(16)}")
        base = r.start
      } else {
        base = (base + r.size - 1) / r.size * r.size
      }

      r match {
        case r: AddrMap =>
          val subMap = new AddrMap(r.entries, base)
          rebasedEntries += AddrMapEntry(name, subMap)
          mapping += name -> subMap
          mapping ++= subMap.mapping.map { case (k, v) => s"$name:$k" -> v }
          slavePorts ++= subMap.slavePorts.map { case (k, v) => s"$name:$k" -> (ind + v) }
        case _ =>
          val e = MemRange(base, r.size, r.attr)
          rebasedEntries += AddrMapEntry(name, e)
          mapping += name -> e
          slavePorts += name -> ind
      }

      ind += r.numSlaves
      base += r.size
      prot |= r.attr.prot
      cacheable &&= r.attr.cacheable
    }
    (base - start, rebasedEntries, MemAttr(prot, cacheable))
  }

  val flatten: Seq[AddrMapEntry] = {
    mapping.toSeq.map {
      case (name, range: MemRange) => Some(AddrMapEntry(name, range))
      case _ => None
    }.flatten.sortBy(_.region.start)
  }

  def toRange: MemRange = MemRange(start, size, attr)
  def apply(name: String): MemRegion = mapping(name)
  def contains(name: String): Boolean = mapping.contains(name)
  def port(name: String): Int = slavePorts(name)
  def subMap(name: String): AddrMap = mapping(name).asInstanceOf[AddrMap]
  def isInRegion(name: String, addr: UInt): Bool = mapping(name).containsAddress(addr)

  def isCacheable(addr: UInt): Bool = {
    flatten.filter(_.region.attr.cacheable).map(
      _.region.containsAddress(addr)
    ).foldLeft(Bool(false))(_ || _)
  }

  def isValid(addr: UInt): Bool = {
    flatten.map(_.region.containsAddress(addr)).foldLeft(Bool(false))(_ || _)
  }

  def getProt(addr: UInt): AddrMapProt = {
    val protForRegion = flatten.map { entry =>
      Mux(entry.region.containsAddress(addr),
        UInt(entry.region.attr.prot, AddrMapProt.SZ), UInt(0))
    }
    new AddrMapProt().fromBits(protForRegion.reduce(_|_))
  }
}
