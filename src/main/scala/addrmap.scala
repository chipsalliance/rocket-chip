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

  val addrMap = new AddrHashMap(p(GlobalAddrMap))
}

abstract class MemRegion {
  def size: BigInt
  def numSlaves: Int
}

case class MemSize(size: BigInt, prot: Int, cacheable: Boolean = false) extends MemRegion {
  def numSlaves = 1
}
case class MemSubmap(size: BigInt, entries: AddrMap) extends MemRegion {
  val numSlaves = entries.countSlaves
}

object AddrMapConsts {
  val R = 0x1
  val W = 0x2
  val X = 0x4
  val RW = R | W
  val RX = R | X
  val RWX = R | W | X
}

class AddrMapProt extends Bundle {
  val x = Bool()
  val w = Bool()
  val r = Bool()
}

case class AddrMapEntry(name: String, region: MemRegion)

case class AddrHashMapEntry(port: Int, start: BigInt, size: BigInt, prot: Int, cacheable: Boolean)

class AddrMap(entries: Seq[AddrMapEntry]) extends scala.collection.IndexedSeq[AddrMapEntry] {
  def apply(index: Int): AddrMapEntry = entries(index)

  def length: Int = entries.size

  def countSlaves: Int = entries.map(_.region.numSlaves).foldLeft(0)(_ + _)

  override def tail: AddrMap = new AddrMap(entries.tail)
}

object AddrMap {
  def apply(elems: AddrMapEntry*): AddrMap = new AddrMap(elems)
}

class AddrHashMap(addrmap: AddrMap, start: BigInt = BigInt(0)) {
  val mapping = new HashMap[String, AddrHashMapEntry]

  private def genPairs(am: AddrMap, start: BigInt): Seq[(String, AddrHashMapEntry)] = {
    var ind = 0
    var base = start
    var pairs = Seq[(String, AddrHashMapEntry)]()
    am.foreach {
      case AddrMapEntry(name, MemSize(size, prot, cacheable)) =>
        pairs = (name, AddrHashMapEntry(ind, base, size, prot, cacheable)) +: pairs
        base += size
        ind += 1
      case AddrMapEntry(name, MemSubmap(size, submap)) =>
        val subpairs = genPairs(submap, base).map {
          case (subname, AddrHashMapEntry(subind, subbase, subsize, prot, cacheable)) =>
            (name + ":" + subname,
              AddrHashMapEntry(ind + subind, subbase, subsize, prot, cacheable))
        }
        pairs = subpairs ++ pairs
        ind += subpairs.size
        base += size
    }
    pairs
  }

  for ((name, ind) <- genPairs(addrmap, start)) { mapping(name) = ind }

  def nEntries: Int = mapping.size
  def apply(name: String): AddrHashMapEntry = mapping(name)
  def get(name: String): Option[AddrHashMapEntry] = mapping.get(name)
  def sortedEntries(): Seq[(String, BigInt, BigInt, Int, Boolean)] = {
    val arr = new Array[(String, BigInt, BigInt, Int, Boolean)](mapping.size)
    mapping.foreach { case (name, AddrHashMapEntry(port, base, size, prot, cacheable)) =>
      arr(port) = (name, base, size, prot, cacheable)
    }
    arr.toSeq
  }

  def isInRegion(name: String, addr: UInt): Bool = {
    val start = mapping(name).start
    val size = mapping(name).size
    UInt(start) <= addr && addr < UInt(start + size)
  }

  def isCacheable(addr: UInt): Bool = {
    sortedEntries().map { case (_, base, size, _, cacheable) =>
      UInt(base) <= addr && addr < UInt(base + size) && Bool(cacheable)
    }.reduce(_ || _)
  }

  def isValid(addr: UInt): Bool = {
    addr < UInt(start) || sortedEntries().map {
      case (_, base, size, _, _) =>
        addr >= UInt(base) && addr < UInt(base + size)
    }.reduceLeft(_ || _)
  }

  def getProt(addr: UInt): AddrMapProt = {
    val protBits = Mux(addr < UInt(start),
      Bits(AddrMapConsts.RWX, 3),
      Mux1H(sortedEntries().map { case (_, base, size, prot, _) =>
        (addr >= UInt(base) && addr < UInt(base + size), Bits(prot, 3))
      }))
    new AddrMapProt().fromBits(protBits)
  }
}
