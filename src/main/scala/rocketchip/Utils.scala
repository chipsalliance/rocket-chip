// See LICENSE for license details.

package rocketchip

import cde.{Parameters, Dump}
import junctions._
import uncore.devices._
import rocket._
import rocket.Util._
import coreplex._

class RangeManager {
  private var finalized = false
  private val l = collection.mutable.HashMap[String, Int]()
  def add(name: String, element: Int) = { require(!finalized); l += (name -> element) }
  def rangeMap = {
    finalized = true
    l map {
      var sum = 0
      x => { sum += x._2; (x._1 -> (sum-x._2, sum)) }
    }
  }
  def range(name: String) = rangeMap(name)
  def print = {
    rangeMap map { case (name, (start, end)) =>
      println(s"${name} on port ${start}-${end-1}")
    }
  }
  def sum = {
    finalized = true
    l.map(_._2).sum
  }
}

class ResourceManager[T] {
  private var finalized = false
  private val l = collection.mutable.ArrayBuffer[T]()
  def add(element: T) = { require(!finalized); l += element }
  def add(list: Seq[T]) = { require(!finalized); l ++= list }
  def get: Seq[T] = { finalized = true; l }
}

class GlobalVariable[T] {
  private var assigned = false
  private var variable: T = _
  def assign(value: T) = { require(!assigned); assigned = true; variable = value }
  def get: T = { require(assigned); variable }
}

object GenerateGlobalAddrMap {
  def apply(p: Parameters, pDevicesEntries: Seq[AddrMapEntry]) = {
    lazy val intIOAddrMap: AddrMap = {
      val entries = collection.mutable.ArrayBuffer[AddrMapEntry]()
      entries += AddrMapEntry("debug", MemSize(4096, MemAttr(AddrMapProt.RWX)))
      entries += AddrMapEntry("bootrom", MemSize(4096, MemAttr(AddrMapProt.RX)))
      entries += AddrMapEntry("plic", MemRange(0x40000000, 0x4000000, MemAttr(AddrMapProt.RW)))
      entries += AddrMapEntry("prci", MemSize(0x4000000, MemAttr(AddrMapProt.RW)))
      if (p(DataScratchpadSize) > 0) { // TODO heterogeneous tiles
        require(p(NTiles) == 1) // TODO relax this
        require(p(NMemoryChannels) == 0) // TODO allow both scratchpad & DRAM
        entries += AddrMapEntry("dmem0", MemRange(0x80000000L, BigInt(p(DataScratchpadSize)), MemAttr(AddrMapProt.RWX)))
      }
      new AddrMap(entries)
    }

    lazy val extIOAddrMap = new AddrMap(
      pDevicesEntries ++ p(ExtMMIOPorts),
      start = BigInt("50000000", 16),
      collapse = true)

    val memBase = 0x80000000L
    val memSize = p(ExtMemSize)
    Dump("MEM_BASE", memBase)

    val intern = AddrMapEntry("int", intIOAddrMap)
    val extern = AddrMapEntry("ext", extIOAddrMap)
    val io = AddrMapEntry("io", AddrMap((intern +: (!extIOAddrMap.isEmpty).option(extern).toSeq):_*))
    val mem = AddrMapEntry("mem", MemRange(memBase, memSize, MemAttr(AddrMapProt.RWX, true)))
    AddrMap((io +: (p(NMemoryChannels) > 0).option(mem).toSeq):_*)
  }
}

object GenerateConfigString {
  def apply(p: Parameters, c: CoreplexConfig, pDevicesEntries: Seq[AddrMapEntry]) = {
    val addrMap = p(GlobalAddrMap).get
    val plicAddr = addrMap("io:int:plic").start
    val prciAddr = addrMap("io:int:prci").start
    val xLen = p(XLen)
    val res = new StringBuilder
    res append  "plic {\n"
    res append s"  priority 0x${plicAddr.toString(16)};\n"
    res append s"  pending 0x${(plicAddr + c.plicKey.pendingBase).toString(16)};\n"
    res append s"  ndevs ${c.plicKey.nDevices};\n"
    res append  "};\n"
    res append  "rtc {\n"
    res append s"  addr 0x${(prciAddr + PRCI.time).toString(16)};\n"
    res append  "};\n"
    if (addrMap contains "mem") {
      res append  "ram {\n"
      res append  "  0 {\n"
      res append s"    addr 0x${addrMap("mem").start.toString(16)};\n"
      res append s"    size 0x${addrMap("mem").size.toString(16)};\n"
      res append  "  };\n"
      res append  "};\n"
    }
    res append  "core {\n"
    for (i <- 0 until c.nTiles) { // TODO heterogeneous tiles
      val isa = {
        val m = if (p(MulDivKey).nonEmpty) "m" else ""
        val a = if (p(UseAtomics)) "a" else ""
        val f = if (p(FPUKey).nonEmpty) "f" else ""
        val d = if (p(FPUKey).nonEmpty && p(XLen) > 32) "d" else ""
        val s = if (c.hasSupervisor) "s" else ""
        s"rv${p(XLen)}i$m$a$f$d$s"
      }
      res append s"  $i {\n"
      res append  "    0 {\n"
      res append s"      isa $isa;\n"
      res append s"      timecmp 0x${(prciAddr + PRCI.timecmp(i)).toString(16)};\n"
      res append s"      ipi 0x${(prciAddr + PRCI.msip(i)).toString(16)};\n"
      res append s"      plic {\n"
      res append s"        m {\n"
      res append s"         ie 0x${(plicAddr + c.plicKey.enableAddr(i, 'M')).toString(16)};\n"
      res append s"         thresh 0x${(plicAddr + c.plicKey.threshAddr(i, 'M')).toString(16)};\n"
      res append s"         claim 0x${(plicAddr + c.plicKey.claimAddr(i, 'M')).toString(16)};\n"
      res append s"        };\n"
      if (c.hasSupervisor) {
        res append s"        s {\n"
        res append s"         ie 0x${(plicAddr + c.plicKey.enableAddr(i, 'S')).toString(16)};\n"
        res append s"         thresh 0x${(plicAddr + c.plicKey.threshAddr(i, 'S')).toString(16)};\n"
        res append s"         claim 0x${(plicAddr + c.plicKey.claimAddr(i, 'S')).toString(16)};\n"
        res append s"        };\n"
      }
      res append  "      };\n"
      res append  "    };\n"
      res append  "  };\n"
    }
    res append  "};\n"
    pDevicesEntries foreach { entry =>
      val region = addrMap("io:ext:" + entry.name)
      res append s"${entry.name} {\n"
      res append s"  addr 0x${region.start.toString(16)};\n"
      res append s"  size 0x${region.size.toString(16)}; \n"
      res append  "}\n"
    }
    res append '\u0000'
    res.toString
  }
}
