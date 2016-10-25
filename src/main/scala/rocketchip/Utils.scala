// See LICENSE for license details.

package rocketchip

import cde.{Parameters, Dump}
import junctions._
import diplomacy._
import uncore.devices._
import rocket._
import coreplex._
import uncore.tilelink2._
import util._

import java.nio.file.{Files, Paths}
import java.nio.{ByteBuffer, ByteOrder}

class RangeManager {
  private var finalized = false
  private val l = collection.mutable.ListBuffer[(String, Int)]()
  def add(name: String, element: Int) = { require(!finalized); l += (name -> element) }
  def rangeMap = {
    finalized = true
    (l map {
      var sum = 0
      x => { sum += x._2; (x._1 -> (sum-x._2, sum)) }
    }).toMap
  }
  def range(name: String) = rangeMap(name)
  def print = {
    rangeMap.toSeq.sortBy(_._2).foreach { case (name, (start, end)) =>
      println(s"${name} on int ${start}-${end-1}")
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
  def apply(p: Parameters, peripheryManagers: Seq[TLManagerParameters]) = {
    lazy val cBusIOAddrMap: AddrMap = {
      val entries = collection.mutable.ArrayBuffer[AddrMapEntry]()
      entries += AddrMapEntry("debug", MemSize(4096, MemAttr(AddrMapProt.RWX)))
      entries += AddrMapEntry("plic", MemRange(0x0C000000, 0x4000000, MemAttr(AddrMapProt.RW)))
      if (p(DataScratchpadSize) > 0) { // TODO heterogeneous tiles
        require(p(NTiles) == 1) // TODO relax this
        require(p(NMemoryChannels) == 0) // TODO allow both scratchpad & DRAM
        entries += AddrMapEntry("dmem0", MemRange(0x80000000L, BigInt(p(DataScratchpadSize)), MemAttr(AddrMapProt.RWX)))
      }
      new AddrMap(entries)
    }

    lazy val tl2Devices = peripheryManagers.map { manager =>
      val cacheable = manager.regionType match {
        case RegionType.CACHED   => true
        case RegionType.TRACKED  => true
        case _ => false
      }
      val attr = MemAttr(
        (if (manager.supportsGet)     AddrMapProt.R else 0) |
        (if (manager.supportsPutFull) AddrMapProt.W else 0) |
        (if (manager.executable)      AddrMapProt.X else 0), cacheable)
      val multi = manager.address.size > 1
      manager.address.zipWithIndex.map { case (address, i) =>
        require (address.contiguous) // TL1 needs this
        val name = manager.name + (if (multi) ".%d".format(i) else "")
        AddrMapEntry(name, MemRange(address.base, address.mask+1, attr))
      }
    }.flatten

    lazy val uniquelyNamedTL2Devices =
      tl2Devices.groupBy(_.name).values.map(_.zipWithIndex.map {
        case (e, i) => if (i == 0) e else e.copy(name = e.name + "_" + i)
      }).flatten.toList

    lazy val tl2AddrMap = new AddrMap(uniquelyNamedTL2Devices, collapse = true)
    lazy val pBusIOAddrMap = new AddrMap(AddrMapEntry("TL2", tl2AddrMap) +: p(ExtMMIOPorts), collapse = true)

    val memBase = 0x80000000L
    val memSize = p(ExtMemSize)
    Dump("MEM_BASE", memBase)

    val cBus = AddrMapEntry("cbus", cBusIOAddrMap)
    val pBus = AddrMapEntry("pbus", pBusIOAddrMap)
    val io = AddrMapEntry("io", AddrMap((cBus +: (!pBusIOAddrMap.isEmpty).option(pBus).toSeq):_*))
    val mem = AddrMapEntry("mem", MemRange(memBase, memSize, MemAttr(AddrMapProt.RWX, true)))
    AddrMap((io +: (p(NMemoryChannels) > 0).option(mem).toSeq):_*)
  }
}

object GenerateConfigString {
  def apply(p: Parameters, c: CoreplexConfig, peripheryManagers: Seq[TLManagerParameters]) = {
    val addrMap = p(GlobalAddrMap)
    val plicAddr = addrMap("io:cbus:plic").start
    val clint = CoreplexLocalInterrupterConfig(0, addrMap("io:pbus:TL2:clint").start)
    val xLen = p(XLen)
    val res = new StringBuilder
    res append  "plic {\n"
    res append s"  priority 0x${plicAddr.toString(16)};\n"
    res append s"  pending 0x${(plicAddr + c.plicKey.pendingBase).toString(16)};\n"
    res append s"  ndevs ${c.plicKey.nDevices};\n"
    res append  "};\n"
    res append  "rtc {\n"
    res append s"  addr 0x${clint.timeAddress.toString(16)};\n"
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
      res append s"      timecmp 0x${clint.timecmpAddress(i).toString(16)};\n"
      res append s"      ipi 0x${clint.msipAddress(i).toString(16)};\n"
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
    peripheryManagers.foreach { manager => res append manager.dts }
    res append '\u0000'
    res.toString
  }
}

object GenerateBootROM {
  def apply(p: Parameters, address: BigInt) = {
    val romdata = Files.readAllBytes(Paths.get(p(BootROMFile)))
    val rom = ByteBuffer.wrap(romdata)

    rom.order(ByteOrder.LITTLE_ENDIAN)

    require(address == address.toInt)
    val configStringAddr = address.toInt + rom.capacity
    require(rom.getInt(12) == 0,
      "Config string address position should not be occupied by code")
    rom.putInt(12, configStringAddr)
    rom.array() ++ (p(ConfigString).getBytes.toSeq)
  }
}
