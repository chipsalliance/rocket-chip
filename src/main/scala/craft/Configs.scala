package craft

import rocketchip._
import cde.{Parameters, Config, CDEMatchError}
import uncore.tilelink._
import uncore.coherence.{MICoherence, NullRepresentation}
import uncore.agents.CacheBlockBytes
import junctions._

class WithCraft extends Config(
  (pname, site, here) => pname match {
    case TLKey("XBar") => site(TLKey("MCtoEdge")).copy(
      nCachingClients = 0,
      nCachelessClients = site(OutPorts),
      maxClientXacts = 4,
      maxClientsPerPort = site(InPorts))
    case TLId => "XBar"
    case InPorts => 2
    case OutPorts => site(GlobalAddrMap).flatten.size
    case GlobalAddrMap => {
      val sizes = Seq(0x400, 0x400)
      val entries = sizes.zipWithIndex.map { case (sz, i) =>
        AddrMapEntry(s"chan$i", MemSize(sz, MemAttr(AddrMapProt.RWX)))
      }
      new AddrMap(entries)
    }
    case ExtMemSize => site(GlobalAddrMap).size.longValue
    case XBarQueueDepth => 2
    case _ => throw new CDEMatchError
  })

class WithInPorts(n: Int) extends Config(
  (pname, site, here) => pname match {
    case InPorts => n
    case _ => throw new CDEMatchError
  })

class WithOutPorts(sizes: Seq[Int]) extends Config(
  (pname, site, here) => pname match {
    case GlobalAddrMap => {
      val entries = sizes.zipWithIndex.map { case (sz, i) =>
        AddrMapEntry(s"chan$i", MemSize(sz, MemAttr(AddrMapProt.RWX)))
      }
      new AddrMap(entries)
    }
    case _ => throw new CDEMatchError
  })

class CraftConfig extends Config(new WithCraft ++ new BaseConfig)

class TwoByTwoConfig extends Config(
  new WithInPorts(2) ++
  new WithOutPorts(Seq(0x600, 0x200)) ++
  new CraftConfig)

class OneBySevenConfig extends Config(
  new WithInPorts(1) ++
  new WithOutPorts(Seq(0x1000, 0x800, 0x400, 0x200, 0x100, 0x80, 0x80)) ++
  new CraftConfig)
