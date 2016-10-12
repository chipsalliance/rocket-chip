package craft

import rocketchip._
import cde.{Parameters, Config, CDEMatchError}
import uncore.tilelink._
import uncore.coherence.{MICoherence, NullRepresentation}
import uncore.agents.CacheBlockBytes

class WithCraft extends Config(
  (pname, site, here) => pname match {
    case TLKey("XBar") => site(TLKey("MCtoEdge")).copy(
      nCachingClients = 0,
      nCachelessClients = site(OutPorts),
      maxClientXacts = 4,
      maxClientsPerPort = site(InPorts))
    case TLId => "XBar"
    case InPorts => 2
    case OutPorts => 2
    case XBarQueueDepth => 2
    case ExtMemSize => 0x800L
    case _ => throw new CDEMatchError
  })

class CraftConfig extends Config(new WithCraft ++ new BaseConfig)
