package rocketchip

import Chisel._
import cde.{Parameters, Field, Config}
import uncore.agents._
import uncore.coherence.{InnerTLId, OuterTLId, MEICoherence}
import rocket._
import uncore.tilelink.{TLKey, TileLinkParameters}


class ManagerToClientStatelessBridge(implicit p: Parameters) extends HierarchicalCoherenceAgent()(p) {
  val icid = io.inner.acquire.bits.client_id.getWidth
  val ixid = io.inner.acquire.bits.client_xact_id.getWidth
  val oxid = io.outer.acquire.bits.client_xact_id.getWidth
  println ("icid is " + icid + " ixid is " + ixid + " oxid is " + oxid)
  require(icid + ixid <= oxid)
  require(icid == io.inner.release.bits.client_id.getWidth)
  require(ixid == io.inner.release.bits.client_xact_id.getWidth)
  require(oxid == io.outer.release.bits.client_xact_id.getWidth)

  io.outer.acquire.valid := io.inner.acquire.valid
  io.inner.acquire.ready := io.outer.acquire.ready
  io.outer.acquire.bits := io.inner.acquire.bits
  io.outer.acquire.bits.client_xact_id := Cat(io.inner.acquire.bits.client_id, io.inner.acquire.bits.client_xact_id)

  io.outer.release.valid := io.inner.release.valid
  io.inner.release.ready := io.outer.release.ready
  io.outer.release.bits := io.inner.release.bits
  io.outer.release.bits.client_xact_id := Cat(io.inner.release.bits.client_id, io.inner.release.bits.client_xact_id)

  io.inner.grant.valid := io.outer.grant.valid
  io.outer.grant.ready := io.inner.grant.ready
  io.inner.grant.bits := io.outer.grant.bits
  io.inner.grant.bits.client_xact_id := io.outer.grant.bits.client_xact_id(ixid-1, 0)
  io.inner.grant.bits.client_id := io.outer.grant.bits.client_xact_id(icid+ixid-1, ixid)

  io.inner.probe.valid := Bool(false)
  io.inner.finish.ready := Bool(true)
}

class WithNExtInts(nExtInts: Int) extends Config(
  (pname,site,here) => pname match{
    case NExtInterrupts => nExtInts
  }
)

class WithNExtAXIMMIOChannels(nExtAXIMMIO: Int) extends Config (
  (pname,site,here)=> pname match{
    case NExtMMIOAXIChannels => nExtAXIMMIO
  }
)

class WithResetVector(resetvec : BigInt) extends Config (
  (pname,site,here) => pname match{
    case ResetVector => resetvec
  }
)

class WithL1CacheSets(sets : Int) extends Config (
  knobValues = {
    case "L1D_SETS" => sets
    case "L1I_SETS" => sets
  }
)

class WithNBreakpoints(hwbp : Int) extends Config (
  (pname,site,here) => pname match{
    case NBreakpoints => hwbp
  }
)

// This is for demo purposes only.
// A cleaner version will be created
// in the future.

class WithStatelessBridge extends Config(
  (pname, site, here) => pname match{
    case BuildL2CoherenceManager => (id: Int, p: Parameters) =>
      Module(new ManagerToClientStatelessBridge()(p.alterPartial({
        case InnerTLId => "L1toL2"
        case OuterTLId => "L2toMC" }
      )))
    case TLKey("L1toL2") =>
        TileLinkParameters(
          coherencePolicy = new MEICoherence(site(L2DirectoryRepresentation)), /*required to use MEI with stateless bridge.*/
          nManagers = site(NBanksPerMemoryChannel)*site(NMemoryChannels) + 1 /* MMIO */,
          nCachingClients = site(NCachedTileLinkPorts),
          nCachelessClients = site(NUncachedTileLinkPorts),
          maxClientXacts = ConfigUtils.max_int(
              // L1 cache
              site(NMSHRs) + 1 /* IOMSHR */,
              // RoCC
              if (site(BuildRoCC).isEmpty) 1 else site(RoccMaxTaggedMemXacts)),
          maxClientsPerPort = if (site(BuildRoCC).isEmpty) 1 else 2,
          maxManagerXacts = site(NAcquireTransactors) + 2,
          dataBeats = (8 * site(CacheBlockBytes)) / 64,//innerDataBeats,
          dataBits = site(CacheBlockBytes)*8)
  }
)

class DefaultTutorialDemoConfig extends Config(
    new WithNBreakpoints(2) ++
    new WithL1CacheSets(128) ++
    new WithResetVector(BigInt(0x60040000)) ++
    new WithNExtInts(31) ++
    new WithNExtAXIMMIOChannels(1) ++
    new WithStatelessBridge ++
    new DefaultRV32Config);

