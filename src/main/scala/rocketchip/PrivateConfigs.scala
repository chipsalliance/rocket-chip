package rocketchip

import Chisel._
import uncore.tilelink._
import uncore.coherence._
import uncore.agents._
import uncore.devices._
import uncore.converters._
import coreplex._
import rocket._
import boom._
import DefaultTestSuites._
import cde.{Parameters, Config, Dump, Knob}

class WithAllBooms extends Config(
   (pname,site,here) => pname match {
      case BuildTiles => {
         val env = if(site(UseVM)) List("p","v") else List("p")
         site(FPUKey) foreach { case cfg =>
            if (site(XLen) == 32) {
               TestGeneration.addSuites(env.map(rv32ufNoDiv))
            } else {
               TestGeneration.addSuite(rv32udBenchmarks)
               TestGeneration.addSuites(env.map(rv64ufNoDiv))
               TestGeneration.addSuites(env.map(rv64udNoDiv))
               if (cfg.divSqrt) {
                  TestGeneration.addSuites(env.map(rv64uf))
                  TestGeneration.addSuites(env.map(rv64ud))
               }
            }
         }
         if (site(UseAtomics)) TestGeneration.addSuites(env.map(if (site(XLen) == 64) rv64ua else rv32ua))
         if (site(UseCompressed)) TestGeneration.addSuites(env.map(if (site(XLen) == 64) rv64uc else rv32uc))
         val (rvi, rvu) =
            if (site(XLen) == 64) ((if (site(UseVM)) rv64i else rv64pi), rv64u)
            else ((if (site(UseVM)) rv32i else rv32pi), rv32u)
         TestGeneration.addSuites(rvi.map(_("p")))
         TestGeneration.addSuites((if(site(UseVM)) List("v") else List()).flatMap(env => rvu.map(_(env))))
         TestGeneration.addSuite(benchmarks)
         List.tabulate(site(NTiles)){ i => (r: Bool, p: Parameters) =>
            Module(new BOOMTile(resetSignal = r)(p.alterPartial({
               case TileId => i
               case TLId => "L1toL2"
               case NUncachedTileLinkPorts => 1 + site(RoccNMemChannels)
            })))
         }
  }}
)

class SmallBOOMConfig  extends Config(new WithSmallBOOMs  ++ new WithAllBooms ++ new DefaultBOOMConfig ++ new BaseConfig)
class MediumBOOMConfig extends Config(new WithMediumBOOMs ++ new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultL2Config)
class MegaBOOMConfig   extends Config(new WithMegaBOOMs   ++ new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultL2Config)

class BOOMConfig extends Config(new WithNPerfCounters(6) ++ new WithMediumBOOMs ++ new WithAllBooms ++ new DefaultBOOMConfig ++ new WithL2Capacity(1024) ++ new DefaultL2Config)
class BOOML1Config extends  Config(new WithNPerfCounters(4) ++ new WithAllBooms ++ new DefaultBOOMConfig ++ new BaseConfig)
class BOOMFPGAConfig   extends Config(new WithNPerfCounters(29) ++ new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultFPGAConfig)
class BOOML2FPGAConfig extends Config(new WithNPerfCounters(29) ++ new WithAllBooms ++ new DefaultBOOMConfig ++ new WithL2Capacity(1024) ++ new WithL2Cache ++ new DefaultFPGAConfig)

