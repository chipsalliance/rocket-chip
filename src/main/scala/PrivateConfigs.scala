package rocketchip

import Chisel._
import uncore._
import rocket._
import boom._
import DefaultTestSuites._
import cde.{Parameters, Config, Dump, Knob}

class WithAllBooms extends Config(
  (pname,site,here) => pname match {
    case BuildTiles => {
      TestGeneration.addSuites(rv64i.map(_("p")))
      TestGeneration.addSuites((if(site(UseVM)) List("pt","v") else List("pt")).flatMap(env => rv64u.map(_(env))))
      TestGeneration.addSuites(if(site(NTiles) > 1) List(mtBmarks, bmarks) else List(bmarks))
      List.fill(site(NTiles)){ (r: Bool, p: Parameters) =>
         Module(new BOOMTile(resetSignal = r)(p.alterPartial({case TLId => "L1toL2"})))
    }
  }}
)

// TODO: update to the latest parameterization system
//class WithBoomAndRocketAlternating extends Config(
//  (pname,site,here) => pname match {
//    case BuildTiles => {
//      TestGeneration.addSuites(rv64i.map(_("p")))
//      TestGeneration.addSuites((if(site(UseVM)) List("pt","v") else List("pt")).flatMap(env => rv64u.map(_(env))))
//      TestGeneration.addSuites(if(site(NTiles) > 1) List(mtBmarks, bmarks) else List(bmarks))
//      (0 until site(NTiles)).map { i =>
//        (r:Bool) => Module({
//          if(i % 2 != 0) new RocketTile(resetSignal = r)
//          else           new BOOMTile(resetSignal = r)
//        }, {case TLId => "L1ToL2"})}}})

class SmallBOOMConfig  extends Config(new WithSmallBOOMs  ++ new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultConfig)
class MediumBOOMConfig extends Config(new WithMediumBOOMs ++ new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultL2Config)
class MegaBOOMConfig   extends Config(new WithMegaBOOMs   ++ new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultL2Config)

class BOOMVLSIConfig extends Config(new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultVLSIConfig ++ new WithNoBoomCounters)
class BOOMFPGAConfig extends Config(new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultFPGAConfig)
class BOOMCPPConfig extends  Config(new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultCPPConfig)

//class HeterogenousBoomConfig extends Config(new WithBoomAndRocketAlternating ++ new BOOMFPGAConfig)

