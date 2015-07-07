package rocketchip

import Chisel._
import uncore._
import rocket._
import BOOM._

class WithAllBooms extends ChiselConfig(
  (pname,site,here) => pname match { 
     case BuildTiles =>
      List.fill(site(NTiles)){ (r:Bool) => Module(new BOOMTile(resetSignal = r), {case TLId => "L1ToL2"}) }
  }
)

class WithBoomAndRocketAlternating extends ChiselConfig(
  (pname,site,here) => pname match { 
    case BuildTiles => {
      (0 until site(NTiles)).map { i =>
        (r:Bool) => Module({
          if(i % 2 != 0) new RocketTile(resetSignal = r)
          else           new BOOMTile(resetSignal = r)
        }, {case TLId => "L1ToL2"})}}})

class SmallBOOMConfig  extends ChiselConfig(new WithSmallBOOMs  ++ new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultConfig)
class MediumBOOMConfig extends ChiselConfig(new WithMediumBOOMs ++ new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultL2Config)
class MegaBOOMConfig   extends ChiselConfig(new WithMegaBOOMs   ++ new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultL2Config)

class BOOMVLSIConfig extends ChiselConfig(new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultVLSIConfig ++ new WithNoBoomCounters) 
class BOOMFPGAConfig extends ChiselConfig(new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultFPGAConfig)
class BOOMCPPConfig extends  ChiselConfig(new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultCPPConfig)

class HeterogenousBoomConfig extends ChiselConfig(new WithBoomAndRocketAlternating ++ new BOOMFPGAConfig)
