package rocketchip

import Chisel._
import cde.{Parameters, Field, Config}
import uncore._
import rocket._
import junctions._

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

class WithAXIMMIOPort () extends Config (
  (pname, site, here) => pname match {
    case ExtMMIOPorts => AddrMap(
      AddrMapEntry("ext", MemRange(0x60000000L, 0x20000000L, MemAttr(AddrMapProt.RWX))))
    case NExtMMIOAXIChannels => 1
  }
)

class WithMulUnroll(unroll: Int) extends Config (
  (pname,site,here) => pname match {
    case MulUnroll => unroll
  }
)

class WithDivEarlyOut(earlyOut : Boolean) extends Config (
  (pname,site,here) => pname match {
    case DivEarlyOut => earlyOut
  }
)

class DefaultTutorialDemoConfig extends Config(
  new WithNBreakpoints(2) ++
    new WithL1CacheSets(128) ++
    new WithResetVector(BigInt(0x60000000)) ++
    new WithNExtInts(31) ++
    new WithMulUnroll(16) ++
    new WithDivEarlyOut(false) ++ 
    new WithAXIMMIOPort ++ 
    new WithStatelessBridge ++
    new DefaultRV32Config);

