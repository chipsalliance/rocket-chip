package uncore

import Chisel._
import cde.{Config, Parameters, ParameterDump, Knob, Dump, CDEMatchError}
import junctions.PAddrBits
import uncore.tilelink._
import uncore.agents._
import uncore.coherence._

object UncoreBuilder extends App {
  val topModuleName = args(0)
  val configClassName = args(1)
  val config = try {
      Class.forName(s"uncore.$configClassName").newInstance.asInstanceOf[Config]
    } catch {
      case e: java.lang.ClassNotFoundException =>
        throwException("Unable to find configClassName \"" + configClassName +
                       "\", did you misspell it?", e)
    }
  val world = config.toInstance
  val paramsFromConfig: Parameters = Parameters.root(world)

  val gen = () => 
    Class.forName(s"uncore.$topModuleName")
      .getConstructor(classOf[cde.Parameters])
      .newInstance(paramsFromConfig)
      .asInstanceOf[Module]

  chiselMain.run(args.drop(2), gen)

  val pdFile = new java.io.FileWriter(s"${Driver.targetDir}/$topModuleName.prm")
  pdFile.write(ParameterDump.getDump)
  pdFile.close

}

class DefaultL2Config extends Config (
  topDefinitions = { (pname,site,here) => 
    pname match {
      case PAddrBits => 32
      case CacheId => 0
      case CacheName => "L2Bank"
      case TLId => "L1toL2"
      case InnerTLId => "L1toL2"
      case OuterTLId => "L2toMC"
      case "N_CACHED" => Dump("N_CACHED",here[Int]("CACHED_CLIENTS_PER_PORT"))
      case "N_UNCACHED" => Dump("N_UNCACHED",here[Int]("MAX_CLIENTS_PER_PORT") - here[Int]("N_CACHED"))
      case "MAX_CLIENT_XACTS" => 4
      case "MAX_CLIENTS_PER_PORT" => Knob("NTILES")
      case "CACHED_CLIENTS_PER_PORT" => Knob("N_CACHED_TILES")
      case TLKey("L1toL2") => 
        TileLinkParameters(
          coherencePolicy = new MESICoherence(site(L2DirectoryRepresentation)),
          nManagers = 1,
          nCachingClients = here[Int]("N_CACHED"),
          nCachelessClients = here[Int]("N_UNCACHED"),
          maxClientXacts = here[Int]("MAX_CLIENT_XACTS"),
          maxClientsPerPort = here[Int]("MAX_CLIENTS_PER_PORT"),
          maxManagerXacts = site(NAcquireTransactors) + 2,
          dataBits = site(CacheBlockBytes)*8,
          dataBeats = 2)
      case TLKey("L2toMC") => 
        TileLinkParameters(
          coherencePolicy = new MEICoherence(new NullRepresentation(1)),
          nManagers = 1,
          nCachingClients = 1,
          nCachelessClients = 0,
          maxClientXacts = 1,
          maxClientsPerPort = site(NAcquireTransactors) + 2,
          maxManagerXacts = 1,
          dataBits = site(CacheBlockBytes)*8,
          dataBeats = 2)
      case CacheBlockBytes => 64
      case CacheBlockOffsetBits => log2Up(here(CacheBlockBytes))
      case "L2_SETS" => Knob("L2_SETS") 
      case NSets => Dump("L2_SETS",here[Int]("L2_SETS"))
      case NWays => Knob("L2_WAYS")
      case RowBits => site(TLKey(site(TLId))).dataBitsPerBeat
      case CacheIdBits => Dump("CACHE_ID_BITS",1)
      case L2StoreDataQueueDepth => 1
      case NAcquireTransactors => Dump("N_ACQUIRE_TRANSACTORS",2)
      case NSecondaryMisses => 4
      case L2DirectoryRepresentation => new FullRepresentation(here[Int]("N_CACHED"))
      case L2Replacer => () => new SeqRandom(site(NWays))
      case ECCCode => None
      case AmoAluOperandBits => 64
      case SplitMetadata => false
      case _ => throw new CDEMatchError
 //     case XLen => 128
  }},
  knobValues = {
    case "L2_WAYS" => 1
    case "L2_SETS" => 1024
    case "NTILES" => 2
    case "N_CACHED_TILES" => 2
    case "L2_CAPACITY_IN_KB" => 256
    case _ => throw new CDEMatchError
  }
)

class WithPLRU extends Config(
  (pname, site, here) => pname match {
    case L2Replacer => () => new SeqPLRU(site(NSets), site(NWays))
    case _ => throw new CDEMatchError
  })

class PLRUL2Config extends Config(new WithPLRU ++ new DefaultL2Config)

class With1L2Ways extends Config(knobValues = { case "L2_WAYS" => 1; case _ => throw new CDEMatchError })
class With2L2Ways extends Config(knobValues = { case "L2_WAYS" => 2; case _ => throw new CDEMatchError })
class With4L2Ways extends Config(knobValues = { case "L2_WAYS" => 4; case _ => throw new CDEMatchError })

class With1Cached extends Config(knobValues = { case "N_CACHED_TILES" => 1; case _ => throw new CDEMatchError })
class With2Cached extends Config(knobValues = { case "N_CACHED_TILES" => 2; case _ => throw new CDEMatchError })


class W1Cached1WaysConfig extends Config(new With1L2Ways ++ new With1Cached ++ new DefaultL2Config)
class W1Cached2WaysConfig extends Config(new With2L2Ways ++ new With1Cached ++ new DefaultL2Config)
class W2Cached1WaysConfig extends Config(new With1L2Ways ++ new With2Cached ++ new DefaultL2Config)
class W2Cached2WaysConfig extends Config(new With2L2Ways ++ new With2Cached ++ new DefaultL2Config)
