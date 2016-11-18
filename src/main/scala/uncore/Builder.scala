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
      .getConstructor(classOf[Parameters])
      .newInstance(paramsFromConfig)
      .asInstanceOf[Module]

  chiselMain.run(args.drop(2), gen)

  val pdFile = new java.io.FileWriter(s"${Driver.targetDir}/$topModuleName.prm")
  pdFile.write(ParameterDump.getDump)
  pdFile.close

}
