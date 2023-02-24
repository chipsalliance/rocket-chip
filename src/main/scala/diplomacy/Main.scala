package org.chipsalliance.diplomacy.elaborate

import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.stage.phases.Elaborate
import firrtl.AnnotationSeq
import firrtl.options.TargetDirAnnotation
import freechips.rocketchip.diplomacy.LazyModule
import mainargs._
import org.chipsalliance.cde.config.{Config, Parameters}

object Main {
  @main def elaborate(
                       @arg(name = "dir") dir: String,
                       @arg(name = "lm") top: String,
                       @arg(name = "configs") config: String
                     ) = {
    val lazyModule = LazyModule(Class.forName(top).getConstructor(classOf[Parameters]).newInstance(
      new Config(config.split(',').foldRight(Parameters.empty) { case (currentName, config) =>
        val currentConfig = Class.forName(currentName).newInstance.asInstanceOf[Config]
        currentConfig ++ config
      })
    ).asInstanceOf[LazyModule])

    Seq(
      new Elaborate,
      new chisel3.hack.Convert
    ).foldLeft(
      Seq(
        TargetDirAnnotation(dir),
        ChiselGeneratorAnnotation(() =>
          lazyModule.module
        )
      ): AnnotationSeq
    ) { case (annos, phase) => phase.transform(annos) }.flatMap {
      case firrtl.stage.FirrtlCircuitAnnotation(circuit) =>
        os.write(os.Path(dir) / s"${lazyModule.moduleName}.fir", circuit.serialize)
        None
      case _: chisel3.stage.DesignAnnotation[_] => None
      case a => Some(a)
    }
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
