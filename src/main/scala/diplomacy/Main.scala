package freechips.rocketchip.diplomacy

import chisel3.RawModule
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.stage.phases.{Elaborate, Convert}
import firrtl.AnnotationSeq
import firrtl.options.TargetDirAnnotation
import org.chipsalliance.diplomacy.lazymodule.LazyModule
import org.chipsalliance.cde.config.{Config, Parameters}
import mainargs._

object Main {
  @main def elaborate(
    @arg(name = "dir", doc = "output directory") dir: String,
    @arg(name = "top", doc = "top Module or LazyModule fullpath") top: String,
    @arg(name = "config", doc = "CDE configs") config: Seq[String]
  ) = {
    var topName: String = null
    val gen = () => 
      Class
        .forName(top)
        .getConstructor(classOf[Parameters])
        .newInstance(new Config(config.foldRight(Parameters.empty) {
          case (currentName, config) =>
            val currentConfig = Class.forName(currentName).getDeclaredConstructor().newInstance().asInstanceOf[Config]
            currentConfig ++ config
        })) match {
          case m: RawModule => m
          case lm: LazyModule => LazyModule(lm).module
        }

    val annos = Seq(
      new Elaborate,
      new Convert
    ).foldLeft(
      Seq(
        TargetDirAnnotation(dir),
        ChiselGeneratorAnnotation(() => gen())
      ): AnnotationSeq
    ) { case (annos, phase) => phase.transform(annos) }
      .flatMap {
        case firrtl.stage.FirrtlCircuitAnnotation(circuit) =>
          topName = circuit.main
          os.write(os.Path(dir) / s"${circuit.main}.fir", circuit.serialize)
          None
        case _: chisel3.stage.ChiselCircuitAnnotation => None
        case _: chisel3.stage.DesignAnnotation[_] => None
        case a => Some(a)
      }
    os.write(os.Path(dir) / s"$topName.anno.json", firrtl.annotations.JsonProtocol.serialize(annos))
    freechips.rocketchip.util.ElaborationArtefacts.files.foreach{ case (ext, contents) => os.write.over(os.Path(dir) / s"${config.mkString("_")}.${ext}", contents()) }
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args.toIndexedSeq)
}

