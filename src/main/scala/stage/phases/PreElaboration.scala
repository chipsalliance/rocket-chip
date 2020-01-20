// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import chisel3.RawModule
import chisel3.stage.ChiselGeneratorAnnotation

import firrtl.AnnotationSeq
import firrtl.options.Phase
import firrtl.options.Viewer.view

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.stage.{ConfigAnnotation, RocketChipOptions, TopModuleAnnotation}

class PreElaboration extends Phase {

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val ropts = view[RocketChipOptions](annotations)

    val top = ropts.topModule.get

    logger.info(s"$top")

    val conf = ropts.config.get

    logger.info(s"$conf")

    val gen = () =>
      ropts
        .topModule
        .get
        .getConstructor(classOf[Parameters])
        .newInstance(ropts.config.get.newInstance) match {
          case a: RawModule => a
          case a: LazyModule => LazyModule(a).module
        }

    ChiselGeneratorAnnotation(gen) +: annotations
  }

}
