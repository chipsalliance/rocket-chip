// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import chisel3.RawModule
import chisel3.stage.ChiselGeneratorAnnotation
import firrtl.AnnotationSeq
import firrtl.options.Viewer.view
import firrtl.options.{Phase, PreservesAll}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.stage.RocketChipOptions

class PreElaboration extends Phase with PreservesAll[Phase] with HasRocketChipStageUtils {

  override val prerequisites = Seq(classOf[Checks])
  override val dependents = Seq(classOf[chisel3.stage.phases.Elaborate])

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {

    val rOpts = view[RocketChipOptions](annotations)
    val topMod = rOpts.topModule.get

    val config = getConfig(rOpts.configNames.get)

    val gen = () =>
      topMod
        .getConstructor(classOf[Parameters])
        .newInstance(config) match {
          case a: RawModule => a
          case a: LazyModule => LazyModule(a).module
        }

    ChiselGeneratorAnnotation(gen) +: annotations
  }

}
