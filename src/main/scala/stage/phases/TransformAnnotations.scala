// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import chisel3.stage.ChiselOutputFileAnnotation
import firrtl.AnnotationSeq
import firrtl.options.Viewer.view
import firrtl.options.{Dependency, Phase, PreservesAll}
import freechips.rocketchip.stage.RocketChipOptions
import freechips.rocketchip.util.HasRocketChipStageUtils

/** Transforms RocketChipAnnotations into those used by other stages */
class TransformAnnotations extends Phase with PreservesAll[Phase] with HasRocketChipStageUtils {

  override val prerequisites = Seq(Dependency[Checks])
  override val dependents = Seq(Dependency[chisel3.stage.phases.AddImplicitOutputFile])

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    /** Construct output file annotation for emission */
    new ChiselOutputFileAnnotation(view[RocketChipOptions](annotations).longName.get) +: annotations
  }
}
