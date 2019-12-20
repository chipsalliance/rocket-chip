// See LICENSE.SiFive for license details.

package freechips.rocketchip

import firrtl.AnnotationSeq
import firrtl.options.OptionsView

import freechips.rocketchip.stage.{ConfigAnnotation, RocketChipOption, RocketChipOptions, TopModuleAnnotation}

package object stage {

  implicit object RocketChipOptionsView extends OptionsView[RocketChipOptions] {

    def view(annotations: AnnotationSeq): RocketChipOptions = annotations
      .collect { case a: RocketChipOption => a }
      .foldLeft(new RocketChipOptions()){ (c, x) =>
        x match {
          case TopModuleAnnotation(a) => c.copy(topModule=Some(a))
          case ConfigAnnotation(a)    => c.copy(config=Some(a))
        }
      }

  }

}
