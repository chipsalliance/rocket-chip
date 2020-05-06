// See LICENSE.SiFive for license details.

package freechips.rocketchip

import _root_.firrtl.AnnotationSeq
import _root_.firrtl.options.OptionsView

package object stage {

  implicit object RocketChipOptionsView extends OptionsView[RocketChipOptions] {

    def view(annotations: AnnotationSeq): RocketChipOptions = annotations
      .collect { case a: RocketChipOption => a }
      .foldLeft(new RocketChipOptions()){ (c, x) =>
        x match {
          case TopModuleAnnotation(a)         => c.copy(topModule = Some(a))
          case ConfigsAnnotation(a)           => c.copy(configNames = Some(a))
          case OutputBaseNameAnnotation(a)    => c.copy(outputBaseName = Some(a))
        }
      }

  }

}
