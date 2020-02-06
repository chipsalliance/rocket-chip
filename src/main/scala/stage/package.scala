// See LICENSE.SiFive for license details.

package freechips.rocketchip

import firrtl.AnnotationSeq
import firrtl.options.OptionsView

package object stage {

  implicit object RocketChipOptionsView extends OptionsView[RocketChipOptions] {

    def view(annotations: AnnotationSeq): RocketChipOptions = annotations
      .collect { case a: RocketChipOption => a }
      .foldLeft(new RocketChipOptions()){ (c, x) =>
        x match {
          case TopPackageAnnotation(a)        => c.copy(topModulePackage = Some(a))
          case TopClassAnnotation(a)          => c.copy(topModuleClass = Some(a))
          case ConfigPackageAnnotation(a)     => c.copy(configsPackage = Some(a))
          case ConfigAnnotation(a)            => c.copy(configs = Some(a))
          case OutputBaseNameAnnotation(a)    => c.copy(outputBaseName = Some(a))
        }
      }

  }

}
