// See LICENSE.SiFive for license details.

package freechips.rocketchip.groundtest

import freechips.rocketchip.util.GeneratorApp

object Generator extends GeneratorApp {
  generateFirrtl
  generateAnno
  generateTestSuiteMakefrags // TODO: Needed only for legacy make targets
  generateArtefacts
}
