// See LICENSE.SiFive for license details.

package freechips.rocketchip.unittest

object Generator extends freechips.rocketchip.util.GeneratorApp {
  generateFirrtl
  generateAnno
  generateTestSuiteMakefrags // TODO: Needed only for legacy make targets
  generateArtefacts
}
