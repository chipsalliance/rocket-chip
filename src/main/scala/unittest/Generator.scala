// See LICENSE for license details.

package unittest

object Generator extends util.GeneratorApp {
  val longName = names.topModuleProject + "." + names.configs
  generateFirrtl
  generateGraphML
  generateTestSuiteMakefrags // TODO: Needed only for legacy make targets
  generateParameterDump // TODO: Needed only for legacy make targets
}
