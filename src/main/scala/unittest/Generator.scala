// See LICENSE for license details.

package unittest

import Chisel._
import util.Generator

object UnitTestGenerator extends Generator
{
  val longName = names.topModuleProject + "." + names.configs
  generateFirrtl
  generateTestSuiteMakefrags // TODO: Needed only for legacy make targets
  generateParameterDump // TODO: Needed only for legacy make targets
}
