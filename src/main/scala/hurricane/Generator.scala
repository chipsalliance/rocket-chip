// See LICENSE for license details.

package hurricane
import testchipip.SCRHeaderOutput

object Generator extends util.GeneratorApp {
  val longName = names.topModuleClass + "." + names.configs
  generateFirrtl
  generateTestSuiteMakefrags // TODO: Needed only for legacy make targets
  generateParameterDump // TODO: Needed only for legacy make targets
  generateSCRHeader

  def generateSCRHeader {
    writeOutputFile(td, s"${names.configs}.scr.h", SCRHeaderOutput.contents.mkString("\n"))
  }
}
