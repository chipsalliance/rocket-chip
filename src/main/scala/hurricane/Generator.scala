// See LICENSE for license details.

package hurricane

object Generator extends util.GeneratorApp {
  val longName = names.topModuleClass + "." + names.configs
  generateFirrtl
  generateTestSuiteMakefrags // TODO: Needed only for legacy make targets
  generateParameterDump // TODO: Needed only for legacy make targets
  generateSCRHeader

  def generateSCRHeader {
    SCRHeaderOutput.contents.foreach(c => writeOutputFile(td, s"${names.configs}.scr.h", c))
  }
}

object SCRHeaderOutput {
  var contents: Option[String] = None
}
