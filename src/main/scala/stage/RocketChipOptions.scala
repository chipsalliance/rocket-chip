// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage

class RocketChipOptions private[stage] (
                                         val topModule:         Option[Class[_ <: Any]] = None,
                                         val configNames:       Option[String] = None,
                                         val outputBaseName:    Option[String] = None) {

  private[stage] def copy(
                           topModule:         Option[Class[_ <: Any]] = topModule,
                           configNames:       Option[String] = configNames,
                           outputBaseName:    Option[String] = outputBaseName,
                         ): RocketChipOptions = {

    new RocketChipOptions(
      topModule=topModule,
      configNames=configNames,
      outputBaseName=outputBaseName,
    )
  }

  lazy val topPackage: Option[String] = topModule match {
    case Some(a) => Some(a.getPackage.getName)
    case _ => None
  }

  lazy val configClass: Option[String] = configNames match {
    case Some(a) => Some(a.split('.').last.replace(',', '_'))
    case _ => None
  }

  lazy val longName: String = outputBaseName.getOrElse(s"${topPackage.get}.${configClass.get}")
}

