// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage

import chisel3.experimental.BaseModule

import freechips.rocketchip.config.Config

class RocketChipOptions private[stage] (
                                         val targetDir:         Option[String] = None,
                                         val topModulePackage:  Option[String] = None,
                                         val topModuleClass:    Option[String] = None,
                                         val configsPackage:    Option[String] = None,
                                         val configs:           Option[String] = None,
                                         val outputBaseName:    Option[String] = None) {

  private[stage] def copy(
                           targetDir:         Option[String] = targetDir,
                           topModulePackage:  Option[String] = topModulePackage,
                           topModuleClass:    Option[String] = topModuleClass,
                           configsPackage:    Option[String] = configsPackage,
                           configs:           Option[String] = configs,
                           outputBaseName:    Option[String] = outputBaseName,
                         ): RocketChipOptions = {

    new RocketChipOptions(
      targetDir=targetDir,
      topModulePackage=topModulePackage,
      topModuleClass=topModuleClass,
      configsPackage=configsPackage,
      configs=configs,
      outputBaseName=outputBaseName,
    )

  }

}

