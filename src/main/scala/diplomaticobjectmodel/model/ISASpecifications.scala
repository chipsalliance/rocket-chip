// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

import freechips.rocketchip.util.BooleanToAugmentedBoolean
import freechips.rocketchip.tile.CoreParams

sealed trait PrivilegedArchitectureExtension extends OMEnum
case object MachineLevelISA extends PrivilegedArchitectureExtension
case object SupervisorLevelISA extends PrivilegedArchitectureExtension

object PrivilegedArchitectureExtensions {
  val specifications = Map[PrivilegedArchitectureExtension, String](
    MachineLevelISA -> "Machine-Level ISA",
    SupervisorLevelISA -> "Supervisor-Level ISA"
  )

  def specVersion(extension: PrivilegedArchitectureExtension, version: String): OMSpecification = OMSpecification(specifications(extension), version)
}

object BaseExtensions {
  val specifications = Map[OMBaseInstructionSet, String](
    RV32E -> "RV32E Base Integer Instruction Set",
    RV32I -> "RV32I Base Integer Instruction Set",
    RV64I -> "RV64I Base Integer Instruction Set"
  )

  def specVersion(extension: OMBaseInstructionSet, version: String): OMSpecification = OMSpecification(specifications(extension), version)
}

object ISAExtensions {
  val specifications = Map[OMExtensionType, String](
    M -> "M Standard Extension for Integer Multiplication and Division",
    A -> "A Standard Extension for Atomic Instruction",
    F -> "F Standard Extension for Single-Precision Floating-Point",
    D -> "D Standard Extension for Double-Precision Floating-Point",
    C -> "C Standard Extension for Compressed Instruction",
    U -> "The RISC‑V Instruction Set Manual, Volume II: Privileged Architecture",
    S -> "Supervisor-Level ISA"
  )

  def specVersion(extension: OMExtensionType, version: String): OMSpecification = OMSpecification(specifications(extension), version)
}


