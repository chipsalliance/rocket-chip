// See LICENSE.SiFive for license details.

package freechips.rocketchip.regmapper

import chisel3.experimental.{ChiselAnnotation, RawModule, RunFirrtlTransform}
import firrtl.annotations._
import firrtl.{CircuitForm, CircuitState, LowForm, Transform}

case class RegFieldDescSer(
  byteOffset: String,
  bitOffset: Int,
  bitWidth: Int,
  name: String,
  resetValue: BigInt,
  accessType: String,
  wrType: String,
  rdAction: String,
  desc: String,
  group: String,
  groupDesc: String,
  volatile: Boolean = false,
  hasReset: Boolean = false,
  enumerations: Map[BigInt, (String, String)] = Map()
)

case class RegFieldSer(
  regFieldName: String,
  desc: RegFieldDescSer
)

case class RegistersSer(
  displayName: String,
  instanceCounter: Int,
  baseAddress: BigInt,
  regFields: Seq[RegFieldSer]
)

case class RegFieldDescMappingAnnotation(
  target: ModuleName,
  regMappingSer: RegistersSer) extends SingleTargetAnnotation[ModuleName] {
    def duplicate(n: ModuleName): RegFieldDescMappingAnnotation = this.copy(target = n)
}

case class RegMappingChiselAnnotation(
  target: RawModule,
  regMappingSer: RegistersSer) extends ChiselAnnotation with RunFirrtlTransform {
  def toFirrtl: RegFieldDescMappingAnnotation = RegFieldDescMappingAnnotation(target.toNamed, regMappingSer)

  def transformClass: Class[RegMappingDumpTransform] = classOf[RegMappingDumpTransform]
}

class RegMappingDumpTransform extends Transform {
  def inputForm: CircuitForm = LowForm

  def outputForm: CircuitForm = LowForm

  def execute(state: CircuitState): CircuitState = {
    state.annotations.foreach {
      case RegFieldDescMappingAnnotation(t, desc) => println(desc)
    }
    state
  }
}
