// See LICENSE.SiFive for license details.

package freechips.rocketchip.regmapper

import chisel3.core.annotate
import chisel3.experimental.{ChiselAnnotation, RawModule, RunFirrtlTransform}
import firrtl.annotations._
import firrtl.{CircuitForm, CircuitState, LowForm, Transform}

case class RegFieldDescAnnotation(
  target: ModuleName,
  desc: String) extends SingleTargetAnnotation[ModuleName] {
  def duplicate(n: ModuleName): RegFieldDescAnnotation = this.copy(n)
}


/** ********************************************************************************/

case class RegFieldDescSer(
  byteOffset: String,
  bitOffset: Int,
  bitWidth: Int,
  name: String,
  resetValue: BigInt,
  accessType: String,
  writeType: String,
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

/**
  * Firrtl annotation
  *
  * @param target
  * @param regMappingSer
  */
case class RegFieldDescMappingAnnotation(
  target: ModuleName,
  regMappingSer: RegistersSer) extends SingleTargetAnnotation[ModuleName] {
  def duplicate(n: ModuleName): RegFieldDescMappingAnnotation = this.copy(target = n)
}

/**
  * Chisel Annotation
  *
  * This creates the firrtl annotation
  *
  * @param target
  * @param regMappingSer
  */
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

object GenRegDescsAnno {
  // Each class which has regmaps has instance counters
  private var instanceCounters = scala.collection.mutable.Map[String, Int]()

  def addInstanceCounter(key: String) : Unit = {
    instanceCounters +=  ( key -> 0)
  }

  def getInstanceCount(name: String, baseAddress: BigInt) : Int = {
    val nameAddress = s"${name}.${baseAddress}"

    if (! (instanceCounters isDefinedAt nameAddress)) {
      addInstanceCounter(nameAddress)
    }
    val cnt = instanceCounters(nameAddress)
    val newCnt = cnt + 1
    instanceCounters(nameAddress) = newCnt
    cnt
  }

  def makeRegMappingSer(rawModule: RawModule,
    moduleName: String,
    baseAddress: BigInt,
    width: Int,
    byteOffset: Int,
    bitOffset: Int,
    regField: RegField): RegFieldSer = {

    val anonRegFieldName = s"unnamedRegField${byteOffset.toHexString}_${bitOffset}"
    val regFieldName = regField.desc.map {_.name}.getOrElse("")
    val selectedRegFieldName = if (regFieldName.isEmpty /* selectedName.isEmpty */) anonRegFieldName else regFieldName
    val map = Map[BigInt, (String, String)]() // TODO

    val desc = regField.desc

    val byteOffsetHex = s"0x${byteOffset.toInt.toHexString}"

    val regFieldDescSer = RegFieldDescSer(
      byteOffset = byteOffsetHex,
      bitOffset = bitOffset,
      bitWidth = width,
      name = selectedRegFieldName,
      desc = desc.map {_.desc}.getOrElse("None"),
      group = desc.map {_.group.getOrElse("None")}.getOrElse("None"),
      groupDesc = desc.map {_.groupDesc.getOrElse("None")}.getOrElse("None"),
      accessType = desc.map {_.access.toString}.getOrElse("UNKNOWN"),
      writeType = desc.map(_.wrType.toString).getOrElse("None"),
      rdAction = desc.map(_.rdAction.toString).getOrElse("None"),
      volatile = desc.map(_.volatile).getOrElse(false),
      hasReset = desc.map { d => d.reset != None }.getOrElse(false),
      resetValue = desc.map{_.reset.getOrElse(BigInt(0))}.getOrElse(BigInt(0)),
      enumerations = map
    )
    RegFieldSer(
      moduleName, //selectedName,
      regFieldDescSer
     )
  }


  def anno(
    rawModule: RawModule,
    moduleName: String,
    instanceCounter: Int,
    baseAddress: BigInt,
    mapping: RegField.Map*): Seq[RegField.Map] = {

    val moduleName = rawModule.name
    val baseHex = s"0x${baseAddress.toInt.toHexString}"
    val displayName = s"${moduleName}.${baseHex}"

    println(s"INFO: GenRegDescsAnno: annotating rawModule: ${moduleName}.${baseHex}")

    val regFieldSers = mapping.flatMap {
      case (byteOffset, seq) =>
        seq.map(_.width).scanLeft(0)(_ + _).zip(seq).map { case (bitOffset, regField) =>
          makeRegMappingSer(
            rawModule,
            moduleName,
            baseAddress,
            regField.width,
            byteOffset,
            bitOffset,
            regField
          )
        }
    }

    val registersSer = RegistersSer(
      displayName = moduleName,
      instanceCounter = instanceCounter,
      baseAddress = baseAddress,
      regFields = regFieldSers // Seq[RegFieldSer]()
    )

    annotate(RegMappingChiselAnnotation(rawModule, registersSer))
    mapping
  }
}
