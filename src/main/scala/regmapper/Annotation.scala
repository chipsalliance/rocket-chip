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
  byteOffset: Int,
  bitOffset: Int,
  bitWidth: Int,
  name: String,
  desc: String,
  group: String,
  groupDesc: String,
  accessType: String,
  wrType: String,
  rdAction: String,
  volatile: Boolean = false,
  hasReset: Boolean = false,
  reset: BigInt,
  enumerations: Map[BigInt, (String, String)] = Map()
)

case class RegFieldSer(
  regFieldName: String,
  desc: RegFieldDescSer
)

case class RegistersSer(
  displayName: String,
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

  def makeRegMappingSer(rawModule: RawModule,
    baseAddress: BigInt,
    width: Int,
    byteOffset: Int,
    bitOffset: Int,
    regField: RegField): RegFieldSer = {

    val anonName = s"unnamedRegField${byteOffset.toHexString}_${bitOffset}"
    val descName = regField.desc.map {
      _.name
    }.getOrElse("")
    val selectedName = if (descName.isEmpty) anonName else descName
    val map = Map[BigInt, (String, String)]() // TODO

    val desc = regField.desc

    val regFieldDescSer = RegFieldDescSer(
      byteOffset = byteOffset,
      bitOffset = bitOffset,
      bitWidth = width,
      name = selectedName,
      desc = desc.map {
        _.desc
      }.getOrElse("None"),
      group = desc.map {
        _.group.getOrElse("None")
      }.getOrElse("None"),
      groupDesc = desc.map {
        _.groupDesc.getOrElse("None")
      }.getOrElse("None"),
      accessType = desc.map {
        _.access.toString
      }.getOrElse("r"), // TODO default?
      wrType = desc.map(_.wrType.toString).getOrElse("None"),
      rdAction = desc.map(_.rdAction.toString).getOrElse("None"),
      volatile = desc.map(_.volatile).getOrElse(false),
      hasReset = desc.map { d => if (d.reset != None) true else false }.getOrElse(false), // TODO ugly
      reset = BigInt(0), // TODO desc.map{_.reset}.getOrElse(BigInt(0))
      enumerations = map
    )
    RegFieldSer(
      selectedName,
      regFieldDescSer
     )
  }

  def anno(rawModule: RawModule,
    //module: LazyModule,
    // baseAddress: AddressSet,
    baseAddress: BigInt,
    mapping: RegField.Map*): Seq[RegField.Map] = {
    //val displayName = module
    val regFieldSers = mapping.flatMap {
      case (byteOffset, seq) =>
        println("ScanLeft start { ")
        seq.map(_.width).scanLeft(0)(_ + _).zip(seq).foreach(println)

        println("} ScanLeft end")

        seq.map(_.width).scanLeft(0)(_ + _).zip(seq).map { case (bitOffset, regField) =>
          makeRegMappingSer(
            rawModule,
            //name, // TODO use the LazyModule name
            baseAddress,
            regField.width,
            byteOffset,
            bitOffset,
            regField
          )
        }
    }

    val baseHex = s"0x${baseAddress.toInt.toHexString}"
    val name = s"deviceAt${baseHex}" //TODO: It would be better to name this other than "Device at ...."

    val registersSer = RegistersSer(
      displayName = name,
      baseAddress = baseAddress,
      regFields = regFieldSers // Seq[RegFieldSer]()
    )

    annotate(RegMappingChiselAnnotation(rawModule, registersSer))
    mapping
  }
}
