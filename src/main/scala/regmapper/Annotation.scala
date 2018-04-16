// See LICENSE.SiFive for license details.

package freechips.rocketchip.regmapper

import chisel3.core.annotate
import chisel3.internal.InstanceId
import chisel3.experimental.{ChiselAnnotation, RawModule, RunFirrtlTransform}
import firrtl.{CircuitForm, CircuitState, LowForm, Transform, bitWidth}
import firrtl.annotations._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
import freechips.rocketchip.regmapper.RegFieldAccessType.RegFieldAccessType
import freechips.rocketchip.regmapper.RegFieldRdAction.RegFieldRdAction
import freechips.rocketchip.regmapper.RegFieldWrType.RegFieldWrType
import freechips.rocketchip.util.ElaborationArtefacts

case class RegFieldDescAnnotation(
    target: ModuleName,
    desc: String) extends SingleTargetAnnotation[ModuleName] {
  def duplicate(n: ModuleName): RegFieldDescAnnotation = this.copy(n)
}

case class DescribedRegChiselAnnotation(
                                         target: RawModule,
                                         desc: RegFieldDesc) extends ChiselAnnotation with RunFirrtlTransform {
  def toFirrtl: RegFieldDescAnnotation = RegFieldDescAnnotation(target.toNamed, desc.toMap.toString)
  def transformClass: Class[DescribedRegDumpTransform] = classOf[DescribedRegDumpTransform]
}

class DescribedRegDumpTransform extends Transform {
  def inputForm: CircuitForm = LowForm
  def outputForm: CircuitForm = LowForm

  def execute(state: CircuitState): CircuitState = {
    state.annotations.foreach {
      case RegFieldDescAnnotation(t, desc) => println(desc)
    }
    state
  }
}

/**********************************************************************************/

import java.io._

@SerialVersionUID(1111111111L)
case class RegFieldDescSer (
                             byteOffset: Int,
                             bitOffset: Int,
                             bitWidth: Int,
                             name: String,
                          desc: String,
                          group: String,
                          groupDesc: String,
                          access: String,
                          wrType: String,
                          rdAction: String,
                          volatile: Boolean = false,
                             hasReset : Boolean = false,
                          reset: BigInt,
                          enumerations: Map[BigInt, (String, String)] = Map()
                        )

@SerialVersionUID(1111111112L)
case class RegFieldSer(width: Int,
                       read: String,
                       write: String,
                       desc: RegFieldDescSer)

@SerialVersionUID(1111111113L)
case class RegMappingSer(
                          displayName: String,
                          byteOffset: Int,
                          bitOffset: Int,
                          regField: RegFieldSer
                          )

case class RegFieldDescMappingAnnotation(
    target: ModuleName,
    regMappingSer: Seq[RegMappingSer]) extends SingleTargetAnnotation[ModuleName] {
  def duplicate(n: ModuleName): RegFieldDescMappingAnnotation = this.copy(target = n)
}


case class RegMappingChiselAnnotation(
                                         target: RawModule,
                                         regMappingSer: Seq[RegMappingSer]) extends ChiselAnnotation with RunFirrtlTransform {
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

  def makeRegMappingSer(name: String,
                        baseAddress: String,
                        width: Int,
                        byteOffset: Int,
                        bitOffset: Int,
                        regField: RegField): RegMappingSer = {

    val anonName = s"unnamedRegField${byteOffset.toHexString}_${bitOffset}"

    val map =  Map[BigInt, (String, String)]() // TODO

    val regFieldDescSer = regField.desc.map{ d =>


      RegFieldDescSer(
        byteOffset = byteOffset,
        bitOffset = bitOffset,
        bitWidth = width,
        name = name, // which name should be used RegField or RegFieldDesc
        desc = d.desc,
        group = d.group.getOrElse("None"),
        groupDesc = d.groupDesc.getOrElse("None"),
        access = d.access.toString,
        wrType = d.wrType.map(_.toString).getOrElse("Error"),
        rdAction = d.rdAction.map(_.toString).getOrElse("Error"),
        volatile = d.volatile,
        hasReset = false,
        reset = BigInt(0),
        enumerations = map
      )
    }.getOrElse(
      RegFieldDescSer(
        byteOffset = 0,
        bitOffset = 0,
        bitWidth = 0,
        name = "",
        desc = "None",
        group = "None",
        groupDesc = "None",
        access = "None",
        wrType ="None",
        rdAction = "None",
        volatile = false,
        hasReset = false,
        reset = BigInt(0),
        enumerations = map
      )
    )

    val regFieldSer = RegFieldSer(
      regField.width,
      "",
      "",
      regFieldDescSer)

    RegMappingSer(
      displayName = anonName,
      bitOffset = bitOffset,
      byteOffset = byteOffset,
      regField = regFieldSer
    )
  }

  def anno(target: RawModule,
           //module: LazyModule,
          // baseAddress: AddressSet,
           baseAddress: BigInt,
           mapping: RegField.Map*): Seq[RegField.Map] = {
    //val displayName = module
    val baseHex = s"0x${baseAddress.toInt.toHexString}"
    val name = s"deviceAt${baseHex}" //TODO: It would be better to name this other than "Device at ...."
    val regs = mapping.flatMap {
      case (byteOffset, seq) =>
        println("ScanLeft start { ")
        seq.map(_.width).scanLeft(0)(_ + _).zip(seq).foreach(println)

        println("} ScanLeft end")

        seq.map(_.width).scanLeft(0)(_ + _).zip(seq).map { case (bitOffset, regField) =>
          val width = 0 // TODO fix me
          makeRegMappingSer(
            name, // TODO use the LazyModule name
            baseAddress.toString(),
            width,
            byteOffset,
            bitOffset,
            regField
          )
        }
    }

    annotate(RegMappingChiselAnnotation(target, regs))
    mapping
  }
}
