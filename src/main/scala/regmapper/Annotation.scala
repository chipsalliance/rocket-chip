// See LICENSE.SiFive for license details.

package freechips.rocketchip.regmapper

import chisel3.core.annotate
import chisel3.internal.InstanceId
import chisel3.experimental.{ChiselAnnotation, RawModule, RunFirrtlTransform}
import firrtl.{CircuitForm, CircuitState, LowForm, Transform}
import firrtl.annotations._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
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

@SerialVersionUID(123L)
case class RegMappingSer(
                          displayName: String,
                          byteOffset: Int,
                          bitOffset: Int,
                          regField: RegField
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

  def makeRegMappingSer(displayName: String,
                        baseAddress: String,
                        //width: Int,
                        byteOffset: Int,
                        bitOffset: Int,
                        regField: RegField): RegMappingSer = {

    val anonName = s"unnamedRegField${byteOffset.toHexString}_${bitOffset}"

    RegMappingSer(
      displayName = anonName,
      bitOffset = bitOffset,
      byteOffset = byteOffset,
      regField = regField
    )
  }

  def anno(target: RawModule,
           module: LazyModule,
           baseAddress: AddressSet,
           mapping: RegField.Map*): Seq[RegField.Map] = {
    //val displayName = module
    val baseAddr = baseAddress.base
    val baseHex = s"0x${baseAddr.toInt.toHexString}"
    val name = s"deviceAt${baseHex}" //TODO: It would be better to name this other than "Device at ...."
    val regs = mapping.flatMap {
      case (byteOffset, seq) =>
        println("ScanLeft start { ")
        seq.map(_.width).scanLeft(0)(_ + _).zip(seq).foreach(println)

        println("} ScanLeft end")

        seq.map(_.width).scanLeft(0)(_ + _).zip(seq).map { case (bitOffset, regField) =>

          makeRegMappingSer(
            name, // TODO use the LazyModule name
            baseAddr.toString(),
            // TODO  width,
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
