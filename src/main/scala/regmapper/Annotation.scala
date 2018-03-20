// See LICENSE.SiFive for license details.

package freechips.rocketchip.regmapper

import chisel3.internal.InstanceId
import chisel3.experimental.{ChiselAnnotation, RunFirrtlTransform}

import firrtl.{CircuitState, LowForm, Transform}
import firrtl.annotations._

import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods.{pretty, render}

case class RegFieldDescAnnotation(
    target: Named,
    desc: String) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named) = this.copy(n)
}

case class DescribedRegChiselAnnotation(
    target: InstanceId,
    desc: RegFieldDesc) extends ChiselAnnotation with RunFirrtlTransform {
  def toFirrtl = RegFieldDescAnnotation(target.toNamed, desc.toMap.toString)
  def transformClass = classOf[DescribedRegDumpTransform]
}

class DescribedRegDumpTransform extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  def execute(state: CircuitState): CircuitState = {
    state.annotations.foreach {
      case RegFieldDescAnnotation(t, desc) => println(desc)
    }
    state
  }
}

case class RegMappingAnnotation(
    target: ModuleName,
    displayName: String,
    baseAddress: String,
    regfields: Seq[RegFieldDesc]) extends SingleTargetAnnotation[ModuleName] {
  def duplicate(n: ModuleName) = this.copy(target = n)
}

object RegMappingAnnotation {

  def serialize(base: BigInt, name: String, mapping: RegField.Map*): String = {
    /*
    val anno = RegMappingAnnotation(name, s"0x${base.toInt.toHexString}", mapping.flatMap { case (byte, seq) => (seq.flatMap(_.desc)) })
    implicit val formats = Serialization.formats(NoTypeHints)
    println(firrtl.annotations.JsonProtocol.serialize(List(anno)))
    */

    val regDescs = mapping.flatMap { case (byte, seq) =>
      seq.map(_.width).scanLeft(0)(_ + _).zip(seq).map { case (bit, f) =>
        val anonName = s"unnamedRegField${byte.toHexString}_${bit}"
        (f.desc.map{ _.name}.getOrElse(anonName)) -> f.toJson(byte, bit)
      }
    }

    pretty(render(
      ("peripheral" -> (
        ("displayName" -> name) ~
        ("baseAddress" -> s"0x${base.toInt.toHexString}") ~
        ("regfields" -> regDescs)))))
  }
}
