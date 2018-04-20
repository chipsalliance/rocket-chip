// See LICENSE.SiFive for license details.

package freechips.rocketchip.regmapper

import chisel3.internal.InstanceId
import chisel3.experimental.{ChiselAnnotation, RunFirrtlTransform, annotate}
import firrtl.{CircuitForm, CircuitState, LowForm, Transform}
import firrtl.annotations._
import freechips.rocketchip.regmapper._
import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods.{pretty, render}
import org.json4s._
import org.json4s.native.JsonMethods._


/**
  * This function is called by DescribedRegChiselAnnotation which creates a RegFieldDescAnnotation
  * A backend Firrtl transform 'looks' for RegFieldDescAnnotation's attached to circuits and then processes
  * the discovered RegFieldDescAnnotation's.
   */
case class RegFieldDescAnnotation(
    target: Named,
    desc: Seq[String]) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named): RegFieldDescAnnotation = this.copy(target = n)
}

object RegFieldDescAnnotation {
  def nameMap(state: CircuitState): Map[String,Seq[String]] = {
    state.annotations.collect {
      case RegFieldDescAnnotation(mname, cname) => mname.toString -> cname
    }.toMap
  }
}


case class DescribedRegChiselAnnotation(
  target: InstanceId,
  jsons: Seq[String]) extends ChiselAnnotation with RunFirrtlTransform {
  def toFirrtl: RegFieldDescAnnotation = {

    RegFieldDescAnnotation(target.toNamed, jsons)
  }

  override def transformClass: Class[_ <: Transform] = classOf[DescribedRegDumpTransform]
}


object DescribedRegChiselAnnotationUtil {

  def get[T](key: String, default: T, params: Map[String, Any]): T = {
    params.get(key).map(_.asInstanceOf[T]).getOrElse(default)
  }

  def toJson (
//    target: InstanceId,
    regField: freechips.rocketchip.regmapper.RegField,
    byteOffset: Int,
    bitOffset: Int): JValue = {

    regField.desc.map { regDesc =>
      val enumerations = regDesc.enumerations.map {
        case (key, (name, edesc)) =>
          (("value" -> key) ~ ("name" -> name) ~ ("description" -> edesc))
      }

      val resetValue: BigInt = regDesc.reset.getOrElse(0)
      val json = (("byteOffset" -> s"0x${byteOffset.toHexString}") ~
        ("bitOffset" -> bitOffset) ~
        ("bitWidth" -> regField.width) ~
        ("name" -> regDesc.name) ~
        ("description" -> regDesc.desc) ~
        ("resetValue" -> regDesc.reset.map(_.toString()).getOrElse("None")) ~
        ("group" -> regDesc.group.getOrElse("")) ~
        ("groupDesc" -> regDesc.groupDesc.getOrElse("")) ~
        ("accessType" -> regDesc.access.toString) ~
        ("writeType" -> new String(regDesc.wrType.map(_.toString).getOrElse("None"))) ~
        ("readAction" -> new String(regDesc.rdAction.map(_.toString).getOrElse("None"))) ~
        ("volatile" -> regDesc.volatile) ~
        ("enumerations" -> enumerations)
        )

      val jsonString: String = pretty(render(json))
      println("DescribedRegChiselAnnotationUtil: " + jsonString)
      json
    }.getOrElse("")
  }

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

case class RegMappingAnnotation(
  target: ModuleName,
  displayName: String,
  baseAddress: String,
  regfields: RegField.Map*) extends SingleTargetAnnotation[ModuleName] {
  def duplicate(n: ModuleName): RegMappingAnnotation = this // TODO this.copy(target = n)
}

object RegMappingAnnotation {

  def serialize(base: BigInt, name: String, mapping: RegField.Map*): String = {
    /*
    val anno = RegMappingAnnotation(name, s"0x${base.toInt.toHexString}", mapping.flatMap { case (byte, seq) => (seq.flatMap(_.desc)) })
    implicit val formats = Serialization.formats(NoTypeHints)
    println(firrtl.annotations.JsonProtocol.serialize(List(anno)))
    */

    val regDescs = mapping.flatMap {
      case (byte, seqRegField) =>

        println("ScanLeft start { ")
        seqRegField.map(_.width).scanLeft(0)(_ + _).zip(seqRegField).foreach(println)
        println("} ScanLeft end")

        // create the list of offsets and zip them with the RegDesc's
        seqRegField.map(_.width).scanLeft(0)(_ + _).zip(seqRegField).map {
          case (offset, regDesc) => {
            val anonName = s"unnamedRegField${byte.toHexString}_${offset}"
            (regDesc.desc.map {
              _.name
            }.getOrElse(anonName)) -> regDesc.toJson(byte, offset)
          }
        }
    }
//    regDescs.map(reg => RegFieldDescAnnotation(reg,))

    println("JSON Render start { ")

    pretty(render(
      ("peripheral" -> (
        ("displayName" -> name) ~
        ("baseAddress" -> s"0x${base.toInt.toHexString}") ~
        ("regfields" -> regDescs)))))
    println("} JSON Render end")
    regDescs.toString()
  }
}

//case class RegFieldhHolder(named: Named, json: String)

import java.io._

@SerialVersionUID(123L)
case class RegFieldDescSer(
  name: String,
  desc: String,
  group: String,
  groupDesc: String,
  access: String,
  wrType: String,
  rdAction: String,
  volatile: Boolean,
  hasReset: Boolean,
  reset: BigInt
  //,
  //enumerations: Map[BigInt, (String, String)] = Map()
)

object RegAnnotationUtil {
  def anno(
            named: InstanceId,
            base: BigInt,
            mapping: Seq[RegField.Map]): Unit = {
    /*
    val anno = RegMappingAnnotation(name, s"0x${base.toInt.toHexString}", mapping.flatMap { case (byte, seq) => (seq.flatMap(_.desc)) })
    implicit val formats = Serialization.formats(NoTypeHints)
    println(firrtl.annotations.JsonProtocol.serialize(List(anno)))
    */

    val jsons = Seq[String]()

    mapping.flatMap {
      case (byte, seqRegField) =>
        seqRegField.map(_.width).scanLeft(0)(_ + _).zip(seqRegField).map {
          case (offset, regField) => {
            val anonName = s"unnamedRegField${byte.toHexString}_${offset}"

            val json = DescribedRegChiselAnnotationUtil.toJson(regField, byte, offset).toString
            jsons ++ json
          }
        }
    }
    annotate(DescribedRegChiselAnnotation(named, jsons))
  }
}
