// See LICENSE.SiFive for license details.

package freechips.rocketchip.regmapper

import chisel3.internal.InstanceId
import chisel3.experimental.{ChiselAnnotation, RunFirrtlTransform, annotate}
import firrtl.{CircuitForm, CircuitState, LowForm, Transform}
import firrtl.annotations._
import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods.{pretty, render}

// Do not call this function
// This function is called by DescribedRegChiselAnnotation
case class RegFieldDescAnnotation(
    target: Named,
    desc: String) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named): RegFieldDescAnnotation = this.copy(target = n)
}

object RegFieldDescAnnotation {
  def nameMap(state: CircuitState): Map[String,String] = {
    state.annotations.collect {
      case RegFieldDescAnnotation(mname, cname) => mname.toString -> cname
    }.toMap
  }
}


case class DescribedRegChiselAnnotation(
  target: InstanceId,
  json: String) extends ChiselAnnotation with RunFirrtlTransform {
  def toFirrtl: RegFieldDescAnnotation = {

    RegFieldDescAnnotation(target.toNamed, json)
  }

  override def transformClass: Class[_ <: Transform] = classOf[DescribedRegDumpTransform]
}

object DescribedRegChiselAnnotationUtil {
  def toJson (
//    target: InstanceId,
    regField: freechips.rocketchip.regmapper.RegField,
    byteOffset: Int,
    bitOffset: Int): JValue = {

    val regDesc: freechips.rocketchip.regmapper.RegFieldDesc = regField.desc.get

    //require("", regDesc.desc != None)

    ( ("byteOffset"   -> s"0x${byteOffset.toHexString}") ~
      ("bitOffset"    -> bitOffset) ~
      ("bitWidth"     -> regField.width) ~
      ("name"         -> regDesc.name) ~
      ("description"  -> regDesc.desc) ~
// TODO      ("resetValue"   -> regDesc.reset.getOrElse("0")) ~
      ("group"        -> regDesc.group.getOrElse("")) ~
      ("groupDesc"    -> regDesc.groupDesc.getOrElse("")) ~
      ("accessType"   -> regDesc.access.toString) ~
// TODO     ("writeType"    -> regDesc.wrType.getOrElse("")) ~
// TODO      ("readAction"   -> regDesc.rdAction.getOrElse("")) ~
      ("volatile"     -> regDesc.volatile)
      //~
//      ("enumerations" -> regDesc.map {d =>
//        Option(d.enumerations.map { case (key, (name, edesc)) =>
//          (("value" -> key) ~ ("name" -> name) ~ ("description" -> edesc))
//        }).filter(_.nonEmpty)})
      )
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

    val regDescs = mapping.flatMap { case (byte, seq) =>

      seq.map(_.width).scanLeft(0)(_ + _).zip(seq).foreach(println)
      seq.map(_.width).scanLeft(0)(_ + _).zip(seq).map {
        case (bit, f) => {
          val anonName = s"unnamedRegField${byte.toHexString}_${bit}"
          (f.desc.map {
            _.name
          }.getOrElse(anonName)) -> f.toJson(byte, bit)
        }
      }
    }
//    regDescs.map(reg => RegFieldDescAnnotation(reg,))


    pretty(render(
      ("peripheral" -> (
        ("displayName" -> name) ~
        ("baseAddress" -> s"0x${base.toInt.toHexString}") ~
        ("regfields" -> regDescs)))))
  }
}

//case class RegFieldhHolder(named: Named, json: String)


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


    mapping.flatMap { case (byte, seqRegField) =>
      println("RegMappingAnnotation: base: " + base)
      println("RegMappingAnnotation: byte" + byte)
      println("RegMappingAnnotation: width" + seqRegField.map(_.width))
      seqRegField.map(_.width).foreach(println)
      println("RegMappingAnnotation: scanleft " + seqRegField.map(_.width).scanLeft(base)(_ + _))
      seqRegField.map(_.width).scanLeft(base)(_ + _).foreach(println)
      println("RegMappingAnnotation: zip  " + seqRegField.map(_.width).scanLeft(base)(_ + _).zip(seqRegField))
      seqRegField.map(_.width).scanLeft(base)(_ + _).zip(seqRegField).foreach(println)
      println("RegMappingAnnotation: base: " + base)
      seqRegField.map(_.width).scanLeft(0)(_ + _).zip(seqRegField).foreach(println)

      seqRegField.map(_.width).scanLeft(0)(_ + _).zip(seqRegField).map {
        case (offset, regField) => {
          val anonName = s"unnamedRegField${byte.toHexString}_${offset}"
          //          (f.desc.map {
          //            _.name
          //          }.getOrElse(anonName)) -> f.toJson(byte, offset)
          //RegFieldhHolder(named, f.toJson(byte, offset).toString)
          val json = regField.toJson(byte, offset).toString

          annotate(DescribedRegChiselAnnotation(named, json.toString))
        }
      }
    }
    //regDescs.map(reg => RegFieldDescAnnotation(named,reg.toJson()))

  }
}