// See LICENSE.SiFive for license details.

package freechips.rocketchip.regmapper

import chisel3.internal.InstanceId
import chisel3.experimental.{ChiselAnnotation, RunFirrtlTransform}
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
  reg: RegFieldDesc) extends ChiselAnnotation with RunFirrtlTransform {
  def toFirrtl: RegFieldDescAnnotation = {
    val byte = 0 // TODO
    val offset = 0 // TODO
    val json = toJson(byte, offset).toString

    RegFieldDescAnnotation(target.toNamed, json)
  }

  def toJson(byteOffset: Int, bitOffset: Int): JValue = {
    (("byteOffset" -> s"0x${byteOffset.toHexString}") ~
      ("bitOffset" -> bitOffset) ~
      ("name" -> reg.name) ~
      ("description" -> reg.desc) ~
      ("resetValue" -> reg.reset) ~
      ("group" -> reg.group) ~
      ("accessType" -> reg.access.toString) ~
      ("writeType" -> reg.wrType.getOrElse("").toString) ~
      ("readAction" -> reg.rdAction.getOrElse("").toString) ~
      ("volatile" -> reg.volatile) ~
      ("enumerations" -> reg.enumerations.map {
        case (key, (name, edesc)) => {
          (("value" -> key) ~ ("name" -> name) ~ ("description" -> edesc))
        }
      })
      )
  }


  // ("bitWidth"     -> reg.width) ~
  // ("groupreg"    -> reg.map{_.groupreg}) ~

  def transformClass = classOf[DescribedRegDumpTransform]
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
  def annotate(
    named: ModuleName,
    base: BigInt,
    mapping: Seq[RegField.Map]): Unit = {
    /*
    val anno = RegMappingAnnotation(name, s"0x${base.toInt.toHexString}", mapping.flatMap { case (byte, seq) => (seq.flatMap(_.desc)) })
    implicit val formats = Serialization.formats(NoTypeHints)
    println(firrtl.annotations.JsonProtocol.serialize(List(anno)))
    */


    val name = named.name

    mapping.flatMap { case (byte, seq) =>
//      println("RegMappingAnnotation: base: " + base)
//      println("RegMappingAnnotation: name: " + name)
//      println("RegMappingAnnotation: byte" + byte)
//      println("RegMappingAnnotation: width" + seq.map(_.width))
//      seq.map(_.width).foreach(println)
//      println("RegMappingAnnotation: scanleft " + seq.map(_.width).scanLeft(base)(_ + _))
//      seq.map(_.width).scanLeft(base)(_ + _).foreach(println)
//      println("RegMappingAnnotation: zip  " + seq.map(_.width).scanLeft(base)(_ + _).zip(seq))
//      seq.map(_.width).scanLeft(base)(_ + _).zip(seq).foreach(println)
//      println("RegMappingAnnotation: base: " + base)
//      println("RegMappingAnnotation: name: " + name)
//      seq.map(_.width).scanLeft(0)(_ + _).zip(seq).foreach(println)

      seq.map(_.width).scanLeft(0)(_ + _).zip(seq).map {
        case (offset, f) => {
          val anonName = s"unnamedRegField${byte.toHexString}_${offset}"
//          (f.desc.map {
//            _.name
//          }.getOrElse(anonName)) -> f.toJson(byte, offset)
          //RegFieldhHolder(named, f.toJson(byte, offset).toString)
          val json = f.toJson(byte, offset).toString
          RegFieldDescAnnotation(named, json.toString)
        }
      }
    }
    //regDescs.map(reg => RegFieldDescAnnotation(named,reg.toJson()))


  }



}