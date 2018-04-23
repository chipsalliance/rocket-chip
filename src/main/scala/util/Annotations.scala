// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._
import chisel3.experimental.{ChiselAnnotation, RawModule, annotate}
import firrtl.annotations._
import freechips.rocketchip.regmapper._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods.{pretty, render}

// TODO: replace this with an implicit class when @chisel unprotects dontTouchPorts
trait DontTouch { self: RawModule =>

  def dontTouch(data: Data): Unit = data match {
     case agg: Aggregate => agg.getElements.foreach(dontTouch)
     case elt: Element => chisel3.core.dontTouch(elt)
  }

  /** Marks every port as don't touch
    *
    * @note This method can only be called after the Module has been fully constructed
    *   (after Module(...))
    */
  def dontTouchPorts(): this.type = {
    self.getModulePorts.foreach(dontTouch(_))
    self
  }

  def dontTouchPortsExcept(f: Data => Boolean): this.type = {
    self.getModulePorts.filterNot(f).foreach(dontTouch(_))
    self
  }
}

/** Marks this module as a candidate for register retiming */
case class RetimeModuleAnnotation(target: ModuleName) extends SingleTargetAnnotation[ModuleName] {
  def duplicate(n: ModuleName) = this.copy(n)
}

/** Mix this into a Module class or instance to mark it for register retiming */
trait ShouldBeRetimed { self: RawModule =>
  chisel3.experimental.annotate(new ChiselAnnotation { def toFirrtl = RetimeModuleAnnotation(self.toNamed) })
}



object GenRegDescJson {

  def serialize(base: BigInt, name: String, mapping: RegField.Map*): String = {


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

    val byteOffsetHex = s"0x${byteOffset.toInt.toHexString}"

    val desc = regField.desc

    val regFieldDescSer = RegFieldDescSer(
      byteOffset = byteOffsetHex,
      bitOffset = bitOffset,
      bitWidth = width,
      name = selectedRegFieldName,
      desc = desc.map {_.desc}.getOrElse("None"),
      group = desc.map {_.group.getOrElse("None")}.getOrElse("None"),
      groupDesc = desc.map {_.groupDesc.getOrElse("None")}.getOrElse("None"),
      accessType = desc.map {_.access.toString}.getOrElse("None"),
      wrType = desc.map(_.wrType.toString).getOrElse("None"),
      rdAction = desc.map(_.rdAction.toString).getOrElse("None"),
      volatile = desc.map(_.volatile).getOrElse(false),
      hasReset = desc.map {_.reset != None }.getOrElse(false),
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
    baseAddress: BigInt,
    mapping: RegField.Map*): Seq[RegField.Map] = {

    val moduleName = rawModule.name
    val baseHex = s"0x${baseAddress.toInt.toHexString}"
    val displayName = s"${moduleName}.${baseHex}"
    val instanceCounter = GenRegDescsAnno.getInstanceCount(moduleName, baseAddress)

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

