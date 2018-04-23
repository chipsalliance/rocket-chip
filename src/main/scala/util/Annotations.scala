// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._
import chisel3.experimental.{ChiselAnnotation, RawModule, annotate}
import firrtl.annotations._
import freechips.rocketchip.regmapper.RegField
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
