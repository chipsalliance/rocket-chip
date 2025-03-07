// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._

import freechips.rocketchip.regmapper.RegField

import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods.{pretty, render}

/** Mix this into a Module class or instance to mark its ports as untouchable */
trait DontTouch { self: RawModule =>
  // TODO: replace this with an implicit class from UserModule that uses getPorts
  // TODO: this is a workaround for firrtl #756
  def dontTouch(data: Data): Unit = data match {
     case agg: Aggregate => agg.getElements.foreach(dontTouch)
     case elt: Element => chisel3.dontTouch(elt)
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

object GenRegDescsAnno {

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

