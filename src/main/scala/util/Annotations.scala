// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._
import chisel3.experimental.{annotate, ChiselAnnotation, RawModule}
import firrtl.annotations._

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

case class RetimeModuleAnnotation(target: Named) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named) = this.copy(n)
}

/** Marks this module as a candidate for register retiming */
trait ShouldBeRetimed { self: RawModule =>
  annotate(new ChiselAnnotation { def toFirrtl = RetimeModuleAnnotation(self.toNamed) })
}
