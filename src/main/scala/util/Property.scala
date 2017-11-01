// See LICENSE.SiFive for license details.

package freechips.rocketchip.util.property

import Chisel._
import chisel3.internal.sourceinfo.{SourceInfo, SourceLine}
import chisel3.util.{ReadyValidIO}
import freechips.rocketchip.config.{Field, Parameters}

case object PropertyLibrary extends Field[BasePropertyLibrary](new DefaultPropertyLibrary)


sealed abstract class PropertyType(name: String) {
  override def toString: String = name
}

object PropertyType {
  object Assert extends PropertyType("Assert")
  object Assume extends PropertyType("Assume")
  object Cover extends PropertyType("Cover")
}

trait BasePropertyParameters { 
  val pType: PropertyType
  val cond: Bool
  val label: String
  val message: String
}

case class CoverPropertyParameters(
    cond: Bool,
    label: String = "",
    message: String = "") extends BasePropertyParameters {
  val pType = PropertyType.Cover
}

abstract class BasePropertyLibrary {
  def generateProperty(prop_param: BasePropertyParameters)(implicit sourceInfo: SourceInfo)
}

class DefaultPropertyLibrary extends BasePropertyLibrary {
  def generateProperty(prop_param: BasePropertyParameters)(implicit sourceInfo: SourceInfo) {
    // default is to do nothing
    Unit
  }
}

object cover {
  def apply(cond: Bool)(implicit sourceInfo: SourceInfo, p: Parameters): Unit = {
    p(PropertyLibrary).generateProperty(CoverPropertyParameters(cond))
  }
  def apply(cond: Bool, label: String)(implicit sourceInfo: SourceInfo, p: Parameters): Unit = {
    p(PropertyLibrary).generateProperty(CoverPropertyParameters(cond, label))
  }
  def apply(cond: Bool, label: String, message: String)(implicit sourceInfo: SourceInfo, p: Parameters): Unit = {
    p(PropertyLibrary).generateProperty(CoverPropertyParameters(cond, label, message))
  }
  def apply[T <: Data](rv: ReadyValidIO[T], label: String, message: String)(implicit sourceInfo: SourceInfo, p: Parameters): Unit = {
    apply( rv.valid &&  rv.ready, label + "_FIRE",  message + ": valid and ready")
    apply( rv.valid && !rv.ready, label + "_STALL", message + ": valid and not ready")
    apply(!rv.valid &&  rv.ready, label + "_IDLE",  message + ": not valid and ready")
    apply(!rv.valid && !rv.ready, label + "_FULL",  message + ": not valid and not ready")
  }
}
