// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3.internal.InstanceId
import chisel3.experimental.{annotate, ChiselAnnotation}

import firrtl.{CircuitState, LowForm, Transform}
import firrtl.annotations._

import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods.{pretty, render}

import freechips.rocketchip.diplomacy.AddressMapEntry
import freechips.rocketchip.tilelink.TLToAXI4IdMapEntry

case class ParamsAnnotation(target: Named, paramsClassName: String, params: Map[String,Any]) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named) = this.copy(n)
}

case class ParamsChiselAnnotation[T <: Product](target: InstanceId, params: T) extends ChiselAnnotation {
  private val paramMap = params.getClass.getDeclaredFields.map(_.getName).zip(params.productIterator.to).toMap
  def toFirrtl = ParamsAnnotation(target.toNamed, params.getClass.getName, paramMap)
}

// TODO, figure out how to type parameterize these, such that they can be deserialized
case class AddressMapAnnotation(target: Named, mapping: Seq[AddressMapEntry]) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named) = this.copy(n)
}

case class TLToAXI4IdMapAnnotation(target: Named, mapping: Seq[TLToAXI4IdMapEntry]) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named) = this.copy(n)
}

object annotated {
  def params[T <: Product](component: InstanceId, params: T): T = {
    annotate(ParamsChiselAnnotation(component, params))
    params
  }

  def addressMapping(component: InstanceId, mapping: Seq[AddressMapEntry]): Seq[AddressMapEntry] = {
    annotate(new ChiselAnnotation { def toFirrtl = AddressMapAnnotation(component.toNamed, mapping) })
    mapping
  }

  def idMapping(component: InstanceId, mapping: Seq[TLToAXI4IdMapEntry]): Seq[TLToAXI4IdMapEntry] = {
    annotate(new ChiselAnnotation { def toFirrtl = TLToAXI4IdMapAnnotation(component.toNamed, mapping) })
    mapping
  }
}
