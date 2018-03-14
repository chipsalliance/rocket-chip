// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3.internal.InstanceId
import chisel3.experimental.{annotate, ChiselAnnotation}

import firrtl.{CircuitState, LowForm, Transform}
import firrtl.annotations._

import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods.{pretty, render}

case class ParamsAnnotation(target: Named, paramsClassName: String, params: Map[String,Any]) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named) = this.copy(n)
}

case class ParamsChiselAnnotation[T <: Product](target: InstanceId, params: T) extends ChiselAnnotation {
  private val paramMap = params.getClass.getDeclaredFields.map(_.getName).zip(params.productIterator.to).toMap
  def toFirrtl = ParamsAnnotation(target.toNamed, params.getClass.getName, paramMap)
}

object annotated {
  def params[T <: Product](component: InstanceId, params: T): T = {
    val anno = ParamsChiselAnnotation(component, params)
    annotate(anno)
    params
  }
}
