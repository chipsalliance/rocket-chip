// See LICENSE.SiFive for license details.

package org.chipsalliance.rocketutils

import chisel3._
import chisel3.experimental.{annotate, ChiselAnnotation}

import firrtl.annotations._

/** Record a sram. */
case class SRAMAnnotation(target: Named,
  address_width: Int,
  name: String,
  data_width: Int,
  depth: BigInt,
  description: String,
  write_mask_granularity: Int) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named) = this.copy(n)
}

/** Record a set of interrupts. */
case class InterruptsPortAnnotation(target: Named, name: String, interruptIndexes: Seq[Int]) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named) = this.copy(n)
}

/** Record a case class that was used to parameterize this target. */
case class GlobalConstantsAnnotation(target: Named, xLen: Int) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named) = this.copy(n)
}

case class GlobalConstantsChiselAnnotation[T <: Product](target: InstanceId, xLen: Int) extends ChiselAnnotation {
  def toFirrtl = GlobalConstantsAnnotation(target.toNamed, xLen)
}

/** Record a case class that was used to parameterize this target. */
case class ParamsAnnotation(target: Named, paramsClassName: String, params: Map[String,Any]) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named) = this.copy(n)
}

case class ParamsChiselAnnotation[T <: Product](target: InstanceId, params: T) extends ChiselAnnotation {
  private val paramMap = params.getClass.getDeclaredFields.map(_.getName).zip(params.productIterator).toMap
  def toFirrtl = ParamsAnnotation(target.toNamed, params.getClass.getName, paramMap)
}

/** Marks this module as a candidate for register retiming */
case class RetimeModuleAnnotation(target: ModuleName) extends SingleTargetAnnotation[ModuleName] {
  def duplicate(n: ModuleName) = this.copy(n)
}

/** Record the resetVector. */
case class ResetVectorAnnotation(target: Named, resetVec: BigInt) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named): ResetVectorAnnotation = this.copy(n)
}

/** Helper object containing methods for applying annotations to targets */
object Annotated {

  def srams(
    component: InstanceId,
    name: String,
    address_width: Int,
    data_width: Int,
    depth: BigInt,
    description: String,
    write_mask_granularity: Int): Unit = {
    annotate(new ChiselAnnotation {def toFirrtl: Annotation = SRAMAnnotation(
      component.toNamed,
      address_width = address_width,
      name = name,
      data_width = data_width,
      depth = depth,
      description = description,
      write_mask_granularity = write_mask_granularity
    )})}

  def interrupts(component: InstanceId, name: String, interrupts: Seq[Int]): Unit = {
    annotate(new ChiselAnnotation {def toFirrtl: Annotation = InterruptsPortAnnotation(
      component.toNamed,
      name,
      interrupts
    )})
  }

  def resetVector(component: InstanceId, resetVec: BigInt): Unit = {
    annotate(new ChiselAnnotation {def toFirrtl: Annotation = ResetVectorAnnotation(component.toNamed, resetVec)})
  }

  def constants(component: InstanceId, xLen: Int): Unit = {
    annotate(GlobalConstantsChiselAnnotation(component, xLen ))
  }

  def params[T <: Product](component: InstanceId, params: T): T = {
    annotate(ParamsChiselAnnotation(component, params))
    params
  }
}

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

/** Mix this into a Module class or instance to mark it for register retiming */
trait ShouldBeRetimed { self: RawModule =>
  chisel3.experimental.annotate(new ChiselAnnotation { def toFirrtl: RetimeModuleAnnotation = RetimeModuleAnnotation(self.toNamed) })
}
