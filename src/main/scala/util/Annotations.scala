// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._
import chisel3.internal.InstanceId
import chisel3.experimental.{annotate, ChiselAnnotation, RawModule}
import firrtl.annotations._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink.TLToAXI4IdMapEntry

/** Record a case class that was used to parameterize this target. */
case class ParamsAnnotation(target: Named, paramsClassName: String, params: Map[String,Any]) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named) = this.copy(n)
}

case class ParamsChiselAnnotation[T <: Product](target: InstanceId, params: T) extends ChiselAnnotation {
  private val paramMap = params.getClass.getDeclaredFields.map(_.getName).zip(params.productIterator.to).toMap
  def toFirrtl = ParamsAnnotation(target.toNamed, params.getClass.getName, paramMap)
}

/** Record an address map. */
case class AddressMapAnnotation(target: Named, mapping: Seq[AddressMapEntry], label: String) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named) = this.copy(n)

  def toUVM: String =
    s"// Instance Name: ${target.serialize}\n" +
      mapping.map(_.range.toUVM).mkString("\n")

  def toJSON: String =
    s"""{\n  "${label}":  [\n""" +
      mapping.map(_.range.toJSON).mkString(",\n") +
      "\n  ]\n}"
}

/** Record a conversion of TL source ids to AXI4 ids. */
case class TLToAXI4IdMapAnnotation(target: Named, mapping: Seq[TLToAXI4IdMapEntry]) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named) = this.copy(n)
}

/** Marks this module as a candidate for register retiming */
case class RetimeModuleAnnotation(target: ModuleName) extends SingleTargetAnnotation[ModuleName] {
  def duplicate(n: ModuleName) = this.copy(n)
}

/** Annotation capturing information about port slave devices. */
case class SlaveAddressMapChiselAnnotation(
    target: InstanceId,
    addresses: Seq[AddressSet],
    perms: ResourcePermissions) extends ChiselAnnotation {
  private val range = AddressRange.fromSets(addresses)
  def toFirrtl = AddressMapAnnotation(
    target = target.toNamed,
    mapping = range.map { r => AddressMapEntry(r, perms, Nil) },
    label = "slaves")
}

/** Record information about a top-level port of the design */
case class TopLevelPortAnnotation(
    target: ComponentName,
    protocol: String,
    tags: Seq[String],
    mapping: Seq[AddressMapEntry]) extends SingleTargetAnnotation[ComponentName] {
  def duplicate(n: ComponentName) = this.copy(n)
}

/** Helper object containing methods for applying annotations to targets */
object annotated {
  def params[T <: Product](component: InstanceId, params: T): T = {
    annotate(ParamsChiselAnnotation(component, params))
    params
  }

  def addressMapping(component: InstanceId, mapping: Seq[AddressMapEntry]): Seq[AddressMapEntry] = {
    annotate(new ChiselAnnotation { def toFirrtl = AddressMapAnnotation(component.toNamed, mapping, "mapping") })
    mapping
  }

  def idMapping(component: InstanceId, mapping: Seq[TLToAXI4IdMapEntry]): Seq[TLToAXI4IdMapEntry] = {
    annotate(new ChiselAnnotation { def toFirrtl = TLToAXI4IdMapAnnotation(component.toNamed, mapping) })
    mapping
  }

  def port[T <: Data](
      data: T,
      protocol: String,
      tags: Seq[String],
      mapping: Seq[AddressMapEntry]): T = {
    annotate(new ChiselAnnotation { def toFirrtl = TopLevelPortAnnotation(data.toNamed, protocol, tags, mapping) })
    data
  }
}

/** Mix this into a Module class or instance to mark its ports as untouchable */
trait DontTouch { self: RawModule =>
  // TODO: replace this with an implicit class from UserModule that uses getPorts
  // TODO: this is a workaround for firrtl #756
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

/** Mix this into a Module class or instance to mark it for register retiming */
trait ShouldBeRetimed { self: RawModule =>
  chisel3.experimental.annotate(new ChiselAnnotation { def toFirrtl = RetimeModuleAnnotation(self.toNamed) })
}
