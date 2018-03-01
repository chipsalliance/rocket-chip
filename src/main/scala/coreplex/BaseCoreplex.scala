// See LICENSE.SiFive for license details.

package freechips.rocketchip.coreplex

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.util._

/** BareCoreplex is the root class for creating a coreplex sub-system */
abstract class BareCoreplex(implicit p: Parameters) extends LazyModule with BindingScope {
  lazy val dts = DTS(bindingTree)
  lazy val dtb = DTB(dts)
  lazy val json = JSON(bindingTree)
}

abstract class BareCoreplexModule[+L <: BareCoreplex](_outer: L) extends LazyModuleImp(_outer) {
  val outer = _outer
  ElaborationArtefacts.add("graphml", outer.graphML)
  ElaborationArtefacts.add("dts", outer.dts)
  ElaborationArtefacts.add("json", outer.json)
  ElaborationArtefacts.add("plusArgs", PlusArgArtefacts.serialize_cHeader)
  println(outer.dts)
}

/** Base Coreplex class with no peripheral devices or ports added */
abstract class BaseCoreplex(implicit p: Parameters) extends BareCoreplex
    with HasInterruptBus
    with HasSystemBus
    with HasPeripheryBus
    with HasMemoryBus {
  override val module: BaseCoreplexModule[BaseCoreplex]

  // Make topManagers an Option[] so as to avoid LM name reflection evaluating it...
  lazy val topManagers = Some(ManagerUnification(sharedMemoryTLEdge.manager.managers))
  ResourceBinding {
    val managers = topManagers.get
    val max = managers.flatMap(_.address).map(_.max).max
    val width = ResourceInt((log2Ceil(max)+31) / 32)
    val model = p(DTSModel)
    val compat = p(DTSCompat)
    val devCompat = (model +: compat).map(s => ResourceString(s + "-dev"))
    val socCompat = (model +: compat).map(s => ResourceString(s + "-soc"))
    devCompat.foreach { Resource(ResourceAnchors.root, "compat").bind(_) }
    socCompat.foreach { Resource(ResourceAnchors.soc,  "compat").bind(_) }
    Resource(ResourceAnchors.root, "model").bind(ResourceString(model))
    Resource(ResourceAnchors.root, "width").bind(width)
    Resource(ResourceAnchors.soc,  "width").bind(width)
    Resource(ResourceAnchors.cpus, "width").bind(ResourceInt(1))

    managers.foreach { case manager =>
      val value = manager.toResource
      manager.resources.foreach { case resource =>
        resource.bind(value)
      }
    }
  }
}

abstract class BaseCoreplexModule[+L <: BaseCoreplex](_outer: L) extends BareCoreplexModule(_outer) {
  println("Generated Address Map")
  private val aw = (outer.sharedMemoryTLEdge.bundle.addressBits-1)/4 + 1
  private val fmt = s"\t%${aw}x - %${aw}x %c%c%c%c%c %s"

  private def collect(path: List[String], value: ResourceValue): List[(String, ResourceAddress)] = {
    value match {
      case r: ResourceAddress => List((path(1), r))
      case b: ResourceMapping => List((path(1), ResourceAddress(b.address, b.permissions)))
      case ResourceMap(value, _) => value.toList.flatMap { case (key, seq) => seq.flatMap(r => collect(key :: path, r)) }
      case _ => Nil
    }
  }
  private val ranges = collect(Nil, outer.bindingTree).groupBy(_._2).toList.flatMap { case (key, seq) =>
    AddressRange.fromSets(key.address).map { r => (r, key.permissions, seq.map(_._1)) }
  }.sortBy(_._1)
  private val json = ranges.map { case (range, ResourcePermissions(r, w, x, c, a), names) =>
    println(fmt.format(
      range.base,
      range.base+range.size,
      if (a) 'A' else ' ',
      if (r) 'R' else ' ',
      if (w) 'W' else ' ',
      if (x) 'X' else ' ',
      if (c) 'C' else ' ',
      names.mkString(", ")))
    s"""{"base":[${range.base}],"size":[${range.size}],"r":[$r],"w":[$w],"x":[$x],"c":[$c],"a":[$a],"names":[${names.map('"'+_+'"').mkString(",")}]}"""
  }
  println("")
  ElaborationArtefacts.add("memmap.json", s"""{"mapping":[${json.mkString(",")}]}""")

  // Confirm that all of memory was described by DTS
  private val dtsRanges = AddressRange.unify(ranges.map(_._1))
  private val allRanges = AddressRange.unify(outer.topManagers.get.flatMap { m => AddressRange.fromSets(m.address) })

  if (dtsRanges != allRanges) {
    println("Address map described by DTS differs from physical implementation:")
    AddressRange.subtract(allRanges, dtsRanges).foreach { case r =>
      println(s"\texists, but undescribed by DTS: ${r}")
    }
    AddressRange.subtract(dtsRanges, allRanges).foreach { case r =>
      println(s"\tdoes not exist, but described by DTS: ${r}")
    }
    println("")
  }
}
