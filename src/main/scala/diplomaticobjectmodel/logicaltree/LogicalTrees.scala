// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.logicaltree

import Chisel.{Data, UInt, Vec}
import chisel3.SyncReadMem
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelAddressing
import freechips.rocketchip.diplomaticobjectmodel.model._
import freechips.rocketchip.util.DescribedSRAM

class CLINTLogicalTreeNode(device: SimpleDevice, f: => OMRegisterMap) extends LogicalTreeNode {

  def getOMCLINT(resourceBindings: ResourceBindings): Seq[OMComponent] = {
    val memRegions : Seq[OMMemoryRegion]= DiplomaticObjectModelAddressing.getOMMemoryRegions("CLINT", resourceBindings, Some(f))

    Seq[OMComponent](
      OMCLINT(
        memoryRegions = memRegions,
        interrupts = Nil,
        specifications = List(
          OMSpecification(
            name = "The RISC-V Instruction Set Manual, Volume II: Privileged Architecture",
            version = "1.10"
          )
        )
      )
    )
  }

  def getOMComponents(resourceBindingsMap: ResourceBindingsMap, components: Seq[OMComponent]): Seq[OMComponent] = {
    DiplomaticObjectModelAddressing.getOMComponentHelper(device, resourceBindingsMap, getOMCLINT)
  }
}

class DebugLogicalTreeNode(device: SimpleDevice, f: => OMRegisterMap, debugModuleParams: DebugModuleParams, exportDebugJTAG: Boolean, exportDebugCJTAG: Boolean, exportDebugDMI: Boolean) extends LogicalTreeNode {
  def getOMDebug(resourceBindings: ResourceBindings): Seq[OMComponent] = {
    val memRegions :Seq[OMMemoryRegion] = DiplomaticObjectModelAddressing.getOMMemoryRegions("Debug", resourceBindings, Some(f))
    val cfg : DebugModuleParams = debugModuleParams

    Seq[OMComponent](
      OMDebug(
        memoryRegions = memRegions,
        interrupts = Nil,
        specifications = List(
          OMSpecification(
            name = "The RISC-V Debug Specification",
            version = "0.13"
          )
        ),
        nAbstractDataWords = cfg.nAbstractDataWords,
        nProgramBufferWords = cfg.nProgramBufferWords,
        interfaceType = OMDebug.getDebugInterfaceType(exportDebugJTAG, exportDebugCJTAG, exportDebugDMI),
      )
    )
  }

  def getOMComponents(resourceBindingsMap: ResourceBindingsMap, components: Seq[OMComponent]): Seq[OMComponent] = {
    DiplomaticObjectModelAddressing.getOMComponentHelper(device, resourceBindingsMap, getOMDebug)
  }
}

class PLICLogicalTreeNode(device: => SimpleDevice, omRegMap: => OMRegisterMap, nPriorities: => Int) extends LogicalTreeNode {
  def getOMPLIC(resourceBindings: ResourceBindings): Seq[OMComponent] = {
    val memRegions : Seq[OMMemoryRegion]= DiplomaticObjectModelAddressing.getOMMemoryRegions("PLIC", resourceBindings, Some(omRegMap))
    val ints = DiplomaticObjectModelAddressing.describeInterrupts(device.describe(resourceBindings).name, resourceBindings)
    val Description(name, mapping) = device.describe(resourceBindings)

    Seq[OMComponent](
      OMPLIC(
        memoryRegions = memRegions,
        interrupts = ints,
        specifications = List(
          OMSpecification(
            name = "The RISC-V Instruction Set Manual, Volume II: Privileged Architecture",
            version = "1.10"
          )
        ),
        latency = 2, // TODO
        nPriorities = nPriorities,
        targets = Nil
      )
    )
  }

  def getOMComponents(resourceBindingsMap: ResourceBindingsMap, components: Seq[OMComponent]): Seq[OMComponent] = {
    DiplomaticObjectModelAddressing.getOMComponentHelper(device, resourceBindingsMap, getOMPLIC)
  }
}

class SubSystemLogicalTreeNode(var getOMInterruptDevice: (ResourceBindingsMap) => Seq[OMInterrupt] = (ResourceBindingsMap) => Nil) extends LogicalTreeNode {
  override def getOMComponents(resourceBindingsMap: ResourceBindingsMap, components: Seq[OMComponent]): Seq[OMComponent] = {
    List(
      OMCoreComplex(
        components = components,
        documentationName = "",
        externalGlobalInterrupts = getOMInterruptDevice(resourceBindingsMap)
      )
    )
  }
}

class MemoryLogicalTreeNode[T <: Data](sram: DescribedSRAM[T]) extends LogicalTreeNode {

  def getOMComponents(resourceBindingsMap: ResourceBindingsMap, components: Seq[OMComponent]): Seq[OMComponent] = {
    val omMemory = DiplomaticObjectModelAddressing.makeOMMemory(sram)

    Seq(omMemory)
  }
}

class ImpLogicalTreeNode extends LogicalTreeNode {
  override def getOMComponents(resourceBindingsMap: ResourceBindingsMap, children: Seq[OMComponent] = Nil): Seq[OMComponent] =
    children
}

class TestMemoryLogicalTreeNode(device: () => Device, ramArchitecture: RAMArchitecture) extends LogicalTreeNode {
  def getOMTestMemory(resourceBindings: ResourceBindings): Seq[OMComponent] = {
    val memRegions : Seq[OMMemoryRegion]= DiplomaticObjectModelAddressing.getOMMemoryRegions("OMMemory", resourceBindings, None)
    val Description(name, mapping) = device().describe(resourceBindings)
    val ints = DiplomaticObjectModelAddressing.describeInterrupts(name, resourceBindings)

    Seq[OMComponent](
      OMTestMemory(
        memoryRegions = memRegions,
        interrupts = ints,
        specifications = Nil,
        memories = Nil,
        architecture = ramArchitecture
      )
    )
  }

  def getOMComponents(resourceBindingsMap: ResourceBindingsMap, components: Seq[OMComponent]): Seq[OMComponent] = {
    val c = DiplomaticObjectModelAddressing.getOMComponentHelper(device(), resourceBindingsMap, getOMTestMemory)
    val cc = components.map(_.asInstanceOf[OMMemory])
    val children = c.toList.head.asInstanceOf[OMTestMemory].copy( memories = cc).asInstanceOf[OMComponent]
    Seq(children)
  }
}

class TestSoCLogicalTreeNode extends LogicalTreeNode {
  override def getOMComponents(resourceBindingsMap: ResourceBindingsMap, children: Seq[OMComponent] = Nil): Seq[OMComponent] = {
    Seq(OMTestHarness( components = children))
  }
}

case object ParentLogicalTreeNodeKey extends Field[Option[LogicalTreeNode]](
  None
)

