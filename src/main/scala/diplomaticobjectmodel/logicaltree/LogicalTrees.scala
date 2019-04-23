// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.logicaltree

import Chisel.{Data, UInt, Vec}
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelAddressing
import freechips.rocketchip.diplomaticobjectmodel.model.{OMComponent, RAMArchitecture, _}

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

case class OMSRAMData[T <: Data](
  data: T,
  resourceBindings: ResourceBindings,
  architecture: RAMArchitecture,
  description: String,
  depth: Int,
  ecc: Option[OMECC] = None,
  hasAtomics: Option[Boolean] = None
)

class SRAMLogicalTreeNode[T <: Data](description: String, architecture: RAMArchitecture, depth: Int, lanes: Int, bits: Int, device: () => Device,
  ecc: Option[OMECC] = None,
  hasAtomics: Option[Boolean] = None) extends LogicalTreeNode {
  def getOMComponents(resourceBindingsMap: ResourceBindingsMap, components: Seq[OMComponent]): Seq[OMComponent] = {

    Seq(
      DiplomaticObjectModelAddressing.makeOMMemory(
        data = Vec(lanes, UInt(width = bits)),
        resourceBindings = resourceBindingsMap.map(device()),
        architecture = architecture,
        description = description,
        depth = depth,
        ecc = ecc,
        hasAtomics = hasAtomics
      )
    )
  }
}

class ImpLogicalTreeNode extends LogicalTreeNode {
  override def getOMComponents(resourceBindingsMap: ResourceBindingsMap, children: Seq[OMComponent] = Nil): Seq[OMComponent] =
    children
}

class TestSoCLogicalTreeNode extends LogicalTreeNode {
  override def getOMComponents(resourceBindingsMap: ResourceBindingsMap, children: Seq[OMComponent] = Nil): Seq[OMComponent] = {
    Seq(OMTestHarness( components = children))
  }
}

case object DeviceKey extends Field[Option[Device]](
  None
)

case object ParentLogicalTreeNodeKey extends Field[Option[LogicalTreeNode]](
  None
)

case object ResourceBindingsMapLogicalTreeNodeKey extends Field[Option[ResourceBindingsMap]](
  None
)

object ParentTreeNode {
  /**
    * Add the parent tree node to the Parameters so that the children can access the tree node via the params
    * @param p
    * @return
    */
  def set(pltnOpt: Option[LogicalTreeNode], ltn: LogicalTreeNode)(implicit p: Parameters): Parameters = {
    pltnOpt match {
      case None => p
      case Some(pltn) =>
        LogicalModuleTree.add(pltn, ltn)
        p.alterPartial { case ParentLogicalTreeNodeKey => Some(ltn) }
    }
  }
}
