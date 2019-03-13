// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.logicaltree

import freechips.rocketchip.devices.debug._
import freechips.rocketchip.devices.tilelink.{CLINT, TLPLIC}
import freechips.rocketchip.diplomacy.{ResourceBindings, ResourceBindingsMap, SimpleDevice}
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelAddressing
import freechips.rocketchip.diplomaticobjectmodel.model._

class CLINTLogicalTree(device: SimpleDevice, f: () => OMRegisterMap) extends LogicalTree {

  def getOMCLINT(resourceBindings: ResourceBindings): Seq[OMComponent] = {
    val memRegions : Seq[OMMemoryRegion]= DiplomaticObjectModelAddressing.getOMMemoryRegions("CLINT", resourceBindings, Some(f()))

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

  override def getOMComponents(resourceBindingsMap: ResourceBindingsMap, components: Seq[OMComponent]): Seq[OMComponent] = {
    DiplomaticObjectModelAddressing.getOMComponentHelper(device, resourceBindingsMap, getOMCLINT)
  }
}

class PLICLogicalTree(device: () => SimpleDevice, f: () => OMRegisterMap, nPriorities: Int) extends LogicalTree {
  private def getOMPLIC(resourceBindings: ResourceBindings): Seq[OMComponent] = {
    val memRegions : Seq[OMMemoryRegion]= DiplomaticObjectModelAddressing.getOMMemoryRegions("PLIC", resourceBindings, Some(f()))

    val ints = DiplomaticObjectModelAddressing.describeInterrupts(device().describe(resourceBindings).name, resourceBindings)

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

  override def getOMComponents(resourceBindingsMap: ResourceBindingsMap, components: Seq[OMComponent]): Seq[OMComponent] = {
    DiplomaticObjectModelAddressing.getOMComponentHelper(device(), resourceBindingsMap, getOMPLIC)
  }
}

class DebugLogicalTree(device: SimpleDevice, f: () => OMRegisterMap, debugModuleParams: DebugModuleParams, exportDebugJTAG: Boolean, exportDebugCJTAG: Boolean, exportDebugDMI: Boolean) extends LogicalTree {
  def getOMDebug(resourceBindings: ResourceBindings): Seq[OMComponent] = {
    val memRegions :Seq[OMMemoryRegion] = DiplomaticObjectModelAddressing.getOMMemoryRegions("Debug", resourceBindings, Some(f()))
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

  override def getOMComponents(resourceBindingsMap: ResourceBindingsMap, components: Seq[OMComponent]): Seq[OMComponent] = {
    DiplomaticObjectModelAddressing.getOMComponentHelper(device, resourceBindingsMap, getOMDebug)
  }
}

