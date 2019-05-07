// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.logicaltree

import freechips.rocketchip.devices.debug._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelAddressing
import freechips.rocketchip.diplomaticobjectmodel.model.{OMComponent, _}

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
    val Description(name, mapping) = device.describe(resourceBindings)
    val ints = DiplomaticObjectModelAddressing.describeInterrupts(name, resourceBindings)
    val targets = getInterruptTargets(resourceBindings)

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
        targets = targets
      )
    )
  }

  private def getInterruptTargets(resourceBindings: ResourceBindings): Seq[OMInterruptTarget] = {
    case class InterruptTarget(device: Device, numberAtReceiver: BigInt)

    // Can't pattern match on BigInts using integer literals, so we define
    // BigInts to match against.
    // These match the RISC-V Privileged ISA spec mcause values.
    val UserMode = BigInt(8)
    val SupervisorMode = BigInt(9)
    val MachineMode = BigInt(11)

    val int = resourceBindings("int")
    val interruptTargets = int.map {
      case Binding(Some(device), ResourceInt(numberAtReceiver)) =>
        InterruptTarget(device, numberAtReceiver)
      case b: Binding => throw new Exception(s"Unexpected binding: $b")
    }
    interruptTargets.groupBy(_.device).map { case (device, targets) =>
      // The interrupt resource bindings on the PLIC actually point to an
      // intermediate SinksExternalInterrupts node, and we have to follow it one
      // more level to get to the actual Device pointing to CPU core.
      val coreDevice = device.parent.get.asInstanceOf[SimpleDevice]
      // We expect the deviceNamePlusAddress to look like "cpu@0"
      val hartId = coreDevice.deviceNamePlusAddress.split("@").last.toInt
      val modes = targets.map {
        case InterruptTarget(_, UserMode) => OMUserMode
        case InterruptTarget(_, SupervisorMode) => OMSupervisorMode
        case InterruptTarget(_, MachineMode) => OMMachineMode
        case InterruptTarget(_, i) => throw new Exception(s"Unexpected interrupt number: $i")
      }

      OMInterruptTarget(hartId = hartId, modes = modes)
    }.toSeq.sortBy(_.hartId)
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
