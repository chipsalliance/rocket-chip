// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.logicaltree

import freechips.rocketchip.config._
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

class DebugLogicalTreeNode(
  device: SimpleDevice,
  f: => OMRegisterMap,
  p: Parameters,
  hasCustom: => Boolean
) extends LogicalTreeNode {
  def getOMDebug(resourceBindings: ResourceBindings): Seq[OMComponent] = {
    val memRegions :Seq[OMMemoryRegion] = DiplomaticObjectModelAddressing
      .getOMMemoryRegions("Debug", resourceBindings, Some(f))
    val cfg : DebugModuleParams = p(DebugModuleParams)

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
        interfaceType = OMDebug.getOMDebugInterfaceType(p),
        nSupportedHarts = 0,
        nAbstractDataWords = cfg.nAbstractDataWords,
        nProgramBufferWords = cfg.nProgramBufferWords,
        nDMIAddressSizeBits = cfg.nDMIAddrSize,
        hasSystemBusAccess = false,
        supportsQuickAccess = cfg.supportQuickAccess,
        supportsHartArray = cfg.supportHartArray,
        hasImplicitEbreak = cfg.hasImplicitEbreak,
        sbcsSBAVersion = 1, // This should just always be 1. 0 has tons of issues.
        sbaAddressSizeBits = cfg.maxSupportedSBAccess,
        hasSBAccess8 = cfg.maxSupportedSBAccess >= 8,
        hasSBAccess16 = cfg.maxSupportedSBAccess >= 16,
        hasSBAccess32 = cfg.maxSupportedSBAccess >= 32,
        hasSBAccess64 = cfg.maxSupportedSBAccess >= 64,
        hasSBAccess128 = cfg.maxSupportedSBAccess == 128,
        hartSeltoHartIDMapping = Nil, // HartSel goes from 0->N but HartID is not contiguious or increasing
        authenticationType = NONE,
        nHartsellenBits = 0, // Number of actually implemented bits of Hartsel
        hasHartInfo = false,
        //supportedHartArrayWindowBits: List[Int], -- Getting rid of this because it makes no sense.
        hasAbstractauto = false,
        cfgStrPtrValid = false,
        nHaltSummaryRegisters = 0,
        nHaltGroups = cfg.nHaltGroups,
        nExtTriggers = cfg.nExtTriggers,
        hasResetHaltReq = false,
        hasHartReset = false,
        hasAbstractAccessFPU = false,
        hasAbstractAccessCSR = false,
        hasAbstractAccessMemory = false,
        hasCustom = hasCustom,
        hasAbstractPostIncrement = false,
        hasAbstractPostExec = false,
        hasClockGate = false
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
