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

class DebugCSRsLogicalTreeNode(
  device: SimpleDevice,
  f: => OMRegisterMap,
  p: Parameters
) extends LogicalTreeNode {
  def getOMDebug(resourceBindings: ResourceBindings): Seq[OMComponent] = {
    val memRegions: Seq[OMMemoryRegion] = DiplomaticObjectModelAddressing
      .getOMMemoryRegions("Debug", resourceBindings, Some(f))
    val cfg: DebugModuleParams = p(DebugModuleParams)

    Seq[OMComponent](
      OMDebugCSRs(
        nDebugScratchRegisters = 0,
        nTriggers = 0,
        supportedOMTriggerTypes = Nil, // List[List[OMTriggerType]], // Each trigger could support different types
        nTriggerChainDepth = 0,
        dscrXdebugver = 0,
        modeMTdataAccessible = Nil, // List[Boolean],
        hasMcontrolHit = Nil, //  List[Boolean],
        supportedMcontrolActions = Nil //  List[TriggerMatchAction], // What to do if trigger hits
      )
    )
  }

  def getOMComponents(resourceBindingsMap: ResourceBindingsMap, components: Seq[OMComponent]): Seq[OMComponent] = {
    DiplomaticObjectModelAddressing.getOMComponentHelper(device, resourceBindingsMap, getOMDebug)
  }
}

class DebugLogicalTreeNode(
  device: SimpleDevice,
  f: => OMRegisterMap,
  p: Parameters,
  m: () => TLDebugModule
) extends LogicalTreeNode {
  def getOMDebug(resourceBindings: ResourceBindings): Seq[OMComponent] = {
    val memRegions :Seq[OMMemoryRegion] = DiplomaticObjectModelAddressing
      .getOMMemoryRegions("Debug", resourceBindings, Some(f))
    val cfg : m.cfg

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
        nSupportedHarts = m.nComponents,
        nAbstractDataWords = cfg.nAbstractDataWords,
        nProgramBufferWords = cfg.nProgramBufferWords,
        nDMIAddressSizeBits = cfg.nDMIAddrSize,
        hasSystemBusAccess = cfg.hasBusMaster,
        supportsQuickAccess = cfg.supportQuickAccess,
        supportsHartArray = cfg.supportHartArray,
        hasImplicitEbreak = cfg.hasImplicitEbreak,
        sbcsSBAVersion = 1,
        sbaAddressSizeBits = cfg.maxSupportedSBAccess,
        hasSBAccess8 = cfg.maxSupportedSBAccess >= 8,
        hasSBAccess16 = cfg.maxSupportedSBAccess >= 16,
        hasSBAccess32 = cfg.maxSupportedSBAccess >= 32,
        hasSBAccess64 = cfg.maxSupportedSBAccess >= 64,
        hasSBAccess128 = cfg.maxSupportedSBAccess == 128,
        hartSeltoHartIDMapping = Nil, // HartSel goes from 0->N but HartID is not contiguious or increasing
        authenticationType = NONE,
        nHartsellenBits = p(MaxHartIdBits), // Number of actually implemented bits of Hartsel
        hasHartInfo = true,
        hasAbstractauto = true,
        cfgStrPtrValid = false,
        nHaltSummaryRegisters = 2,
        nHaltGroups = cfg.nHaltGroups,
        nExtTriggers = cfg.nExtTriggers,
        hasResetHaltReq = true,
        hasHartReset = false,
        hasAbstractAccessFPU = false,
        hasAbstractAccessCSR = false,
        hasAbstractAccessMemory = false,
        hasCustom = m.needsCustom,
        hasAbstractPostIncrement = false,
        hasAbstractPostExec = false,
        hasClockGate = cfg.clockGate
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
