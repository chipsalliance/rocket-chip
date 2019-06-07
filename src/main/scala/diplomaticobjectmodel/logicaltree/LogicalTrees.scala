// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.logicaltree

import freechips.rocketchip.config._
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelAddressing
import freechips.rocketchip.diplomaticobjectmodel.model._
import freechips.rocketchip.tile.MaxHartIdBits

class CLINTLogicalTreeNode(device: SimpleDevice, f: => OMRegisterMap) extends LogicalTreeNode(() => Some(device)) {

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

  def getOMComponents(resourceBindings: ResourceBindings, components: Seq[OMComponent]): Seq[OMComponent] = {
    DiplomaticObjectModelAddressing.getOMComponentHelper(resourceBindings, getOMCLINT)
  }
}

class DebugLogicalTreeNode(
  device: SimpleDevice,
  dmOuter: () => TLDebugModuleOuterAsync,
  dmInner: () => TLDebugModuleInnerAsync
)(implicit val p: Parameters) extends LogicalTreeNode(() => Some(device)) {
  def getOMDebug(resourceBindings: ResourceBindings): Seq[OMComponent] = {
    val nComponents: Int = dmOuter().dmOuter.module.getNComponents()
    val needCustom: Boolean = dmInner().dmInner.module.getNeedCustom()
    val omRegMap: OMRegisterMap = dmInner().dmInner.module.omRegMap
    val cfg: DebugModuleParams = dmInner().dmInner.getCfg()

    val memRegions :Seq[OMMemoryRegion] = DiplomaticObjectModelAddressing
      .getOMMemoryRegions("Debug", resourceBindings, Some(omRegMap))

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
        nSupportedHarts = nComponents,
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
        hasResetHaltReq = false,
        hasHartReset = false,
        hasAbstractAccessFPU = false,
        hasAbstractAccessCSR = false,
        hasAbstractAccessMemory = false,
        hasCustom = needCustom,
        hasAbstractPostIncrement = false,
        hasAbstractPostExec = true,
        hasClockGate = cfg.clockGate
    )
    )
  }

  def getOMComponents(resourceBindings: ResourceBindings, components: Seq[OMComponent]): Seq[OMComponent] = {
    DiplomaticObjectModelAddressing.getOMComponentHelper(resourceBindings, getOMDebug)
  }
}

class PLICLogicalTreeNode(
  device: => SimpleDevice,
  omRegMap: => OMRegisterMap,
  nPriorities: => Int,
  nInterrupts: => Int) extends LogicalTreeNode(() => Some(device)) {
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
        nInterrupts = nInterrupts,
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

  def getOMComponents(resourceBindings: ResourceBindings, components: Seq[OMComponent]): Seq[OMComponent] = {
    DiplomaticObjectModelAddressing.getOMComponentHelper(resourceBindings, getOMPLIC)
  }
}

class BusMemoryLogicalTreeNode(
  device: Device,
  omSRAMs: Seq[OMSRAM],
  busProtocol: OMProtocol,
  dataECC: Option[OMECC] = None,
  hasAtomics: Option[Boolean] = None,
  busProtocolSpecification: Option[OMSpecification] = None) extends LogicalTreeNode(() => Some(device)) {
  def getOMBusMemory(resourceBindings: ResourceBindings): Seq[OMComponent] = {
    val memRegions: Seq[OMMemoryRegion] = DiplomaticObjectModelAddressing.getOMMemoryRegions("OMMemory", resourceBindings, None)

    val omBusMemory = OMBusMemory(
      memoryRegions = memRegions,
      interrupts = Nil,
      specifications = Nil,
      busProtocol = Some(busProtocol),
      dataECC = dataECC.getOrElse(OMECCIdentity),
      hasAtomics = hasAtomics.getOrElse(false),
      memories = omSRAMs
    )
    Seq(omBusMemory)
  }

  def getOMComponents(resourceBindings: ResourceBindings, children: Seq[OMComponent]): Seq[OMComponent] = {
    DiplomaticObjectModelAddressing.getOMComponentHelper(resourceBindings, getOMBusMemory)
  }
}

class SubSystemLogicalTreeNode(var getOMInterruptDevice: (ResourceBindings) => Seq[OMInterrupt] = (ResourceBindings) => Nil)
  extends LogicalTreeNode(() => None) {
  override def getOMComponents(resourceBindings: ResourceBindings, components: Seq[OMComponent]): Seq[OMComponent] = {
    List(
      OMCoreComplex(
        components = components,
        documentationName = ""
      )
    )
  }
}


class BusErrorLogicalTreeNode(device: => SimpleDevice, f: => OMRegisterMap) extends LogicalTreeNode(() => Some(device)) {
  def getOMBusError(resourceBindings: ResourceBindings): Seq[OMComponent] = {
    val Description(name, mapping) = device.describe(resourceBindings)

    val memRegions : Seq[OMMemoryRegion]= DiplomaticObjectModelAddressing.getOMMemoryRegions("BusError", resourceBindings, Some(f))

    Seq[OMComponent](
      OMBusError(
        memoryRegions = memRegions,
        interrupts = DiplomaticObjectModelAddressing.describeGlobalInterrupts(name, resourceBindings), // outgoing interrupts
        specifications = Nil
      )
    )
  }

  def getOMComponents(resourceBindings: ResourceBindings, components: Seq[OMComponent]): Seq[OMComponent] = {
    DiplomaticObjectModelAddressing.getOMComponentHelper(resourceBindings, getOMBusError)
  }
}
