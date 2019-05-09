// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.logicaltree

import Chisel.{Data, UInt, Vec}
import chisel3.SyncReadMem
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelAddressing
import freechips.rocketchip.diplomaticobjectmodel.model.{OMComponent, _}

class CLINTLogicalTreeNode(device: Device, f: => OMRegisterMap) extends LogicalTreeNode(Some(() => device)) {
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

  def getOMComponents(resourceBindings: ResourceBindings, children: Seq[OMComponent]): Seq[OMComponent] = {

    DiplomaticObjectModelAddressing.getOMComponentHelper(device, resourceBindings, getOMCLINT)
  }
}

class DebugLogicalTreeNode(device: SimpleDevice, f: => OMRegisterMap, debugModuleParams: DebugModuleParams, exportDebugJTAG: Boolean, exportDebugCJTAG: Boolean, exportDebugDMI: Boolean) extends LogicalTreeNode(Some(() => device)) {
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

    def getOMComponents(resourceBindings: ResourceBindings, children: Seq[OMComponent]): Seq[OMComponent] = {

      DiplomaticObjectModelAddressing.getOMComponentHelper(device, resourceBindings, getOMDebug)
  }
}

class PLICLogicalTreeNode(device: => SimpleDevice, omRegMap: => OMRegisterMap, nPriorities: => Int) extends LogicalTreeNode(Some(() => device)) {
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

    def getOMComponents(resourceBindings: ResourceBindings, children: Seq[OMComponent]): Seq[OMComponent] = {

      DiplomaticObjectModelAddressing.getOMComponentHelper(device, resourceBindings, getOMPLIC)
  }
}

class SubSystemLogicalTreeNode(var getOMInterruptDevice: () => Seq[OMInterrupt] = () => Nil) extends LogicalTreeNode(None) {
  override   def getOMComponents(resourceBindings: ResourceBindings, children: Seq[OMComponent]): Seq[OMComponent] = {
    List(
      OMCoreComplex(
        components = children,
        documentationName = "",
        externalGlobalInterrupts = getOMInterruptDevice()
      )
    )
  }
}

class BusMemoryLogicalTreeNode(
  device: () => Device,
  omSRAMs: Seq[OMSRAM],
  busProtocol: OMProtocol,
  dataECC: Option[OMECC] = None,
  hasAtomics: Option[Boolean] = None,
  busProtocolSpecification: Option[OMSpecification] = None) extends LogicalTreeNode(Some(device)) {
  def getOMBusMemory(resourceBindings: ResourceBindings): Seq[OMComponent] = {
    val memRegions : Seq[OMMemoryRegion]= DiplomaticObjectModelAddressing.getOMMemoryRegions("OMMemory", resourceBindings, None)
    val Description(name, mapping) = device().describe(resourceBindings)

    val omBusMemory = OMBusMemory(
      memoryRegions = Nil, //  Seq[OMMemoryRegion],
      interrupts = Nil, //: Seq[OMInterrupt],
      specifications = Nil, //: List[OMSpecification],
      busProtocol = Some(new AXI4(busProtocolSpecification)), // busProtocol,
      dataECC = dataECC.getOrElse(OMECC.Identity),
      hasAtomics = hasAtomics.getOrElse(false),
      memories = omSRAMs
    )

    val ints = DiplomaticObjectModelAddressing.describeInterrupts(name, resourceBindings)
    Seq(omBusMemory)
  }

  def getOMComponents(resourceBindings: ResourceBindings, children: Seq[OMComponent]): Seq[OMComponent] = {
      val resourceBindings = BindingScope.getResourceBindings(device())
      DiplomaticObjectModelAddressing.getOMComponentHelper(device(), resourceBindings, getOMBusMemory)
  }
}

class TestSoCLogicalTreeNode extends LogicalTreeNode(None) {
  override   def getOMComponents(resourceBindings: ResourceBindings, children: Seq[OMComponent] = Nil): Seq[OMComponent] = {
    Seq(OMTestHarness( components = children))
  }
}

case object ParentLogicalTreeNodeKey extends Field[Option[LogicalTreeNode]](
  None
)
