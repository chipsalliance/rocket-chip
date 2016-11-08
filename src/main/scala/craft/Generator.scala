package craft

import util.GeneratorApp
import org.accellera.spirit.v1685_2009._
import javax.xml.bind.{JAXBContext, Marshaller}
import java.io.{File, FileOutputStream}
import scala.collection.JavaConverters
import java.util.Collection
import java.math.BigInteger
import rocketchip._
import junctions._

object Generator extends GeneratorApp {
  def toCollection[T](seq: Seq[T]): Collection[T] =
    JavaConverters.asJavaCollectionConverter(seq).asJavaCollection

  def makeOutputBridge(i: Int): BusInterfaceType.Slave.Bridge = {
    val bridge = new BusInterfaceType.Slave.Bridge
    bridge.setMasterRef(s"io_out_$i")
    bridge.setOpaque(true)
    bridge
  }

  def makePortMap(logicalPortName: String, physicalPortName: String): BusInterfaceType.PortMaps.PortMap = {
    val logicalPort = new BusInterfaceType.PortMaps.PortMap.LogicalPort
    logicalPort.setName(logicalPortName)

    val physicalPort = new BusInterfaceType.PortMaps.PortMap.PhysicalPort
    physicalPort.setName(physicalPortName)

    val portmap = new BusInterfaceType.PortMaps.PortMap
    portmap.setLogicalPort(logicalPort)
    portmap.setPhysicalPort(physicalPort)
    portmap
  }

  def makePortMaps(mappings: Seq[(String, String)]): BusInterfaceType.PortMaps = {
    val portmaps = new BusInterfaceType.PortMaps
    portmaps.getPortMap().addAll(toCollection(
      mappings.map { case (log, phys) => makePortMap(log, phys) }))
    portmaps
  }

  def makeAXIPortMaps(prefix: String): BusInterfaceType.PortMaps = {
    makePortMaps(Seq(
      "ACLK"     -> "clock",
      "ARESETn"  -> "reset",
      "ARVALID"  -> s"${prefix}_ar_valid",
      "ARREADY"  -> s"${prefix}_ar_ready",
      "ARID"     -> s"${prefix}_ar_bits_id",
      "ARADDR"   -> s"${prefix}_ar_bits_addr",
      "ARSIZE"   -> s"${prefix}_ar_bits_size",
      "ARLEN"    -> s"${prefix}_ar_bits_len",
      "ARBURST"  -> s"${prefix}_ar_bits_burst",
      "ARPROT"   -> s"${prefix}_ar_bits_prot",
      "ARLOCK"   -> s"${prefix}_ar_bits_lock",
      "ARQOS"    -> s"${prefix}_ar_bits_qos",
      "ARREGION" -> s"${prefix}_ar_bits_region",
      "ARCACHE"  -> s"${prefix}_ar_bits_cache",
      "ARUSER"   -> s"${prefix}_ar_bits_user",
      "AWVALID"  -> s"${prefix}_aw_valid",
      "AWREADY"  -> s"${prefix}_aw_ready",
      "AWID"     -> s"${prefix}_aw_bits_id",
      "AWADDR"   -> s"${prefix}_aw_bits_addr",
      "AWSIZE"   -> s"${prefix}_aw_bits_size",
      "AWLEN"    -> s"${prefix}_aw_bits_len",
      "AWBURST"  -> s"${prefix}_aw_bits_burst",
      "AWPROT"   -> s"${prefix}_aw_bits_prot",
      "AWLOCK"   -> s"${prefix}_aw_bits_lock",
      "AWQOS"    -> s"${prefix}_aw_bits_qos",
      "AWREGION" -> s"${prefix}_aw_bits_region",
      "AWCACHE"  -> s"${prefix}_aw_bits_cache",
      "AWUSER"   -> s"${prefix}_aw_bits_user",
      "WVALID"   -> s"${prefix}_w_valid",
      "WREADY"   -> s"${prefix}_w_ready",
      "WDATA"    -> s"${prefix}_w_bits_data",
      "WSTRB"    -> s"${prefix}_w_bits_strb",
      "WLAST"    -> s"${prefix}_w_bits_last",
      "WUSER"    -> s"${prefix}_w_bits_user",
      "RVALID"   -> s"${prefix}_r_valid",
      "RREADY"   -> s"${prefix}_r_ready",
      "RID"      -> s"${prefix}_r_bits_id",
      "RRESP"    -> s"${prefix}_r_bits_resp",
      "RDATA"    -> s"${prefix}_r_bits_data",
      "RLAST"    -> s"${prefix}_r_bits_last",
      "RUSER"    -> s"${prefix}_r_bits_user",
      "BVALID"   -> s"${prefix}_b_valid",
      "BREADY"   -> s"${prefix}_b_ready",
      "BID"      -> s"${prefix}_b_bits_id",
      "BRESP"    -> s"${prefix}_b_bits_resp",
      "BUSER"    -> s"${prefix}_b_bits_user"))
  }

  def makeInputInterface(i: Int, nOut: Int): BusInterfaceType = {
    val busType = new LibraryRefType
    busType.setVendor("amba.com")
    busType.setLibrary("AMBA4")
    busType.setName("AXI4")
    busType.setVersion("r0p0_0")

    val abstractionType = new LibraryRefType
    abstractionType.setVendor("amba.com")
    abstractionType.setLibrary("AMBA4")
    abstractionType.setName("AXI4_rtl")
    abstractionType.setVersion("r0p0_0")

    val mmapRef = new MemoryMapRefType
    mmapRef.setMemoryMapRef(s"m${i}_mm")

    val slave = new BusInterfaceType.Slave()
    slave.setMemoryMapRef(mmapRef)
    slave.getBridge().addAll(toCollection(
      (0 until nOut).map(makeOutputBridge _)))

    val portMaps = makeAXIPortMaps(s"io_in_$i")

    val busif = new BusInterfaceType
    busif.setName(s"io_in_$i")
    busif.setBusType(busType)
    busif.setAbstractionType(abstractionType)
    busif.setSlave(slave)
    busif.setPortMaps(portMaps)
    busif
  }

  def makeOutputInterface(i: Int): BusInterfaceType = {
    val busType = new LibraryRefType
    busType.setVendor("amba.com")
    busType.setLibrary("AMBA4")
    busType.setName("AXI4")
    busType.setVersion("r0p0_0")

    val abstractionType = new LibraryRefType
    abstractionType.setVendor("amba.com")
    abstractionType.setLibrary("AMBA4")
    abstractionType.setName("AXI4_rtl")
    abstractionType.setVersion("r0p0_0")

    val addrSpaceRef = new BusInterfaceType.Master.AddressSpaceRef
    addrSpaceRef.setAddressSpaceRef(s"s${i}_as")

    val master = new BusInterfaceType.Master
    master.setAddressSpaceRef(addrSpaceRef)

    val portMaps = makeAXIPortMaps(s"io_out_$i")

    val busif = new BusInterfaceType
    busif.setName(s"io_out_$i")
    busif.setBusType(busType)
    busif.setAbstractionType(abstractionType)
    busif.setMaster(master)
    busif.setPortMaps(portMaps)
    busif
  }

  def makeAddressSpace(name: String, size: Long): AddressSpaces.AddressSpace = {
    val addressSpace = new AddressSpaces.AddressSpace
    addressSpace.setName(name)
    var range = new BankedBlockType.Range
    range.setValue("0x" + size.toHexString)
    addressSpace.setRange(range)
    var width = new BankedBlockType.Width
    width.setValue(BigInteger.valueOf(32))
    addressSpace.setWidth(width)
    addressSpace.setAddressUnitBits(BigInteger.valueOf(8))
    addressSpace
  }

  def makeMemoryMap(name: String, signalMaps: Seq[(String, Long)]): MemoryMapType = {
    // Generate the subspaceMaps, one for each baseAddress.
    val memoryMap = new MemoryMapType
    var subspaceMaps = memoryMap.getMemoryMap()
    memoryMap.setName(name)
    for ((signal, address) <- signalMaps) {
      val subSpaceMap = new SubspaceRefType
      subSpaceMap.setMasterRef(signal)
      subSpaceMap.setName("subspacemap_" + name + "_" + signal + "_" + address.toHexString)
      val baseAddress = new BaseAddress
      baseAddress.setValue("0x" + address.toHexString)
      subSpaceMap.setBaseAddress(baseAddress)
      subspaceMaps.add(subSpaceMap)
    }
    memoryMap.setAddressUnitBits(BigInteger.valueOf(8))
    memoryMap
  }

  def generateIPXact {
    val nInputs = params(InPorts)
    val nOutputs = params(OutPorts)
    val extMemSize = params(ExtMemSize)
    val regionSize = extMemSize / nOutputs

    val busInterfaces = new BusInterfaces
    busInterfaces.getBusInterface().addAll(toCollection(
      (0 until nInputs).map(makeInputInterface(_, nOutputs)) ++
      (0 until nOutputs).map(makeOutputInterface _)))

    val addressSpaces = new AddressSpaces
    addressSpaces.getAddressSpace.addAll(toCollection(
      (0 until nOutputs).map(i => makeAddressSpace(s"s${i}_as", regionSize))
    ))
    val signalMaps = (0 until nOutputs).map(i => (s"io_out_$i", regionSize * i))
    val memoryMaps = new MemoryMaps
    memoryMaps.getMemoryMap().addAll(toCollection(
      (0 until nInputs).map(i => makeMemoryMap(s"m${i}_mm", signalMaps))
    ))

    val model = new ModelType
    val views = new ModelType.Views
    var view = new ViewType
    var envIds = view.getEnvIdentifier
    view.setName("RTL")
    envIds.add("::")
    var verilogSource = new FileSetRef
    verilogSource.setLocalName("hdlSource")
    var fileSetRefs = view.getFileSetRef
    fileSetRefs.add(verilogSource)
    views.getView.add(view)
    model.setViews(views)

    val ports = new ModelType.Ports

    val componentType = new ComponentType
    componentType.setLibrary("ucb-bar")
    componentType.setName("CraftXBar")
    componentType.setVendor("edu.berkeley.cs")
    componentType.setVersion("1.0")
    componentType.setBusInterfaces(busInterfaces)
    componentType.setAddressSpaces(addressSpaces)
    componentType.setMemoryMaps(memoryMaps)
    componentType.setModel(model)

    val factory = new ObjectFactory
    val component = factory.createComponent(componentType)

    val fos = new FileOutputStream(new File(td, s"$longName.xml"))
    val context = JAXBContext.newInstance(classOf[ComponentInstance])
    val marshaller = context.createMarshaller()
    marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true)
    marshaller.marshal(component, fos)
  }

  val longName = names.topModuleProject + "." + names.configs
  generateFirrtl
  generateTestSuiteMakefrags // TODO: Needed only for legacy make targets
  generateParameterDump // TODO: Needed only for legacy make targets
  generateIPXact
}
