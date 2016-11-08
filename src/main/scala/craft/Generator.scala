package craft

import util.GeneratorApp
import org.accellera.spirit.v1685_2009.{File => SpiritFile, _}
import javax.xml.bind.{JAXBContext, Marshaller}
import java.io.{File, FileOutputStream}
import scala.collection.JavaConverters
import java.util.Collection
import java.math.BigInteger
import rocketchip._
import junctions._
import cde.Parameters
import uncore.axi4.AXI4Parameters

class NastiParameters(implicit val p: Parameters) extends HasNastiParameters {}

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
      mappings.sorted.map { case (log, phys) => makePortMap(log, phys) }))
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

  def makePort(name: String, direction: Boolean, width: Int): PortType = {
    val port = new PortType
    val wire = new PortWireType
    
    wire.setDirection(
      if (direction) ComponentPortDirectionType.OUT
      else ComponentPortDirectionType.IN)
    if (width > 1) {
      val vector = new Vector
      val left = new Vector.Left
      val right = new Vector.Right
      left.setValue(BigInteger.valueOf(width - 1))
      right.setValue(BigInteger.valueOf(0))
      vector.setLeft(left)
      vector.setRight(right)
      wire.setVector(vector)
    }

    port.setWire(wire)
    port.setName(name)
    port
  }

  def makeAXIPorts(prefix: String, direction: Boolean, config: HasNastiParameters): Seq[PortType] = {
    val ports = Seq(
      ("ar_valid", direction, 1),
      ("ar_ready", !direction, 1),
      ("ar_bits_id", direction, config.nastiXIdBits),
      ("ar_bits_addr", direction, config.nastiXAddrBits),
      ("ar_bits_size", direction, config.nastiXSizeBits),
      ("ar_bits_len", direction, config.nastiXLenBits),
      ("ar_bits_burst", direction, config.nastiXBurstBits),
      ("ar_bits_lock", direction, AXI4Parameters.lockBits),
      ("ar_bits_cache", direction, config.nastiXCacheBits),
      ("ar_bits_prot", direction, config.nastiXProtBits),
      ("ar_bits_qos", direction, config.nastiXQosBits),
      ("ar_bits_region", direction, config.nastiXRegionBits),
      ("ar_bits_user", direction, config.nastiXUserBits),
      ("aw_valid", direction, 1),
      ("aw_ready", !direction, 1),
      ("aw_bits_id", direction, config.nastiXIdBits),
      ("aw_bits_addr", direction, config.nastiXAddrBits),
      ("aw_bits_size", direction, config.nastiXSizeBits),
      ("aw_bits_len", direction, config.nastiXLenBits),
      ("aw_bits_burst", direction, config.nastiXBurstBits),
      ("aw_bits_lock", direction, AXI4Parameters.lockBits),
      ("aw_bits_cache", direction, config.nastiXCacheBits),
      ("aw_bits_prot", direction, config.nastiXProtBits),
      ("aw_bits_qos", direction, config.nastiXQosBits),
      ("aw_bits_region", direction, config.nastiXRegionBits),
      ("aw_bits_user", direction, config.nastiXUserBits),
      ("w_valid", direction, 1),
      ("w_ready", !direction, 1),
      ("w_bits_data", direction, config.nastiXDataBits),
      ("w_bits_strb", direction, config.nastiWStrobeBits),
      ("w_bits_last", direction, 1),
      ("w_bits_user", direction, config.nastiXUserBits),
      ("r_valid", !direction, 1),
      ("r_ready", direction, 1),
      ("r_bits_id", !direction, config.nastiXIdBits),
      ("r_bits_resp", !direction, config.nastiXRespBits),
      ("r_bits_data", !direction, config.nastiXDataBits),
      ("r_bits_last", !direction, 1),
      ("r_bits_user", !direction, config.nastiXUserBits),
      ("b_valid", !direction, 1),
      ("b_ready", direction, 1),
      ("b_bits_id", !direction, config.nastiXIdBits),
      ("b_bits_resp", !direction, config.nastiXRespBits),
      ("b_bits_user", !direction, config.nastiXUserBits))

    ports.sorted.map { case (name, portdir, width) =>
      makePort(s"${prefix}_${name}", portdir, width) }
  }

  def makeAllPorts(nInputs: Int, nOutputs: Int): ModelType.Ports = {
    val config = new NastiParameters()(params)
    val inPorts = (0 until nInputs).map(
      i => makeAXIPorts(s"io_in_$i", false, config))
    val outPorts = (0 until nOutputs).map(
      i => makeAXIPorts(s"io_out_$i", true, config))
    val globalPorts = Seq(
      makePort("clock", false, 1),
      makePort("reset", false, 1))
    val ports = new ModelType.Ports
    ports.getPort().addAll(toCollection(globalPorts ++ (inPorts ++ outPorts).flatten))
    ports
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

  def makeFileSets(factory: ObjectFactory): FileSets = {
    val fileName = new SpiritFile.Name
    fileName.setValue(s"../verilog/${longName}.v")

    val file = new SpiritFile
    file.getFileType.add(factory.createFileFileType("verilogSource"))
    file.setName(fileName)

    val fileSet = new FileSetType
    fileSet.setName("hdlSource")
    fileSet.getFile.add(file)

    val fileSets = new FileSets
    fileSets.getFileSet().add(fileSet)
    fileSets
  }

  def generateIPXact {
    val nInputs = params(InPorts)
    val nOutputs = params(OutPorts)
    val extMemSize = params(ExtMemSize)
    val regionSize = extMemSize / nOutputs
    val factory = new ObjectFactory

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
    model.setPorts(makeAllPorts(nInputs, nOutputs))

    val componentType = new ComponentType
    componentType.setLibrary("ucb-bar")
    componentType.setName("CraftXBar")
    componentType.setVendor("edu.berkeley.cs")
    componentType.setVersion("1.0")
    componentType.setBusInterfaces(busInterfaces)
    componentType.setAddressSpaces(addressSpaces)
    componentType.setMemoryMaps(memoryMaps)
    componentType.setModel(model)
    componentType.setFileSets(makeFileSets(factory))

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
