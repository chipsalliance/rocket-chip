// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

import freechips.rocketchip.diplomacy.{ResourceBindings, ResourceBindingsMap}
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelAddressing
import freechips.rocketchip.diplomaticobjectmodel.model._

sealed trait PortType extends OMEnum
case object SystemPortType extends PortType
case object PeripheralPortType extends PortType
case object MemoryPortType extends PortType
case object FrontPortType extends PortType

sealed trait ProtocolType extends OMEnum
case object AXI4Protocol extends ProtocolType
case object AHBProtocol extends ProtocolType
case object APBProtocol extends ProtocolType
case object TLProtocol extends ProtocolType

sealed trait SubProtocolType extends OMEnum
trait AXI4SubProtocol extends SubProtocolType
trait AHBSubProtocol extends SubProtocolType
trait APBSubProtocol extends SubProtocolType
trait TLSubProtocol extends SubProtocolType

case object AXI4SubProtocol extends AHBSubProtocol
case object AXI4LiteSubProtocol extends AHBSubProtocol
case object AHBLiteSubProtocol extends AHBSubProtocol
case object APBSubProtocol extends APBSubProtocol
case object TL_ULSubProtocol extends TLSubProtocol
case object TL_UHSubProtocol extends TLSubProtocol
case object TL_CSubProtocol extends TLSubProtocol

trait OMProtocol extends OMCompoundType {
  def specification: Option[OMSpecification]
}

trait AMBA extends OMProtocol

case class AXI4(
  specification: Option[OMSpecification],
  _types: Seq[String] = Seq("AXI4", "AMBA",  "OMProtocol")
) extends AMBA

case class AXI4_Lite(
  specification: Option[OMSpecification],
  val _types: Seq[String] = Seq("AXI4_Lite", "AMBA",  "OMProtocol")
) extends AMBA

trait AHB extends AMBA

case class AHB_Lite(
  specification: Option[OMSpecification],
  val _types: Seq[String] = Seq("AHB_Lite", "AMBA",  "OMProtocol")
) extends AMBA

case class APB(
  specification: Option[OMSpecification],
  val _types: Seq[String] = Seq("APB", "AMBA",  "OMProtocol")
) extends AMBA

trait TL extends OMProtocol

case class TL_UL(
  specification: Option[OMSpecification],
  val _types: Seq[String] = Seq("TL_UL", "TL",  "OMProtocol")
) extends TL
case class TL_UH(
  specification: Option[OMSpecification],
  val _types: Seq[String] = Seq("TL_UH", "TL",  "OMProtocol")
) extends TL
case class TL_C(
  specification: Option[OMSpecification],
  val _types: Seq[String] = Seq("TL_C", "TL",  "OMProtocol")
) extends TL

trait OMPort extends OMDevice {
  memoryRegions: Seq[OMMemoryRegion]
  interrupts: Seq[OMInterrupt]
  def signalNamePrefix: String
  def width: Int
  def protocol: OMProtocol
}

trait InboundPort extends OMPort
trait OutboundPort extends OMPort

case class FrontPort(
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt],
  signalNamePrefix: String,
  width: Int,
  protocol: OMProtocol,
  _types: Seq[String] = Seq("FrontPort", "InboundPort", "OMPort", "OMDevice", "OMComponent", "OMCompoundType")
) extends InboundPort

case class MemoryPort(
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt],
  signalNamePrefix: String,
  width: Int,
  protocol: OMProtocol,
  _types: Seq[String] = Seq("MemoryPort", "OutboundPort", "OMPort", "OMDevice", "OMComponent", "OMCompoundType")) extends OutboundPort

case class PeripheralPort(
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt],
  signalNamePrefix: String,
  width: Int,
  protocol: OMProtocol,
  _types: Seq[String] = Seq("PeripheralPort", "OutboundPort", "OMPort", "OMDevice", "OMComponent", "OMCompoundType")) extends OutboundPort

case class SystemPort(
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt],
  signalNamePrefix: String,
  width: Int,
  protocol: OMProtocol,
  _types: Seq[String] = Seq("SystemPort", "OutboundPort", "OMPort", "OMDevice", "OMComponent", "OMCompoundType")) extends OutboundPort

object OMPortMaker {
  val protocolSpecifications = Map[ProtocolType, String](
    AHBProtocol -> "AMBA 3 AHB-Lite Protocol",
    AXI4Protocol -> "AMBA 3 AXI4-Lite Protocol",
    APBProtocol -> "AMBA 3 APB Protocol",
    TLProtocol -> "Tile Link specification"
  )

  val protocolSpecificationVersions = Map[ProtocolType, String](
    AHBProtocol -> "1.0",
    AXI4Protocol -> "1.0",
    APBProtocol -> "1.0",
    TLProtocol -> "1.8"
  )

  def specVersion(protocol: ProtocolType, version: String): Option[OMSpecification] = Some(OMSpecification(protocolSpecifications(protocol), version))

  val portNames = Map[PortType, String](
    SystemPortType -> "System Port",
    PeripheralPortType -> "Peripheral Port",
    MemoryPortType -> "Memory Port",
    FrontPortType -> "Front Port"
  )

  def port(
    resourceBindings: Option[ResourceBindings],
    signalNamePrefix: String,
    portType: PortType,
    protocol: ProtocolType,
    subProtocol: SubProtocolType,
    version: String,
    beatBytes: Int): OMPort = {
    val documentationName = portNames(portType)

    val omProtocol = (protocol, subProtocol) match {
      case (AXI4Protocol, AXI4SubProtocol) => AXI4(specification = specVersion(protocol, version))
      case (AXI4Protocol, AXI4LiteSubProtocol) => AXI4_Lite(specification = specVersion(protocol, version))
      case (AHBProtocol, AHBLiteSubProtocol) => AHB_Lite(specification = specVersion(protocol, version))
      case (APBProtocol, APBSubProtocol) => APB(specification = specVersion(protocol, version))
      case (TLProtocol, TL_UHSubProtocol) => TL_UH(specification = specVersion(protocol, version))
      case (TLProtocol, TL_ULSubProtocol) => TL_UL(specification = specVersion(protocol, version))
      case (TLProtocol, TL_CSubProtocol) => TL_C(specification = specVersion(protocol, version))
      case _ => throw new IllegalArgumentException(s"protocol $protocol, subProtocol $subProtocol")
    }

    resourceBindings match {
      case Some(rb) =>
        val memRegions = DiplomaticObjectModelAddressing.getOMPortMemoryRegions(name = documentationName, rb)
        portType match {
          case SystemPortType => SystemPort(memoryRegions = memRegions, interrupts = Nil, signalNamePrefix = signalNamePrefix,
            width = beatBytes * 8, protocol = omProtocol)
          case PeripheralPortType => PeripheralPort(memoryRegions = memRegions, interrupts = Nil, signalNamePrefix = signalNamePrefix,
            width = beatBytes * 8, protocol = omProtocol)
          case MemoryPortType => MemoryPort(memoryRegions = memRegions, interrupts = Nil, signalNamePrefix = signalNamePrefix,
            width = beatBytes * 8, protocol = omProtocol)
          case FrontPortType => throw new IllegalArgumentException
          case _ => throw new IllegalArgumentException
        }
      case None => {
        portType match {
          case FrontPortType => FrontPort(memoryRegions = Nil, interrupts = Nil,
            signalNamePrefix = signalNamePrefix, width = beatBytes * 8, protocol = omProtocol)
          case _ => throw new IllegalArgumentException
        }
      }
    }
  }
}


