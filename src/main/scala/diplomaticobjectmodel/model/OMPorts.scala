// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMProtocol(
  specification: Option[OMSpecification]
) extends OMCompoundType

trait AMBA extends OMProtocol
trait AXI4 extends AMBA
trait AXI4_Lite extends AMBA
trait AHB extends AMBA
trait AHB_Lite extends AMBA
trait APB extends AMBA

trait TL extends OMProtocol
trait TL_UL extends TL
trait TL_UH extends TL
trait TL_C extends TL

case class OMPort(
  memoryRegions: List[OMMemoryRegion],
  interrupts: List[OMInterrupt],
  width: Int,
  protocol: OMProtocol
) extends OMDevice

trait InboundPort extends OMPort
trait OutboundPort extends OMPort

trait FrontPort      extends InboundPort
trait MemoryPort     extends OutboundPort
trait PeripheralPort extends OutboundPort
trait SystemPort     extends OutboundPort

