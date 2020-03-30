// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.Location

// These fields control parameters of the five traditional tilelink bus wrappers.
//   They continue to exist for backwards compatiblity reasons but could eventually be retired.
case object SystemBusKey extends Field[SystemBusParams]
case object FrontBusKey extends Field[FrontBusParams]
case object PeripheryBusKey extends Field[PeripheryBusParams]
case object ControlBusKey extends Field[PeripheryBusParams]
case object MemoryBusKey extends Field[MemoryBusParams]

// These objects serve as labels for specified attachment locations
//   from amongst the five traditional tilelink bus wrappers.
//   While they represent some tradtionally popular locations to attach devices,
//   there is no guarantee that they will exist in subsystems with
//   dynamically-configured topologies.
class TLBusWrapperLocation(name: String) extends Location[TLBusWrapper](name)
case object SBUS extends TLBusWrapperLocation("subsystem_sbus")
case object PBUS extends TLBusWrapperLocation("subsystem_pbus")
case object FBUS extends TLBusWrapperLocation("subsystem_fbus")
case object MBUS extends TLBusWrapperLocation("subsystem_mbus")
case object CBUS extends TLBusWrapperLocation("subsystem_cbus")
case object L2   extends TLBusWrapperLocation("subsystem_l2")

// This case class parameterizes the subsystem in terms of the optional clock-crossings
//   which are insertable between some of the five traditional tilelink bus wrappers.
//   They continue to exist for backwards compatiblity reasons but could eventually be retired.
case class SubsystemCrossingParams(
  sbusToCbusXType: ClockCrossingType = NoCrossing,
  cbusToPbusXType: ClockCrossingType = SynchronousCrossing(),
  fbusToSbusXType: ClockCrossingType = SynchronousCrossing()
)

// This case class provides a backwards-compatibility parameterization of a subsystem
//  bus topology that contains the five traditional tilelink bus wrappers.
//  Users desiring a different topology are free to define a similar subclass,
//  or just populate an instance of TLBusWrapperTopology via some other mechanism.

case class JustOneBusTopologyParams(
  sbus: SystemBusParams,
) extends TLBusWrapperTopology(
  instantiations = List((SBUS, sbus)),
  connections = Nil
)

case class HierarchicalBusTopologyParams(
  pbus: PeripheryBusParams,
  fbus: FrontBusParams,
  cbus: PeripheryBusParams,
  xTypes: SubsystemCrossingParams
) extends TLBusWrapperTopology(
  instantiations = List(
    (PBUS, pbus),
    (FBUS, fbus),
    (CBUS, cbus)),
  connections = List(
    (SBUS, CBUS, TLBusWrapperConnection  .crossTo(xTypes.sbusToCbusXType)),
    (CBUS, PBUS, TLBusWrapperConnection  .crossTo(xTypes.cbusToPbusXType)),
    (FBUS, SBUS, TLBusWrapperConnection.crossFrom(xTypes.fbusToSbusXType)))
)

case class CoherentBusTopologyParams(
  sbus: SystemBusParams, // TODO remove this after better width propagation
  mbus: MemoryBusParams,
  l2: BankedL2Params
) extends TLBusWrapperTopology(
  instantiations = (if (l2.nBanks == 0) Nil else List(
    (MBUS, mbus),
    (L2, CoherenceManagerWrapperParams(sbus.blockBytes, sbus.beatBytes, L2.name)(l2.coherenceManager)))),
  connections = (if (l2.nBanks == 0) Nil else List(
    (SBUS, L2,   TLBusWrapperConnection.crossTo(NoCrossing)),
    (L2,   MBUS, TLBusWrapperConnection.crossTo(NoCrossing))
  ))
)
