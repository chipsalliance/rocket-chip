// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import org.chipsalliance.cde.config.Field
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

/** Parameterizes the subsystem in terms of optional clock-crossings
  *   that are insertable between some of the five traditional tilelink bus wrappers.
  *   This class exists for backwards compatiblity reasons but could eventually be retired
  *   in favor of manually filling in crossing types within each custom TLBusWrapperTopology.
  */
case class SubsystemCrossingParams(
  sbusToCbusXType: ClockCrossingType = NoCrossing,
  cbusToPbusXType: ClockCrossingType = SynchronousCrossing(),
  fbusToSbusXType: ClockCrossingType = SynchronousCrossing()
)

/** Keys to parameterize the most common crossings between the five traditional TL buses. Used to populated
  * [[SubsystemCrossingParams]].
  */
case object SbusToMbusXTypeKey extends Field[ClockCrossingType](NoCrossing)
case object SbusToCbusXTypeKey extends Field[ClockCrossingType](NoCrossing)
case object CbusToPbusXTypeKey extends Field[ClockCrossingType](SynchronousCrossing())
case object FbusToSbusXTypeKey extends Field[ClockCrossingType](SynchronousCrossing())


/** By default, ClockSources for the five traditional TL buses are provided from
  * connections that originate at the SBUS. Setting this to false removes these connections and permits
  * supplying diplomatic clocks to each bus independently.
  */
case object DriveClocksFromSBus extends Field[Boolean](true)

// Taken together these case classes provide a backwards-compatibility parameterization
//  of a bus topology that contains the five traditional tilelink bus wrappers.
//  Users desiring a different topology are free to define a similar subclass,
//  or just populate an instance of TLBusWrapperTopology via some other mechanism.

/** Parameterization of a topology containing a single bus named "subsystem_sbus". */
case class JustOneBusTopologyParams(
  sbus: SystemBusParams,
) extends TLBusWrapperTopology(
  instantiations = List((SBUS, sbus)),
  connections = Nil
)

/** Parameterization of a topology containing three additional, optional buses for attaching MMIO devices. */
case class HierarchicalBusTopologyParams(
  pbus: PeripheryBusParams,
  fbus: FrontBusParams,
  cbus: PeripheryBusParams,
  xTypes: SubsystemCrossingParams,
  driveClocksFromSBus: Boolean = true
) extends TLBusWrapperTopology(
  instantiations = List(
    (PBUS, pbus),
    (FBUS, fbus),
    (CBUS, cbus)),
  connections = List(
    (SBUS, CBUS, TLBusWrapperConnection  .crossTo(xTypes.sbusToCbusXType, if (driveClocksFromSBus) Some(true) else None)),
    (CBUS, PBUS, TLBusWrapperConnection  .crossTo(xTypes.cbusToPbusXType, if (driveClocksFromSBus) Some(true) else None)),
    (FBUS, SBUS, TLBusWrapperConnection.crossFrom(xTypes.fbusToSbusXType, if (driveClocksFromSBus) Some(false) else None)))
)

/** Parameterization of a topology containing a banked coherence manager and a bus for attaching memory devices. */
case class CoherentBusTopologyParams(
  sbus: SystemBusParams, // TODO remove this after better width propagation
  mbus: MemoryBusParams,
  l2: BankedL2Params,
  sbusToMbusXType: ClockCrossingType = NoCrossing,
  driveMBusClockFromSBus: Boolean = true
) extends TLBusWrapperTopology(
  instantiations = (if (l2.nBanks == 0) Nil else List(
    (MBUS, mbus),
    (L2, CoherenceManagerWrapperParams(mbus.blockBytes, mbus.beatBytes, l2.nBanks, L2.name, sbus.dtsFrequency)(l2.coherenceManager)))),
  connections = if (l2.nBanks == 0) Nil else List(
    (SBUS, L2,   TLBusWrapperConnection(driveClockFromMaster = Some(true), nodeBinding = BIND_STAR)()),
    (L2,  MBUS,  TLBusWrapperConnection.crossTo(
      xType = sbusToMbusXType,
      driveClockFromMaster = if (driveMBusClockFromSBus) Some(true) else None,
      nodeBinding = BIND_QUERY))
  )
)
