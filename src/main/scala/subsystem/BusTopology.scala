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
case object SBUS extends TLBusWrapperLocation("sbus")
case object PBUS extends TLBusWrapperLocation("pbus")
case object FBUS extends TLBusWrapperLocation("fbus")
case object MBUS extends TLBusWrapperLocation("mbus")
case object CBUS extends TLBusWrapperLocation("cbus")
case object COH  extends TLBusWrapperLocation("coh")
case class CSBUS(clusterId: Int) extends TLBusWrapperLocation(s"csbus$clusterId")
case class CMBUS(clusterId: Int) extends TLBusWrapperLocation(s"cmbus$clusterId")
case class CCBUS(clusterId: Int) extends TLBusWrapperLocation(s"ccbus$clusterId")
case class CCOH (clusterId: Int) extends TLBusWrapperLocation(s"ccoh$clusterId")

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
  mbus: MemoryBusParams,
  coherence: BankedCoherenceParams,
  sbusToMbusXType: ClockCrossingType = NoCrossing,
  driveMBusClockFromSBus: Boolean = true
) extends TLBusWrapperTopology(
  instantiations = (if (coherence.nBanks == 0) Nil else List(
    (MBUS, mbus),
    (COH, CoherenceManagerWrapperParams(mbus.blockBytes, mbus.beatBytes, coherence.nBanks, COH.name)(coherence.coherenceManager)))),
  connections = if (coherence.nBanks == 0) Nil else List(
    (SBUS, COH,   TLBusWrapperConnection(driveClockFromMaster = Some(true), nodeBinding = BIND_STAR)()),
    (COH,  MBUS,  TLBusWrapperConnection.crossTo(
      xType = sbusToMbusXType,
      driveClockFromMaster = if (driveMBusClockFromSBus) Some(true) else None,
      nodeBinding = BIND_QUERY))
  )
)

case class ClusterBusTopologyParams(
  clusterId: Int,
  csbus: SystemBusParams,
  ccbus: PeripheryBusParams,
  coherence: BankedCoherenceParams
) extends TLBusWrapperTopology(
  instantiations = List(
    (CSBUS(clusterId), csbus),
    (CCBUS(clusterId), ccbus)) ++ (if (coherence.nBanks == 0) Nil else List(
    (CMBUS(clusterId), csbus),
    (CCOH (clusterId), CoherenceManagerWrapperParams(csbus.blockBytes, csbus.beatBytes, coherence.nBanks, CCOH(clusterId).name)(coherence.coherenceManager)))),
  connections = if (coherence.nBanks == 0) Nil else List(
    (CSBUS(clusterId), CCOH (clusterId), TLBusWrapperConnection(driveClockFromMaster = Some(true), nodeBinding = BIND_STAR)()),
    (CCOH (clusterId), CMBUS(clusterId), TLBusWrapperConnection.crossTo(
      xType = NoCrossing,
      driveClockFromMaster = Some(true),
      nodeBinding = BIND_QUERY))
  )
)

