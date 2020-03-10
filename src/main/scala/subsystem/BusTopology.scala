// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.Location

// These fields control parameters of the five traditional tilelink bus wrappers
case object SystemBusKey extends Field[SystemBusParams]
case object FrontBusKey extends Field[FrontBusParams]
case object PeripheryBusKey extends Field[PeripheryBusParams]
case object ControlBusKey extends Field[PeripheryBusParams]
case object MemoryBusKey extends Field[MemoryBusParams]

// These object serve as labels for specified attachement targets
//   from amongst the five traditional tilelink bus wrappers
class TLBusWrapperLocation(name: String) extends Location[TLBusWrapper](name)
case object SBUS extends TLBusWrapperLocation("subsystem_sbus")
case object PBUS extends TLBusWrapperLocation("subsystem_pbus")
case object FBUS extends TLBusWrapperLocation("subsystem_fbus")
case object MBUS extends TLBusWrapperLocation("subsystem_mbus")
case object CBUS extends TLBusWrapperLocation("subsystem_cbus")
case object L2   extends TLBusWrapperLocation("subsystem_l2")

// This case class parameterizes the subsystem in terms of the optional clock-crossings
//   which are insertable between some of the five traditional tilelink bus wrappers
case class SubsystemCrossingParams(
  sbusToCbusXType: ClockCrossingType = NoCrossing,
  cbusToPbusXType: ClockCrossingType = SynchronousCrossing(),
  fbusToSbusXType: ClockCrossingType = SynchronousCrossing()
)

// This case class provides a backwards-compatibility parameterization of a subsystem
//  bus topology that contains thfive traditional tilelink bus wrappers
case class HierarchicalBusTopologyParams(
  sbus: SystemBusParams,
  pbus: PeripheryBusParams,
  fbus: FrontBusParams,
  mbus: MemoryBusParams,
  cbus: PeripheryBusParams,
  l2: BankedL2Params,
  xTypes: SubsystemCrossingParams
) extends TLBusWrapperTopology(
  instantiations = List(
    (SBUS, sbus),
    (PBUS, pbus),
    (FBUS, fbus),
    (CBUS, cbus),
    (MBUS, mbus),
    (L2, CoherenceManagerWrapperParams(sbus.blockBytes, sbus.beatBytes, L2.name)(l2.coherenceManager))),
  connections = List(
    (SBUS, CBUS, TLBusWrapperCrossToConnection  (xTypes.sbusToCbusXType)()),
    (CBUS, PBUS, TLBusWrapperCrossToConnection  (xTypes.cbusToPbusXType)()),
    (SBUS, FBUS, TLBusWrapperCrossFromConnection(xTypes.fbusToSbusXType)())) ++
    (if (l2.nBanks == 0) Nil else List( // TODO this should be two topologies once val mbus in subsystem is truly optional
      (SBUS, L2,   TLBusWrapperCrossToConnection  (NoCrossing)()),
      (L2,   MBUS, TLBusWrapperCrossToConnection  (NoCrossing)
        (inject = { implicit p => BankBinder(p(CacheBlockBytes) * (l2.nBanks-1)) }))
    ))
)
