// See LICENSE for license details.

package rocketchip

import Chisel._
import uncore._
import cde.Parameters

/** RocketChipNetworks combine a TileLink protocol with a particular physical
  * network implementation and chip layout.
  *
  * Specifically, they provide mappings between ClientTileLinkIO/ 
  * ManagerTileLinkIO channels and LogicalNetwork ports (i.e. generic
  * TileLinkIO with networking headers). Channels coming into the network have
  * appropriate networking headers appended and outgoing channels have their
  * headers stripped.
  *
  * @constructor base class constructor for Rocket NoC
  * @param addrToManagerId a mapping from a physical address to the network
  *        id of a coherence manager
  * @param sharerToClientId a mapping from the id of a particular coherent
  *        client (as determined by e.g. the directory) and the network id
  *        of that client
  * @param clientDepths the depths of the queue that should be used to buffer
  *        each channel on the client side of the network
  * @param managerDepths the depths of the queue that should be used to buffer
  *        each channel on the manager side of the network
  */
abstract class RocketChipNetwork(
      addrToManagerId: UInt => UInt,
      sharerToClientId: UInt => UInt,
      clientDepths: TileLinkDepths, 
      managerDepths: TileLinkDepths)
    (implicit p: Parameters) extends TLModule()(p) {
  val nClients = tlNClients
  val nManagers = tlNManagers
  val io = new Bundle {
    val clients = Vec(nClients, new ClientTileLinkIO).flip
    val managers = Vec(nManagers, new ManagerTileLinkIO).flip
  }

  val clients = io.clients.zipWithIndex.map { 
    case (c, i) => {
      val port = Module(new ClientTileLinkNetworkPort(i, addrToManagerId))
      val qs = Module(new TileLinkEnqueuer(clientDepths))
      port.io.client <> c
      qs.io.client <> port.io.network
      qs.io.manager 
    }
  }

  val managers = io.managers.zipWithIndex.map {
    case (m, i) => {
      val port = Module(new ManagerTileLinkNetworkPort(i, sharerToClientId))
      val qs = Module(new TileLinkEnqueuer(managerDepths))
      port.io.manager <> m
      port.io.network <> qs.io.manager
      qs.io.client
    }
  }
}

/** A simple arbiter for each channel that also deals with header-based routing.
  * Assumes a single manager agent. */
class RocketChipTileLinkArbiter(
      sharerToClientId: UInt => UInt = (u: UInt) => u,
      clientDepths: TileLinkDepths = TileLinkDepths(0,0,0,0,0), 
      managerDepths: TileLinkDepths = TileLinkDepths(0,0,0,0,0))
    (implicit p: Parameters)
      extends RocketChipNetwork(u => UInt(0), sharerToClientId, clientDepths, managerDepths)(p)
        with TileLinkArbiterLike
        with PassesId {
  val arbN = nClients
  require(nManagers == 1)
  if(arbN > 1) {
    hookupClientSource(clients.map(_.acquire), managers.head.acquire)
    hookupClientSource(clients.map(_.release), managers.head.release)
    hookupFinish(clients.map(_.finish), managers.head.finish)
    hookupManagerSourceWithHeader(clients.map(_.probe), managers.head.probe)
    hookupManagerSourceWithHeader(clients.map(_.grant), managers.head.grant)
  } else {
    managers.head <> clients.head
  }
}

/** Provides a separate physical crossbar for each channel. Assumes multiple manager
  * agents. Managers are assigned to higher physical network port ids than
  * clients, and translations between logical network id and physical crossbar
  * port id are done automatically.
  */
class RocketChipTileLinkCrossbar(
      addrToManagerId: UInt => UInt = u => UInt(0),
      sharerToClientId: UInt => UInt = u => u,
      clientDepths: TileLinkDepths = TileLinkDepths(0,0,0,0,0), 
      managerDepths: TileLinkDepths = TileLinkDepths(0,0,0,0,0))
    (implicit p: Parameters)
      extends RocketChipNetwork(addrToManagerId, sharerToClientId, clientDepths, managerDepths)(p) {
  val n = p(LNEndpoints)
  val count = tlDataBeats
  // Actually instantiate the particular networks required for TileLink
  val acqNet = Module(new BasicCrossbar(n, new Acquire, count, Some((a: PhysicalNetworkIO[Acquire]) => a.payload.hasMultibeatData())))
  val relNet = Module(new BasicCrossbar(n, new Release, count, Some((r: PhysicalNetworkIO[Release]) => r.payload.hasMultibeatData())))
  val prbNet = Module(new BasicCrossbar(n, new Probe))
  val gntNet = Module(new BasicCrossbar(n, new Grant, count, Some((g: PhysicalNetworkIO[Grant]) => g.payload.hasMultibeatData())))
  val ackNet = Module(new BasicCrossbar(n, new Finish))

  // Aliases for the various network IO bundle types
  type PNIO[T <: Data] = DecoupledIO[PhysicalNetworkIO[T]]
  type LNIO[T <: Data] = DecoupledIO[LogicalNetworkIO[T]]
  type FromCrossbar[T <: Data] = PNIO[T] => LNIO[T]
  type ToCrossbar[T <: Data] = LNIO[T] => PNIO[T]

  // Shims for converting between logical network IOs and physical network IOs
  def crossbarToManagerShim[T <: Data](in: PNIO[T]): LNIO[T] = {
    val out = DefaultFromPhysicalShim(in)
    out.bits.header.src := in.bits.header.src - UInt(nManagers)
    out
  }
  def crossbarToClientShim[T <: Data](in: PNIO[T]): LNIO[T] = {
    val out = DefaultFromPhysicalShim(in)
    out.bits.header.dst := in.bits.header.dst - UInt(nManagers)
    out
  }
  def managerToCrossbarShim[T <: Data](in: LNIO[T]): PNIO[T] = {
    val out = DefaultToPhysicalShim(n, in)
    out.bits.header.dst := in.bits.header.dst + UInt(nManagers)
    out
  }
  def clientToCrossbarShim[T <: Data](in: LNIO[T]): PNIO[T] = {
    val out = DefaultToPhysicalShim(n, in)
    out.bits.header.src := in.bits.header.src + UInt(nManagers)
    out
  }

  // Make an individual connection between virtual and physical ports using
  // a particular shim. Also pin the unused Decoupled control signal low.
  def doDecoupledInputHookup[T <: Data](phys_in: PNIO[T], phys_out: PNIO[T], log_io: LNIO[T], shim: ToCrossbar[T]) = {
    val s = shim(log_io)
    phys_in.valid := s.valid
    phys_in.bits := s.bits
    s.ready := phys_in.ready
    phys_out.ready := Bool(false)
  }

  def doDecoupledOutputHookup[T <: Data](phys_in: PNIO[T], phys_out: PNIO[T], log_io: LNIO[T], shim: FromCrossbar[T]) = {
    val s = shim(phys_out)
    log_io.valid := s.valid
    log_io.bits := s.bits
    s.ready := log_io.ready
    phys_in.valid := Bool(false)
  }

  //Hookup all instances of a particular subbundle of TileLink
  def doDecoupledHookups[T <: Data](physIO: BasicCrossbarIO[T], getLogIO: TileLinkIO => LNIO[T]) = {
    physIO.in.head.bits.payload match {
      case c: ClientToManagerChannel => {
        managers.zipWithIndex.map { case (i, id) => 
          doDecoupledOutputHookup(physIO.in(id), physIO.out(id), getLogIO(i), crossbarToManagerShim[T])
        }
        clients.zipWithIndex.map { case (i, id) =>
          doDecoupledInputHookup(physIO.in(id+nManagers), physIO.out(id+nManagers), getLogIO(i), clientToCrossbarShim[T])
        }
      }
      case m: ManagerToClientChannel => {
        managers.zipWithIndex.map { case (i, id) =>
          doDecoupledInputHookup(physIO.in(id), physIO.out(id), getLogIO(i), managerToCrossbarShim[T])
        }
        clients.zipWithIndex.map { case (i, id) =>
          doDecoupledOutputHookup(physIO.in(id+nManagers), physIO.out(id+nManagers), getLogIO(i), crossbarToClientShim[T])
        }
      }
    }
  }

  doDecoupledHookups(acqNet.io, (tl: TileLinkIO) => tl.acquire)
  doDecoupledHookups(relNet.io, (tl: TileLinkIO) => tl.release)
  doDecoupledHookups(prbNet.io, (tl: TileLinkIO) => tl.probe)
  doDecoupledHookups(gntNet.io, (tl: TileLinkIO) => tl.grant)
  doDecoupledHookups(ackNet.io, (tl: TileLinkIO) => tl.finish)
}
