package uncore

import Chisel._
import junctions._
import scala.collection.mutable.ArraySeq
import cde.{Parameters, Field}


/** PortedTileLinkNetworks combine a TileLink protocol with a particular physical
  * network implementation.
  *
  * Specifically, they provide mappings between ClientTileLinkIO/ 
  * ManagerTileLinkIO channels and LogicalNetwork ports (i.e. generic
  * TileLinkIO with networking headers). Channels coming into the network have
  * appropriate networking headers appended and outgoing channels have their
  * headers stripped.
  *
  * @constructor base class constructor for Ported TileLink NoC
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
abstract class PortedTileLinkNetwork(
      addrToManagerId: UInt => UInt,
      sharerToClientId: UInt => UInt,
      clientDepths: TileLinkDepths, 
      managerDepths: TileLinkDepths)
    (implicit p: Parameters) extends TLModule()(p) {
  val nClients = tlNClients
  val nManagers = tlNManagers
  val io = new Bundle {
    val clients_cached = Vec(tlNCachingClients, new ClientTileLinkIO).flip
    val clients_uncached = Vec(tlNCachelessClients, new ClientUncachedTileLinkIO).flip
    val managers = Vec(nManagers, new ManagerTileLinkIO).flip
  }

  val clients = (io.clients_cached ++ io.clients_uncached).zipWithIndex.map { 
    case (io, idx) => {
      val qs = Module(new TileLinkEnqueuer(clientDepths))
      io match {
        case c: ClientTileLinkIO => {
          val port = Module(new ClientTileLinkNetworkPort(idx, addrToManagerId))
          port.io.client <> c
          qs.io.client <> port.io.network
          qs.io.manager 
        }
        case u: ClientUncachedTileLinkIO => {
          val port = Module(new ClientUncachedTileLinkNetworkPort(idx, addrToManagerId))
          port.io.client <> u
          qs.io.client <> port.io.network
          qs.io.manager 
        }
      }
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
class PortedTileLinkArbiter(
      sharerToClientId: UInt => UInt = (u: UInt) => u,
      clientDepths: TileLinkDepths = TileLinkDepths(0,0,0,0,0), 
      managerDepths: TileLinkDepths = TileLinkDepths(0,0,0,0,0))
    (implicit p: Parameters)
      extends PortedTileLinkNetwork(u => UInt(0), sharerToClientId, clientDepths, managerDepths)(p)
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
class PortedTileLinkCrossbar(
      addrToManagerId: UInt => UInt = u => UInt(0),
      sharerToClientId: UInt => UInt = u => u,
      clientDepths: TileLinkDepths = TileLinkDepths(0,0,0,0,0), 
      managerDepths: TileLinkDepths = TileLinkDepths(0,0,0,0,0))
    (implicit p: Parameters)
      extends PortedTileLinkNetwork(addrToManagerId, sharerToClientId, clientDepths, managerDepths)(p) {
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

class ClientUncachedTileLinkIORouter(
    nOuter: Int, routeSel: UInt => UInt)(implicit p: Parameters)
    extends TLModule {

  val io = new Bundle {
    val in = (new ClientUncachedTileLinkIO).flip
    val out =  Vec(nOuter, new ClientUncachedTileLinkIO)
  }

  val acq_route = routeSel(io.in.acquire.bits.full_addr())

  io.in.acquire.ready := Bool(false)

  io.out.zipWithIndex.foreach { case (out, i) =>
    out.acquire.valid := io.in.acquire.valid && acq_route(i)
    out.acquire.bits := io.in.acquire.bits
    when (acq_route(i)) { io.in.acquire.ready := out.acquire.ready }
  }

  val gnt_arb = Module(new LockingRRArbiter(
    new Grant, nOuter, tlDataBeats, Some((gnt: Grant) => gnt.hasMultibeatData())))
  gnt_arb.io.in <> io.out.map(_.grant)
  io.in.grant <> gnt_arb.io.out
}

class TileLinkInterconnectIO(val nInner: Int, val nOuter: Int)
    (implicit p: Parameters) extends Bundle {
  val in = Vec(nInner, new ClientUncachedTileLinkIO).flip
  val out = Vec(nOuter, new ClientUncachedTileLinkIO)
}

class ClientUncachedTileLinkIOCrossbar(
    nInner: Int, nOuter: Int, routeSel: UInt => UInt)
    (implicit p: Parameters) extends TLModule {

  val io = new TileLinkInterconnectIO(nInner, nOuter)

  if (nInner == 1) {
    val router = Module(new ClientUncachedTileLinkIORouter(nOuter, routeSel))
    router.io.in <> io.in.head
    io.out <> router.io.out
  } else {
    val routers = List.fill(nInner) {
      Module(new ClientUncachedTileLinkIORouter(nOuter, routeSel)) }
    val arbiters = List.fill(nOuter) {
      Module(new ClientUncachedTileLinkIOArbiter(nInner)) }

    for (i <- 0 until nInner) {
      routers(i).io.in <> io.in(i)
    }

    for (i <- 0 until nOuter) {
      arbiters(i).io.in <> routers.map(r => r.io.out(i))
      io.out(i) <> arbiters(i).io.out
    }
  }
}

abstract class TileLinkInterconnect(implicit p: Parameters) extends TLModule()(p) {
  val nInner: Int
  val nOuter: Int

  lazy val io = new TileLinkInterconnectIO(nInner, nOuter)
}

class TileLinkRecursiveInterconnect(
    val nInner: Int, val nOuter: Int,
    addrmap: AddrMap, base: BigInt)
    (implicit p: Parameters) extends TileLinkInterconnect()(p) {
  var lastEnd = base
  var outInd = 0
  val levelSize = addrmap.size
  val realAddrMap = new ArraySeq[(BigInt, BigInt)](addrmap.size)

  addrmap.zipWithIndex.foreach { case (AddrMapEntry(name, startOpt, region), i) =>
    val start = startOpt.getOrElse(lastEnd)
    val size = region.size

    require(bigIntPow2(size),
      s"Region $name size $size is not a power of 2")
    require(start % size == 0,
      f"Region $name start address 0x$start%x not divisible by 0x$size%x" )
    require(start >= lastEnd,
      f"Region $name start address 0x$start%x before previous region end")

    realAddrMap(i) = (start, size)
    lastEnd = start + size
  }

  val routeSel = (addr: UInt) => {
    Cat(realAddrMap.map { case (start, size) =>
      addr >= UInt(start) && addr < UInt(start + size)
    }.reverse)
  }

  val xbar = Module(new ClientUncachedTileLinkIOCrossbar(nInner, levelSize, routeSel))
  xbar.io.in <> io.in

  addrmap.zip(realAddrMap).zip(xbar.io.out).zipWithIndex.foreach {
    case (((entry, (start, size)), xbarOut), i) => {
      entry.region match {
        case MemSize(_, _) =>
          io.out(outInd) <> xbarOut
          outInd += 1
        case MemSubmap(_, submap) =>
          if (submap.isEmpty) {
            xbarOut.acquire.ready := Bool(false)
            xbarOut.grant.valid := Bool(false)
          } else {
            val subSlaves = submap.countSlaves
            val outputs = io.out.drop(outInd).take(subSlaves)
            val ic = Module(new TileLinkRecursiveInterconnect(1, subSlaves, submap, start))
            ic.io.in.head <> xbarOut
            for ((o, s) <- outputs zip ic.io.out)
              o <> s
            outInd += subSlaves
          }
      }
    }
  }
}

class TileLinkMemoryInterconnect(
    nBanksPerChannel: Int, nChannels: Int)
    (implicit p: Parameters) extends TileLinkInterconnect()(p) {

  val nBanks = nBanksPerChannel * nChannels
  val nInner = nBanks
  val nOuter = nChannels

  def connectChannel(outer: ClientUncachedTileLinkIO, inner: ClientUncachedTileLinkIO) {
    outer <> inner
    outer.acquire.bits.addr_block := inner.acquire.bits.addr_block >> UInt(log2Ceil(nChannels))
  }

  for (i <- 0 until nChannels) {
    /* Bank assignments to channels are strided so that consecutive banks
     * map to different channels. That way, consecutive cache lines also
     * map to different channels */
    val banks = (i until nBanks by nChannels).map(j => io.in(j))

    val channelArb = Module(new ClientUncachedTileLinkIOArbiter(nBanksPerChannel))
    channelArb.io.in <> banks
    connectChannel(io.out(i), channelArb.io.out)
  }
}

/** Allows users to switch between various memory configurations.  Note that
  * this is a dangerous operation: not only does switching the select input to
  * this module violate TileLink, it also causes the memory of the machine to
  * become garbled.  It's expected that select only changes at boot time, as
  * part of the memory controller configuration. */
class TileLinkMemorySelectorIO(val nBanks: Int, val maxMemChannels: Int, nConfigs: Int)
                           (implicit p: Parameters)
                           extends TileLinkInterconnectIO(nBanks, maxMemChannels) {
  val select  = UInt(INPUT, width = log2Up(nConfigs))
  override def cloneType =
    new TileLinkMemorySelectorIO(nBanks, maxMemChannels, nConfigs).asInstanceOf[this.type]
}

class TileLinkMemorySelector(nBanks: Int, maxMemChannels: Int, configs: Seq[Int])
                         (implicit p: Parameters)
                         extends TileLinkInterconnect()(p) {
  val nInner = nBanks
  val nOuter = maxMemChannels
  val nConfigs = configs.size

  override lazy val io = new TileLinkMemorySelectorIO(nBanks, maxMemChannels, nConfigs)

  def muxOnSelect[T <: Data](up: DecoupledIO[T], dn: DecoupledIO[T], active: Bool): Unit = {
    when (active) { dn.bits  := up.bits  }
    when (active) { up.ready := dn.ready }
    when (active) { dn.valid := up.valid }
  }

  def muxOnSelect(up: ClientUncachedTileLinkIO, dn: ClientUncachedTileLinkIO, active: Bool): Unit = {
    muxOnSelect(up.acquire, dn.acquire, active)
    muxOnSelect(dn.grant, up.grant, active)
  }

  def muxOnSelect(up: Vec[ClientUncachedTileLinkIO], dn: Vec[ClientUncachedTileLinkIO], active: Bool) : Unit = {
    for (i <- 0 until up.size)
      muxOnSelect(up(i), dn(i), active)
  }

  /* Disconnects a vector of TileLink ports, which involves setting them to
   * invalid.  Due to Chisel reasons, we need to also set the bits to 0 (since
   * there can't be any unconnected inputs). */
  def disconnectOuter(outer: Vec[ClientUncachedTileLinkIO]) = {
    outer.foreach{ m =>
      m.acquire.valid := Bool(false)
      m.acquire.bits := m.acquire.bits.fromBits(UInt(0))
      m.grant.ready := Bool(false)
    }
  }

  def disconnectInner(inner: Vec[ClientUncachedTileLinkIO]) = {
    inner.foreach { m =>
      m.grant.valid := Bool(false)
      m.grant.bits := m.grant.bits.fromBits(UInt(0))
      m.acquire.ready := Bool(false)
    }
  }

  /* Provides default wires on all our outputs. */
  disconnectOuter(io.out)
  disconnectInner(io.in)

  /* Constructs interconnects for each of the layouts suggested by the
   * configuration and switches between them based on the select input. */
  configs.zipWithIndex.foreach{ case (nChannels, select) =>
    val nBanksPerChannel = nBanks / nChannels
    val ic = Module(new TileLinkMemoryInterconnect(nBanksPerChannel, nChannels))
    disconnectInner(ic.io.out)
    disconnectOuter(ic.io.in)
    muxOnSelect(io.in, ic.io.in, io.select === UInt(select))
    muxOnSelect(ic.io.out, io.out, io.select === UInt(select))
  }
}
