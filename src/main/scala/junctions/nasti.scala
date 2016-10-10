/// See LICENSE for license details.

package junctions
import Chisel._
import scala.math.max
import scala.collection.mutable.ArraySeq
import util._
import cde.{Parameters, Field}

case object NastiKey extends Field[NastiParameters]

case class NastiParameters(dataBits: Int, addrBits: Int, idBits: Int)

trait HasNastiParameters {
  implicit val p: Parameters
  val nastiExternal = p(NastiKey)
  val nastiXDataBits = nastiExternal.dataBits
  val nastiWStrobeBits = nastiXDataBits / 8
  val nastiXAddrBits = nastiExternal.addrBits
  val nastiWIdBits = nastiExternal.idBits
  val nastiRIdBits = nastiExternal.idBits
  val nastiXIdBits = max(nastiWIdBits, nastiRIdBits)
  val nastiXUserBits = 1
  val nastiAWUserBits = nastiXUserBits
  val nastiWUserBits = nastiXUserBits
  val nastiBUserBits = nastiXUserBits
  val nastiARUserBits = nastiXUserBits
  val nastiRUserBits = nastiXUserBits
  val nastiXLenBits = 8
  val nastiXSizeBits = 3
  val nastiXBurstBits = 2
  val nastiXCacheBits = 4
  val nastiXProtBits = 3
  val nastiXQosBits = 4
  val nastiXRegionBits = 4
  val nastiXRespBits = 2

  def bytesToXSize(bytes: UInt) = MuxLookup(bytes, UInt("b111"), Array(
    UInt(1) -> UInt(0),
    UInt(2) -> UInt(1),
    UInt(4) -> UInt(2),
    UInt(8) -> UInt(3),
    UInt(16) -> UInt(4),
    UInt(32) -> UInt(5),
    UInt(64) -> UInt(6),
    UInt(128) -> UInt(7)))
}

abstract class NastiModule(implicit val p: Parameters) extends Module
  with HasNastiParameters
abstract class NastiBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasNastiParameters

abstract class NastiChannel(implicit p: Parameters) extends NastiBundle()(p)
abstract class NastiMasterToSlaveChannel(implicit p: Parameters) extends NastiChannel()(p)
abstract class NastiSlaveToMasterChannel(implicit p: Parameters) extends NastiChannel()(p)

trait HasNastiMetadata extends HasNastiParameters {
  val addr   = UInt(width = nastiXAddrBits)
  val len    = UInt(width = nastiXLenBits)
  val size   = UInt(width = nastiXSizeBits)
  val burst  = UInt(width = nastiXBurstBits)
  val lock   = Bool()
  val cache  = UInt(width = nastiXCacheBits)
  val prot   = UInt(width = nastiXProtBits)
  val qos    = UInt(width = nastiXQosBits)
  val region = UInt(width = nastiXRegionBits)
}

trait HasNastiData extends HasNastiParameters {
  val data = UInt(width = nastiXDataBits)
  val last = Bool()
}

class NastiReadIO(implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val ar = Decoupled(new NastiReadAddressChannel)
  val r  = Decoupled(new NastiReadDataChannel).flip
}

class NastiWriteIO(implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val aw = Decoupled(new NastiWriteAddressChannel)
  val w  = Decoupled(new NastiWriteDataChannel)
  val b  = Decoupled(new NastiWriteResponseChannel).flip
}

class NastiIO(implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val aw = Decoupled(new NastiWriteAddressChannel)
  val w  = Decoupled(new NastiWriteDataChannel)
  val b  = Decoupled(new NastiWriteResponseChannel).flip
  val ar = Decoupled(new NastiReadAddressChannel)
  val r  = Decoupled(new NastiReadDataChannel).flip
}

class NastiAddressChannel(implicit p: Parameters) extends NastiMasterToSlaveChannel()(p)
    with HasNastiMetadata

class NastiResponseChannel(implicit p: Parameters) extends NastiSlaveToMasterChannel()(p) {
  val resp = UInt(width = nastiXRespBits)
}

class NastiWriteAddressChannel(implicit p: Parameters) extends NastiAddressChannel()(p) {
  val id   = UInt(width = nastiWIdBits)
  val user = UInt(width = nastiAWUserBits)
}

class NastiWriteDataChannel(implicit p: Parameters) extends NastiMasterToSlaveChannel()(p)
    with HasNastiData {
  val id   = UInt(width = nastiWIdBits)
  val strb = UInt(width = nastiWStrobeBits)
  val user = UInt(width = nastiWUserBits)
}

class NastiWriteResponseChannel(implicit p: Parameters) extends NastiResponseChannel()(p) {
  val id   = UInt(width = nastiWIdBits)
  val user = UInt(width = nastiBUserBits)
}

class NastiReadAddressChannel(implicit p: Parameters) extends NastiAddressChannel()(p) {
  val id   = UInt(width = nastiRIdBits)
  val user = UInt(width = nastiARUserBits)
}

class NastiReadDataChannel(implicit p: Parameters) extends NastiResponseChannel()(p)
    with HasNastiData {
  val id   = UInt(width = nastiRIdBits)
  val user = UInt(width = nastiRUserBits)
}

object NastiConstants {
  val BURST_FIXED = UInt("b00")
  val BURST_INCR  = UInt("b01")
  val BURST_WRAP  = UInt("b10")

  val RESP_OKAY = UInt("b00")
  val RESP_EXOKAY = UInt("b01")
  val RESP_SLVERR = UInt("b10")
  val RESP_DECERR = UInt("b11")

  val CACHE_DEVICE_NOBUF = UInt("b0000")
  val CACHE_DEVICE_BUF   = UInt("b0001")
  val CACHE_NORMAL_NOCACHE_NOBUF = UInt("b0010")
  val CACHE_NORMAL_NOCACHE_BUF   = UInt("b0011")

  def AXPROT(instruction: Bool, nonsecure: Bool, privileged: Bool): UInt =
    Cat(instruction, nonsecure, privileged)

  def AXPROT(instruction: Boolean, nonsecure: Boolean, privileged: Boolean): UInt =
    AXPROT(Bool(instruction), Bool(nonsecure), Bool(privileged))
}

import NastiConstants._

object NastiWriteAddressChannel {
  def apply(id: UInt, addr: UInt, size: UInt,
      len: UInt = UInt(0), burst: UInt = BURST_INCR)
      (implicit p: Parameters) = {
    val aw = Wire(new NastiWriteAddressChannel)
    aw.id := id
    aw.addr := addr
    aw.len := len
    aw.size := size
    aw.burst := burst
    aw.lock := Bool(false)
    aw.cache := CACHE_DEVICE_NOBUF
    aw.prot := AXPROT(false, false, false)
    aw.qos := UInt("b0000")
    aw.region := UInt("b0000")
    aw.user := UInt(0)
    aw
  }
}

object NastiReadAddressChannel {
  def apply(id: UInt, addr: UInt, size: UInt,
      len: UInt = UInt(0), burst: UInt = BURST_INCR)
      (implicit p: Parameters) = {
    val ar = Wire(new NastiReadAddressChannel)
    ar.id := id
    ar.addr := addr
    ar.len := len
    ar.size := size
    ar.burst := burst
    ar.lock := Bool(false)
    ar.cache := CACHE_DEVICE_NOBUF
    ar.prot := AXPROT(false, false, false)
    ar.qos := UInt(0)
    ar.region := UInt(0)
    ar.user := UInt(0)
    ar
  }
}

object NastiWriteDataChannel {
  def apply(data: UInt, strb: Option[UInt] = None,
            last: Bool = Bool(true), id: UInt = UInt(0))
           (implicit p: Parameters): NastiWriteDataChannel = {
    val w = Wire(new NastiWriteDataChannel)
    w.strb := strb.getOrElse(Fill(w.nastiWStrobeBits, UInt(1, 1)))
    w.data := data
    w.last := last
    w.id   := id
    w.user := UInt(0)
    w
  }
}

object NastiReadDataChannel {
  def apply(id: UInt, data: UInt, last: Bool = Bool(true), resp: UInt = UInt(0))(
      implicit p: Parameters) = {
    val r = Wire(new NastiReadDataChannel)
    r.id := id
    r.data := data
    r.last := last
    r.resp := resp
    r.user := UInt(0)
    r
  }
}

object NastiWriteResponseChannel {
  def apply(id: UInt, resp: UInt = UInt(0))(implicit p: Parameters) = {
    val b = Wire(new NastiWriteResponseChannel)
    b.id := id
    b.resp := resp
    b.user := UInt(0)
    b
  }
}

class NastiArbiterIO(arbN: Int)(implicit p: Parameters) extends Bundle {
  val master = Vec(arbN, new NastiIO).flip
  val slave = new NastiIO
  override def cloneType =
    new NastiArbiterIO(arbN).asInstanceOf[this.type]
}

/** Arbitrate among arbN masters requesting to a single slave */
class NastiArbiter(val arbN: Int)(implicit p: Parameters) extends NastiModule {
  val io = new NastiArbiterIO(arbN)

  if (arbN > 1) {
    val arbIdBits = log2Up(arbN)

    val ar_arb = Module(new RRArbiter(new NastiReadAddressChannel, arbN))
    val aw_arb = Module(new RRArbiter(new NastiWriteAddressChannel, arbN))

    val slave_r_arb_id = io.slave.r.bits.id(arbIdBits - 1, 0)
    val slave_b_arb_id = io.slave.b.bits.id(arbIdBits - 1, 0)

    val w_chosen = Reg(UInt(width = arbIdBits))
    val w_done = Reg(init = Bool(true))

    when (aw_arb.io.out.fire()) {
      w_chosen := aw_arb.io.chosen
      w_done := Bool(false)
    }

    when (io.slave.w.fire() && io.slave.w.bits.last) {
      w_done := Bool(true)
    }

    for (i <- 0 until arbN) {
      val m_ar = io.master(i).ar
      val m_aw = io.master(i).aw
      val m_r = io.master(i).r
      val m_b = io.master(i).b
      val a_ar = ar_arb.io.in(i)
      val a_aw = aw_arb.io.in(i)
      val m_w = io.master(i).w

      a_ar <> m_ar
      a_ar.bits.id := Cat(m_ar.bits.id, UInt(i, arbIdBits))

      a_aw <> m_aw
      a_aw.bits.id := Cat(m_aw.bits.id, UInt(i, arbIdBits))

      m_r.valid := io.slave.r.valid && slave_r_arb_id === UInt(i)
      m_r.bits := io.slave.r.bits
      m_r.bits.id := io.slave.r.bits.id >> UInt(arbIdBits)

      m_b.valid := io.slave.b.valid && slave_b_arb_id === UInt(i)
      m_b.bits := io.slave.b.bits
      m_b.bits.id := io.slave.b.bits.id >> UInt(arbIdBits)

      m_w.ready := io.slave.w.ready && w_chosen === UInt(i) && !w_done
    }

    io.slave.r.ready := io.master(slave_r_arb_id).r.ready
    io.slave.b.ready := io.master(slave_b_arb_id).b.ready

    io.slave.w.bits := io.master(w_chosen).w.bits
    io.slave.w.valid := io.master(w_chosen).w.valid && !w_done

    io.slave.ar <> ar_arb.io.out

    io.slave.aw.bits <> aw_arb.io.out.bits
    io.slave.aw.valid := aw_arb.io.out.valid && w_done
    aw_arb.io.out.ready := io.slave.aw.ready && w_done

  } else { io.slave <> io.master.head }
}

/** A slave that send decode error for every request it receives */
class NastiErrorSlave(implicit p: Parameters) extends NastiModule {
  val io = (new NastiIO).flip

  when (io.ar.fire()) { printf("Invalid read address %x\n", io.ar.bits.addr) }
  when (io.aw.fire()) { printf("Invalid write address %x\n", io.aw.bits.addr) }

  val r_queue = Module(new Queue(new NastiReadAddressChannel, 1))
  r_queue.io.enq <> io.ar

  val responding = Reg(init = Bool(false))
  val beats_left = Reg(init = UInt(0, nastiXLenBits))

  when (!responding && r_queue.io.deq.valid) {
    responding := Bool(true)
    beats_left := r_queue.io.deq.bits.len
  }

  io.r.valid := r_queue.io.deq.valid && responding
  io.r.bits.id := r_queue.io.deq.bits.id
  io.r.bits.data := UInt(0)
  io.r.bits.resp := RESP_DECERR
  io.r.bits.last := beats_left === UInt(0)

  r_queue.io.deq.ready := io.r.fire() && io.r.bits.last

  when (io.r.fire()) {
    when (beats_left === UInt(0)) {
      responding := Bool(false)
    } .otherwise {
      beats_left := beats_left - UInt(1)
    }
  }

  val draining = Reg(init = Bool(false))
  io.w.ready := draining

  when (io.aw.fire()) { draining := Bool(true) }
  when (io.w.fire() && io.w.bits.last) { draining := Bool(false) }

  val b_queue = Module(new Queue(UInt(width = nastiWIdBits), 1))
  b_queue.io.enq.valid := io.aw.valid && !draining
  b_queue.io.enq.bits := io.aw.bits.id
  io.aw.ready := b_queue.io.enq.ready && !draining
  io.b.valid := b_queue.io.deq.valid && !draining
  io.b.bits.id := b_queue.io.deq.bits
  io.b.bits.resp := RESP_DECERR
  b_queue.io.deq.ready := io.b.ready && !draining
}

class NastiRouterIO(nSlaves: Int)(implicit p: Parameters) extends Bundle {
  val master = (new NastiIO).flip
  val slave = Vec(nSlaves, new NastiIO)
  override def cloneType =
    new NastiRouterIO(nSlaves).asInstanceOf[this.type]
}

/** Take a single Nasti master and route its requests to various slaves
 *  @param nSlaves the number of slaves
 *  @param routeSel a function which takes an address and produces
 *  a one-hot encoded selection of the slave to write to */
class NastiRouter(nSlaves: Int, routeSel: UInt => UInt)(implicit p: Parameters)
    extends NastiModule {

  val io = new NastiRouterIO(nSlaves)

  val ar_route = routeSel(io.master.ar.bits.addr)
  val aw_route = routeSel(io.master.aw.bits.addr)

  var ar_ready = Bool(false)
  var aw_ready = Bool(false)
  var w_ready = Bool(false)

  io.slave.zipWithIndex.foreach { case (s, i) =>
    s.ar.valid := io.master.ar.valid && ar_route(i)
    s.ar.bits := io.master.ar.bits
    ar_ready = ar_ready || (s.ar.ready && ar_route(i))

    s.aw.valid := io.master.aw.valid && aw_route(i)
    s.aw.bits := io.master.aw.bits
    aw_ready = aw_ready || (s.aw.ready && aw_route(i))

    val chosen = Reg(init = Bool(false))
    when (s.w.fire() && s.w.bits.last) { chosen := Bool(false) }
    when (s.aw.fire()) { chosen := Bool(true) }

    s.w.valid := io.master.w.valid && chosen
    s.w.bits := io.master.w.bits
    w_ready = w_ready || (s.w.ready && chosen)
  }

  val r_invalid = !ar_route.orR
  val w_invalid = !aw_route.orR

  val err_slave = Module(new NastiErrorSlave)
  err_slave.io.ar.valid := r_invalid && io.master.ar.valid
  err_slave.io.ar.bits := io.master.ar.bits
  err_slave.io.aw.valid := w_invalid && io.master.aw.valid
  err_slave.io.aw.bits := io.master.aw.bits
  err_slave.io.w.valid := io.master.w.valid
  err_slave.io.w.bits := io.master.w.bits

  io.master.ar.ready := ar_ready || (r_invalid && err_slave.io.ar.ready)
  io.master.aw.ready := aw_ready || (w_invalid && err_slave.io.aw.ready)
  io.master.w.ready := w_ready || err_slave.io.w.ready

  val b_arb = Module(new RRArbiter(new NastiWriteResponseChannel, nSlaves + 1))
  val r_arb = Module(new HellaPeekingArbiter(
    new NastiReadDataChannel, nSlaves + 1,
    // we can unlock if it's the last beat
    (r: NastiReadDataChannel) => r.last))

  for (i <- 0 until nSlaves) {
    b_arb.io.in(i) <> io.slave(i).b
    r_arb.io.in(i) <> io.slave(i).r
  }

  b_arb.io.in(nSlaves) <> err_slave.io.b
  r_arb.io.in(nSlaves) <> err_slave.io.r

  io.master.b <> b_arb.io.out
  io.master.r <> r_arb.io.out
}

/** Crossbar between multiple Nasti masters and slaves
 *  @param nMasters the number of Nasti masters
 *  @param nSlaves the number of Nasti slaves
 *  @param routeSel a function selecting the slave to route an address to */
class NastiCrossbar(nMasters: Int, nSlaves: Int, routeSel: UInt => UInt)
                   (implicit p: Parameters) extends NastiModule {
  val io = new Bundle {
    val masters = Vec(nMasters, new NastiIO).flip
    val slaves = Vec(nSlaves, new NastiIO)
  }

  if (nMasters == 1) {
    val router = Module(new NastiRouter(nSlaves, routeSel))
    router.io.master <> io.masters.head
    io.slaves <> router.io.slave
  } else {
    val routers = Vec.fill(nMasters) { Module(new NastiRouter(nSlaves, routeSel)).io }
    val arbiters = Vec.fill(nSlaves) { Module(new NastiArbiter(nMasters)).io }

    for (i <- 0 until nMasters) {
      routers(i).master <> io.masters(i)
    }

    for (i <- 0 until nSlaves) {
      arbiters(i).master <> Vec(routers.map(r => r.slave(i)))
      io.slaves(i) <> arbiters(i).slave
    }
  }
}

class NastiInterconnectIO(val nMasters: Int, val nSlaves: Int)
                         (implicit p: Parameters) extends Bundle {
  /* This is a bit confusing. The interconnect is a slave to the masters and
   * a master to the slaves. Hence why the declarations seem to be backwards. */
  val masters = Vec(nMasters, new NastiIO).flip
  val slaves = Vec(nSlaves, new NastiIO)
  override def cloneType =
    new NastiInterconnectIO(nMasters, nSlaves).asInstanceOf[this.type]
}

abstract class NastiInterconnect(implicit p: Parameters) extends NastiModule()(p) {
  val nMasters: Int
  val nSlaves: Int

  lazy val io = new NastiInterconnectIO(nMasters, nSlaves)
}

class NastiRecursiveInterconnect(val nMasters: Int, addrMap: AddrMap)
    (implicit p: Parameters) extends NastiInterconnect()(p) {
  def port(name: String) = io.slaves(addrMap.port(name))
  val nSlaves = addrMap.numSlaves
  val routeSel = (addr: UInt) =>
    Cat(addrMap.entries.map(e => addrMap(e.name).containsAddress(addr)).reverse)

  val xbar = Module(new NastiCrossbar(nMasters, addrMap.length, routeSel))
  xbar.io.masters <> io.masters

  io.slaves <> addrMap.entries.zip(xbar.io.slaves).flatMap {
    case (entry, xbarSlave) => {
      entry.region match {
        case submap: AddrMap if submap.entries.isEmpty =>
          val err_slave = Module(new NastiErrorSlave)
          err_slave.io <> xbarSlave
          None
        case submap: AddrMap =>
          val ic = Module(new NastiRecursiveInterconnect(1, submap))
          ic.io.masters.head <> xbarSlave
          ic.io.slaves
        case r: MemRange =>
          Some(xbarSlave)
      }
    }
  }
}

object AsyncNastiCrossing {
  // takes from_source from the 'from' clock domain to the 'to' clock domain
  def apply(from_clock: Clock, from_reset: Bool, from_source: NastiIO, to_clock: Clock, to_reset: Bool, depth: Int = 8, sync: Int = 3) = {
    val to_sink = Wire(new NastiIO()(from_source.p))

    to_sink.aw <> AsyncDecoupledCrossing(from_clock, from_reset, from_source.aw, to_clock, to_reset, depth, sync)
    to_sink.ar <> AsyncDecoupledCrossing(from_clock, from_reset, from_source.ar, to_clock, to_reset, depth, sync)
    to_sink.w  <> AsyncDecoupledCrossing(from_clock, from_reset, from_source.w,  to_clock, to_reset, depth, sync)
    from_source.b <> AsyncDecoupledCrossing(to_clock, to_reset, to_sink.b, from_clock, from_reset, depth, sync)
    from_source.r <> AsyncDecoupledCrossing(to_clock, to_reset, to_sink.r, from_clock, from_reset, depth, sync)

    to_sink // is now to_source
  }
}

object AsyncNastiTo {
  // takes source from your clock domain and puts it into the 'to' clock domain
  def apply(to_clock: Clock, to_reset: Bool, source: NastiIO, depth: Int = 8, sync: Int = 3): NastiIO = {
    val scope = AsyncScope()
    AsyncNastiCrossing(scope.clock, scope.reset, source, to_clock, to_reset, depth, sync)
  }
}

object AsyncNastiFrom {
  // takes from_source from the 'from' clock domain and puts it into your clock domain
  def apply(from_clock: Clock, from_reset: Bool, from_source: NastiIO, depth: Int = 8, sync: Int = 3): NastiIO = {
    val scope = AsyncScope()
    AsyncNastiCrossing(from_clock, from_reset, from_source, scope.clock, scope.reset, depth, sync)
  }
}

class NastiFragmenter(implicit p: Parameters) extends NastiModule()(p) {
  val io = new Bundle {
    val in = (new NastiIO).flip
    val out = new NastiIO
  }

  val nXacts = 1 << nastiXIdBits
  val writeBusy = Reg(init = UInt(0, nXacts))
  val readBusy = Reg(init = UInt(0, nXacts))
  val writeLen = Reg(Vec(nXacts, UInt(width = nastiXLenBits)))
  val readLen = Reg(Vec(nXacts, UInt(width = nastiXLenBits)))

  val writeInflight = Reg(init = Bool(false))
  val writeId = Reg(UInt(width = nastiXIdBits))
  val writeAddr = Reg(UInt(width = nastiXAddrBits))
  val writeSize = Reg(UInt(width = nastiXSizeBits))
  val writeIncr = Reg(Bool())

  when (io.in.aw.fire()) {
    writeLen(io.in.aw.bits.id) := io.in.aw.bits.len
    writeSize := io.in.aw.bits.size
    writeInflight := Bool(true)
    writeId := io.in.aw.bits.id
    writeAddr := io.in.aw.bits.addr
    writeIncr := io.in.aw.bits.burst =/= BURST_FIXED
  }

  when (io.in.w.fire()) {
    when (writeIncr) { writeAddr := writeAddr + (UInt(1) << writeSize) }
    when (io.in.w.bits.last) { writeInflight := Bool(false) }
  }

  val writeHelper = DecoupledHelper(
    io.in.w.valid,
    io.out.aw.ready,
    io.out.w.ready)

  io.out.aw.valid := writeHelper.fire(io.out.aw.ready, writeInflight)
  io.out.aw.bits := NastiWriteAddressChannel(
    id = writeId,
    addr = writeAddr,
    len = UInt(0),
    size = writeSize)

  io.out.w.valid := writeHelper.fire(io.out.w.ready, writeInflight)
  io.out.w.bits := NastiWriteDataChannel(
    data = io.in.w.bits.data,
    strb = Some(io.in.w.bits.strb),
    last = Bool(true),
    id = writeId)

  io.in.w.ready := writeHelper.fire(io.in.w.valid, writeInflight)
  io.in.aw.ready := !writeInflight && !writeBusy(io.in.aw.bits.id)

  val bid = io.out.b.bits.id

  when (io.out.b.fire()) {
    writeLen(bid) := writeLen(bid) - UInt(1)
  }

  io.in.b.valid := io.out.b.valid && writeLen(bid) === UInt(0)
  io.in.b.bits := io.out.b.bits
  io.out.b.ready := writeLen(bid) =/= UInt(0) || io.in.b.ready

  val readInflight = Reg(init = Bool(false))
  val readId = Reg(UInt(width = nastiXIdBits))
  val readAddr = Reg(UInt(width = nastiXAddrBits))
  val readBeats = Reg(UInt(width = nastiXLenBits))
  val readSize = Reg(UInt(width = nastiXSizeBits))
  val readIncr = Reg(Bool())

  when (io.in.ar.fire()) {
    readLen(io.in.ar.bits.id) := io.in.ar.bits.len
    readId := io.in.ar.bits.id
    readAddr := io.in.ar.bits.addr
    readBeats := io.in.ar.bits.len
    readSize := io.in.ar.bits.size
    readIncr := io.in.ar.bits.burst =/= BURST_FIXED
    readInflight := Bool(true)
  }

  io.out.ar.valid := readInflight
  io.out.ar.bits := NastiReadAddressChannel(
    id = readId,
    addr = readAddr,
    len = UInt(0),
    size = readSize)

  when (io.out.ar.fire()) {
    when (readBeats === UInt(0)) { readInflight := Bool(false) }
    readBeats := readBeats - UInt(1)
    when (readIncr) { readAddr := readAddr + (UInt(1) << readSize) }
  }

  io.in.ar.ready := !readInflight && !readBusy(io.in.ar.bits.id)

  val rid = io.out.r.bits.id

  when (io.out.r.fire()) {
    readLen(rid) := readLen(rid) - UInt(1)
  }

  io.in.r.valid := io.out.r.valid
  io.out.r.ready := io.in.r.ready
  io.in.r.bits := NastiReadDataChannel(
    id = io.out.r.bits.id,
    resp = io.out.r.bits.resp,
    data = io.out.r.bits.data,
    last = readLen(rid) === UInt(0))

  writeBusy := (writeBusy |
    Mux(io.in.aw.fire(), UIntToOH(io.in.aw.bits.id), UInt(0))) &
    ~Mux(io.in.b.fire(), UIntToOH(io.in.b.bits.id), UInt(0))

  readBusy := (readBusy |
    Mux(io.in.ar.fire(), UIntToOH(io.in.ar.bits.id), UInt(0))) &
    ~Mux(io.in.r.fire() && io.in.r.bits.last, UIntToOH(io.in.r.bits.id), UInt(0))
}
