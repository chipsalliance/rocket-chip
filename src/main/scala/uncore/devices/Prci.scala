// See LICENSE for license details.

package uncore.devices

import Chisel._
import rocket.Util._
import junctions._
import junctions.NastiConstants._
import uncore.tilelink._
import uncore.util._
import cde.{Parameters, Field}

/** Number of tiles */
case object NTiles extends Field[Int]

class PRCIInterrupts extends Bundle {
  val meip = Bool()
  val seip = Bool()
  val debug = Bool()
}

class PRCITileIO(implicit p: Parameters) extends Bundle {
  val reset = Bool(OUTPUT)
  val id = UInt(OUTPUT, log2Up(p(NTiles)))
  val interrupts = {
    val result = new PRCIInterrupts {
      val mtip = Bool()
      val msip = Bool()
    }
    result.asOutput
  }

  override def cloneType: this.type = new PRCITileIO().asInstanceOf[this.type]
}

object PRCI {
  def msip(hart: Int) = hart * msipBytes
  def timecmp(hart: Int) = 0x4000 + hart * timecmpBytes
  def time = 0xbff8
  def msipBytes = 4
  def timecmpBytes = 8
  def size = 0xc000
}

/** Power, Reset, Clock, Interrupt */
class PRCI(implicit val p: Parameters) extends Module
    with HasTileLinkParameters
    with HasAddrMapParameters {
  val io = new Bundle {
    val interrupts = Vec(p(NTiles), new PRCIInterrupts).asInput
    val tl = new ClientUncachedTileLinkIO().flip
    val tiles = Vec(p(NTiles), new PRCITileIO)
    val rtcTick = Bool(INPUT)
  }

  val timeWidth = 64
  val timecmp = Reg(Vec(p(NTiles), UInt(width = timeWidth)))
  val time = Reg(init=UInt(0, timeWidth))
  when (io.rtcTick) { time := time + UInt(1) }

  val ipi = Reg(init=Vec.fill(p(NTiles))(UInt(0, 32)))

  val acq = Queue(io.tl.acquire, 1)
  val addr = acq.bits.full_addr()(log2Ceil(PRCI.size)-1,0)
  val read = acq.bits.isBuiltInType(Acquire.getType)
  val rdata = Wire(init=UInt(0))
  io.tl.grant.valid := acq.valid
  acq.ready := io.tl.grant.ready
  io.tl.grant.bits := Grant(
    is_builtin_type = Bool(true),
    g_type = acq.bits.getBuiltInGrantType(),
    client_xact_id = acq.bits.client_xact_id,
    manager_xact_id = UInt(0),
    addr_beat = UInt(0),
    data = rdata)

  when (addr(log2Floor(PRCI.time))) {
    require(log2Floor(PRCI.timecmp(p(NTiles)-1)) < log2Floor(PRCI.time))
    rdata := store(Seq(time), acq.bits, io.tl.grant.fire())
  }.elsewhen (addr >= PRCI.timecmp(0)) {
    rdata := store(timecmp, acq.bits, io.tl.grant.fire())
  }.otherwise {
    rdata := store(ipi, acq.bits, io.tl.grant.fire()) & Fill(tlDataBits/32, UInt(1, 32))
  }

  for ((tile, i) <- io.tiles zipWithIndex) {
    tile.interrupts := io.interrupts(i)
    tile.interrupts.msip := ipi(i)(0)
    tile.interrupts.mtip := time >= timecmp(i)
    tile.id := UInt(i)
  }

  // TODO generalize these to help other TL slaves
  def load(v: Seq[UInt], acq: Acquire): UInt = {
    val w = v.head.getWidth
    val a = acq.full_addr()
    require(isPow2(w) && w >= 8)
    if (w > tlDataBits) {
      (v(a.extract(log2Ceil(w/8*v.size)-1,log2Ceil(w/8))) >> a.extract(log2Ceil(w/8)-1,log2Ceil(tlDataBytes)))(tlDataBits-1,0)
    } else {
      val row: Seq[UInt] = for (i <- 0 until v.size by tlDataBits/w)
        yield Cat(v.slice(i, i + tlDataBits/w).reverse)
      if (row.size == 1) row.head
      else row(a(log2Ceil(w/8*v.size)-1,log2Ceil(tlDataBytes)))
    }
  }

  def store(v: Seq[UInt], acq: Acquire, en: Bool): UInt = {
    val w = v.head.getWidth
    require(isPow2(w) && w >= 8)
    val a = acq.full_addr()
    val rdata = load(v, acq)
    val wdata = (acq.data & acq.full_wmask()) | (rdata & ~acq.full_wmask())
    when (en && acq.isBuiltInType(Acquire.putType)) {
      if (w <= tlDataBits) {
        val word =
          if (tlDataBits/w >= v.size) UInt(0)
          else a(log2Up(w/8*v.size)-1,log2Up(tlDataBytes))
        for (i <- 0 until v.size) when (word === i/(tlDataBits/w)) {
          val base = i % (tlDataBits/w)
          v(i) := wdata >> (w * (i % (tlDataBits/w)))
        }
      } else {
        val i = a.extract(log2Ceil(w/8*v.size)-1,log2Ceil(w/8))
        val mask = FillInterleaved(tlDataBits, UIntToOH(a.extract(log2Ceil(w/8)-1,log2Ceil(tlDataBytes))))
        v(i) := (wdata & mask) | (v(i) & ~mask)
      }
    }
    rdata
  }
}
