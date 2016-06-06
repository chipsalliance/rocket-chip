// See LICENSE for license details.

package uncore

import Chisel._
import Chisel.ImplicitConversions._
import junctions._
import junctions.NastiConstants._
import cde.{Parameters, Field}

/** Number of tiles */
case object NTiles extends Field[Int]

class PRCIInterrupts extends Bundle {
  val mtip = Bool()
  val meip = Bool()
  val seip = Bool()
  val debug = Bool()
}

class PRCITileIO(implicit p: Parameters) extends Bundle {
  val reset = Bool(OUTPUT)
  val id = UInt(OUTPUT, log2Up(p(NTiles)))
  val interrupts = new PRCIInterrupts {
    val msip = Bool()
  }.asOutput

  override def cloneType: this.type = new PRCITileIO().asInstanceOf[this.type]
}

/** Power, Reset, Clock, Interrupt */
class PRCI(implicit val p: Parameters) extends Module
    with HasTileLinkParameters
    with HasAddrMapParameters {
  val io = new Bundle {
    val interrupts = Vec(p(NTiles), new PRCIInterrupts).asInput
    val tl = new ClientUncachedTileLinkIO().flip
    val tiles = Vec(p(NTiles), new PRCITileIO)
  }

  val ipi = Reg(init=Vec.fill(p(NTiles))(Bool(false)))

  val acq = Queue(io.tl.acquire, 1)
  val addr = acq.bits.full_addr()
  val read = acq.bits.isBuiltInType(Acquire.getType)
  val write = acq.bits.isBuiltInType(Acquire.putType)
  val rdata = Wire(init=UInt(0))
  val masked_wdata = (acq.bits.data & acq.bits.full_wmask()) | (rdata & ~acq.bits.full_wmask())
  io.tl.grant.valid := acq.valid
  acq.ready := io.tl.grant.ready
  io.tl.grant.bits := Grant(
    is_builtin_type = Bool(true),
    g_type = acq.bits.getBuiltInGrantType(),
    client_xact_id = acq.bits.client_xact_id,
    manager_xact_id = UInt(0),
    addr_beat = UInt(0),
    data = rdata)

  when (write) {
    val ipiRegBytes = 4
    val regsPerBeat = tlDataBytes/ipiRegBytes
    val word =
      if (regsPerBeat >= ipi.size) UInt(0)
      else addr(log2Up(ipi.size*ipiRegBytes)-1,log2Up(tlDataBytes))
    for (i <- 0 until ipi.size by regsPerBeat) {
      when (word === i/regsPerBeat) {
        rdata := Cat(ipi.slice(i, i + regsPerBeat).map(p => Cat(UInt(0, 8*ipiRegBytes-1), p)).reverse)
        for (j <- 0 until (regsPerBeat min (ipi.size - i))) {
          when (write) { ipi(i+j) := masked_wdata(j*8*ipiRegBytes) }
        }
      }
    }
  }

  for ((tile, i) <- io.tiles zipWithIndex) {
    tile.interrupts := io.interrupts(i)
    tile.interrupts.msip := ipi(i)
    tile.id := UInt(i)
  }
}
