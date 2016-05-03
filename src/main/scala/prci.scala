// See LICENSE for license details.

package uncore

import Chisel._
import Chisel.ImplicitConversions._
import junctions._
import junctions.NastiConstants._
import cde.{Parameters, Field}

/** Number of tiles */
case object NTiles extends Field[Int]

class PRCITileIO(implicit p: Parameters) extends Bundle {
  val reset = Bool(OUTPUT)
  val id = UInt(OUTPUT, log2Up(p(NTiles)))
  val interrupts = new Bundle {
    val mtip = Bool()
    val msip = Bool()
    val meip = Bool()
    val seip = Bool()
    val debug = Bool()
  }.asOutput

  override def cloneType: this.type = new PRCITileIO().asInstanceOf[this.type]
}

class PRCI(implicit val p: Parameters) extends Module
    with HasTileLinkParameters
    with HasAddrMapParameters {
  val io = new Bundle {
    val id = UInt(INPUT, log2Up(p(NTiles)))
    val interrupts = new Bundle {
      val mtip = Bool()
      val meip = Bool()
      val seip = Bool()
      val debug = Bool()
    }.asInput
    val tl = new ClientUncachedTileLinkIO().flip
    val tile = new PRCITileIO
  }

  val ipi = Reg(init=Bool(false))

  val acq = Queue(io.tl.acquire, 1)
  val read = acq.bits.isBuiltInType(Acquire.getType)
  val write = acq.bits.isBuiltInType(Acquire.putType)
  val rdata = Wire(init=ipi)
  io.tl.grant.valid := acq.valid
  acq.ready := io.tl.grant.ready
  io.tl.grant.bits := Grant(
    is_builtin_type = Bool(true),
    g_type = acq.bits.getBuiltInGrantType(),
    client_xact_id = acq.bits.client_xact_id,
    manager_xact_id = UInt(0),
    addr_beat = UInt(0),
    data = rdata)

  val regSize = 16
  val nRegs = 2
  val addr = acq.bits.full_addr()(log2Up(regSize*nRegs)-1,log2Up(regSize))

  when (addr === UInt(0) && write) {
    ipi := acq.bits.data(0)
  }

  io.tile.interrupts := io.interrupts
  io.tile.interrupts.msip := ipi
  io.tile.id := io.id
}
