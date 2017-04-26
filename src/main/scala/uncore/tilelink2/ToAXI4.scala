// See LICENSE.SiFive for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import config._
import diplomacy._
import util.PositionalMultiQueue
import uncore.axi4._
import scala.math.{min, max}

case class TLToAXI4Node(beatBytes: Int) extends MixedAdapterNode(TLImp, AXI4Imp)(
  dFn = { p =>
    val idSize = p.clients.map { c => if (c.requestFifo) 1 else c.sourceId.size }
    val idStart = idSize.scanLeft(0)(_+_).init
    val masters = ((idStart zip idSize) zip p.clients) map { case ((start, size), c) =>
      AXI4MasterParameters(
        id       = IdRange(start, start+size),
        aligned  = true,
        nodePath = c.nodePath)
    }
    AXI4MasterPortParameters(
      masters  = masters,
      userBits = log2Ceil(p.endSourceId) + 4 + log2Ceil(beatBytes))
  },
  uFn = { p => TLManagerPortParameters(
    managers = p.slaves.map { case s =>
      TLManagerParameters(
        address            = s.address,
        resources          = s.resources,
        regionType         = s.regionType,
        executable         = s.executable,
        nodePath           = s.nodePath,
        supportsGet        = s.supportsRead,
        supportsPutFull    = s.supportsWrite,
        supportsPutPartial = s.supportsWrite,
        fifoId             = Some(0))},
      beatBytes = p.beatBytes,
      minLatency = p.minLatency)
  })

class TLToAXI4(beatBytes: Int, combinational: Boolean = true)(implicit p: Parameters) extends LazyModule
{
  val node = TLToAXI4Node(beatBytes)

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in = node.bundleIn
      val out = node.bundleOut
    }

    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>
      val slaves  = edgeOut.slave.slaves

      // All pairs of slaves must promise that they will never interleave data
      require (slaves(0).interleavedId.isDefined)
      slaves.foreach { s => require (s.interleavedId == slaves(0).interleavedId) }

      // Construct the source=>ID mapping table
      val idTable = Wire(Vec(edgeIn.client.endSourceId, out.aw.bits.id))
      (edgeIn.client.clients zip edgeOut.master.masters) foreach { case (c, m) =>
        for (i <- 0 until c.sourceId.size) {
          idTable(c.sourceId.start + i) := UInt(m.id.start + (if (c.requestFifo) 0 else i))
        }
      }

      // We need to keep the following state from A => D: (addr_lo, size, source)
      // All of those fields could potentially require 0 bits (argh. Chisel.)
      // We will pack all of that extra information into the user bits.

      val sourceBits = log2Ceil(edgeIn.client.endSourceId)
      val sizeBits = log2Ceil(edgeIn.maxLgSize+1)
      val addrBits = log2Ceil(edgeIn.manager.beatBytes)
      val stateBits = addrBits + sizeBits + sourceBits // could be 0
      require (stateBits <= out.aw.bits.params.userBits)

      val a_address = edgeIn.address(in.a.bits)
      val a_addr_lo = edgeIn.addr_lo(a_address)
      val a_source  = in.a.bits.source
      val a_size    = edgeIn.size(in.a.bits)
      val a_isPut   = edgeIn.hasData(in.a.bits)
      val a_last    = edgeIn.last(in.a)

      // Make sure the fields are within the bounds we assumed
      assert (a_source  < UInt(BigInt(1) << sourceBits))
      assert (a_size    < UInt(BigInt(1) << sizeBits))
      assert (a_addr_lo < UInt(BigInt(1) << addrBits))

      // Carefully pack/unpack fields into the state we send
      val baseEnd = 0
      val (sourceEnd, sourceOff) = (sourceBits + baseEnd,   baseEnd)
      val (sizeEnd,   sizeOff)   = (sizeBits   + sourceEnd, sourceEnd)
      val (addrEnd,   addrOff)   = (addrBits   + sizeEnd,   sizeEnd)
      require (addrEnd == stateBits)

      val a_state = (a_source << sourceOff) | (a_size << sizeOff) | (a_addr_lo << addrOff)

      val r_state = out.r.bits.user.getOrElse(UInt(0))
      val r_source  = if (sourceBits > 0) r_state(sourceEnd-1, sourceOff) else UInt(0)
      val r_size    = if (sizeBits   > 0) r_state(sizeEnd  -1, sizeOff)   else UInt(0)
      val r_addr_lo = if (addrBits   > 0) r_state(addrEnd  -1, addrOff)   else UInt(0)

      val b_state = out.b.bits.user.getOrElse(UInt(0))
      val b_source  = if (sourceBits > 0) b_state(sourceEnd-1, sourceOff) else UInt(0)
      val b_size    = if (sizeBits   > 0) b_state(sizeEnd  -1, sizeOff)   else UInt(0)
      val b_addr_lo = if (addrBits   > 0) b_state(addrEnd  -1, addrOff)   else UInt(0)

      // We need these Queues because AXI4 queues are irrevocable
      val depth = if (combinational) 1 else 2
      val out_arw = Wire(Decoupled(new AXI4BundleARW(out.params)))
      val out_w = Wire(out.w)
      out.w <> Queue.irrevocable(out_w, entries=depth, flow=combinational)
      val queue_arw = Queue.irrevocable(out_arw, entries=depth, flow=combinational)

      // Fan out the ARW channel to AR and AW
      out.ar.bits := queue_arw.bits
      out.aw.bits := queue_arw.bits
      out.ar.valid := queue_arw.valid && !queue_arw.bits.wen
      out.aw.valid := queue_arw.valid &&  queue_arw.bits.wen
      queue_arw.ready := Mux(queue_arw.bits.wen, out.aw.ready, out.ar.ready)

      val beatBytes = edgeIn.manager.beatBytes
      val maxSize   = UInt(log2Ceil(beatBytes))
      val doneAW    = RegInit(Bool(false))
      when (in.a.fire()) { doneAW := !a_last }

      val arw = out_arw.bits
      arw.wen   := a_isPut
      arw.id    := idTable(a_source)
      arw.addr  := a_address
      arw.len   := UIntToOH1(a_size, AXI4Parameters.lenBits + log2Ceil(beatBytes)) >> log2Ceil(beatBytes)
      arw.size  := Mux(a_size >= maxSize, maxSize, a_size)
      arw.burst := AXI4Parameters.BURST_INCR
      arw.lock  := UInt(0) // not exclusive (LR/SC unsupported b/c no forward progress guarantee)
      arw.cache := UInt(0) // do not allow AXI to modify our transactions
      arw.prot  := AXI4Parameters.PROT_PRIVILEDGED
      arw.qos   := UInt(0) // no QoS
      arw.user.foreach { _ := a_state }

      // !!! Mix R-W stall here
      in.a.ready := Mux(a_isPut, (doneAW || out_arw.ready) && out_w.ready, out_arw.ready)
      out_arw.valid := in.a.valid && Mux(a_isPut, !doneAW && out_w.ready, Bool(true))

      out_w.valid := in.a.valid && a_isPut && (doneAW || out_arw.ready)
      out_w.bits.data := in.a.bits.data
      out_w.bits.strb := in.a.bits.mask
      out_w.bits.last := a_last

      // R and B => D arbitration
      val r_holds_d = RegInit(Bool(false))
      when (out.r.fire()) { r_holds_d := !out.r.bits.last }
      // Give R higher priority than B
      val r_wins = out.r.valid || r_holds_d

      out.r.ready := in.d.ready
      out.b.ready := in.d.ready && !r_wins
      in.d.valid := Mux(r_wins, out.r.valid, out.b.valid)

      val r_error = out.r.bits.resp =/= AXI4Parameters.RESP_OKAY
      val b_error = out.b.bits.resp =/= AXI4Parameters.RESP_OKAY

      val r_d = edgeIn.AccessAck(r_addr_lo, UInt(0), r_source, r_size, UInt(0), r_error)
      val b_d = edgeIn.AccessAck(b_addr_lo, UInt(0), b_source, b_size, b_error)

      in.d.bits := Mux(r_wins, r_d, b_d)
      in.d.bits.data := out.r.bits.data // avoid a costly Mux

      // Tie off unused channels
      in.b.valid := Bool(false)
      in.c.ready := Bool(true)
      in.e.ready := Bool(true)
    }
  }
}

object TLToAXI4
{
  // applied to the TL source node; y.node := TLToAXI4(beatBytes)(x.node)
  def apply(beatBytes: Int, combinational: Boolean = true)(x: TLOutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): AXI4OutwardNode = {
    val axi4 = LazyModule(new TLToAXI4(beatBytes, combinational))
    axi4.node := x
    axi4.node
  }
}
