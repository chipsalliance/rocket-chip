// See LICENSE.SiFive for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import config._
import diplomacy._
import util.PositionalMultiQueue
import uncore.axi4._
import scala.math.{min, max}

case class TLToAXI4Node(idBits: Int) extends MixedAdapterNode(TLImp, AXI4Imp)(
  dFn = { _ =>
    // We must erase all client information, because we crush their source Ids
    val masters = Seq(
      AXI4MasterParameters(
        id      = IdRange(0, 1 << idBits),
        aligned = true))
    AXI4MasterPortParameters(masters)
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
        supportsPutPartial = s.supportsWrite)},
        // AXI4 is NEVER fifo in TL sense (R+W are independent)
      beatBytes = p.beatBytes,
      minLatency = p.minLatency)
  })

class TLToAXI4(idBits: Int, combinational: Boolean = true)(implicit p: Parameters) extends LazyModule
{
  val node = TLToAXI4Node(idBits)

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

      // We need to ensure that a slave does not stall trying to send B while we need to receive R
      // Since R&W have independent flow control, it is possible for a W to cut in-line and get into
      // a slave's buffers, preventing us from getting all the R responses we need to release D for B.
      // This risk is compounded by an AXI fragmentation. Even a slave which responds completely to
      // AR before working on AW might have an AW slipped between two AR fragments.
      val out_b = Queue.irrevocable(out.b, entries=edgeIn.client.endSourceId, flow=combinational)

      // We need to keep the following state from A => D: (addr_lo, size, source)
      // All of those fields could potentially require 0 bits (argh. Chisel.)
      // We will pack as many of the lowest bits of state as fit into the AXI ID.
      // Any bits left-over must be put into a bank of Queues.
      // The Queues are indexed by as many of the source bits as fit into the AXI ID.
      // The Queues are deep enough that every source has guaranteed space in its Queue.

      val sourceBits = log2Ceil(edgeIn.client.endSourceId)
      val sizeBits = log2Ceil(edgeIn.maxLgSize+1)
      val addrBits = log2Ceil(edgeIn.manager.beatBytes)
      val stateBits = addrBits + sizeBits + sourceBits // could be 0

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
      val a_id = if (idBits == 0) UInt(0) else a_state

      val r_state = Wire(UInt(width = stateBits))
      val r_source  = if (sourceBits > 0) r_state(sourceEnd-1, sourceOff) else UInt(0)
      val r_size    = if (sizeBits   > 0) r_state(sizeEnd  -1, sizeOff)   else UInt(0)
      val r_addr_lo = if (addrBits   > 0) r_state(addrEnd  -1, addrOff)   else UInt(0)

      val b_state = Wire(UInt(width = stateBits))
      val b_source  = if (sourceBits > 0) b_state(sourceEnd-1, sourceOff) else UInt(0)
      val b_size    = if (sizeBits   > 0) b_state(sizeEnd  -1, sizeOff)   else UInt(0)
      val b_addr_lo = if (addrBits   > 0) b_state(addrEnd  -1, addrOff)   else UInt(0)

      val r_last = out.r.bits.last
      val r_id = out.r.bits.id
      val b_id = out_b.bits.id

      if (stateBits <= idBits) { // No need for any state tracking
        r_state := r_id
        b_state := b_id
      } else {
        val bankIndexBits = min(sourceBits, idBits)
        val posBits = max(0, sourceBits - idBits)
        val implicitBits = max(idBits, sourceBits)
        val bankBits = stateBits - implicitBits
        val numBanks = min(1 << bankIndexBits, edgeIn.client.endSourceId)
        def bankEntries(i: Int) = (edgeIn.client.endSourceId+numBanks-i-1) / numBanks

        val banks = Seq.tabulate(numBanks) { i =>
          // We know there can only be as many outstanding requests as TL sources
          // However, AXI read and write queues are not mutually FIFO.
          // Therefore, we want to pop them individually, but share the storage.
          val bypass = combinational && edgeOut.slave.minLatency == 0
          PositionalMultiQueue(UInt(width=max(1,bankBits)), positions=bankEntries(i), ways=2, combinational=bypass)
        }

        val a_bankPosition = if (posBits == 0) UInt(0) else a_source(sourceBits-1, idBits)
        val a_bankIndex = if (bankIndexBits == 0) UInt(0) else a_source(bankIndexBits-1, 0)
        val r_bankIndex = if (bankIndexBits == 0) UInt(0) else r_id(bankIndexBits-1, 0)
        val b_bankIndex = if (bankIndexBits == 0) UInt(0) else b_id(bankIndexBits-1, 0)
        val a_bankSelect = UIntToOH(a_bankIndex, numBanks)
        val r_bankSelect = UIntToOH(r_bankIndex, numBanks)
        val b_bankSelect = UIntToOH(b_bankIndex, numBanks)

        banks.zipWithIndex.foreach { case (q, i) =>
          // Push a_state into the banks
          q.io.enq.valid := in.a.fire() && a_last && a_bankSelect(i)
          q.io.enq.bits.pos  := a_bankPosition
          q.io.enq.bits.data := a_state >> implicitBits
          q.io.enq.bits.way  := Mux(a_isPut, UInt(0), UInt(1))
          // Pop the bank's ways
          q.io.deq(0).ready := out_b.fire() && b_bankSelect(i)
          q.io.deq(1).ready := out.r.fire() && r_bankSelect(i) && r_last
          // The FIFOs must be valid when we're ready to pop them...
          assert (q.io.deq(0).valid || !q.io.deq(0).ready)
          assert (q.io.deq(1).valid || !q.io.deq(1).ready)
        }

        val b_bankData = Vec(banks.map(_.io.deq(0).bits.data))(b_bankIndex)
        val b_bankPos  = Vec(banks.map(_.io.deq(0).bits.pos ))(b_bankIndex)
        val r_bankData = Vec(banks.map(_.io.deq(1).bits.data))(r_bankIndex)
        val r_bankPos  = Vec(banks.map(_.io.deq(1).bits.pos ))(r_bankIndex)

        def optCat(x: (Boolean, UInt)*) = { Cat(x.toList.filter(_._1).map(_._2)) }
        b_state := optCat((bankBits > 0, b_bankData), (posBits > 0, b_bankPos), (idBits > 0, b_id))
        r_state := optCat((bankBits > 0, r_bankData), (posBits > 0, r_bankPos), (idBits > 0, r_id))
      }

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
      arw.id    := a_id // truncated
      arw.addr  := a_address
      arw.len   := UIntToOH1(a_size, AXI4Parameters.lenBits + log2Ceil(beatBytes)) >> log2Ceil(beatBytes)
      arw.size  := Mux(a_size >= maxSize, maxSize, a_size)
      arw.burst := AXI4Parameters.BURST_INCR
      arw.lock  := UInt(0) // not exclusive (LR/SC unsupported b/c no forward progress guarantee)
      arw.cache := UInt(0) // do not allow AXI to modify our transactions
      arw.prot  := AXI4Parameters.PROT_PRIVILEDGED
      arw.qos   := UInt(0) // no QoS

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
      out_b.ready := in.d.ready && !r_wins
      in.d.valid := Mux(r_wins, out.r.valid, out_b.valid)

      val r_error = out.r.bits.resp =/= AXI4Parameters.RESP_OKAY
      val b_error = out_b.bits.resp =/= AXI4Parameters.RESP_OKAY

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
  // applied to the TL source node; y.node := TLToAXI4(idBits)(x.node)
  def apply(idBits: Int, combinational: Boolean = true)(x: TLOutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): AXI4OutwardNode = {
    val axi4 = LazyModule(new TLToAXI4(idBits, combinational))
    axi4.node := x
    axi4.node
  }
}
