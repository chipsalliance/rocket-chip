// See LICENSE.SiFive for license details.

package uncore.axi4

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util.IrrevocableIO
import config._
import diplomacy._
import scala.math.{min,max}
import uncore.tilelink2.{leftOR, rightOR, UIntToOH1, OH1ToOH}

class AXI4Deinterleaver(maxReadBytes: Int)(implicit p: Parameters) extends LazyModule
{
  require (maxReadBytes >= 1 && isPow2(maxReadBytes))

  val node = AXI4AdapterNode(
    masterFn = { mp => mp },
    slaveFn  = { sp => sp.copy(slaves = sp.slaves.map(s => s.copy(
      supportsRead = s.supportsRead.intersect(TransferSizes(1, maxReadBytes)),
      interleavedId = Some(0))))
  })

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>
      val queues = edgeOut.master.endId
      val beatBytes = edgeOut.slave.beatBytes
      val beats = (maxReadBytes+beatBytes-1) / beatBytes

      // This adapter leaves the control + write paths completely untouched
      out.ar <> in.ar
      out.aw <> in.aw
      out.w <> in.w
      in.b <> out.b

      if (queues == 1) {
        // Gracefully do nothing
        in.r <> out.r
      } else {
        // Buffer R response
        val count = RegInit(Vec.fill(queues) { UInt(0, width=log2Ceil(beats+1)) })
        val qs = Seq.fill(queues) { Module(new Queue(out.r.bits, beats)) }

        // Which ID is being enqueued and dequeued?
        val locked = RegInit(Bool(false))
        val deq_id = Reg(UInt(width=log2Ceil(queues)))
        val enq_id = out.r.bits.id
        val deq_OH = UIntToOH(deq_id, queues)
        val enq_OH = UIntToOH(enq_id, queues)

        // Track the number of completely received bursts per FIFO id
        val next_count = Wire(count)
        ((count zip next_count) zip (enq_OH.toBools zip deq_OH.toBools)) foreach { case ((p, n), (i, d)) =>
          val inc = i && out.r.fire() && out.r.bits.last
          val dec = d && in.r.fire() && in.r.bits.last
          n := p + inc.asUInt - dec.asUInt
          // Bounds checking
          assert (!dec || p =/= UInt(0))
          assert (!inc || p =/= UInt(beats))
        }
        count := next_count

        // Select which Q will we start sending next cycle
        val pending = Cat(next_count.map(_ =/= UInt(0)).reverse)
        val winner  = pending & ~(leftOR(pending) << 1)
        when (!locked || (in.r.fire() && in.r.bits.last)) {
          locked := pending.orR
          deq_id := OHToUInt(winner)
        }

        // Transmit the selected burst to inner
        in.r.valid := locked
        in.r.bits  := Vec(qs.map(_.io.deq.bits))(deq_id)
        (deq_OH.toBools zip qs) foreach { case (s, q) =>
          q.io.deq.ready := s && in.r.fire()
        }

        // Feed response into matching Q
        out.r.ready := Vec(qs.map(_.io.enq.ready))(enq_id)
        (enq_OH.toBools zip qs) foreach { case (s, q) =>
          q.io.enq.valid := s && out.r.valid
          q.io.enq.bits := out.r.bits
        }
      }
    }
  }
}

object AXI4Deinterleaver
{
  // applied to the AXI4 source node; y.node := AXI4Deinterleaver()(x.node)
  def apply(maxReadBytes: Int)(x: AXI4OutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): AXI4OutwardNode = {
    val deinterleaver = LazyModule(new AXI4Deinterleaver(maxReadBytes))
    deinterleaver.node := x
    deinterleaver.node
  }
}
