// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import chisel3._
import chisel3.util.{Cat, isPow2, log2Ceil, ReadyValidIO,
  log2Up, OHToUInt, Queue, QueueIO, UIntToOH}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.leftOR

class AXI4Deinterleaver(maxReadBytes: Int, buffer: BufferParams = BufferParams.default)(implicit p: Parameters) extends LazyModule
{
  require (maxReadBytes >= 1 && isPow2(maxReadBytes))

  val node = AXI4AdapterNode(
    masterFn = { mp => mp },
    slaveFn  = { sp => sp.copy(slaves = sp.slaves.map(s => s.copy(
      supportsRead = s.supportsRead.intersect(TransferSizes(1, maxReadBytes)),
      interleavedId = Some(0))))
  })

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val endId = edgeOut.master.endId
      val beatBytes = edgeOut.slave.beatBytes
      val beats = (maxReadBytes+beatBytes-1) / beatBytes

      // This adapter leaves the control + write paths completely untouched
      out.ar :<> in.ar
      out.aw :<> in.aw
      out.w :<> in.w
      in.b :<> out.b

      if (beats <= 1) {
        // Nothing to do if only single-beat R
        in.r.asInstanceOf[ReadyValidIO[AXI4BundleR]] :<> buffer.irrevocable(out.r)
      } else {
        // Queues to buffer R responses
        val qs = Seq.tabulate(endId) { i =>
          val depth = edgeOut.master.masters.find(_.id.contains(i)).flatMap(_.maxFlight).getOrElse(0)
          if (depth > 0) {
            val q = Module(new Queue(out.r.bits.cloneType, beats))
            q.suggestName(s"queue_${i}")
            q.io
          } else {
            // These are unused IDs and should be never used.
            // But, to satisfy type checks we must produce a Wire of the
            // correct type.
            val q = Wire(new QueueIO(out.r.bits.cloneType, beats))
            q.suggestName(s"queue_wire_${i}")
            assert(!q.enq.valid, s"ID ${i} should not be used")
            q := DontCare
            q
          }
        }

        // Which ID is being enqueued and dequeued?
        val locked = RegInit(false.B)
        val deq_id = Reg(UInt(log2Up(endId).W))
        val enq_id = out.r.bits.id
        val deq_OH = UIntToOH(deq_id, endId)
        val enq_OH = UIntToOH(enq_id, endId)

        // Track the number of completely received bursts per FIFO id
        val pending = Cat(Seq.tabulate(endId) { i =>
          val depth = edgeOut.master.masters.find(_.id.contains(i)).flatMap(_.maxFlight).getOrElse(0)
          if (depth == 0) {
            false.B
          } else {
            val count = RegInit(0.U(log2Ceil(beats+1).W))
            val next = Wire(chiselTypeOf(count))
            val inc = enq_OH(i) && out.r.fire() && out.r.bits.last
            val dec = deq_OH(i) && in.r.fire() && in.r.bits.last
            next := count + inc.asUInt - dec.asUInt
            count := next
            // Bounds checking
            assert (!dec || count =/= 0.U)
            assert (!inc || count =/= beats.U)
            next =/= 0.U
          }
        }.reverse)

        // Select which Q will we start sending next cycle
        val winner  = pending & ~(leftOR(pending) << 1)
        when (!locked || (in.r.fire() && in.r.bits.last)) {
          locked := pending.orR
          deq_id := OHToUInt(winner)
        }

        // Transmit the selected burst to inner
        in.r.valid := locked
        val deq_bits = VecInit(qs.map(_.deq.bits))
        in.r.bits  := deq_bits(deq_id)
        val deq_OH_bools = deq_OH.asBools
        require(deq_OH_bools.size == qs.size, s"deq_OH.size != qs.size (${deq_OH_bools.size} vs ${qs.size})")
        (deq_OH_bools zip qs) foreach { case (s, q) =>
          q.deq.ready := s && in.r.fire()
        }

        val enq_OH_bools = enq_OH.asBools
        require(enq_OH_bools.size == qs.size, s"enq_OH.size != qs.size (${enq_OH_bools.size} vs ${qs.size})")
        // Feed response into matching Q
        val enq_readys = VecInit(qs.map(_.enq.ready))
        out.r.ready := enq_readys(enq_id)
        (enq_OH_bools zip qs) foreach { case (s, q) =>
          q.enq.valid := s && out.r.valid
          q.enq.bits := out.r.bits
        }
      }
    }
  }
}

object AXI4Deinterleaver
{
  def apply(maxReadBytes: Int, buffer: BufferParams = BufferParams.default)(implicit p: Parameters): AXI4Node =
  {
    val axi4deint = LazyModule(new AXI4Deinterleaver(maxReadBytes, buffer))
    axi4deint.node
  }
}
