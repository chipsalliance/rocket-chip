// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import chisel3._
import chisel3.util.{Cat, isPow2, log2Ceil, ReadyValidIO,
  log2Up, OHToUInt, Queue, QueueIO, UIntToOH}
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.leftOR

/** This adapter deinterleaves read responses on the R channel.
  *
  * Deinterleaving guarantees that once the first beat of a read response
  * has been accepted by the recipient, all further presented read responses will
  * be from the same burst transaction, until the burst is complete.
  *
  * @param maxReadBytes is the maximum supported read burst size that this adapter
  *   has been provisioned to support.
  * @param buffer is the internal buffering to provide in the case where no deinterleaving is required.
  */
class AXI4Deinterleaver(maxReadBytes: Int, buffer: BufferParams = BufferParams.default)(implicit p: Parameters) extends LazyModule
{
  require (maxReadBytes >= 1, s"AXI4Deinterleaver: maxReadBytes must be at least 1, not $maxReadBytes")
  require (isPow2(maxReadBytes), s"AXI4Deinterleaver: maxReadBytes must be a power of two, not $maxReadBytes")

  private def maxBeats(slave: AXI4SlavePortParameters): Int =
    (maxReadBytes+slave.beatBytes-1) / slave.beatBytes

  // Nothing to do if R channel only uses a single beat
  private def nothingToDeinterleave(slave: AXI4SlavePortParameters): Boolean =
    maxBeats(slave) <= 1

  val node = new AXI4AdapterNode(
    masterFn = { mp => mp },
    slaveFn  = { sp => sp.copy(slaves = sp.slaves.map(s => s.copy(
      supportsRead = s.supportsRead.intersect(TransferSizes(1, maxReadBytes)),
      interleavedId = Some(0))))
  }) {
    override def circuitIdentity = edges.out.map(_.slave).forall(nothingToDeinterleave)
  }

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val endId = edgeOut.master.endId
      val beats = maxBeats(edgeOut.slave)

      // This adapter passes through the AR/AW control + W/B write data channels
      out.ar :<>= in.ar
      out.aw :<>= in.aw
      out.w :<>= in.w
      in.b :<>= out.b

      // Only the R channel has the possibility of being changed
      if (nothingToDeinterleave(edgeOut.slave)) {
        in.r.asInstanceOf[ReadyValidIO[AXI4BundleR]] :<>= buffer.irrevocable(out.r)
      } else {
        // We only care to deinterleave ids that are actually in use
        val maxFlightPerId = Seq.tabulate(endId) { i =>
          edgeOut.master.masters.find(_.id.contains(i)).flatMap(_.maxFlight).getOrElse(0)
        }

        // Queues to buffer R responses
        val qs = maxFlightPerId.zipWithIndex.map { case (mf, i) =>
          if (mf > 0) {
            val q = Module(new Queue(out.r.bits.cloneType, entries = beats))
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
        val pending = Cat(maxFlightPerId.zipWithIndex.map {
          case (0, _) => false.B // any id not in use
          case (_, i) => {       // i is an id in use
            val count = RegInit(0.U(log2Ceil(beats+1).W))
            val next = Wire(chiselTypeOf(count))
            val inc = enq_OH(i) && out.r.fire && out.r.bits.last
            val dec = deq_OH(i) && in.r.fire && in.r.bits.last
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
        when (!locked || (in.r.fire && in.r.bits.last)) {
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
          q.deq.ready := s && in.r.fire
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
