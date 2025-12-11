// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import chisel3._
import chisel3.util.{Mux1H, Queue, IrrevocableIO, log2Ceil, UIntToOH}

import org.chipsalliance.cde.config.Parameters

import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

import freechips.rocketchip.diplomacy.{AddressDecoder, AddressSet, TransferSizes}
import freechips.rocketchip.util.{ControlKey, SimpleBundleField, rightOR, leftOR, OH1ToOH, UIntToOH1}

case object AXI4FragLast extends ControlKey[Bool]("real_last")
case class AXI4FragLastField() extends SimpleBundleField(AXI4FragLast)(Output(Bool()), false.B)

/**
  * AXI4 fragmenter. It breaks AXI4 burst transfer to single beat transfers.
  */
class AXI4Fragmenter()(implicit p: Parameters) extends LazyModule
{
  val maxBeats = 1 << AXI4Parameters.lenBits
  def expandTransfer(x: TransferSizes, beatBytes: Int, alignment: BigInt) =
    if (!x) x else TransferSizes(x.min, alignment.min(maxBeats*beatBytes).intValue)
  def mapSlave(s: AXI4SlaveParameters, beatBytes: Int) = s.copy(
    supportsWrite = expandTransfer(s.supportsWrite, beatBytes, s.minAlignment),
    supportsRead  = expandTransfer(s.supportsRead,  beatBytes, s.minAlignment),
    interleavedId = None) // this breaks interleaving guarantees
  def mapMaster(m: AXI4MasterParameters) = m.copy(aligned = true, maxFlight = None)

  val node = AXI4AdapterNode(
    masterFn = { mp => mp.copy(masters = mp.masters.map(m => mapMaster(m)), echoFields = AXI4FragLastField() +: mp.echoFields) },
    slaveFn  = { sp => sp.copy(slaves  = sp.slaves .map(s => mapSlave(s, sp.beatBytes))) })

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val slave     = edgeOut.slave
      val slaves    = slave.slaves
      val beatBytes = slave.beatBytes
      val lgBytes   = log2Ceil(beatBytes)
      val master    = edgeIn.master
      val masters   = master.masters

      // We don't support fragmenting to sub-beat accesses
      slaves.foreach { s =>
        require (!s.supportsRead  || s.supportsRead.contains(beatBytes))
        require (!s.supportsWrite || s.supportsWrite.contains(beatBytes))
      }

      /* We need to decompose a request into 
       *   FIXED => each beat is a new request
       *   WRAP/INCR => take xfr up to next power of two, capped by max size of target
       *
       * On AR and AW, we fragment one request into many
       * On W we set 'last' on beats which are fragment boundaries
       * On R we clear 'last' on the fragments being reassembled
       * On B we clear 'valid' on the responses for the injected fragments
       *
       * AR=>R and AW+W=>B are completely independent state machines.
       */

      /* Returns the number of beats to execute and the new address */
      def fragment(a: IrrevocableIO[AXI4BundleA], supportedSizes1: Seq[Int]): (IrrevocableIO[AXI4BundleA], Bool, UInt) = {
        val out = Wire(chiselTypeOf(a))

        val busy   = RegInit(false.B)
        val r_addr = Reg(UInt(a.bits.params.addrBits.W))
        val r_len  = Reg(UInt(AXI4Parameters.lenBits.W))

        val len  = Mux(busy, r_len,  a.bits.len)
        val addr = Mux(busy, r_addr, a.bits.addr)

        val lo = if (lgBytes == 0) 0.U else addr(lgBytes-1, 0)
        val cutoff = AXI4Parameters.lenBits + lgBytes
        val alignment = addr((a.bits.params.addrBits min cutoff)-1, lgBytes)

        // We don't care about illegal addresses; bursts or no bursts... whatever circuit is simpler (AXI4ToTL will fix it)
        // !!! think about this more -- what if illegal?
        val sizes1 = (supportedSizes1 zip slave.slaves.map(_.address)).filter(_._1 >= 0).groupBy(_._1).view.mapValues(_.flatMap(_._2)).toMap
        val reductionMask = AddressDecoder(sizes1.values.toList)
        val support1 = Mux1H(sizes1.toList.map { case (v, a) => // maximum supported size-1 based on target address
          (AddressSet.unify(a.map(_.widen(~reductionMask)).distinct).map(_.contains(addr)).reduce(_||_), v.U)
        })

        /* We need to compute the largest transfer allowed by the AXI len.
         * len+1 is the number of beats to execute.
         * We want the MSB(len+1)-1; one less than the largest power of two we could execute.
         * There are two cases; either len is 2^n-1 in which case we leave it unchanged, ELSE
         *   fill the bits from highest to lowest, and shift right by one bit.
         */
        val fillLow  = rightOR(len) >> 1   // set   all bits in positions <  a set     bit
        val wipeHigh = ~(leftOR(~len))       // clear all bits in position  >= a cleared bit
        val remain1  = fillLow | wipeHigh  // MSB(a.len+1)-1
        val align1   = ~leftOR(alignment)  // transfer size limited by address alignment
        val maxSupported1 = remain1 & align1 & support1 // Take the minimum of all the limits

        // Things that cause us to degenerate to a single beat
        val fixed = a.bits.burst === AXI4Parameters.BURST_FIXED
        val narrow = a.bits.size =/= lgBytes.U
        val bad = fixed || narrow

        // The number of beats-1 to execute
        val beats1 = Mux(bad, 0.U, maxSupported1)
        val beats = OH1ToOH(beats1) // beats1 + 1

        val inc_addr = addr + (beats << a.bits.size) // address after adding transfer
        val wrapMask = a.bits.bytes1() // only these bits may change, if wrapping
        val mux_addr = WireDefault(inc_addr)
        when (a.bits.burst === AXI4Parameters.BURST_WRAP) {
          mux_addr := (inc_addr & wrapMask) | ~(~a.bits.addr | wrapMask)
        }
        when (a.bits.burst === AXI4Parameters.BURST_FIXED) {
          mux_addr := a.bits.addr
        }

        val last = beats1 === len
        a.ready := out.ready && last
        out.valid := a.valid

        out.bits :<= a.bits
        out.bits.len := beats1

        // We forcibly align every access. If the first beat was misaligned, the strb bits
        // for the lower addresses must not have been set. Therefore, rounding the address
        // down is harmless. We can do this after the address update algorithm, because the
        // incremented values will be rounded down the same way. Furthermore, a subword
        // offset cannot cause a premature wrap-around.
        out.bits.addr := ~(~addr | UIntToOH1(a.bits.size, lgBytes))

        when (out.fire) {
          busy := !last
          r_addr := mux_addr
          r_len  := len - beats
        }

        (out, last, beats)
      }

      // The size to which we will fragment the access
      val readSizes1  = slaves.map(s => s.supportsRead .max/beatBytes-1)
      val writeSizes1 = slaves.map(s => s.supportsWrite.max/beatBytes-1)

      // Irrevocable queues in front because we want to accept the request before responses come back
      val (in_ar, ar_last, _)       = fragment(Queue.irrevocable(in.ar, 1, flow=true), readSizes1)
      val (in_aw, aw_last, w_beats) = fragment(Queue.irrevocable(in.aw, 1, flow=true), writeSizes1)

      // AXI ready may not depend on valid of other channels
      // We cut wready here along with awready and arready before AXI4ToTL
      val in_w = Queue.irrevocable(in.w, 1, flow=true)

      // AR flow control; super easy
      Connectable.waiveUnmatched(out.ar, in_ar) match {
        case (lhs, rhs) => lhs :<>= rhs
      }
      out.ar.bits.echo(AXI4FragLast) := ar_last

      // When does W channel start counting a new transfer
      val wbeats_latched = RegInit(false.B)
      val wbeats_ready = Wire(Bool())
      val wbeats_valid = Wire(Bool())
      when (wbeats_valid && wbeats_ready) { wbeats_latched := true.B }
      when (out.aw.fire) { wbeats_latched := false.B }

      // AW flow control
      out.aw.valid := in_aw.valid && (wbeats_ready || wbeats_latched)
      in_aw.ready := out.aw.ready && (wbeats_ready || wbeats_latched)
      wbeats_valid := in_aw.valid && !wbeats_latched
      Connectable.waiveUnmatched(out.aw.bits, in_aw.bits) match {
        case (lhs, rhs) => lhs :<>= rhs
      }
      out.aw.bits.echo(AXI4FragLast) := aw_last

      // We need to inject 'last' into the W channel fragments, count!
      val w_counter = RegInit(0.U((AXI4Parameters.lenBits+1).W))
      val w_idle = w_counter === 0.U
      val w_todo = Mux(w_idle, Mux(wbeats_valid, w_beats, 0.U), w_counter)
      val w_last = w_todo === 1.U
      w_counter := w_todo - out.w.fire
      assert (!out.w.fire || w_todo =/= 0.U) // underflow impossible

      // W flow control
      wbeats_ready := w_idle
      out.w.valid := in_w.valid && (!wbeats_ready || wbeats_valid)
      in_w.ready := out.w.ready && (!wbeats_ready || wbeats_valid)
      out.w.bits :<= in_w.bits
      out.w.bits.last := w_last
      // We should also recreate the last last
      assert (!out.w.valid || !in_w.bits.last || w_last)

      // R flow control
      val r_last = out.r.bits.echo(AXI4FragLast)
      Connectable.waiveUnmatched(in.r, out.r) match {
        case (lhs, rhs) => lhs :<>= rhs
      }

      in.r.bits.last := out.r.bits.last && r_last

      // B flow control
      val b_last = out.b.bits.echo(AXI4FragLast)

      Connectable.waiveUnmatched(in.b, out.b) match {
        case (lhs, rhs) => lhs :<>= rhs
      }

      in.b.valid := out.b.valid && b_last
      out.b.ready := in.b.ready || !b_last

      // Merge errors from dropped B responses
      val error = RegInit(VecInit.fill(edgeIn.master.endId)(0.U(AXI4Parameters.respBits.W)))
      in.b.bits.resp := out.b.bits.resp | error(out.b.bits.id)
      (error zip UIntToOH(out.b.bits.id, edgeIn.master.endId).asBools) foreach { case (reg, sel) =>
        when (sel && out.b.fire) { reg := Mux(b_last, 0.U, reg | out.b.bits.resp) }
      }
    }
  }
}

object AXI4Fragmenter
{
  def apply()(implicit p: Parameters): AXI4Node =
  {
    val axi4frag = LazyModule(new AXI4Fragmenter)
    axi4frag.node
  }
}
