// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

// We detect concurrent puts that put memory into an undefined state.
// put0, put0Ack, put1, put1Ack => ok: defined
// put0, put1, put0Ack, put1Ack => ok: put1 clears valid (it sees busy>0)	defined for FIFO
// put0, put1, put1Ack, put0Ack => ok: put1 clears valid (it sees busy>0)	defined for FIFO
// When the region is FIFO, all writes leave 'valid' set (concurrent puts have defined behaviour)

// We detect concurrent puts that invalidate an inflight get.
// get, getAck, put, putAck => ok: defined
// get, put, getAck, putAck => ok: detected by getAck (it sees busy>0)
// get, put, putAck, getAck => ok: putAck uses CAM to wipe get validity		impossible for FIFO
// put, putAck, get, getAck => ok: defined
// put, get, putAck, getAck => ok: putAck uses CAM to wipe get validity		defined for FIFO
// put, get, getAck, putAck => ok: detected by getAck (it sees busy>0)		impossible for FIFO
// If FIFO, the getAck should check data even if its validity was wiped

class TLRAMModel(log: String = "", ignoreCorruptData: Boolean = false, ignoreDeniedData: Boolean = true)(implicit p: Parameters) extends LazyModule
{
  val node = TLAdapterNode()

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val edge         = edgeIn
      val endAddress   = edge.manager.maxAddress + 1
      val endSourceId  = edge.client.endSourceId
      val maxTransfer  = edge.manager.maxTransfer
      val beatBytes    = edge.manager.beatBytes
      val endAddressHi = (endAddress / beatBytes).intValue
      val maxLgBeats   = log2Up(maxTransfer/beatBytes)
      val shift        = log2Ceil(beatBytes)
      val decTrees     = log2Up(maxTransfer/beatBytes)
      val addressBits  = log2Up(endAddress)
      val countBits    = log2Up(endSourceId)
      val sizeBits     = edge.bundle.sizeBits
      val divisor      = CRC.CRC_16F_4_2

      // Reset control logic
      val wipeIndex = RegInit(UInt(0, width = log2Ceil(endAddressHi) + 1))
      val wipe = !wipeIndex(log2Ceil(endAddressHi))
      wipeIndex := wipeIndex + wipe.asUInt

      // Block traffic while wiping Mems
      in.a.ready := out.a.ready && !wipe
      out.a.valid := in.a.valid && !wipe
      out.a.bits  := in.a.bits
      out.d.ready := in.d.ready && !wipe
      in.d.valid := out.d.valid && !wipe
      in.d.bits  := out.d.bits

      // BCE unsupported
      in.b.valid := Bool(false)
      out.c.valid := Bool(false)
      out.e.valid := Bool(false)
      out.b.ready := Bool(true)
      in.c.ready := Bool(true)
      in.e.ready := Bool(true)

      val params = TLRAMModel.MonitorParameters(addressBits, sizeBits)

      // Infer as simple dual port BRAM/M10k with write-first/new-data semantics (bypass needed)
      val shadow = Seq.fill(beatBytes) { Mem(endAddressHi, new TLRAMModel.ByteMonitor(params)) }
      val inc_bytes = Seq.fill(beatBytes) { Mem(endAddressHi, UInt(width = countBits)) }
      val dec_bytes = Seq.fill(beatBytes) { Mem(endAddressHi, UInt(width = countBits)) }
      val inc_trees = Seq.tabulate(decTrees) { i => Mem(endAddressHi >> (i+1), UInt(width = countBits)) }
      val dec_trees = Seq.tabulate(decTrees) { i => Mem(endAddressHi >> (i+1), UInt(width = countBits)) }

      val shadow_wen = Wire(init = Fill(beatBytes, wipe))
      val inc_bytes_wen = Wire(init = Fill(beatBytes, wipe))
      val dec_bytes_wen = Wire(init = Fill(beatBytes, wipe))
      val inc_trees_wen = Wire(init = Fill(decTrees, wipe))
      val dec_trees_wen = Wire(init = Fill(decTrees, wipe))

      // This must be registers b/c we build a CAM from it
      val flight = Reg(Vec(endSourceId, new TLRAMModel.FlightMonitor(params)))
      val valid = Reg(Vec(endSourceId, Bool()))

      // We want to cross flight data from A to D in the same cycle (for combinational TL2 devices)
      val a_flight = Wire(new TLRAMModel.FlightMonitor(params))
      a_flight.base   := edge.address(in.a.bits)
      a_flight.size   := edge.size(in.a.bits)
      a_flight.opcode := in.a.bits.opcode

      when (in.a.fire()) { flight(in.a.bits.source) := a_flight }
      val bypass = if (edge.manager.minLatency > 0) Bool(false) else in.a.valid && in.a.bits.source === out.d.bits.source
      val d_flight = RegEnable(Mux(bypass, a_flight, flight(out.d.bits.source)), edge.first(out.d))

      // Process A access requests
      val a = Reg(next = in.a.bits)
      val a_fire = Reg(next = in.a.fire(), init = Bool(false))
      val (a_first, a_last, _, a_address_inc) = edge.addr_inc(a, a_fire)
      val a_size = edge.size(a)
      val a_sizeOH = UIntToOH(a_size)
      val a_address = a.address | a_address_inc
      val a_addr_hi = edge.addr_hi(a_address)
      val a_base = edge.address(a)
      val a_mask = edge.mask(a_base, a_size)
      val a_fifo = edge.manager.hasFifoIdFast(a_base) && edge.client.requestFifo(a.source)

      // Grab the concurrency state we need
      val a_inc_bytes = inc_bytes.map(_.read(a_addr_hi))
      val a_dec_bytes = dec_bytes.map(_.read(a_addr_hi))
      val a_inc_trees = inc_trees.zipWithIndex.map{ case (m, i) => m.read(a_addr_hi >> (i+1)) }
      val a_dec_trees = dec_trees.zipWithIndex.map{ case (m, i) => m.read(a_addr_hi >> (i+1)) }
      val a_inc_tree = a_inc_trees.fold(UInt(0))(_ + _)
      val a_dec_tree = a_dec_trees.fold(UInt(0))(_ + _)
      val a_inc = a_inc_bytes.map(_ + a_inc_tree)
      val a_dec = a_dec_bytes.map(_ + a_dec_tree)

      when (a_fire) {
        // Record the request so we can handle it's response
        assert (a.opcode =/= TLMessages.AcquireBlock && a.opcode =/= TLMessages.AcquirePerm)

        // Mark the operation as valid
        valid(a.source) := Bool(true)

        // Increase the per-byte flight counter for the whole transaction
        when (a_first && a.opcode =/= TLMessages.Hint && a.opcode =/= TLMessages.Get) {
          when (a_size <= UInt(shift)) {
            inc_bytes_wen := a_mask
          }
          inc_trees_wen := a_sizeOH >> (shift+1)
        }

        when (a.opcode === TLMessages.PutFullData || a.opcode === TLMessages.PutPartialData ||
              a.opcode === TLMessages.ArithmeticData || a.opcode === TLMessages.LogicalData) {
          shadow_wen := a.mask
          for (i <- 0 until beatBytes) {
            val busy = a_inc(i) - a_dec(i) - (!a_first).asUInt
            val byte = a.data(8*(i+1)-1, 8*i)
            when (a.mask(i)) {
              printf(log + " ")
              when (a.opcode === TLMessages.PutFullData) { printf("PF") }
              when (a.opcode === TLMessages.PutPartialData) { printf("PP") }
              when (a.opcode === TLMessages.ArithmeticData) { printf("A ") }
              when (a.opcode === TLMessages.LogicalData) { printf("L ") }
              printf(" 0x%x := 0x%x #%d %x\n", a_addr_hi << shift | UInt(i), byte, busy, a.param)
            }
          }
        }

        when (a.opcode === TLMessages.Get) {
          printf(log + " G  0x%x - 0x%x\n", a_base, a_base | UIntToOH1(a_size, addressBits))
        }
      }

      val a_waddr = Mux(wipe, wipeIndex, a_addr_hi)
      val a_shadow = shadow.map(_.read(a_waddr))
      val a_known_old = !(Cat(a_shadow.map(!_.valid).reverse) & a_mask).orR
      val alu = Module(new Atomics(a.params))
      alu.io.write := Bool(false)
      alu.io.a := a
      alu.io.data_in := Cat(a_shadow.map(_.value).reverse)

      val crc = Mem(endSourceId, UInt(width = 16))
      val crc_valid = Mem(endSourceId, Bool())
      val a_crc_acc = Mux(a_first, UInt(0), crc(a.source))
      val a_crc_new = Cat(a_shadow.zipWithIndex.map { case (z, i) => Mux(a_mask(i), z.value, UInt(0)) }.reverse)
      val a_crc = CRC(divisor, Cat(a_crc_acc, a_crc_new), 16 + beatBytes*8)
      val a_crc_valid = a_known_old && Mux(a_first, Bool(true), crc_valid(a.source))
      when (a_fire) {
        crc.write(a.source, a_crc)
        crc_valid.write(a.source, a_crc_valid)
      }

      for (i <- 0 until beatBytes) {
        val data = Wire(new TLRAMModel.ByteMonitor(params))
        val busy = a_inc(i) =/= a_dec(i) + (!a_first).asUInt
        val amo = a.opcode === TLMessages.ArithmeticData || a.opcode === TLMessages.LogicalData
        val beat_amo = a.size <= UInt(log2Ceil(beatBytes))
        data.valid := Mux(wipe, Bool(false), (!busy || a_fifo) && (!amo || (a_known_old && beat_amo)))
        data.value := alu.io.data_out(8*(i+1)-1, 8*i)
        when (shadow_wen(i)) {
          shadow(i).write(a_waddr, data)
        }
      }

      for (i <- 0 until beatBytes) {
        val data = Mux(wipe, UInt(0), a_inc_bytes(i) + UInt(1))
        when (inc_bytes_wen(i)) {
          inc_bytes(i).write(a_waddr, data)
        }
      }

      for (i <- 0 until inc_trees.size) {
        val data = Mux(wipe, UInt(0), a_inc_trees(i) + UInt(1))
        when (inc_trees_wen(i)) {
          inc_trees(i).write(a_waddr >> (i+1), data)
        }
      }

      // Process D access responses
      val d = RegNext(out.d.bits)
      val d_fire = Reg(next = out.d.fire(), init = Bool(false))
      val (d_first, d_last, _, d_address_inc) = edge.addr_inc(d, d_fire)
      val d_size = edge.size(d)
      val d_sizeOH = UIntToOH(d_size)
      val d_base = d_flight.base
      val d_address = d_base | d_address_inc
      val d_addr_hi = edge.addr_hi(d_address)
      val d_mask = edge.mask(d_base, d_size)
      val d_fifo = edge.manager.hasFifoIdFast(d_flight.base) && edge.client.requestFifo(d.source)

      // Grab the concurrency state we need
      val d_inc_bytes = inc_bytes.map(_.read(d_addr_hi))
      val d_dec_bytes = dec_bytes.map(_.read(d_addr_hi))
      val d_inc_trees = inc_trees.zipWithIndex.map{ case (m, i) => m.read(d_addr_hi >> (i+1)) }
      val d_dec_trees = dec_trees.zipWithIndex.map{ case (m, i) => m.read(d_addr_hi >> (i+1)) }
      val d_inc_tree = d_inc_trees.fold(UInt(0))(_ + _)
      val d_dec_tree = d_dec_trees.fold(UInt(0))(_ + _)
      val d_inc = d_inc_bytes.map(_ + d_inc_tree)
      val d_dec = d_dec_bytes.map(_ + d_dec_tree)
      val d_shadow = shadow.map(_.read(d_addr_hi))
      val d_valid = valid(d.source) holdUnless d_first

      // CRC check
      val d_crc_reg = Reg(UInt(width = 16))
      val d_crc_acc = Mux(d_first, UInt(0), d_crc_reg)
      val d_crc_new = FillInterleaved(8, d_mask) & d.data
      val d_crc = CRC(divisor, Cat(d_crc_acc, d_crc_new), 16 + beatBytes*8)
      val crc_bypass = if (edge.manager.minLatency > 0) Bool(false) else a_fire && a.source === d.source
      val d_crc_valid = Mux(crc_bypass, a_crc_valid, crc_valid.read(d.source)) holdUnless d_first
      val d_crc_check = Mux(crc_bypass, a_crc, crc.read(d.source)) holdUnless d_first

      val d_no_race_reg = Reg(Bool())
      val d_no_race = Wire(init = d_no_race_reg)

      when (d_fire) {
        d_crc_reg := d_crc
        d_no_race_reg := d_no_race

        // Check the response is correct
        assert (d_size === d_flight.size)
        // addr_lo is allowed to differ

        when (d_flight.opcode === TLMessages.Hint) {
          assert (d.opcode === TLMessages.HintAck)
        }

        // Decrease the per-byte flight counter for the whole transaction
        when (d_last && d_flight.opcode =/= TLMessages.Hint && d_flight.opcode =/= TLMessages.Get) {
          when (d_size <= UInt(shift)) {
            dec_bytes_wen := d_mask
          }
          dec_trees_wen := d_sizeOH >> (shift+1)
          // NOTE: D channel carries uninterrupted multibeast op, so updating on last is fine
          for (i <- 0 until endSourceId) {
            // Does this modification overlap a Get? => wipe it's valid
            val f_base = flight(i).base
            val f_size = flight(i).size
            val f_bits = UIntToOH1(f_size, addressBits)
            val d_bits = UIntToOH1(d_size, addressBits)
            val overlap = ~(~(f_base ^ d_base) | (f_bits | d_bits)) === UInt(0)
            when (overlap) { valid(i) := Bool(false) }
          }
        }

        when (d_flight.opcode === TLMessages.PutFullData || d_flight.opcode === TLMessages.PutPartialData) {
          assert (d.opcode === TLMessages.AccessAck)
          printf(log + " ")
          when (d_flight.opcode === TLMessages.PutFullData) { printf("pf") }
          when (d_flight.opcode === TLMessages.PutPartialData) { printf("pp") }
          printf(" 0x%x - 0x%x\n", d_base, d_base | UIntToOH1(d_size, addressBits))
        }

        when (d_flight.opcode === TLMessages.Get || d_flight.opcode === TLMessages.ArithmeticData || d_flight.opcode === TLMessages.LogicalData) {
          assert (d.opcode === TLMessages.AccessAckData)
          for (i <- 0 until beatBytes) {
            val got = d.data(8*(i+1)-1, 8*i)
            val shadow = Wire(init = d_shadow(i))
            when (d_mask(i)) {
              val d_addr = d_addr_hi << shift | UInt(i)
              printf(log + " ")
              when (d_flight.opcode === TLMessages.Get) { printf("g ") }
              when (d_flight.opcode === TLMessages.ArithmeticData) { printf("a ") }
              when (d_flight.opcode === TLMessages.LogicalData) { printf("l ") }
              printf(" 0x%x := 0x%x", d_addr, got)
              when (!shadow.valid) {
                printf(", undefined (uninitialized or prior overlapping puts)\n")
              } .elsewhen (d_inc(i) =/= d_dec(i)) {
                printf(", undefined (concurrent incomplete puts #%d)\n", d_inc(i) - d_dec(i))
              } .elsewhen (!d_fifo && !d_valid) {
                printf(", undefined (concurrent completed put)\n")
              } .elsewhen (Bool(ignoreDeniedData) && d.denied) {
                printf(", undefined (denied result)\n")
              } .elsewhen (Bool(ignoreCorruptData) && d.corrupt) {
                printf(", undefined (corrupt result)\n")
              } .otherwise {
                printf("\n")
                when (shadow.value =/= got) { printf("EXPECTED: 0x%x\n", shadow.value) }
                assert (shadow.value === got)
              }
            }
          }
        }

        when (d_flight.opcode === TLMessages.ArithmeticData || d_flight.opcode === TLMessages.LogicalData) {
          val race = (d_inc zip d_dec) map { case (i, d) => i - d =/= UInt(1) }
          when (d_first) { d_no_race := Bool(true) }
          when ((Cat(race.reverse) & d_mask).orR) { d_no_race := Bool(false) }
          when (d_last) {
            val must_match = d_crc_valid && (d_fifo || (d_valid && d_no_race))
            val corrupt = (Bool(ignoreCorruptData) && d.corrupt) || (Bool(ignoreDeniedData) && d.denied)
            printf(log + " crc = 0x%x %d\n", d_crc, must_match.asUInt)
            when (!corrupt && must_match && d_crc =/= d_crc_check) { printf("EXPECTED: 0x%x\n", d_crc_check) }
            assert (corrupt || !must_match || d_crc === d_crc_check)
          }
        }
      }

      val d_waddr = Mux(wipe, wipeIndex, d_addr_hi)
      for (i <- 0 until beatBytes) {
        val data = Mux(wipe, UInt(0), d_dec_bytes(i) + UInt(1))
        when (dec_bytes_wen(i)) {
          dec_bytes(i).write(d_waddr, data)
        }
      }

      for (i <- 0 until dec_trees.size) {
        val data = Mux(wipe, UInt(0), d_dec_trees(i) + UInt(1))
        when (dec_trees_wen(i)) {
          dec_trees(i).write(d_waddr >> (i+1), data)
        }
      }
    }
  }
}

object TLRAMModel
{
  def apply(log: String = "", ignoreCorruptData: Boolean = false, ignoreDeniedData: Boolean = true)(implicit p: Parameters): TLNode =
  {
    val model = LazyModule(new TLRAMModel(log, ignoreCorruptData, ignoreDeniedData))
    model.node
  }

  case class MonitorParameters(addressBits: Int, sizeBits: Int)

  class ByteMonitor(params: MonitorParameters) extends GenericParameterizedBundle(params) {
    val valid = Bool()
    val value = UInt(width = 8)
  }
  class FlightMonitor(params: MonitorParameters) extends GenericParameterizedBundle(params) {
    val base    = UInt(width = params.addressBits)
    val size    = UInt(width = params.sizeBits)
    val opcode  = UInt(width = 3)
  }
}
