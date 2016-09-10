// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import chisel3.util.LFSR16

class TLRAMModel extends LazyModule
{
  val node = TLIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in = node.bundleIn
      val out = node.bundleOut
    }

    // Pass through all signals unchanged
    io.out <> io.in
    require (io.out.size == 1) // !!! support multiple clients

    val edge         = node.edgesIn(0)
    val endAddress   = edge.manager.maxAddress + 1
    val endSourceId  = edge.client.endSourceId
    val maxTransfer  = edge.manager.maxTransfer
    val beatBytes    = edge.manager.beatBytes
    val endAddressHi = (endAddress / beatBytes).intValue
    val maxLgBeats   = log2Up(maxTransfer/beatBytes)
    val shift        = log2Ceil(beatBytes)
    val decTrees     = log2Ceil(maxTransfer/beatBytes)
    val addressBits  = log2Up(endAddress)
    val countBits    = log2Up(endSourceId)
    val sizeBits     = edge.bundle.sizeBits

    class ByteMonitor extends Bundle {
      val valid = Bool()
      val value = UInt(width = 8)
    }
    class FlightMonitor extends Bundle {
      val base    = UInt(width = addressBits)
      val size    = UInt(width = sizeBits)
      val opcode  = UInt(width = 3)
    }

    // !!! Must somehow power-on with these all initialized with 0
    val shadow = Seq.fill(beatBytes) { Mem(endAddressHi, new ByteMonitor) }
    val inc_bytes = Seq.fill(beatBytes) { Mem(endAddressHi, UInt(width = countBits)) }
    val dec_bytes = Seq.fill(beatBytes) { Mem(endAddressHi, UInt(width = countBits)) }
    val inc_trees = Seq.tabulate(decTrees) { i => Mem(endAddressHi >> (i+1), UInt(width = countBits)) }
    val dec_trees = Seq.tabulate(decTrees) { i => Mem(endAddressHi >> (i+1), UInt(width = countBits)) }
    
    // Don't care on power-up
    val flight = Reg(Vec(endSourceId, new FlightMonitor))

    // Process A access requests
    val a = RegNext(io.in(0).a)
    val a_beats1 = edge.numBeats1(a.bits)
    val a_size = edge.size(a.bits)
    val a_sizeOH = UIntToOH(a_size)
    val a_counter = RegInit(UInt(0, width = maxLgBeats))
    val a_counter1 = a_counter - UInt(1)
    val a_first = a_counter === UInt(0)
    val a_addr_hi = a.bits.addr_hi | (a_beats1 & ~a_counter1)
    val a_base = edge.address(a.bits)
    val a_mask = edge.mask(a_base, a_size)

    // What is the request?
    val a_flight = Wire(new FlightMonitor)
    a_flight.base   := a_base
    a_flight.size   := a_size
    a_flight.opcode := a.bits.opcode

    // Grab the concurrency state we need
    val a_inc_bytes = inc_bytes.map(_.read(a_addr_hi))
    val a_dec_bytes = dec_bytes.map(_.read(a_addr_hi))
    val a_inc_trees = inc_trees.zipWithIndex.map{ case (m, i) => m.read(a_addr_hi >> (i+1)) }
    val a_dec_trees = dec_trees.zipWithIndex.map{ case (m, i) => m.read(a_addr_hi >> (i+1)) }
    val a_inc_tree = a_inc_trees.fold(UInt(0))(_ + _)
    val a_dec_tree = a_dec_trees.fold(UInt(0))(_ + _)
    val a_inc = a_inc_bytes.map(_ + a_inc_tree)
    val a_dec = a_dec_bytes.map(_ + a_dec_tree)

    when (a.fire()) {
      // Record the request so we can handle it's response
      flight(a.bits.source) := a_flight
      a_counter := Mux(a_first, a_beats1, a_counter1)

      // !!! atomics
      assert (a.bits.opcode =/= TLMessages.Acquire)

      // Increase the per-byte flight counter for the whole transaction
      when (a_first && a.bits.opcode =/= TLMessages.Hint) {
        when (a_size <= UInt(shift)) {
          for (i <- 0 until beatBytes) {
            when (a_mask(i)) { // not a.bits.mask; the full mask
              inc_bytes(i).write(a_addr_hi, a_inc_bytes(i) + UInt(1))
            }
          }
        }
        for (i <- 0 until inc_trees.size) {
          when (a_sizeOH(i+shift+1)) {
            inc_trees(i).write(a_addr_hi >> (i+1), a_inc_trees(i) + UInt(1))
          }
        }
      }

      when (a.bits.opcode === TLMessages.PutFullData || a.bits.opcode === TLMessages.PutPartialData) {
        for (i <- 0 until beatBytes) {
          val set = Wire(new ByteMonitor)
          val busy = a_inc(i) - a_dec(i) - (!a_first).asUInt
          set.valid := busy === UInt(0)
          set.value := a.bits.data(8*(i+1)-1, 8*i)
          when (a.bits.mask(i)) {
            shadow(i).write(a_addr_hi, set)
            printf("P 0x%x := 0x%x #%d\n", a_addr_hi << shift | UInt(i), set.value, busy)
          }
        }
      }
    }

    // Process D access responses
    val d = RegNext(io.out(0).d)
    val d_bypass = a.valid && d.bits.source === a.bits.source
    val d_flight = Mux(d_bypass, a_flight, flight(d.bits.source))
    val d_beats1 = edge.numBeats1(d.bits)
    val d_size = edge.size(d.bits)
    val d_sizeOH = UIntToOH(d_size)
    val d_counter = RegInit(UInt(0, width = maxLgBeats))
    val d_counter1 = d_counter - UInt(1)
    val d_first = d_counter === UInt(0)
    val d_last  = d_counter === UInt(1) || d_beats1 === UInt(0)
    val d_base = d_flight.base
    val d_addr_hi = d_base >> shift | (d_beats1 & ~d_counter1)
    val d_mask = edge.mask(d_base, d_size)

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

    when (d.fire()) {
      assert (d_size === d_flight.size)
      d_counter := Mux(d_first, d_beats1, d_counter1)

      when (d_flight.opcode === TLMessages.Hint) {
        assert (d.bits.opcode === TLMessages.HintAck)
      }

      // Decreaes the per-byte flight counter for the whole transaction
      when (d_last && d_flight.opcode =/= TLMessages.Hint) {
        when (d_size <= UInt(shift)) {
          for (i <- 0 until beatBytes) {
            when (d_mask(i)) {
              dec_bytes(i).write(d_addr_hi, d_dec_bytes(i) + UInt(1))
            }
          }
        }
        for (i <- 0 until dec_trees.size) {
          when (d_sizeOH(i+shift+1)) {
            dec_trees(i).write(d_addr_hi >> (i+1), d_dec_trees(i) + UInt(1))
          }
        }
      }

      when (d_flight.opcode === TLMessages.PutFullData || d_flight.opcode === TLMessages.PutPartialData) {
        assert (d.bits.opcode === TLMessages.AccessAck)
      }

      // !!! atomics

      when (d_flight.opcode === TLMessages.Get) {
        assert (d.bits.opcode === TLMessages.AccessAckData)
        for (i <- 0 until beatBytes) {
          val got = d.bits.data(8*(i+1)-1, 8*i)
          val shadow = Wire(init = d_shadow(i))
          when (d_mask(i)) {
            when (!shadow.valid) {
              printf("G 0x%x := undefined due to concurrent accesses\n", d_addr_hi << shift | UInt(i))
            } .otherwise {
              printf("G 0x%x := 0x%x\n", d_addr_hi << shift | UInt(i), shadow.value)
              assert (shadow.value === got)
            }
          }
        }
      }
    }
  }
}
