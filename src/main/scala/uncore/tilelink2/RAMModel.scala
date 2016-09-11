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

    // !!! support multiple clients via clock division
    require (io.out.size == 1)

    val in = io.in(0)
    val out = io.out(0)

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

    class ByteMonitor extends Bundle {
      val valid = Bool()
      val value = UInt(width = 8)
    }
    class FlightMonitor extends Bundle {
      val base    = UInt(width = addressBits)
      val size    = UInt(width = sizeBits)
      val opcode  = UInt(width = 3)
    }

    // Infer as simple dual port BRAM/M10k with write-first/new-data semantics (bypass needed)
    val shadow = Seq.fill(beatBytes) { Mem(endAddressHi, new ByteMonitor) }
    val inc_bytes = Seq.fill(beatBytes) { Mem(endAddressHi, UInt(width = countBits)) }
    val dec_bytes = Seq.fill(beatBytes) { Mem(endAddressHi, UInt(width = countBits)) }
    val inc_trees = Seq.tabulate(decTrees) { i => Mem(endAddressHi >> (i+1), UInt(width = countBits)) }
    val dec_trees = Seq.tabulate(decTrees) { i => Mem(endAddressHi >> (i+1), UInt(width = countBits)) }

    val shadow_wen = Wire(init = Fill(beatBytes, wipe))
    val inc_bytes_wen = Wire(init = Fill(beatBytes, wipe))
    val dec_bytes_wen = Wire(init = Fill(beatBytes, wipe))
    val inc_trees_wen = Wire(init = Fill(decTrees, wipe))
    val dec_trees_wen = Wire(init = Fill(decTrees, wipe))

    // This requires either distributed memory or registers (no register on either input or output)
    val flight = Mem(endSourceId, new FlightMonitor)

    // We want to cross flight data from A to D in the same cycle (for combinational TL2 devices)
    val a_flight = Wire(new FlightMonitor)
    a_flight.base   := edge.address(in.a.bits)
    a_flight.size   := edge.size(in.a.bits)
    a_flight.opcode := in.a.bits.opcode

    flight.write(in.a.bits.source, a_flight)
    val bypass = in.a.valid && in.a.bits.source === out.d.bits.source
    val d_flight = RegNext(Mux(bypass, a_flight, flight.read(out.d.bits.source)))

    // Process A access requests
    val a = Reg(next = in.a.bits)
    val a_fire = Reg(next = in.a.fire(), init = Bool(false))
    val a_beats1 = edge.numBeats1(a)
    val a_size = edge.size(a)
    val a_sizeOH = UIntToOH(a_size)
    val a_counter = RegInit(UInt(0, width = maxLgBeats))
    val a_counter1 = a_counter - UInt(1)
    val a_first = a_counter === UInt(0)
    val a_addr_hi = a.addr_hi | (a_beats1 & ~a_counter1)
    val a_base = edge.address(a)
    val a_mask = edge.mask(a_base, a_size)

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
      a_counter := Mux(a_first, a_beats1, a_counter1)

      // !!! atomics
      assert (a.opcode =/= TLMessages.Acquire)

      // Increase the per-byte flight counter for the whole transaction
      when (a_first && a.opcode =/= TLMessages.Hint) {
        when (a_size <= UInt(shift)) {
          inc_bytes_wen := a_mask
        }
        inc_trees_wen := a_sizeOH >> (shift+1)
      }

      when (a.opcode === TLMessages.PutFullData || a.opcode === TLMessages.PutPartialData) {
        shadow_wen := a.mask
        for (i <- 0 until beatBytes) {
          val busy = a_inc(i) - a_dec(i) - (!a_first).asUInt
          val byte = a.data(8*(i+1)-1, 8*i)
          when (a.mask(i)) {
            printf("P 0x%x := 0x%x #%d\n", a_addr_hi << shift | UInt(i), byte, busy)
          }
        }
      }
    }

    val a_waddr = Mux(wipe, wipeIndex, a_addr_hi)
    for (i <- 0 until beatBytes) {
      val data = Wire(new ByteMonitor)
      val busy = a_inc(i) - a_dec(i) - (!a_first).asUInt
      data.valid := Mux(wipe, Bool(false), busy === UInt(0))
      data.value := a.data(8*(i+1)-1, 8*i)
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
    val d_bypass = a_fire && d.source === a.source
    val d_beats1 = edge.numBeats1(d)
    val d_size = edge.size(d)
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

    when (d_fire) {
      assert (d_size === d_flight.size)
      d_counter := Mux(d_first, d_beats1, d_counter1)

      when (d_flight.opcode === TLMessages.Hint) {
        assert (d.opcode === TLMessages.HintAck)
      }

      // Decreaes the per-byte flight counter for the whole transaction
      when (d_last && d_flight.opcode =/= TLMessages.Hint) {
        when (d_size <= UInt(shift)) {
          dec_bytes_wen := d_mask
        }
        dec_trees_wen := d_sizeOH >> (shift+1)
      }

      when (d_flight.opcode === TLMessages.PutFullData || d_flight.opcode === TLMessages.PutPartialData) {
        assert (d.opcode === TLMessages.AccessAck)
      }

      // !!! atomics

      when (d_flight.opcode === TLMessages.Get) {
        assert (d.opcode === TLMessages.AccessAckData)
        for (i <- 0 until beatBytes) {
          val got = d.data(8*(i+1)-1, 8*i)
          val shadow = Wire(init = d_shadow(i))
          when (d_mask(i)) {
            when (!shadow.valid) {
              printf("G 0x%x := undefined\n", d_addr_hi << shift | UInt(i))
            } .otherwise {
              printf("G 0x%x := 0x%x\n", d_addr_hi << shift | UInt(i), shadow.value)
              assert (shadow.value === got)
            }
          }
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
