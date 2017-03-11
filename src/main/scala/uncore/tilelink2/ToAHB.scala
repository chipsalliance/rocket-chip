// See LICENSE.SiFive for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import config._
import diplomacy._
import uncore.ahb._
import scala.math.{min, max}
import AHBParameters._

case class TLToAHBNode() extends MixedAdapterNode(TLImp, AHBImp)(
  dFn = { case TLClientPortParameters(clients, unsafeAtomics, minLatency) =>
    val masters = clients.map { case c => AHBMasterParameters(nodePath = c.nodePath) }
    AHBMasterPortParameters(masters)
  },
  uFn = { case AHBSlavePortParameters(slaves, beatBytes) =>
    val managers = slaves.map { case s =>
      TLManagerParameters(
        address            = s.address,
        resources          = s.resources,
        regionType         = s.regionType,
        executable         = s.executable,
        nodePath           = s.nodePath,
        supportsGet        = s.supportsRead,
        supportsPutFull    = s.supportsWrite, // but not PutPartial
        fifoId             = Some(0)) // a common FIFO domain
    }
    TLManagerPortParameters(managers, beatBytes, 1, 1)
  })

class TLToAHB(val combinational: Boolean = true)(implicit p: Parameters) extends LazyModule
{
  val node = TLToAHBNode()

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in = node.bundleIn
      val out = node.bundleOut
    }

    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>
      val beatBytes = edgeOut.slave.beatBytes
      val maxTransfer = edgeOut.slave.maxTransfer
      val lgMax = log2Ceil(maxTransfer)
      val lgBytes = log2Ceil(beatBytes)

      // AHB has no cache coherence
      in.b.valid := Bool(false)
      in.c.ready := Bool(true)
      in.e.ready := Bool(true)

      // We need a skidpad to capture D output:
      // We cannot know if the D response will be accepted until we have
      // presented it on D as valid.  We also can't back-pressure AHB in the
      // data phase.  Therefore, we must have enough space to save the data
      // phase result.  Whenever we have a queued response, we can not allow
      // AHB to present new responses, so we must quash the address phase.
      val d = Wire(in.d)
      in.d <> Queue(d, 1, flow = true)
      val a_quash = in.d.valid && !in.d.ready

      // Record what is coming out in d_phase
      val d_valid   = RegInit(Bool(false))
      val d_hasData = Reg(Bool())
      val d_error   = Reg(Bool())
      val d_addr_lo = Reg(UInt(width = lgBytes))
      val d_source  = Reg(UInt())
      val d_size    = Reg(UInt())

      when (out.hreadyout) { d_error := d_error || out.hresp }
      when (d.fire()) { d_valid := Bool(false) }

      d.valid := d_valid && out.hreadyout
      d.bits  := edgeIn.AccessAck(d_addr_lo, UInt(0), d_source, d_size, out.hrdata, out.hresp || d_error)
      d.bits.opcode := Mux(d_hasData, TLMessages.AccessAckData, TLMessages.AccessAck)

      // We need an irrevocable input for AHB to stall on read bursts
      // We also need the values to NOT change when valid goes low => 1 entry only
      val a = Queue(in.a, 1, flow = combinational, pipe = !combinational)
      val a_valid = a.valid && !a_quash

      // This is lot like TLEdge.firstlast, but counts beats also for single-beat TL types
      val a_size = edgeIn.size(a.bits)
      val a_beats1 = UIntToOH1(a_size, lgMax) >> lgBytes
      val a_counter = RegInit(UInt(0, width = log2Up(maxTransfer/beatBytes)))
      val a_counter1 = a_counter - UInt(1)
      val a_first = a_counter === UInt(0)
      val a_last = a_counter === UInt(1) || a_beats1 === UInt(0)
      val a_offset = (a_beats1 & ~a_counter1) << lgBytes
      val a_hasData = edgeIn.hasData(a.bits)

      // Expand no-data A-channel requests into multiple beats
      a.ready := (a_hasData || a_last) && out.hreadyout && !a_quash
      when (a_valid && out.hreadyout) {
        a_counter := Mux(a_first, a_beats1, a_counter1)
        d_valid := !a_hasData || a_last
        // Record what will be in the data phase
        when (a_first) {
          d_hasData := !a_hasData
          d_error   := Bool(false)
          d_addr_lo := a.bits.address
          d_source  := a.bits.source
          d_size    := a.bits.size
        }
      }

      // Transform TL size into AHB hsize+hburst
      val a_size_bits = a_size.getWidth
      val a_sizeDelta = Cat(UInt(0, width = 1), a_size) - UInt(lgBytes+1)
      val a_singleBeat = a_sizeDelta(a_size_bits)
      val a_logBeats1 = a_sizeDelta(a_size_bits-1, 0)

      out.hmastlock := Bool(false) // for now
      out.htrans    := Mux(a_valid, Mux(a_first, TRANS_NONSEQ, TRANS_SEQ), Mux(a_first, TRANS_IDLE, TRANS_BUSY))
      out.hsel      := a_valid || !a_first
      out.hready    := out.hreadyout
      out.hwrite    := a_hasData
      out.haddr     := a.bits.address | a_offset
      out.hsize     := Mux(a_singleBeat, a.bits.size, UInt(lgBytes))
      out.hburst    := Mux(a_singleBeat, BURST_SINGLE, (a_logBeats1<<1) | UInt(1))
      out.hprot     := PROT_DEFAULT
      out.hwdata    := RegEnable(a.bits.data, a.fire())
    }
  }
}

object TLToAHB
{
  // applied to the TL source node; y.node := TLToAHB()(x.node)
  def apply(combinational: Boolean = true)(x: TLOutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): AHBOutwardNode = {
    val ahb = LazyModule(new TLToAHB(combinational))
    ahb.node := x
    ahb.node
  }
}
