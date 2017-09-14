// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.amba.ahb._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import scala.math.{min, max}
import AHBParameters._

case class TLToAHBNode()(implicit valName: ValName) extends MixedAdapterNode(TLImp, AHBImp)(
  dFn = { case TLClientPortParameters(clients, unsafeAtomics, minLatency) =>
    val masters = clients.map { case c => AHBMasterParameters(name = c.name, nodePath = c.nodePath) }
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
        fifoId             = Some(0))
    }
    TLManagerPortParameters(managers, beatBytes, 1, 1)
  })

class AHBControlBundle(params: TLEdge) extends GenericParameterizedBundle(params)
{
  val full   = Bool()
  val send   = Bool() // => full+data
  val first  = Bool()
  val last   = Bool()
  val write  = Bool()
  val size   = UInt(width = params.bundle.sizeBits)
  val source = UInt(width = params.bundle.sourceBits)
  val hsize  = UInt(width = AHBParameters.sizeBits)
  val hburst = UInt(width = AHBParameters.burstBits)
  val addr   = UInt(width = params.bundle.addressBits)
  val data   = UInt(width = params.bundle.dataBits)
}

// The input side has either a flow queue (aFlow=true) or a pipe queue (aFlow=false)
// The output side always has a flow queue
class TLToAHB(val aFlow: Boolean = false)(implicit p: Parameters) extends LazyModule
{
  val node = TLToAHBNode()

  lazy val module = new LazyModuleImp(this) {
   (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val beatBytes = edgeOut.slave.beatBytes
      val maxTransfer = edgeOut.slave.maxTransfer
      val lgMax = log2Ceil(maxTransfer)
      val lgBytes = log2Ceil(beatBytes)

      // Initial FSM state
      val resetState = Wire(new AHBControlBundle(edgeIn))
      resetState.full  := Bool(false)
      resetState.send  := Bool(false)
      resetState.first := Bool(true)
      // These are needed to appease AHB VIP:
      resetState.hsize := UInt(0)
      resetState.hburst:= UInt(0)
      resetState.addr  := UInt(0)

      // The stages of the combinational pipeline
      val reg  = RegInit(resetState)
      val send = Wire(init = reg)
      val step = Wire(init = send)
      val next = Wire(init = step)
      reg := next

      // Advance the FSM based on the result of this AHB beat
      when (send.send && !out.hreadyout) /* retry AHB */ {
        step.full  := Bool(true)
        step.send  := Bool(true)
      } .elsewhen (send.full && !send.send) /* retry beat */ {
        step.full  := Bool(true)
        step.send  := Bool(false)
      } .elsewhen (send.full && !send.last) /* continue burst */ {
        step.full  := Bool(true)
        step.send  := Bool(false) // => looks like a retry to injector
        step.first := Bool(false)
        step.last  := (if (lgBytes + 1 >= lgMax) Bool(true) else
                       !((UIntToOH1(send.size, lgMax) & ~send.addr) >> (lgBytes + 1)).orR())
        step.addr  := Cat(send.addr(edgeIn.bundle.addressBits-1, lgMax), send.addr(lgMax-1, 0) + UInt(beatBytes))
      } .otherwise /* new burst */ {
        step.full  := Bool(false)
        step.send  := Bool(false)
        step.first := Bool(true)
      }

      val d_block = Wire(Bool())
      val pre  = if (aFlow) reg else step
      val post = if (aFlow) send else next

      // Transform TL size into AHB hsize+hburst
      val a_sizeDelta = Cat(UInt(0, width = 1), in.a.bits.size) - UInt(lgBytes+1)
      val a_singleBeat = Bool(lgBytes >= lgMax) || a_sizeDelta(edgeIn.bundle.sizeBits)
      val a_logBeats1 = a_sizeDelta(edgeIn.bundle.sizeBits-1, 0)

      // Pulse this every time we commit to sending an AHB request
      val a_commit = Wire(Bool())

      // Inject A channel into FSM
      when (pre.send) /* busy */ {
        a_commit := Bool(false)
        in.a.ready := Bool(false)
      } .elsewhen (pre.full) /* retry beat (or continue burst) */ {
        post.send  := !d_block && (!pre.write || in.a.valid)
        post.data  := in.a.bits.data
        a_commit   := !d_block && !pre.write // only read beats commit to a D beat answer
        in.a.ready := !d_block && pre.write
      } .otherwise /* new burst */ {
        a_commit := in.a.fire() // every first beat commits to a D beat answer
        in.a.ready := !d_block
        when (in.a.fire()) {
          post.full  := Bool(true)
          post.send  := Bool(true)
          post.last  := a_singleBeat
          post.write := edgeIn.hasData(in.a.bits)
          post.size  := in.a.bits.size
          post.source:= in.a.bits.source
          post.hsize := Mux(a_singleBeat, in.a.bits.size, UInt(lgBytes))
          post.hburst:= Mux(a_singleBeat, BURST_SINGLE, (a_logBeats1<<1) | UInt(1))
          post.addr  := in.a.bits.address
          post.data  := in.a.bits.data
        }
      }

      out.hmastlock := Bool(false) // for now
      out.htrans    := Mux(send.send, Mux(send.first, TRANS_NONSEQ, TRANS_SEQ), Mux(send.first, TRANS_IDLE, TRANS_BUSY))
      out.hsel      := send.send || !send.first
      out.hready    := out.hreadyout
      out.hwrite    := send.write
      out.haddr     := send.addr
      out.hsize     := send.hsize
      out.hburst    := send.hburst
      out.hprot     := PROT_DEFAULT
      out.hwdata    := RegEnable(send.data, out.hreadyout)

      // We need a skidpad to capture D output:
      // We cannot know if the D response will be accepted until we have
      // presented it on D as valid.  We also can't back-pressure AHB in the
      // data phase.  Therefore, we must have enough space to save the all
      // commited AHB requests (A+D phases = 2). To decouple d_ready from
      // a_ready and htrans, we add another entry for aFlow=false.
      val depth = if (aFlow) 2 else 3
      val d = Wire(in.d)
      in.d <> Queue(d, depth, flow=true)
      assert (!d.valid || d.ready)

      val d_flight = RegInit(UInt(0, width = 2))
      assert (d_flight <= UInt(depth))
      d_flight := d_flight + a_commit.asUInt - in.d.fire().asUInt
      d_block := d_flight >= UInt(depth)

      val d_valid   = RegInit(Bool(false))
      val d_error   = Reg(Bool())
      val d_write   = RegEnable(send.write,  out.hreadyout)
      val d_source  = RegEnable(send.source, out.hreadyout)
      val d_size    = RegEnable(send.size,   out.hreadyout)

      when (out.hreadyout) {
        d_valid := send.send && (send.last || !send.write)
        when (out.hresp)  { d_error := d_write }
        when (send.first) { d_error := Bool(false) }
      }

      d.valid := d_valid && out.hreadyout
      d.bits  := edgeIn.AccessAck(d_source, d_size, out.hrdata, out.hresp || d_error)
      d.bits.opcode := Mux(d_write, TLMessages.AccessAck, TLMessages.AccessAckData)

      // AHB has no cache coherence
      in.b.valid := Bool(false)
      in.c.ready := Bool(true)
      in.e.ready := Bool(true)
    }
  }
}

object TLToAHB
{
  // applied to the TL source node; y.node := TLToAHB()(x.node)
  def apply(aFlow: Boolean = true)(x: TLOutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): AHBOutwardNode = {
    val ahb = LazyModule(new TLToAHB(aFlow))
    ahb.node :=? x
    ahb.node
  }
}
