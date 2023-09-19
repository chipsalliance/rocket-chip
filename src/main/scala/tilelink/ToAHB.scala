// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba._
import freechips.rocketchip.amba.ahb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import AHBParameters._
import chisel3.util.{RegEnable, Queue, Cat, log2Ceil}

case class TLToAHBNode(supportHints: Boolean)(implicit valName: ValName) extends MixedAdapterNode(TLImp, AHBImpMaster)(
  dFn = { cp =>
    AHBMasterPortParameters(
      masters = cp.clients.map { case c => AHBMasterParameters(name = c.name, nodePath = c.nodePath) },
      requestFields = cp.requestFields.filter(!_.isInstanceOf[AMBAProtField]),
      responseKeys  = cp.responseKeys)
  },
  uFn = { case sp =>
    val managers = sp.slaves.map { case s =>
      TLSlaveParameters.v1(
        address            = s.address,
        resources          = s.resources,
        regionType         = s.regionType,
        executable         = s.executable,
        nodePath           = s.nodePath,
        supportsGet        = s.supportsRead,
        supportsPutFull    = s.supportsWrite, // but not PutPartial
        supportsHint       = if (!supportHints) TransferSizes.none else
                             if (s.supportsRead) s.supportsRead    else
                             if (s.supportsWrite) s.supportsWrite  else
                             TransferSizes(1, sp.beatBytes),
        fifoId             = Some(0),
        mayDenyPut         = true)
    }
    TLSlavePortParameters.v1(
      managers   = managers,
      beatBytes  = sp.beatBytes,
      endSinkId  = 0,
      minLatency = 1,
      responseFields = sp.responseFields,
      requestKeys    = AMBAProt +: sp.requestKeys)
  })

class AHBControlBundle(val params: TLEdge) extends Bundle
{
  val full   = Bool()
  val send   = Bool() // => full+data
  val first  = Bool()
  val last   = Bool()
  val hint   = Bool()
  val write  = Bool()
  val size   = UInt(params.bundle.sizeBits.W)
  val source = UInt(params.bundle.sourceBits.W)
  val hsize  = UInt(AHBParameters.sizeBits.W)
  val hburst = UInt(AHBParameters.burstBits.W)
  val addr   = UInt(params.bundle.addressBits.W)
  val data   = UInt(params.bundle.dataBits.W)
  val hauser = BundleMap(params.bundle.requestFields)
  val echo   = BundleMap(params.bundle.echoFields)
}

// The input side has either a flow queue (aFlow=true) or a pipe queue (aFlow=false)
// The output side always has a flow queue
class TLToAHB(val aFlow: Boolean = false, val supportHints: Boolean = true, val supportsRETRY: Boolean = true)(implicit p: Parameters) extends LazyModule
{
  val node = TLToAHBNode(supportHints)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
   (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val beatBytes = edgeOut.slave.beatBytes
      val maxTransfer = edgeOut.slave.maxTransfer
      val lgMax = log2Ceil(maxTransfer)
      val lgBytes = log2Ceil(beatBytes)

      // Initial FSM state
      val resetState = Wire(new AHBControlBundle(edgeIn))
      resetState := DontCare
      resetState.full  := false.B
      resetState.send  := false.B
      resetState.first := true.B
      // These are needed to appease AHB VIP:
      resetState.hsize := 0.U
      resetState.hburst:= 0.U
      resetState.addr  := 0.U

      // The stages of the combinational pipeline
      val reg  = RegInit(resetState)
      val send = WireDefault(reg)
      val step = WireDefault(send)
      val next = WireDefault(step)
      reg := next

      // A- and D-phase readiness
      val a_flow = Wire(Bool())
      val d_flow = Wire(Bool())

      // Advance the FSM based on the result of this AHB beat
      when (send.send && !a_flow) /* retry AHB */ {
        step.full  := true.B
        step.send  := true.B
      } .elsewhen (send.full && !send.send) /* retry beat */ {
        step.full  := true.B
        step.send  := false.B
      } .elsewhen (send.full && !send.last) /* continue burst */ {
        step.full  := true.B
        step.send  := false.B // => looks like a retry to injector
        step.first := false.B
        step.last  := (if (lgBytes + 1 >= lgMax) true.B else
                       !((UIntToOH1(send.size, lgMax) & ~send.addr) >> (lgBytes + 1)).orR)
        step.addr  := Cat(send.addr(edgeIn.bundle.addressBits-1, lgMax), send.addr(lgMax-1, 0) + beatBytes.U)
      } .otherwise /* new burst */ {
        step.full  := false.B
        step.send  := false.B
        step.first := true.B
      }

      val d_block = Wire(Bool())
      val pre  = if (aFlow) reg else step
      val post = if (aFlow) send else next

      // Transform TL size into AHB hsize+hburst
      val a_sizeDelta = Cat(0.U(1.W), in.a.bits.size) - (lgBytes+1).U
      val a_hint = in.a.bits.opcode === TLMessages.Hint && supportHints.B
      val a_singleBeat = a_hint || (lgBytes >= lgMax).B || a_sizeDelta(edgeIn.bundle.sizeBits)
      val a_logBeats1 = a_sizeDelta(edgeIn.bundle.sizeBits-1, 0)

      // Pulse this every time we commit to sending an AHB request
      val a_commit = Wire(Bool())

      // Inject A channel into FSM
      when (pre.send) /* busy */ {
        a_commit := false.B
        in.a.ready := false.B
      } .elsewhen (pre.full) /* retry beat (or continue burst) */ {
        post.send  := !d_block && (!pre.write || in.a.valid)
        post.data  := in.a.bits.data
        a_commit   := !d_block && !pre.write // only read beats commit to a D beat answer
        in.a.ready := !d_block && pre.write
      } .otherwise /* new burst */ {
        a_commit := in.a.fire // every first beat commits to a D beat answer
        in.a.ready := !d_block
        when (in.a.fire) {
          post.full  := true.B
          post.send  := true.B
          post.last  := a_singleBeat
          post.hint  := a_hint
          post.size  := in.a.bits.size
          post.source:= in.a.bits.source
          post.hauser:<= in.a.bits.user
          post.echo  :<= in.a.bits.echo
        }
        when (in.a.fire && !a_hint) {
          post.write := edgeIn.hasData(in.a.bits)
          post.hsize := Mux(a_singleBeat, in.a.bits.size, lgBytes.U)
          post.hburst:= Mux(a_singleBeat, BURST_SINGLE, (a_logBeats1<<1) | 1.U)
          post.addr  := in.a.bits.address
          post.data  := in.a.bits.data
        }
      }

      // For SPLIT/RETRY, a burst being reissued from D-phase state
      val retry = Wire(Bool())

      val granted   = RegEnable(out.grant(), out.hready)
      val rebuild   = RegInit(false.B) // rewrite as NSEQ       (for next-beat  EBT)
      val increment = RegInit(false.B) // rewrite as BURST_INCR (for same-burst EBT)
      when (out.hready && granted && !retry) {
        when (send.send)    { rebuild := false.B }
        when (!out.grant()) { rebuild := true.B }
        when (out.busreq() && !out.grant()) { increment := true.B }
        when (send.send && send.last)       { increment := false.B }
      }

      out.lock()  := false.B // for now
      out.busreq():= (send.send && !send.hint) || !send.first
      out.htrans  := Mux(send.send && !send.hint,
                       Mux(send.first || rebuild, TRANS_NONSEQ, TRANS_SEQ),
                       Mux(send.first || rebuild, TRANS_IDLE,   TRANS_BUSY))
      out.hwrite  := send.write
      out.haddr   := send.addr
      out.hsize   := send.hsize
      out.hburst  := Mux(increment, BURST_INCR, send.hburst)
      out.hprot   := PROT_DEFAULT
      out.hauser  := send.hauser
      out.hwdata  := RegEnable(send.data, a_flow)

      // Set prot bits if we have extra meta-data
      send.hauser.lift(AMBAProt).foreach { x =>
        val hprot = Wire(Vec(4, Bool()))
        hprot(0) := !x.fetch
        hprot(1) :=  x.privileged
        hprot(2) :=  x.bufferable
        hprot(3) :=  x.modifiable && (x.readalloc || x.writealloc) // cacheable
        out.hprot := Cat(hprot.reverse)
      }

      // We need a skidpad to capture D output:
      // We cannot know if the D response will be accepted until we have
      // presented it on D as valid.  We also can't back-pressure AHB in the
      // data phase.  Therefore, we must have enough space to save the all
      // commited AHB requests (A+D phases = 2). To decouple d_ready from
      // a_ready and htrans, we add another entry for aFlow=false.
      val depth = if (aFlow) 2 else 3
      val d = Wire(new DecoupledIO(new TLBundleD(edgeIn.bundle)))
      in.d :<>= Queue(d, depth, flow=true)
      assert (!d.valid || d.ready)

      val d_flight = RegInit(0.U(2.W))
      assert (d_flight <= depth.U)
      d_flight := d_flight + a_commit.asUInt - in.d.fire.asUInt
      d_block := d_flight >= depth.U

      val d_valid   = RegInit(false.B)
      val d_denied  = Reg(Bool())
      val d_hint    = RegEnable(send.hint,   a_flow && send.send)
      val d_write   = RegEnable(send.write,  a_flow && send.send)
      val d_source  = RegEnable(send.source, a_flow && send.send)
      val d_size    = RegEnable(send.size,   a_flow && send.send)
      val d_echo    = RegEnable(send.echo,   a_flow && send.send)

      when (d_flow) {
        d_valid := send.send && (send.last || !send.write) && a_flow
        when (out.hresp(0))  { d_denied := true.B }
        when (send.first)    { d_denied := false.B }
      }

      d.valid := d_valid && d_flow
      d.bits  := edgeIn.AccessAck(d_source, d_size, out.hrdata)
      d.bits.opcode := Mux(d_hint, TLMessages.HintAck, Mux(d_write, TLMessages.AccessAck, TLMessages.AccessAckData))
      d.bits.denied  := (out.hresp(0) || d_denied) && d_write && !d_hint
      d.bits.corrupt := out.hresp(0) && !d_write && !d_hint
      d.bits.user :<= out.hduser
      d.bits.echo :<= d_echo

      // If the only operations in the pipe are Hints, don't stall based on hready
      val skip = supportHints.B && send.hint && (!d_valid || d_hint)
      a_flow := ((granted && out.hready) || skip) && !retry
      d_flow := (out.hready || d_hint) && !retry
      assert (!d_valid || d_flow || !a_flow); // (d_valid && !d_flow) => !a_flow

      // On RETRY, we stall the pipeline and bypass the D-phase state back to A-phase
      if (edgeOut.slave.lite) {
        retry := false.B
      } else if (!supportsRETRY) {
        assert (!d_flow || !out.hresp(1), "TLToAHB not configured with support for SPLIT/RETRY responses")
        retry := false.B
      } else {
        val d_full  = RegInit(false.B)
        val d_retry = RegInit(false.B)
        val d_idle  = RegInit(false.B)
        val d_addr  = RegEnable(send.addr,  a_flow && send.send)
        val d_hsize = RegEnable(send.hsize, a_flow && send.send)
        retry := d_retry

        when (d_flow) {
          d_full := send.send && !send.hint && a_flow
        }

        when (out.hresp(1) && d_full) {
          d_retry   := true.B
          d_idle    := true.B
          increment := true.B
        }

        when (!out.hresp(1) && out.hready && granted) {
          d_retry := false.B
        }

        when (out.hready) {
          d_idle  := false.B
        }

        when (d_retry) {
          out.busreq():= true.B
          out.htrans  := Mux(d_idle, TRANS_IDLE, TRANS_NONSEQ)
          out.hwrite  := d_write
          out.haddr   := d_addr
          out.hsize   := d_hsize
        }
      }

      // AHB has no cache coherence
      in.b.valid := false.B
      in.c.ready := true.B
      in.e.ready := true.B
    }
  }
}

object TLToAHB
{
  def apply(aFlow: Boolean = true, supportHints: Boolean = true, supportsRETRY: Boolean = true)(implicit p: Parameters) =
  {
    val tl2ahb = LazyModule(new TLToAHB(aFlow, supportHints, supportsRETRY))
    tl2ahb.node
  }
}
