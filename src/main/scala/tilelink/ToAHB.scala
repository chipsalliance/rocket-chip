// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.amba._
import freechips.rocketchip.amba.ahb._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import AHBParameters._

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

class AHBControlBundle(params: TLEdge) extends GenericParameterizedBundle(params)
{
  val full   = Bool()
  val send   = Bool() // => full+data
  val first  = Bool()
  val last   = Bool()
  val hint   = Bool()
  val write  = Bool()
  val size   = UInt(width = params.bundle.sizeBits)
  val source = UInt(width = params.bundle.sourceBits)
  val hsize  = UInt(width = AHBParameters.sizeBits)
  val hburst = UInt(width = AHBParameters.burstBits)
  val addr   = UInt(width = params.bundle.addressBits)
  val data   = UInt(width = params.bundle.dataBits)
  val hauser = BundleMap(params.bundle.requestFields)
  val echo   = BundleMap(params.bundle.echoFields)
}

// The input side has either a flow queue (aFlow=true) or a pipe queue (aFlow=false)
// The output side always has a flow queue
class TLToAHB(val aFlow: Boolean = false, val supportHints: Boolean = true, val supportsRETRY: Boolean = true)(implicit p: Parameters) extends LazyModule
{
  val node = TLToAHBNode(supportHints)

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

      // A- and D-phase readiness
      val a_flow = Wire(Bool())
      val d_flow = Wire(Bool())

      // Advance the FSM based on the result of this AHB beat
      when (send.send && !a_flow) /* retry AHB */ {
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
      val a_hint = in.a.bits.opcode === TLMessages.Hint && Bool(supportHints)
      val a_singleBeat = a_hint || Bool(lgBytes >= lgMax) || a_sizeDelta(edgeIn.bundle.sizeBits)
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
          post.hint  := a_hint
          post.size  := in.a.bits.size
          post.source:= in.a.bits.source
          post.hauser:<= in.a.bits.user
          post.echo  :<= in.a.bits.echo
        }
        when (in.a.fire() && !a_hint) {
          post.write := edgeIn.hasData(in.a.bits)
          post.hsize := Mux(a_singleBeat, in.a.bits.size, UInt(lgBytes))
          post.hburst:= Mux(a_singleBeat, BURST_SINGLE, (a_logBeats1<<1) | UInt(1))
          post.addr  := in.a.bits.address
          post.data  := in.a.bits.data
        }
      }

      // For SPLIT/RETRY, a burst being reissued from D-phase state
      val retry = Wire(Bool())

      val granted   = RegEnable(out.grant(), out.hready)
      val rebuild   = RegInit(Bool(false)) // rewrite as NSEQ       (for next-beat  EBT)
      val increment = RegInit(Bool(false)) // rewrite as BURST_INCR (for same-burst EBT)
      when (out.hready && granted && !retry) {
        when (send.send)    { rebuild := Bool(false) }
        when (!out.grant()) { rebuild := Bool(true) }
        when (out.busreq() && !out.grant()) { increment := Bool(true) }
        when (send.send && send.last)       { increment := Bool(false) }
      }

      out.lock()  := Bool(false) // for now
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
        hprot(3) :=  x.modifiable
        out.hprot := Cat(hprot.reverse)
      }

      // We need a skidpad to capture D output:
      // We cannot know if the D response will be accepted until we have
      // presented it on D as valid.  We also can't back-pressure AHB in the
      // data phase.  Therefore, we must have enough space to save the all
      // commited AHB requests (A+D phases = 2). To decouple d_ready from
      // a_ready and htrans, we add another entry for aFlow=false.
      val depth = if (aFlow) 2 else 3
      val d = Wire(in.d)
      in.d :<> Queue(d, depth, flow=true)
      assert (!d.valid || d.ready)

      val d_flight = RegInit(UInt(0, width = 2))
      assert (d_flight <= UInt(depth))
      d_flight := d_flight + a_commit.asUInt - in.d.fire().asUInt
      d_block := d_flight >= UInt(depth)

      val d_valid   = RegInit(Bool(false))
      val d_denied  = Reg(Bool())
      val d_hint    = RegEnable(send.hint,   a_flow && send.send)
      val d_write   = RegEnable(send.write,  a_flow && send.send)
      val d_source  = RegEnable(send.source, a_flow && send.send)
      val d_size    = RegEnable(send.size,   a_flow && send.send)
      val d_echo    = RegEnable(send.echo,   a_flow && send.send)

      when (d_flow) {
        d_valid := send.send && (send.last || !send.write) && a_flow
        when (out.hresp(0))  { d_denied := Bool(true) }
        when (send.first)    { d_denied := Bool(false) }
      }

      d.valid := d_valid && d_flow
      d.bits  := edgeIn.AccessAck(d_source, d_size, out.hrdata)
      d.bits.opcode := Mux(d_hint, TLMessages.HintAck, Mux(d_write, TLMessages.AccessAck, TLMessages.AccessAckData))
      d.bits.denied  := (out.hresp(0) || d_denied) && d_write && !d_hint
      d.bits.corrupt := out.hresp(0) && !d_write && !d_hint
      d.bits.user :<= out.hduser
      d.bits.echo :<= d_echo

      // If the only operations in the pipe are Hints, don't stall based on hready
      val skip = Bool(supportHints) && send.hint && (!d_valid || d_hint)
      a_flow := ((granted && out.hready) || skip) && !retry
      d_flow := (out.hready || d_hint) && !retry
      assert (!d_valid || d_flow || !a_flow); // (d_valid && !d_flow) => !a_flow

      // On RETRY, we stall the pipeline and bypass the D-phase state back to A-phase
      if (edgeOut.slave.lite) {
        retry := Bool(false)
      } else if (!supportsRETRY) {
        assert (!d_flow || !out.hresp(1), "TLToAHB not configured with support for SPLIT/RETRY responses")
        retry := Bool(false)
      } else {
        val d_full  = RegInit(Bool(false))
        val d_retry = RegInit(Bool(false))
        val d_idle  = RegInit(Bool(false))
        val d_addr  = RegEnable(send.addr,  a_flow && send.send)
        val d_hsize = RegEnable(send.hsize, a_flow && send.send)
        retry := d_retry

        when (d_flow) {
          d_full := send.send && !send.hint && a_flow
        }

        when (out.hresp(1) && d_full) {
          d_retry   := Bool(true)
          d_idle    := Bool(true)
          increment := Bool(true)
        }

        when (!out.hresp(1) && out.hready && granted) {
          d_retry := Bool(false)
        }

        when (out.hready) {
          d_idle  := Bool(false)
        }

        when (d_retry) {
          out.busreq():= Bool(true)
          out.htrans  := Mux(d_idle, TRANS_IDLE, TRANS_NONSEQ)
          out.hwrite  := d_write
          out.haddr   := d_addr
          out.hsize   := d_hsize
        }
      }

      // AHB has no cache coherence
      in.b.valid := Bool(false)
      in.c.ready := Bool(true)
      in.e.ready := Bool(true)
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
