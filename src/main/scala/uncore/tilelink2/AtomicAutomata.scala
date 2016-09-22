// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import scala.math.{min,max}

// Ensures that all downstream RW managers support Atomic operationss.
// If !passthrough, intercept all Atomics. Otherwise, only intercept those unsupported downstream.
class TLAtomicAutomata(logical: Boolean = true, arithmetic: Boolean = true, concurrency: Int = 1, passthrough: Boolean = true) extends LazyModule
{
  require (concurrency >= 1)

  val node = TLAdapterNode(
    clientFn  = { case Seq(cp) => require (!cp.unsafeAtomics); cp.copy(unsafeAtomics = true) },
    managerFn = { case Seq(mp) => mp.copy(managers = mp.managers.map { m =>
      val ourSupport = TransferSizes(1, mp.beatBytes)
      def widen(x: TransferSizes) = if (passthrough && x.min <= 2*mp.beatBytes) TransferSizes(1, max(mp.beatBytes, x.max)) else ourSupport
      val canDoit = m.supportsPutFull.contains(ourSupport) && m.supportsGet.contains(ourSupport)
      // Blow up if there are devices to which we cannot add Atomics, because their R|W are too inflexible
      require (!m.supportsPutFull || !m.supportsGet || canDoit)
      m.copy(
        supportsArithmetic = if (!arithmetic || !canDoit) m.supportsArithmetic else widen(m.supportsArithmetic),
        supportsLogical    = if (!logical    || !canDoit) m.supportsLogical    else widen(m.supportsLogical))
    })})

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    val in  = io.in(0)
    val out = io.out(0)
    val edgeIn  = node.edgesIn(0)
    val edgeOut = node.edgesOut(0)
    val managers = edgeOut.manager.managers

    // To which managers are we adding atomic support?
    val ourSupport = TransferSizes(1, edgeOut.manager.beatBytes)
    val managersNeedingHelp = managers.filter { m =>
      m.supportsPutFull.contains(ourSupport) &&
      m.supportsGet.contains(ourSupport) &&
      ((logical    && !m.supportsLogical   .contains(ourSupport)) ||
       (arithmetic && !m.supportsArithmetic.contains(ourSupport)) ||
       !passthrough) // we will do atomics for everyone we can
    }
    // We cannot add atomcis to a non-FIFO manager
    managersNeedingHelp foreach { m => require (m.fifoId.isDefined) }
    // We need to preserve FIFO semantics across FIFO domains, not managers
    // Suppose you have Put(42) Atomic(+1) both inflight; valid results: 42 or 43
    // If we allow Put(42) Get() Put(+1) concurrent; valid results: 42 43 OR undef
    // Making non-FIFO work requires waiting for all Acks to come back (=> use FIFOFixer)
    val domainsNeedingHelp = managersNeedingHelp.map(_.fifoId.get).distinct
    // Don't overprovision the CAM
    val camSize = min(domainsNeedingHelp.size, concurrency)
    // Compact the fifoIds to only those we care about
    val camFifoIds = managers.map(m => UInt(m.fifoId.map(id => max(0, domainsNeedingHelp.indexOf(id))).getOrElse(0)))

    // CAM entry state machine
    val FREE = UInt(0) // unused                   waiting on Atomic from A
    val GET  = UInt(3) // Get sent down A          waiting on AccessDataAck from D
    val AMO  = UInt(2) // AccessDataAck sent up D  waiting for A availability
    val ACK  = UInt(1) // Put sent down A          waiting for PutAck from D

    class CAMEntry extends Bundle {
      val state = UInt(width = 2)
      val fifoId = UInt(width = log2Up(domainsNeedingHelp.size))
      val bits  = new TLBundleA(in.a.bits.params)
    }

    def helper(select: Seq[Bool], x: Seq[TransferSizes], lgSize: UInt) =
      if (!passthrough) Bool(false) else
      if (x.map(_ == x(0)).reduce(_ && _)) x(0).containsLg(lgSize) else
      Mux1H(select, x.map(_.containsLg(lgSize))) 

    // Do we need to do anything at all?
    if (camSize > 0) {
      class CAM_S extends Bundle {
        val state = UInt(width = 2)
      }
      class CAM_A extends Bundle {
        val bits    = new TLBundleA(out.a.bits.params)
        val fifoId  = UInt(width = log2Up(domainsNeedingHelp.size))
      }
      class CAM_D extends Bundle {
        val data = UInt(width = out.a.bits.params.dataBits)
      }

      val initval = Wire(new CAM_S)
      initval.state := FREE
      val cam_s = RegInit(Vec.fill(camSize)(initval))
      val cam_a = Reg(Vec(camSize, new CAM_A))
      val cam_d = Reg(Vec(camSize, new CAM_D))

      val cam_free   = cam_s.map(_.state === FREE)
      val cam_amo    = cam_s.map(_.state === AMO)
      val cam_abusy  = cam_s.map(e => e.state === GET || e.state === AMO) // A is blocked
      val cam_dmatch = cam_s.map(e => e.state === GET || e.state === ACK) // D should inspect these entries

      // Can the manager already handle this message?
      val a_size = edgeIn.size(in.a.bits)
      val a_select = edgeOut.manager.findFast(edgeIn.address(in.a.bits))
      val a_canLogical    = helper(a_select, managers.map(_.supportsLogical),    a_size)
      val a_canArithmetic = helper(a_select, managers.map(_.supportsArithmetic), a_size)
      val a_isLogical    = in.a.bits.opcode === TLMessages.LogicalData
      val a_isArithmetic = in.a.bits.opcode === TLMessages.ArithmeticData
      val a_isSupported = Mux(a_isLogical, a_canLogical, Mux(a_isArithmetic, a_canArithmetic, Bool(true)))

      // Must we do a Put?
      val a_cam_any_put = cam_amo.reduce(_ || _)
      val a_cam_por_put = cam_amo.scanLeft(Bool(false))(_||_).init
      val a_cam_sel_put = (cam_amo zip a_cam_por_put) map { case (a, b) => a && !b }
      val a_cam_adata = PriorityMux(cam_amo, cam_a.map(_.bits))
      val a_cam_ddata = PriorityMux(cam_amo, cam_d.map(_.data))

      // Does the A request conflict with an inflight AMO?
      val a_fifoId  = Mux1H(a_select, camFifoIds)
      val a_cam_busy = (cam_abusy zip cam_a.map(_.fifoId === a_fifoId)) map { case (a,b) => a&&b } reduce (_||_)

      // (Where) are we are allocating in the CAM?
      val a_cam_any_free = cam_free.reduce(_ || _)
      val a_cam_por_free = cam_free.scanLeft(Bool(false))(_||_).init
      val a_cam_sel_free = (cam_free zip a_cam_por_free) map { case (a,b) => a && !b }

      // !!! perform the AMO op
      val amo_data = a_cam_adata.data + a_cam_ddata

      // Potentially mutate the message from inner
      val source_i = Wire(in.a)
      val a_allow = !a_cam_busy && (a_isSupported || a_cam_any_free)
      in.a.ready := source_i.ready && a_allow
      source_i.valid := in.a.valid && a_allow
      source_i.bits  := in.a.bits
      when (!a_isSupported) { // minimal mux difference
        source_i.bits.opcode := TLMessages.Get
        source_i.bits.param  := UInt(0)
      }

      // Potentially take the message from the CAM
      val source_c = Wire(in.a)
      source_c.valid := a_cam_any_put
      source_c.bits := edgeOut.Put(a_cam_adata.source, edgeIn.address(a_cam_adata), a_cam_adata.size, amo_data)._2

      // Finishing an AMO from the CAM has highest priority
      TLArbiter(TLArbiter.lowestIndexFirst)(out.a, (UInt(1), source_c), (edgeOut.numBeats(in.a.bits), source_i))

      // Capture the A state into the CAM
      when (source_i.fire() && !a_isSupported) {
        (a_cam_sel_free zip cam_a) foreach { case (en, r) =>
          when (en) {
            r.fifoId := a_fifoId
            r.bits   := in.a.bits
          }
        }
        (a_cam_sel_free zip cam_s) foreach { case (en, r) =>
          when (en) {
            r.state := GET
          }
        }
      }

      // Advance the put state
      when (source_c.fire()) {
        (a_cam_sel_put zip cam_s) foreach { case (en, r) =>
          when (en) {
            r.state := ACK
          }
        }
      }

      // We need to deal with a potential D response in the same cycle as the A request
      val d_cam_sel_raw = cam_a.map(_.bits.source === in.d.bits.source)
      val d_cam_sel_match = (d_cam_sel_raw zip cam_dmatch) map { case (a,b) => a&&b }
      val d_cam_data = Mux1H(d_cam_sel_match, cam_d.map(_.data))
      val d_cam_sel_bypass = if (edgeOut.manager.minLatency > 0) Bool(false) else
                             out.d.bits.source === in.a.bits.source && in.a.valid && out.d.valid && !a_isSupported
      val d_cam_sel = (a_cam_sel_free zip d_cam_sel_match) map { case (a,d) => Mux(d_cam_sel_bypass, a, d) }
      val d_cam_sel_any = d_cam_sel_bypass || d_cam_sel_match.reduce(_ || _)
      val d_ackd = out.d.bits.opcode === TLMessages.AccessAckData
      val d_ack  = out.d.bits.opcode === TLMessages.AccessAck

      when (out.d.fire()) {
        (d_cam_sel zip cam_d) foreach { case (en, r) =>
          when (en && d_ackd) {
            r.data := out.d.bits.data
          }
        }
        (d_cam_sel zip cam_s) foreach { case (en, r) =>
          when (en) {
            // Note: it is important that this comes AFTER the := GET, so we can go FREE=>GET=>AMO in one cycle
            r.state := Mux(d_ackd, AMO, FREE)
          }
        }
      }

      val d_drop = d_ackd && d_cam_sel_any
      val d_replace = d_ack && d_cam_sel_match.reduce(_ || _)

      in.d.valid := out.d.valid && !d_drop
      out.d.ready := in.d.ready || d_drop

      in.d.bits := out.d.bits
      when (d_replace) { // minimal muxes
        in.d.bits.opcode := TLMessages.AccessAckData
        in.d.bits.data := d_cam_data
      }
    } else {
      out.a.valid := in.a.valid
      in.a.ready := out.a.ready
      out.a.bits := in.a.bits

      in.d.valid := out.d.valid
      out.d.ready := in.d.ready
      in.d.bits := out.d.bits
    }

    if (edgeOut.manager.anySupportAcquire && edgeIn.client.anySupportProbe) {
      in.b.valid := out.b.valid
      out.b.ready := in.b.ready
      in.b.bits := out.b.bits

      out.c.valid := in.c.valid
      in.c.ready := out.c.ready
      out.c.bits := in.c.bits

      out.e.valid := in.e.valid
      in.e.ready := out.e.ready
      out.e.bits := in.e.bits
    } else {
      in.b.valid := Bool(false)
      in.c.ready := Bool(true)
      in.e.ready := Bool(true)
      out.b.ready := Bool(true)
      out.c.valid := Bool(false)
      out.e.valid := Bool(false)
    }
  }
}

object TLAtomicAutomata
{
  // applied to the TL source node; y.node := TLAtomicAutomata(x.node)
  def apply(logical: Boolean = true, arithmetic: Boolean = true, concurrency: Int = 1, passthrough: Boolean = true)(x: TLBaseNode)(implicit sourceInfo: SourceInfo): TLBaseNode = {
    val atomics = LazyModule(new TLAtomicAutomata(logical, arithmetic, concurrency, passthrough))
    atomics.node := x
    atomics.node
  }
}
