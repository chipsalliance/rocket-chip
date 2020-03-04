// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import scala.math.{min,max}

// Ensures that all downstream RW managers support Atomic operationss.
// If !passthrough, intercept all Atomics. Otherwise, only intercept those unsupported downstream.
class TLAtomicAutomata(logical: Boolean = true, arithmetic: Boolean = true, concurrency: Int = 1, passthrough: Boolean = true)(implicit p: Parameters) extends LazyModule
{
  require (concurrency >= 1)

  val node = TLAdapterNode(
    managerFn = { case mp => mp.copy(managers = mp.managers.map { m =>
      val ourSupport = TransferSizes(1, mp.beatBytes)
      def widen(x: TransferSizes) = if (passthrough && x.min <= 2*mp.beatBytes) TransferSizes(1, max(mp.beatBytes, x.max)) else ourSupport
      val canDoit = m.supportsPutFull.contains(ourSupport) && m.supportsGet.contains(ourSupport)
      // Blow up if there are devices to which we cannot add Atomics, because their R|W are too inflexible
      require (!m.supportsPutFull || !m.supportsGet || canDoit, s"${m.name} has $ourSupport, needed PutFull(${m.supportsPutFull}) or Get(${m.supportsGet})")
      m.copy(
        supportsArithmetic = if (!arithmetic || !canDoit) m.supportsArithmetic else widen(m.supportsArithmetic),
        supportsLogical    = if (!logical    || !canDoit) m.supportsLogical    else widen(m.supportsLogical),
        mayDenyGet         = m.mayDenyGet || m.mayDenyPut)
    })})

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val managers = edgeOut.manager.managers
      val beatBytes = edgeOut.manager.beatBytes

      // To which managers are we adding atomic support?
      val ourSupport = TransferSizes(1, beatBytes)
      val managersNeedingHelp = managers.filter { m =>
        m.supportsPutFull.contains(ourSupport) &&
        m.supportsGet.contains(ourSupport) &&
        ((logical    && !m.supportsLogical   .contains(ourSupport)) ||
         (arithmetic && !m.supportsArithmetic.contains(ourSupport)) ||
         !passthrough) // we will do atomics for everyone we can
      }

      // Managers that need help with atomics must necessarily have this node as the root of a tree in the node graph.
      // (But they must also ensure no sideband operations can get between the read and write.)
      val violations = managersNeedingHelp.flatMap(_.findTreeViolation).map { node => (node.name, node.inputs.map(_._1.name)) }
      require(violations.isEmpty,
        s"AtomicAutomata can only help nodes for which it is at the root of a diplomatic node tree," +
        "but the following violations were found:\n" +
        violations.map(v => s"(${v._1} has parents ${v._2})").mkString("\n"))

      // We cannot add atomics to a non-FIFO manager
      managersNeedingHelp foreach { m => require (m.fifoId.isDefined) }
      // We need to preserve FIFO semantics across FIFO domains, not managers
      // Suppose you have Put(42) Atomic(+1) both inflight; valid results: 42 or 43
      // If we allow Put(42) Get() Put(+1) concurrent; valid results: 42 43 OR undef
      // Making non-FIFO work requires waiting for all Acks to come back (=> use FIFOFixer)
      val domainsNeedingHelp = managersNeedingHelp.map(_.fifoId.get).distinct
      // Don't overprovision the CAM
      val camSize = min(domainsNeedingHelp.size, concurrency)
      // Compact the fifoIds to only those we care about
      def camFifoId(m: TLManagerParameters) = m.fifoId.map(id => max(0, domainsNeedingHelp.indexOf(id))).getOrElse(0)

      // CAM entry state machine
      val FREE = UInt(0) // unused                   waiting on Atomic from A
      val GET  = UInt(3) // Get sent down A          waiting on AccessDataAck from D
      val AMO  = UInt(2) // AccessDataAck sent up D  waiting for A availability
      val ACK  = UInt(1) // Put sent down A          waiting for PutAck from D

      val params = TLAtomicAutomata.CAMParams(out.a.bits.params, domainsNeedingHelp.size)
      // Do we need to do anything at all?
      if (camSize > 0) {
        val initval = Wire(new TLAtomicAutomata.CAM_S(params))
        initval.state := FREE
        val cam_s = RegInit(Vec.fill(camSize)(initval))
        val cam_a = Reg(Vec(camSize, new TLAtomicAutomata.CAM_A(params)))
        val cam_d = Reg(Vec(camSize, new TLAtomicAutomata.CAM_D(params)))

        val cam_free   = cam_s.map(_.state === FREE)
        val cam_amo    = cam_s.map(_.state === AMO)
        val cam_abusy  = cam_s.map(e => e.state === GET || e.state === AMO) // A is blocked
        val cam_dmatch = cam_s.map(e => e.state =/= FREE) // D should inspect these entries

        // Can the manager already handle this message?
        val a_address = edgeIn.address(in.a.bits)
        val a_size = edgeIn.size(in.a.bits)
        val a_canLogical    = Bool(passthrough) && edgeOut.manager.supportsLogicalFast   (a_address, a_size)
        val a_canArithmetic = Bool(passthrough) && edgeOut.manager.supportsArithmeticFast(a_address, a_size)
        val a_isLogical    = in.a.bits.opcode === TLMessages.LogicalData
        val a_isArithmetic = in.a.bits.opcode === TLMessages.ArithmeticData
        val a_isSupported = Mux(a_isLogical, a_canLogical, Mux(a_isArithmetic, a_canArithmetic, Bool(true)))

        // Must we do a Put?
        val a_cam_any_put = cam_amo.reduce(_ || _)
        val a_cam_por_put = cam_amo.scanLeft(Bool(false))(_||_).init
        val a_cam_sel_put = (cam_amo zip a_cam_por_put) map { case (a, b) => a && !b }
        val a_cam_a = PriorityMux(cam_amo, cam_a)
        val a_cam_d = PriorityMux(cam_amo, cam_d)
        val a_a = a_cam_a.bits.data
        val a_d = a_cam_d.data

        // Does the A request conflict with an inflight AMO?
        val a_fifoId  = edgeOut.manager.fastProperty(a_address, camFifoId _, (i:Int) => UInt(i))
        val a_cam_busy = (cam_abusy zip cam_a.map(_.fifoId === a_fifoId)) map { case (a,b) => a&&b } reduce (_||_)

        // (Where) are we are allocating in the CAM?
        val a_cam_any_free = cam_free.reduce(_ || _)
        val a_cam_por_free = cam_free.scanLeft(Bool(false))(_||_).init
        val a_cam_sel_free = (cam_free zip a_cam_por_free) map { case (a,b) => a && !b }

        // Logical AMO
        val indexes = Seq.tabulate(beatBytes*8) { i => Cat(a_a(i,i), a_d(i,i)) }
        val logic_out = Cat(indexes.map(x => a_cam_a.lut(x).asUInt).reverse)

        // Arithmetic AMO
        val unsigned = a_cam_a.bits.param(1)
        val take_max = a_cam_a.bits.param(0)
        val adder = a_cam_a.bits.param(2)
        val mask = a_cam_a.bits.mask
        val signSel = ~(~mask | (mask >> 1))
        val signbits_a = Cat(Seq.tabulate(beatBytes) { i => a_a(8*i+7,8*i+7) } .reverse)
        val signbits_d = Cat(Seq.tabulate(beatBytes) { i => a_d(8*i+7,8*i+7) } .reverse)
        // Move the selected sign bit into the first byte position it will extend
        val signbit_a = ((signbits_a & signSel) << 1)(beatBytes-1, 0)
        val signbit_d = ((signbits_d & signSel) << 1)(beatBytes-1, 0)
        val signext_a = FillInterleaved(8, leftOR(signbit_a))
        val signext_d = FillInterleaved(8, leftOR(signbit_d))
        // NOTE: sign-extension does not change the relative ordering in EITHER unsigned or signed arithmetic
        val wide_mask = FillInterleaved(8, mask)
        val a_a_ext = (a_a & wide_mask) | signext_a
        val a_d_ext = (a_d & wide_mask) | signext_d
        val a_d_inv = Mux(adder, a_d_ext, ~a_d_ext)
        val adder_out = a_a_ext + a_d_inv
        val h = 8*beatBytes-1 // now sign-extended; use biggest bit
        val a_bigger_uneq = unsigned === a_a_ext(h) // result if high bits are unequal
        val a_bigger = Mux(a_a_ext(h) === a_d_ext(h), !adder_out(h), a_bigger_uneq)
        val pick_a = take_max === a_bigger
        val arith_out = Mux(adder, adder_out, Mux(pick_a, a_a, a_d))

        // AMO result data
        val amo_data =
          if (!logical)    arith_out else
          if (!arithmetic) logic_out else
          Mux(a_cam_a.bits.opcode(0), logic_out, arith_out)

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
        source_c.bits := edgeOut.Put(
          fromSource = a_cam_a.bits.source,
          toAddress  = edgeIn.address(a_cam_a.bits),
          lgSize     = a_cam_a.bits.size,
          data       = amo_data,
          corrupt    = a_cam_a.bits.corrupt || a_cam_d.corrupt)._2
        source_c.bits.user.map { _ := a_cam_a.bits.user.get }

        // Finishing an AMO from the CAM has highest priority
        TLArbiter(TLArbiter.lowestIndexFirst)(out.a, (UInt(0), source_c), (edgeOut.numBeats1(in.a.bits), source_i))

        // Capture the A state into the CAM
        when (source_i.fire() && !a_isSupported) {
          (a_cam_sel_free zip cam_a) foreach { case (en, r) =>
            when (en) {
              r.fifoId := a_fifoId
              r.bits   := in.a.bits
              r.lut    := MuxLookup(in.a.bits.param(1, 0), UInt(0, width = 4), Array(
                TLAtomics.AND  -> UInt(0x8),
                TLAtomics.OR   -> UInt(0xe),
                TLAtomics.XOR  -> UInt(0x6),
                TLAtomics.SWAP -> UInt(0xc)))
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
        val d_first = edgeOut.first(out.d)
        val d_cam_sel_raw = cam_a.map(_.bits.source === in.d.bits.source)
        val d_cam_sel_match = (d_cam_sel_raw zip cam_dmatch) map { case (a,b) => a&&b }
        val d_cam_data = Mux1H(d_cam_sel_match, cam_d.map(_.data))
        val d_cam_denied = Mux1H(d_cam_sel_match, cam_d.map(_.denied))
        val d_cam_corrupt = Mux1H(d_cam_sel_match, cam_d.map(_.corrupt))
        val d_cam_sel_bypass = if (edgeOut.manager.minLatency > 0) Bool(false) else
                               out.d.bits.source === in.a.bits.source && in.a.valid && !a_isSupported
        val d_cam_sel = (a_cam_sel_free zip d_cam_sel_match) map { case (a,d) => Mux(d_cam_sel_bypass, a, d) }
        val d_cam_sel_any = d_cam_sel_bypass || d_cam_sel_match.reduce(_ || _)
        val d_ackd = out.d.bits.opcode === TLMessages.AccessAckData
        val d_ack  = out.d.bits.opcode === TLMessages.AccessAck

        when (out.d.fire() && d_first) {
          (d_cam_sel zip cam_d) foreach { case (en, r) =>
            when (en && d_ackd) {
              r.data := out.d.bits.data
              r.denied := out.d.bits.denied
              r.corrupt := out.d.bits.corrupt
            }
          }
          (d_cam_sel zip cam_s) foreach { case (en, r) =>
            when (en) {
              // Note: it is important that this comes AFTER the := GET, so we can go FREE=>GET=>AMO in one cycle
              r.state := Mux(d_ackd, AMO, FREE)
            }
          }
        }

        val d_drop = d_first && d_ackd && d_cam_sel_any
        val d_replace = d_first && d_ack && d_cam_sel_match.reduce(_ || _)

        in.d.valid := out.d.valid && !d_drop
        out.d.ready := in.d.ready || d_drop

        in.d.bits := out.d.bits
        when (d_replace) { // minimal muxes
          in.d.bits.opcode := TLMessages.AccessAckData
          in.d.bits.data := d_cam_data
          in.d.bits.corrupt := d_cam_corrupt || out.d.bits.denied
          in.d.bits.denied  := d_cam_denied  || out.d.bits.denied
        }
      } else {
        out.a.valid := in.a.valid
        in.a.ready := out.a.ready
        out.a.bits := in.a.bits

        in.d.valid := out.d.valid
        out.d.ready := in.d.ready
        in.d.bits := out.d.bits
      }

      if (edgeOut.manager.anySupportAcquireB && edgeIn.client.anySupportProbe) {
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
}

object TLAtomicAutomata
{
  def apply(logical: Boolean = true, arithmetic: Boolean = true, concurrency: Int = 1, passthrough: Boolean = true)(implicit p: Parameters): TLNode =
  {
    val atomics = LazyModule(new TLAtomicAutomata(logical, arithmetic, concurrency, passthrough))
    atomics.node
  }

  case class CAMParams(a: TLBundleParameters, domainsNeedingHelp: Int)

  class CAM_S(params: CAMParams) extends GenericParameterizedBundle(params) {
    val state = UInt(width = 2)
  }
  class CAM_A(params: CAMParams) extends GenericParameterizedBundle(params) {
    val bits    = new TLBundleA(params.a)
    val fifoId  = UInt(width = log2Up(params.domainsNeedingHelp))
    val lut     = UInt(width = 4)
  }
  class CAM_D(params: CAMParams) extends GenericParameterizedBundle(params) {
    val data    = UInt(width = params.a.dataBits)
    val denied  = Bool()
    val corrupt = Bool()
  }
}

/** Synthesizeable unit tests */
import freechips.rocketchip.unittest._

class TLRAMAtomicAutomata(txns: Int)(implicit p: Parameters) extends LazyModule {
  val fuzz = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("AtomicAutomata"))
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x3ff)))

  // Confirm that the AtomicAutomata combines read + write errors
  import TLMessages._
  val test = new RequestPattern({a: TLBundleA =>
    val doesA = a.opcode === ArithmeticData || a.opcode === LogicalData
    val doesR = a.opcode === Get || doesA
    val doesW = a.opcode === PutFullData || a.opcode === PutPartialData || doesA
    (doesR && RequestPattern.overlaps(Seq(AddressSet(0x08, ~0x08)))(a)) ||
    (doesW && RequestPattern.overlaps(Seq(AddressSet(0x10, ~0x10)))(a))
  })

  (ram.node
    := TLErrorEvaluator(test)
    := TLFragmenter(4, 256)
    := TLDelayer(0.1)
    := TLAtomicAutomata()
    := TLDelayer(0.1)
    := TLErrorEvaluator(test, testOn=true, testOff=true)
    := model.node
    := fuzz.node)

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class TLRAMAtomicAutomataTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new TLRAMAtomicAutomata(txns)).module)
  io.finished := dut.io.finished
}
