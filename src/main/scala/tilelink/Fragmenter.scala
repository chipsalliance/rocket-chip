// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.diplomacy.{AddressSet, BufferParams, IdRange, TransferSizes}
import freechips.rocketchip.util.{Repeater, OH1ToUInt, UIntToOH1}

import scala.math.min

import freechips.rocketchip.util.DataToAugmentedData

object EarlyAck {
  sealed trait T
  case object AllPuts extends T
  case object PutFulls extends T
  case object None extends T
}

// minSize: minimum size of transfers supported by all outward managers
// maxSize: maximum size of transfers supported after the Fragmenter is applied
// alwaysMin: fragment all requests down to minSize (else fragment to maximum supported by manager)
// earlyAck: should a multibeat Put should be acknowledged on the first beat or last beat
// holdFirstDeny: allow the Fragmenter to unsafely combine multibeat Gets by taking the first denied for the whole burst
// nameSuffix: appends a suffix to the module name
// Fragmenter modifies: PutFull, PutPartial, LogicalData, Get, Hint
// Fragmenter passes: ArithmeticData (truncated to minSize if alwaysMin)
// Fragmenter cannot modify acquire (could livelock); thus it is unsafe to put caches on both sides
class TLFragmenter(val minSize: Int, val maxSize: Int, val alwaysMin: Boolean = false, val earlyAck: EarlyAck.T = EarlyAck.None, val holdFirstDeny: Boolean = false, val nameSuffix: Option[String] = None)(implicit p: Parameters) extends LazyModule
{
  require(isPow2 (maxSize), s"TLFragmenter expects pow2(maxSize), but got $maxSize")
  require(isPow2 (minSize), s"TLFragmenter expects pow2(minSize), but got $minSize")
  require(minSize <= maxSize, s"TLFragmenter expects min <= max, but got $minSize > $maxSize")

  val fragmentBits = log2Ceil(maxSize / minSize)
  val fullBits = if (earlyAck == EarlyAck.PutFulls) 1 else 0
  val toggleBits = 1
  val addedBits = fragmentBits + toggleBits + fullBits

  def expandTransfer(x: TransferSizes, op: String) = if (!x) x else {
    // validate that we can apply the fragmenter correctly
    require (x.max >= minSize, s"TLFragmenter (with parent $parent) max transfer size $op(${x.max}) must be >= min transfer size (${minSize})")
    TransferSizes(x.min, maxSize)
  }

  private def noChangeRequired = minSize == maxSize
  private def shrinkTransfer(x: TransferSizes) =
    if (!alwaysMin) x
    else if (x.min <= minSize) TransferSizes(x.min, min(minSize, x.max))
    else TransferSizes.none

  private def mapManager(m: TLManagerParameters) = m.v1copy(
    supportsArithmetic = shrinkTransfer(m.supportsArithmetic),
    supportsLogical    = shrinkTransfer(m.supportsLogical),
    supportsGet        = expandTransfer(m.supportsGet, "Get"),
    supportsPutFull    = expandTransfer(m.supportsPutFull, "PutFull"),
    supportsPutPartial = expandTransfer(m.supportsPutPartial, "PutParital"),
    supportsHint       = expandTransfer(m.supportsHint, "Hint"))

  val node = new TLAdapterNode(
    // We require that all the responses are mutually FIFO
    // Thus we need to compact all of the clients into one big client
    clientFn  = { c => (if (noChangeRequired) c else c.v2copy(
      clients = Seq(TLClientParameters.v2(
        name        = "TLFragmenter",
        sourceId    = IdRange(0, if (minSize == maxSize) c.endSourceId else (c.endSourceId << addedBits)),
        requestFifo = true,
        emits       = TLClientToManagerTransferSizes(
          acquireT    = shrinkTransfer(c.clients.map(_.emits.acquireT)  .reduce(_ mincover _)),
          acquireB    = shrinkTransfer(c.clients.map(_.emits.acquireB)  .reduce(_ mincover _)),
          arithmetic  = shrinkTransfer(c.clients.map(_.emits.arithmetic).reduce(_ mincover _)),
          logical     = shrinkTransfer(c.clients.map(_.emits.logical)   .reduce(_ mincover _)),
          get         = shrinkTransfer(c.clients.map(_.emits.get)       .reduce(_ mincover _)),
          putFull     = shrinkTransfer(c.clients.map(_.emits.putFull)   .reduce(_ mincover _)),
          putPartial  = shrinkTransfer(c.clients.map(_.emits.putPartial).reduce(_ mincover _)),
          hint        = shrinkTransfer(c.clients.map(_.emits.hint)      .reduce(_ mincover _))
        )
      ))
    ))},
    managerFn = { m => if (noChangeRequired) m else m.v2copy(managers = m.managers.map(mapManager)) }
  ) {
    override def circuitIdentity = noChangeRequired
  }

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    override def desiredName = (Seq("TLFragmenter") ++ nameSuffix).mkString("_")
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      if (noChangeRequired) {
        out <> in
      } else {
        // All managers must share a common FIFO domain (responses might end up interleaved)
        val manager   = edgeOut.manager
        val managers  = manager.managers
        val beatBytes = manager.beatBytes
        val fifoId = managers(0).fifoId
        require (fifoId.isDefined && managers.map(_.fifoId == fifoId).reduce(_ && _))
        require (!manager.anySupportAcquireB || !edgeOut.client.anySupportProbe,
          s"TLFragmenter (with parent $parent) can't fragment a caching client's requests into a cacheable region")
        
        require (minSize >= beatBytes, s"TLFragmenter (with parent $parent) can't support fragmenting ($minSize) to sub-beat ($beatBytes) accesses")
        // We can't support devices which are cached on both sides of us
        require (!edgeOut.manager.anySupportAcquireB || !edgeIn.client.anySupportProbe)
        // We can't support denied because we reassemble fragments
        require (!edgeOut.manager.mayDenyGet || holdFirstDeny, s"TLFragmenter (with parent $parent) can't support denials without holdFirstDeny=true")
        require (!edgeOut.manager.mayDenyPut || earlyAck == EarlyAck.None)

        /* The Fragmenter is a bit tricky, because there are 5 sizes in play:
         *   max  size -- the maximum transfer size possible
         *   orig size -- the original pre-fragmenter size
         *   frag size -- the modified post-fragmenter size
         *   min  size -- the threshold below which frag=orig
         *   beat size -- the amount transfered on any given beat
         *
         * The relationships are as follows:
         *   max >= orig >= frag
         *   max >  min  >= beat
         * It IS possible that orig <= min (then frag=orig; ie: no fragmentation)
         *
         * The fragment# (sent via TL.source) is measured in multiples of min size.
         * Meanwhile, to track the progress, counters measure in multiples of beat size.
         *
         * Here is an example of a bus with max=256, min=8, beat=4 and a device supporting 16.
         *
         * in.A    out.A (frag#)  out.D (frag#)  in.D     gen# ack#
         * get64   get16  6       ackD16  6      ackD64    12   15
         *                        ackD16  6      ackD64         14
         *                        ackD16  6      ackD64         13
         *                        ackD16  6      ackD64         12
         *         get16  4       ackD16  4      ackD64    8    11
         *                        ackD16  4      ackD64         10
         *                        ackD16  4      ackD64         9
         *                        ackD16  4      ackD64         8
         *         get16  2       ackD16  2      ackD64    4    7
         *                        ackD16  2      ackD64         6
         *                        ackD16  2      ackD64         5
         *                        ackD16  2      ackD64         4
         *         get16  0       ackD16  0      ackD64    0    3
         *                        ackD16  0      ackD64         2
         *                        ackD16  0      ackD64         1
         *                        ackD16  0      ackD64         0
         *
         * get8    get8   0       ackD8   0      ackD8     0    1
         *                        ackD8   0      ackD8          0
         *
         * get4    get4   0       ackD4   0      ackD4     0    0
         * get1    get1   0       ackD1   0      ackD1     0    0
         *
         * put64   put16  6                                15
         * put64   put16  6                                14
         * put64   put16  6                                13
         * put64   put16  6       ack16   6                12    12
         * put64   put16  4                                11
         * put64   put16  4                                10
         * put64   put16  4                                9
         * put64   put16  4       ack16   4                8     8
         * put64   put16  2                                7
         * put64   put16  2                                6
         * put64   put16  2                                5
         * put64   put16  2       ack16   2                4     4
         * put64   put16  0                                3
         * put64   put16  0                                2
         * put64   put16  0                                1
         * put64   put16  0       ack16   0      ack64     0     0
         *
         * put8    put8   0                                1
         * put8    put8   0       ack8    0      ack8      0     0
         *
         * put4    put4   0       ack4    0      ack4      0     0
         * put1    put1   0       ack1    0      ack1      0     0
         */

        val counterBits = log2Up(maxSize/beatBytes)
        val maxDownSize = if (alwaysMin) minSize else min(manager.maxTransfer, maxSize)

        // Consider the following waveform for two 4-beat bursts:
        // ---A----A------------
        // -------D-----DDD-DDDD
        // Under TL rules, the second A can use the same source as the first A,
        // because the source is released for reuse on the first response beat.
        //
        // However, if we fragment the requests, it looks like this:
        // ---3210-3210---------
        // -------3-----210-3210
        // ... now we've broken the rules because 210 are twice inflight.
        //
        // This phenomenon means we can have essentially 2*maxSize/minSize-1
        // fragmented transactions in flight per original transaction source.
        //
        // To keep the source unique, we encode the beat counter in the low
        // bits of the source. To solve the overlap, we use a toggle bit.
        // Whatever toggle bit the D is reassembling, A will use the opposite.

        // First, handle the return path
        val acknum = RegInit(0.U(counterBits.W))
        val dOrig = Reg(UInt())
        val dToggle = RegInit(false.B)
        val dFragnum = out.d.bits.source(fragmentBits-1, 0)
        val dFirst = acknum === 0.U
        val dLast = dFragnum === 0.U // only for AccessAck (!Data)
        val dsizeOH  = UIntToOH (out.d.bits.size, log2Ceil(maxDownSize)+1)
        val dsizeOH1 = UIntToOH1(out.d.bits.size, log2Up(maxDownSize))
        val dHasData = edgeOut.hasData(out.d.bits)

        // calculate new acknum
        val acknum_fragment = dFragnum << log2Ceil(minSize/beatBytes)
        val acknum_size = dsizeOH1 >> log2Ceil(beatBytes)
        assert (!out.d.valid || (acknum_fragment & acknum_size) === 0.U)
        val dFirst_acknum = acknum_fragment | Mux(dHasData, acknum_size, 0.U)
        val ack_decrement = Mux(dHasData, 1.U, dsizeOH >> log2Ceil(beatBytes))
        // calculate the original size
        val dFirst_size = OH1ToUInt((dFragnum << log2Ceil(minSize)) | dsizeOH1)

        when (out.d.fire) {
          acknum := Mux(dFirst, dFirst_acknum, acknum - ack_decrement)
          when (dFirst) {
            dOrig := dFirst_size
            dToggle := out.d.bits.source(fragmentBits)
          }
        }

        // Swallow up non-data ack fragments
        val doEarlyAck = earlyAck match {
          case EarlyAck.AllPuts  => true.B
          case EarlyAck.PutFulls => out.d.bits.source(fragmentBits+1)
          case EarlyAck.None     => false.B
        }
        val drop = !dHasData && !Mux(doEarlyAck, dFirst, dLast)
        out.d.ready := in.d.ready || drop
        in.d.valid  := out.d.valid && !drop
        in.d.bits   := out.d.bits // pass most stuff unchanged
        in.d.bits.source := out.d.bits.source >> addedBits
        in.d.bits.size   := Mux(dFirst, dFirst_size, dOrig)

        if (edgeOut.manager.mayDenyPut) {
          val r_denied = Reg(Bool())
          val d_denied = (!dFirst && r_denied) || out.d.bits.denied
          when (out.d.fire) { r_denied := d_denied }
          in.d.bits.denied := d_denied
        }
        if (edgeOut.manager.mayDenyGet) {
          // Take denied only from the first beat and hold that value
          val d_denied = out.d.bits.denied holdUnless dFirst
          when (dHasData) {
            in.d.bits.denied  := d_denied
            in.d.bits.corrupt := d_denied || out.d.bits.corrupt
          }
        }

        // What maximum transfer sizes do downstream devices support?
        val maxArithmetics = managers.map(_.supportsArithmetic.max)
        val maxLogicals    = managers.map(_.supportsLogical.max)
        val maxGets        = managers.map(_.supportsGet.max)
        val maxPutFulls    = managers.map(_.supportsPutFull.max)
        val maxPutPartials = managers.map(_.supportsPutPartial.max)
        val maxHints       = managers.map(m => if (m.supportsHint) maxDownSize else 0)

        // We assume that the request is valid => size 0 is impossible
        val lgMinSize = log2Ceil(minSize).U
        val maxLgArithmetics = maxArithmetics.map(m => if (m == 0) lgMinSize else log2Ceil(m).U)
        val maxLgLogicals    = maxLogicals   .map(m => if (m == 0) lgMinSize else log2Ceil(m).U)
        val maxLgGets        = maxGets       .map(m => if (m == 0) lgMinSize else log2Ceil(m).U)
        val maxLgPutFulls    = maxPutFulls   .map(m => if (m == 0) lgMinSize else log2Ceil(m).U)
        val maxLgPutPartials = maxPutPartials.map(m => if (m == 0) lgMinSize else log2Ceil(m).U)
        val maxLgHints       = maxHints      .map(m => if (m == 0) lgMinSize else log2Ceil(m).U)

        // Make the request repeatable
        val repeater = Module(new Repeater(in.a.bits))
        repeater.io.enq <> in.a
        val in_a = repeater.io.deq

        // If this is infront of a single manager, these become constants
        val find = manager.findFast(edgeIn.address(in_a.bits))
        val maxLgArithmetic  = Mux1H(find, maxLgArithmetics)
        val maxLgLogical     = Mux1H(find, maxLgLogicals)
        val maxLgGet         = Mux1H(find, maxLgGets)
        val maxLgPutFull     = Mux1H(find, maxLgPutFulls)
        val maxLgPutPartial  = Mux1H(find, maxLgPutPartials)
        val maxLgHint        = Mux1H(find, maxLgHints)

        val limit = if (alwaysMin) lgMinSize else
          MuxLookup(in_a.bits.opcode, lgMinSize)(Array(
            TLMessages.PutFullData    -> maxLgPutFull,
            TLMessages.PutPartialData -> maxLgPutPartial,
            TLMessages.ArithmeticData -> maxLgArithmetic,
            TLMessages.LogicalData    -> maxLgLogical,
            TLMessages.Get            -> maxLgGet,
            TLMessages.Hint           -> maxLgHint))

        val aOrig = in_a.bits.size
        val aFrag = Mux(aOrig > limit, limit, aOrig)
        val aOrigOH1 = UIntToOH1(aOrig, log2Ceil(maxSize))
        val aFragOH1 = UIntToOH1(aFrag, log2Up(maxDownSize))
        val aHasData = edgeIn.hasData(in_a.bits)
        val aMask = Mux(aHasData, 0.U, aFragOH1)

        val gennum = RegInit(0.U(counterBits.W))
        val aFirst = gennum === 0.U
        val old_gennum1 = Mux(aFirst, aOrigOH1 >> log2Ceil(beatBytes), gennum - 1.U)
        val new_gennum = ~(~old_gennum1 | (aMask >> log2Ceil(beatBytes))) // ~(~x|y) is width safe
        val aFragnum = ~(~(old_gennum1 >> log2Ceil(minSize/beatBytes)) | (aFragOH1 >> log2Ceil(minSize)))
        val aLast = aFragnum === 0.U
        val aToggle = !Mux(aFirst, dToggle, RegEnable(dToggle, aFirst))
        val aFull = if (earlyAck == EarlyAck.PutFulls) Some(in_a.bits.opcode === TLMessages.PutFullData) else None

        when (out.a.fire) { gennum := new_gennum }

        repeater.io.repeat := !aHasData && aFragnum =/= 0.U
        out.a <> in_a
        out.a.bits.address := in_a.bits.address | ~(old_gennum1 << log2Ceil(beatBytes) | ~aOrigOH1 | aFragOH1 | (minSize-1).U)
        out.a.bits.source := Cat(Seq(in_a.bits.source) ++ aFull ++ Seq(aToggle.asUInt, aFragnum))
        out.a.bits.size := aFrag

        // Optimize away some of the Repeater's registers
        assert (!repeater.io.full || !aHasData)
        out.a.bits.data := in.a.bits.data
        val fullMask = ((BigInt(1) << beatBytes) - 1).U
        assert (!repeater.io.full || in_a.bits.mask === fullMask)
        out.a.bits.mask := Mux(repeater.io.full, fullMask, in.a.bits.mask)
        out.a.bits.user.waiveAll :<= in.a.bits.user.subset(_.isData)

        // Tie off unused channels
        in.b.valid := false.B
        in.c.ready := true.B
        in.e.ready := true.B
        out.b.ready := true.B
        out.c.valid := false.B
        out.e.valid := false.B
      }
    }
  }
}

object TLFragmenter
{
  def apply(minSize: Int, maxSize: Int, alwaysMin: Boolean = false, earlyAck: EarlyAck.T = EarlyAck.None, holdFirstDeny: Boolean = false, nameSuffix: Option[String] = None)(implicit p: Parameters): TLNode =
  {
    if (minSize <= maxSize) {
      val fragmenter = LazyModule(new TLFragmenter(minSize, maxSize, alwaysMin, earlyAck, holdFirstDeny, nameSuffix))
      fragmenter.node
    } else { TLEphemeralNode()(ValName("no_fragmenter")) }
  }

  def apply(wrapper: TLBusWrapper, nameSuffix: Option[String])(implicit p: Parameters): TLNode = apply(wrapper.beatBytes, wrapper.blockBytes, nameSuffix = nameSuffix)

  def apply(wrapper: TLBusWrapper)(implicit p: Parameters): TLNode = apply(wrapper, None)
}

// Synthesizable unit tests
import freechips.rocketchip.unittest._

class TLRAMFragmenter(ramBeatBytes: Int, maxSize: Int, txns: Int)(implicit p: Parameters) extends LazyModule {
  val fuzz = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("Fragmenter"))
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x3ff), beatBytes = ramBeatBytes))

  (ram.node
    := TLDelayer(0.1)
    := TLBuffer(BufferParams.flow)
    := TLDelayer(0.1)
    := TLFragmenter(ramBeatBytes, maxSize, earlyAck = EarlyAck.AllPuts)
    := TLDelayer(0.1)
    := TLBuffer(BufferParams.flow)
    := TLFragmenter(ramBeatBytes, maxSize/2)
    := TLDelayer(0.1)
    := TLBuffer(BufferParams.flow)
    := model.node
    := fuzz.node)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class TLRAMFragmenterTest(ramBeatBytes: Int, maxSize: Int, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new TLRAMFragmenter(ramBeatBytes,maxSize,txns)).module)
  io.finished := dut.io.finished
  dut.io.start := io.start
}
