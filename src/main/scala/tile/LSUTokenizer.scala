package freechips.rocketchip.tile

import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.util.experimental.BitSet
import chisel3.util.{BitPat, Decoupled, DecoupledIO, Queue, RegEnable, Valid, ValidIO, log2Ceil}
import chisel3.util.experimental.decode.{TruthTable, decoder}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.TwoWayCounter
import org.chipsalliance.cde.config.Parameters

case class LSUTokenizerDiplomaticParameter(nodeName: String, outstandingSize: Int, giveTokenToMe: BitSet)
class LSUTokenizerBundle extends Bundle {

  /** Grant token from Tokenizer to LSU. */
  val grant: Bool = Bool()

  /** Release token from LSU to Tokenizer. */
  val release: Bool = Flipped(Bool())
}

object LSUTokenizerNodeImp
    extends SimpleNodeImp[
      LSUTokenizerDiplomaticParameter,
      LSUTokenizerDiplomaticParameter,
      LSUTokenizerDiplomaticParameter,
      LSUTokenizerBundle
    ] {

  override def edge(
    pd:         LSUTokenizerDiplomaticParameter,
    pu:         LSUTokenizerDiplomaticParameter,
    p:          Parameters,
    sourceInfo: SourceInfo
  ): LSUTokenizerDiplomaticParameter =
    LSUTokenizerDiplomaticParameter(pu.nodeName, pu.outstandingSize, pu.giveTokenToMe)

  override def bundle(e: LSUTokenizerDiplomaticParameter): LSUTokenizerBundle = new LSUTokenizerBundle

  override def render(e: LSUTokenizerDiplomaticParameter) = RenderedEdge(colour = "#00cc00")
}

class LSUTokenizerSinkNode(parameters: Seq[LSUTokenizerDiplomaticParameter])
    extends SinkNode(LSUTokenizerNodeImp)(parameters) {
  require(parameters.size == 1, "not allow request more then one LSU from one node.")
}

class LSUTokenizerCoreBundleBridge extends Bundle {
  val executeInstruction: Valid[UInt] = Valid(UInt(32.W))
  val flush = Bool()
  val memoryHazard: Bool = Flipped(Bool())
}
class LSUTokenizerSourceNode extends SourceNode(LSUTokenizerNodeImp)(Seq())

/** A Component at Tile level to consume instruction at execute stage,
  * using token to maintain memory order among different LSUs,
  * including scalar, vector, RoCCs.
  * The detail uArch of this module is:
  * - it takes instruction from Execute stage(to be configured to be super-scalar in the future, if other cores needed);
  * - there is an issue queue(size is determined by outstanding size of downstream nodes);
  * - it will grant token to specific node if the current lsu type is the type register, in the same time, counter for this node will increase 1;
  * - LSUs from vector, RoCC, D$ etc should capture this grant and increase the token counter inside them;
  * - the counter inside LSUs should stall the TileLink outward transaction when token is zero;
  * - for each LSU they should release token to [[LSUTokenizer]] when LSU finished a memory instruction.(got memory ack)
  */
class LSUTokenizer()(implicit p: Parameters) extends LazyModule {
  lazy val module: LSUTokenizerImp = new LSUTokenizerImp(this)
  val coreBundle = BundleBridgeSink[LSUTokenizerCoreBundleBridge]
  val sourceNode = new LSUTokenizerSourceNode()
}

class LSUTokenizerImp(outer: LSUTokenizer) extends LazyModuleImp(outer) {
  val coreBundle = outer.coreBundle.in.headOption.map(_._1).get

  /** for each lsu, it has its own counter to record how many token is already issued to which. */
  val lsuCounters: Seq[UInt] = outer.sourceNode.in.map(_._2).map {
    case LSUTokenizerDiplomaticParameter(nodeName: String, outstandingSize: Int, _) =>
      RegInit(0.U(log2Ceil(outstandingSize).W)).suggestName(s"${nodeName}OutstandingCounter")
  }

  /** issue queue to store the current instruction, the element is the OH encode for LSU. */
  val issueQueue = new Queue(UInt(outer.sourceNode.in.size.W), outer.sourceNode.in.map(_._2.outstandingSize).sum)

  issueQueue.io.enq.valid := coreBundle.executeInstruction.valid
  // create an OH decoder from downstream instructions
  issueQueue.io.enq.bits := decoder(
    coreBundle.executeInstruction.bits,
    TruthTable(outer.sourceNode.in
      .map(_._2.giveTokenToMe).zipWithIndex.flatMap {
        case (bs: BitSet, index: Int) =>
          bs.terms.map(t => t -> BitPat((1 << index).U(outer.sourceNode.in.size.W)))
      }, BitPat.N(outer.sourceNode.in.size)
    )
  )
  assert(issueQueue.io.enq.ready, "issueQueue should always ready.")

  /** OH encode to show which lsu is occupied by LSU. */
  val currentLSU: UInt = RegEnable(issueQueue.io.deq.bits, 0.U(outer.sourceNode.in.size.W), issueQueue.io.deq.fire)

  /** if issueQueue dequeues, pull-up corresponding grant signal. */
  val grants: Seq[Bool] = outer.sourceNode.out.map(_._2.nodeName).lazyZip(issueQueue.io.deq.bits.asBools).map {
    case (nodeName, src) =>
      WireDefault(src && issueQueue.io.deq.fire).suggestName(s"${nodeName}Grant")
  }

  /** if token is given back, pull-up corresponding release signal. */
  val releases: Seq[Bool] = outer.sourceNode.out.map {
    case (dstBundle, LSUTokenizerDiplomaticParameter(nodeName, _, _)) =>
      WireDefault(dstBundle.release).suggestName(s"${nodeName}Release")
  }

  /** maintain a counter for each LSU. */
  val outstandingCounters: Seq[UInt] = outer.sourceNode.out.map(_._2).map {
    case LSUTokenizerDiplomaticParameter(nodeName, outstandingSize, _) =>
      RegInit(0.U(log2Ceil(outstandingSize).W)).suggestName(s"${nodeName}OutstandingCounter")
  }

  // increase or decrease each counters
  grants.lazyZip(releases).lazyZip(outstandingCounters).foreach {
    case (grant: Bool, release: Bool, outstandingCounter: UInt) =>
      when(grant && !release) {
        outstandingCounter := outstandingCounter + 1.U
      }
      when(release && !grant) {
        outstandingCounter := outstandingCounter - 1.U
      }
  }

  // fanin each grants.
  outer.sourceNode.out.map(_._1.grant).zip(grants).foreach { case (dst, src) => dst := src }

  val allLSUFree: Bool = VecInit(outstandingCounters.map(_ === 0.U)).asUInt.andR

  issueQueue.io.deq.ready := issueQueue.io.deq.bits === currentLSU && allLSUFree

  coreBundle.memoryHazard := allLSUFree && !issueQueue.io.deq.valid
}
