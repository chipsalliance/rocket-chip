package freechips.rocketchip.tile

import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.util.{BitPat, Decoupled, DecoupledIO, Queue, RegEnable, Valid, ValidIO, log2Ceil}
import chisel3.util.experimental.decode.{TruthTable, decoder}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.TwoWayCounter
import org.chipsalliance.cde.config.Parameters

case class LSUTokenizerDiplomaticParameter(nodeName: String, outstandingSize: Int, giveTokenToMe: TruthTable) {
  require(giveTokenToMe.default.value == 0, "doesn't allow default to 1.")
}
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
  issueQueue.io.enq.bits := decoder(
    coreBundle.executeInstruction.bits,
    outer.sourceNode.in
      .map(_._2.giveTokenToMe)
      .reduce((l, r) =>
        TruthTable(
          // Merge all table together by extending rhs value to 0
          l.table.map(t => (t._1, t._2 ## BitPat.N(r.table.head._2.width))) ++ r.table
            .map(t => (t._1, BitPat.N(l.table.head._2.width) ## t._2)),
          // Set all default to N
          BitPat.N(l.table.head._2.width + r.table.head._2.width)
        )
      )
  )
  assert(issueQueue.io.enq.ready, "issueQueue should always ready.")

  /** OH encode to show which lsu is occupied by LSU. */
  val currentLSU: UInt = RegEnable(issueQueue.io.deq.bits, 0.U(outer.sourceNode.in.size.W), issueQueue.io.deq.fire)

  val outstandingCounters: Seq[UInt] = outer.sourceNode.out.zip(issueQueue.io.deq.bits.asBools).map {
    case ((dstBundle, dstParameter), src) =>
      val grant: Bool = WireDefault(src && issueQueue.io.deq.fire).suggestName(s"${dstParameter.nodeName}Grant")
      dstBundle.grant := grant

      val release: Bool = WireDefault(dstBundle.release).suggestName(s"${dstParameter.nodeName}Release")

      val outstandingCounter = RegInit(0.U(log2Ceil(dstParameter.outstandingSize).W)).suggestName(s"${dstParameter.nodeName}OutstandingCounter")
      when(grant && !release) {
        outstandingCounter := outstandingCounter + 1.U
      }
      when(release && !grant) {
        outstandingCounter := outstandingCounter - 1.U
      }
      outstandingCounter
  }

  val allLSUFree: Bool = VecInit(outstandingCounters.map(_ === 0.U)).asUInt.andR

  issueQueue.io.deq.ready := issueQueue.io.deq.bits === currentLSU && allLSUFree

  coreBundle.memoryHazard := allLSUFree && !issueQueue.io.deq.valid

}
