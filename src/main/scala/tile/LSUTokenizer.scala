package freechips.rocketchip.tile

import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.util.BitPat
import chisel3.util.experimental.decode.TruthTable
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters

case class LSUTokenizerSourceParameter()
case class LSUTokenizerSinkParameter(outstandingSize: Int,
                                     giveTokenToMe: TruthTable) {
  require(giveTokenToMe.default.value == 0, "doesn't allow default to 1.")
}
case class LSUTokenizerEdgeParameter(outstandingSize: Int,
                                     giveTokenToMe: TruthTable)
class LSUTokenizerBundle extends Bundle {
  /** Grant token from Tokenizer to LSU. */
  val grant: Bool = Bool()
  /** Release token from LSU to Tokenizer. */
  val release: Bool = Flipped(Bool())
}

object LSUTokenizerNodeImp extends SimpleNodeImp[
  LSUTokenizerSourceParameter,
  LSUTokenizerSinkParameter,
  LSUTokenizerEdgeParameter,
  LSUTokenizerBundle] {

  override def edge(pd: LSUTokenizerSourceParameter,
                    pu: LSUTokenizerSinkParameter,
                    p: Parameters,
                    sourceInfo: SourceInfo): LSUTokenizerEdgeParameter = LSUTokenizerEdgeParameter(pu.outstandingSize, pu.giveTokenToMe)

  override def bundle(e: LSUTokenizerEdgeParameter): LSUTokenizerBundle = new LSUTokenizerBundle

  override def render(e: LSUTokenizerEdgeParameter) = RenderedEdge(colour = "#00cc00")
}

class LSUTokenizerSourceNode(parameters: Seq[LSUTokenizerSourceParameter]) extends SourceNode(LSUTokenizerNodeImp)(parameters) {
  require(parameters.size == 1)
}

class LSUTokenizerSinkNode(parameters: Seq[LSUTokenizerSinkParameter]) extends SinkNode(LSUTokenizerNodeImp)(parameters)

class LSUTokenizerNexusNode extends NexusNode(LSUTokenizerNodeImp)(
  dFn = _ => LSUTokenizerSourceParameter(),
  uFn = fns => LSUTokenizerSinkParameter(
    // add all outstanding together
    fns.map(_.outstandingSize).sum,
    fns.map(_.giveTokenToMe).reduce((l, r) => TruthTable(
      // Merge all table together by extending rhs value to 0
      l.table.map(t => (t._1, t._2 ## BitPat.N(r.table.head._2.width))) ++ r.table.map(t => (t._1, BitPat.N(l.table.head._2.width) ## t._2)),
      // Set all default to N
      BitPat.N(l.table.head._2.width + r.table.head._2.width)))
  )
)

/** A Component at Tile level to consume issued instructions from core,
  * using token to maintain memory order among different LSUs,
  * including scalar, vector, RoCCs.
  * The detail uArch of this module is:
  * - it takes instruction from Execute stage(to be configured to be super-scalar in the future, if other cores needed);
  * - there is an issue queue(size is determined by outstanding size of downstream nodes);
  * - it will grant token to specific node if the current lsu type is the type register, in the same time, counter for this node will increase 1;
  * - LSUs from vector, RoCC, D$ etc should capture this grant and increase the token counter inside them;
  * - the counter inside LSUs should stall the TileLink outward transaction when token is zero;
  * - for each LSU they should release token to [[LSUTokenizer]] when 
  */
class LSUTokenizer()(implicit p: Parameters) extends LazyModule {
  lazy val module: LazyModuleImpLike = ???

  val lsuMasterNode = new LSUTokenizerSourceNode(Seq(LSUTokenizerSourceParameter()))

}


