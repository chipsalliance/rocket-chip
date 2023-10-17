package freechips.rocketchip.tile

import chisel3._
import chisel3.connectable.Alignment
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config._

case object BuildVector extends Field[Option[Parameters => AbstractLazyT1]](None)

/** Request from CPU to Vector.
  * aligned: core -> vector
  * flipped: vector -> core
  */
class VectorRequest(xLen: Int) extends Bundle {

  /** instruction fetched by scalar processor. */
  val instruction: UInt = UInt(32.W)

  /** data read from scalar RF RS1. */
  val rs1Data: UInt = UInt(xLen.W)

  /** data read from scalar RF RS2. */
  val rs2Data: UInt = UInt(xLen.W)
}

/** Response from Vector to CPU.
  * aligned: vector -> core
  * flipped: core -> vector
  */
class VectorResponse(xLen: Int) extends Bundle {

  /** data write to scalar rd. */
  val data: UInt = UInt(xLen.W)

  /** assert of [[rd.valid]] indicate vector need to write rd,
    * the [[rd.bits]] is the index of rd
    */
  val rd: Valid[UInt] = Valid(UInt(log2Ceil(32).W))

  /** Vector Fixed-Point Saturation Flag, propagate to vcsr in CSR.
    * This is not maintained in the vector coprocessor since it is not used in the Vector processor.
    */
  val vxsat: Bool = Bool()
}

/** IO for maintaining the memory hazard between Scalar and Vector core.
  * aligned: core -> vector
  * flipped: vector -> core
  */
class VectorHazardControl extends Bundle {
  /** vector issue queue is empty, there are no pending vector instructions, scalar can handle interrupt if it is asserted. */
  val issueQueueFull: Bool = Flipped(Bool())

  /** vector issue queue is full, scalar core cannot issue any vector instructions,
    * should back pressure the vector issue datapath(but don't block the entire pipeline).
    */
  val issueQueueEmpty: Bool = Flipped(Bool())

  /** Scalar core store buffer is cleared. So Vector memory can start to issue to memory subsystem. */
  val storeBufferClear: Bool = Flipped(Bool())

  /** tile grant an token. */
  val loadStoreTokenGrant: Bool = Flipped(Bool())

  /** scalar release an token. */
  val loadStoreTokenRelease: Bool = Flipped(Bool())
}

/** Scalar -> Vector IO.
  * aligned: vector -> core
  * flipped: core -> vector
  */
class T1CoreIO(xLen: Int) extends Bundle {
  /** Scalar to Vector Datapath.
    * at last stage of Core, [[request.valid]] is asserted.
    */
  val request: Valid[VectorRequest] = Valid(new VectorRequest(xLen))

  /** Vector to Scalar Datapath.
    * when [[response.valid]], a vector instruction should assert.
    */
  val response: ValidIO[VectorResponse] = Flipped(Valid(new VectorResponse(xLen)))
}

/** The hierarchy under [[BaseTile]]. */
abstract class AbstractLazyT1()(implicit p: Parameters) extends LazyModule {
  def module: AbstractLazyT1ModuleImp
  def xLen: Int
  def vLen: Int
  def banks: Int
  def uarchName: String
  def sourceIdSize: Int
  def banksMapping(bank: Int): AddressSet

  val vectorMasterNode = TLClientNode(Seq.tabulate(banks)(bank => TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = s"${uarchName}_bank$bank",
      sourceId = IdRange(0, sourceIdSize - 1),
      visibility = Seq(banksMapping(bank)),
    ))
  )))

  val requestNode: BundleBridgeSource[ValidIO[VectorRequest]] = BundleBridgeSource(() => Flipped(Valid(new VectorRequest(xLen))))
  val responseNode: BundleBridgeSource[ValidIO[VectorResponse]] = BundleBridgeSource(() => Valid(new VectorResponse(xLen)))
  val hazardControlNode: BundleBridgeSource[VectorHazardControl] = BundleBridgeSource(() => Output(new VectorHazardControl))
}

/** This is a vector interface comply to chipsalliance/t1 project.
  * but is should be configurable module for fitting different vector architectures
  */
abstract class AbstractLazyT1ModuleImp(outer: AbstractLazyT1)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val request: Valid[VectorRequest] = outer.requestNode.bundle
  val response: ValidIO[VectorResponse] = outer.responseNode.bundle
  val hazardControl: VectorHazardControl = outer.hazardControlNode.bundle
}

trait HasLazyT1 { this: BaseTile =>
  val vector = p(BuildVector).map(_(p))
  val requestSinkNode: Option[BundleBridgeSink[ValidIO[VectorRequest]]] = vector.map(_.requestNode.makeSink())
  val responseSinkNode: Option[BundleBridgeSink[ValidIO[VectorResponse]]] = vector.map(_.responseNode.makeSink())
  val hazardControlSinkNode: Option[BundleBridgeSink[VectorHazardControl]] = vector.map(_.hazardControlNode.makeSink())
  vector.foreach(tlMasterXbar.node :=* _.vectorMasterNode)
}

trait HasLazyT1Module  { this: RocketTileModuleImp =>

  outer.vector.map(_.module).foreach { t1Module =>
    // TODO: make it configurable
    val maxCount: Int = 32
    /** at commit stage, CPU says: issued a vector load instruction! */
    val vectorGrant = WireDefault(false.B) // from [[core.io]]
    /** after vector finish a load, release this token. */
    val vectorRelease = WireDefault(false.B) // from [[t1Module.]]

    // for scalar token, we forget it for now.
    /** the memory load store token issuer:
      * it will hazard scalar load store instructions for waiting there is not pending vector load store in the pipeline.
      * TODO: when resolving interrupt/exception, the counter should be 0.
      */
    val vectorCounter = RegInit(0.U(log2Up(maxCount).W))
    /** vector load store counter is full, so don't all acquire. */
    val vectorCounterIsFull = WireDefault(false.B)

    when(!vectorCounterIsFull && (vectorGrant && !vectorRelease)) {
      vectorCounter := vectorCounter + 1.U
    }
    when(!vectorCounterIsFull && (vectorGrant && !vectorRelease)) {
      vectorCounter := vectorCounter - 1.U
    }
    vectorCounterIsFull := vectorCounter === maxCount.U

    // pull bundle bridge from T1 here:
    // TODO: I wanna make RocketCore diplomatic...
    (core.t1Request zip outer.requestSinkNode.map(_.bundle)).foreach { case (c, v) =>
      v <> c
    }
    (core.t1Response zip outer.responseSinkNode.map(_.bundle)).foreach { case (c, v) =>
      c <> v
    }
    outer.hazardControlSinkNode.map(_.bundle).lazyZip(core.t1IssueQueueEmpty).lazyZip(core.t1IssueQueueFull).foreach {
      case (c, e, f) =>
        c.loadStoreTokenGrant := vectorGrant
        vectorRelease := c.loadStoreTokenRelease
        c.issueQueueEmpty := false.B
        c.issueQueueFull := false.B
        // TODO: fixme
        c.storeBufferClear := true.B
    }
  }
}