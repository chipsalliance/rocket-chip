package freechips.rocketchip.tile

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.TwoWayCounter
import org.chipsalliance.cde.config._

case object VLen extends Field[Int]
case object BuildVector extends Field[Option[Parameters => LazyVector]](None)

/** Request from CPU to Vector. */
class VectorRequest(xLen: Int, vlWidth: Int) extends Bundle {

  /** instruction fetched by scalar processor. */
  val instruction: UInt = UInt(32.W)

  /** data read from scalar RF RS1. */
  val rs1Data: UInt = UInt(xLen.W)

  /** data read from scalar RF RS2. */
  val rs2Data: UInt = UInt(xLen.W)

  // CSRs below will follow datapath going to Vector issue queue for chaining efficiency

  /** Vector Length Register `vl`,
    * see [[https://github.com/riscv/riscv-v-spec/blob/8c8a53ccc70519755a25203e14c10068a814d4fd/v-spec.adoc#35-vector-length-register-vl]]
    */
  val vl: UInt = UInt(vlWidth.W)

  /** Vector Start Index CSR `vstart`,
    * see [[https://github.com/riscv/riscv-v-spec/blob/8c8a53ccc70519755a25203e14c10068a814d4fd/v-spec.adoc#37-vector-start-index-csr-vstart]]
    */
  val vstart: UInt = UInt(vlWidth.W)

  /** Vector Register Grouping `vlmul[2:0]`
    * subfield of `vtype`
    * see table in [[https://github.com/riscv/riscv-v-spec/blob/8c8a53ccc70519755a25203e14c10068a814d4fd/v-spec.adoc#342-vector-register-grouping-vlmul20]]
    */
  val vlmul: UInt = UInt(3.W)

  /** Vector Register Grouping (vlmul[2:0])
    * subfield of `vtype`
    * see [[https://github.com/riscv/riscv-v-spec/blob/8c8a53ccc70519755a25203e14c10068a814d4fd/v-spec.adoc#341-vector-selected-element-width-vsew20]]
    */
  val vsew: UInt = UInt(2.W)

  /** Rounding mode register
    * see [[https://github.com/riscv/riscv-v-spec/blob/8c8a53ccc70519755a25203e14c10068a814d4fd/v-spec.adoc#38-vector-fixed-point-rounding-mode-register-vxrm]]
    */
  val vxrm: UInt = UInt(2.W)

  /** Vector Tail Agnostic
    * see [[https://github.com/riscv/riscv-v-spec/blob/8c8a53ccc70519755a25203e14c10068a814d4fd/v-spec.adoc#38-vector-fixed-point-rounding-mode-register-vxrm]]
    *
    * @note
    * T1 always keep the undisturbed behavior, since there is no rename here.
    */
  val vta: Bool = Bool()

  /** Vector Mask Agnostic
    * see [[https://github.com/riscv/riscv-v-spec/blob/8c8a53ccc70519755a25203e14c10068a814d4fd/v-spec.adoc#38-vector-fixed-point-rounding-mode-register-vxrm]]
    *
    * @note
    * T1 always keep the undisturbed behavior, since there is no rename here.
    */
  val vma: Bool = Bool()
}

/** Response from Vector to CPU. */
class VectorResponse(xLen: Int) extends Bundle {

  /** data write to scalar rd. */
  val data: UInt = UInt(xLen.W)

  /** assert of [[rd.valid]] indicate vector need to write rd,
    * the [[rd.bits]] is the index of rd
    */
  val rd: ValidIO[UInt] = Valid(UInt(log2Ceil(32).W))

  /** Vector Fixed-Point Saturation Flag, propagate to vcsr in CSR.
    * This is not maintained in the vector coprocessor since it is not used in the Vector processor.
    */
  val vxsat: Bool = Bool()
}

/** IO for maintaining the memory hazard between Scalar and Vector core. */
class VectorControl extends Bundle {
  /** Scalar core store buffer is cleared. So Vector memory can start to issue to memory subsystem. */
  val storeBufferClear: Bool = Bool()

  /** when [[memTokenAcquire]] is asserted, indicate one of vector load store is issued in Scalar Core. */
  val memTokenAcquire: Bool = Bool()

  /** when [[memTokenRelease]] is asserted, indicate one of vector load store instruction finished accessing memory. */
  val memTokenRelease: Bool = Flipped(Bool())
}
/** Vector -> Scalar IO. */
class VectorCoreIO(xLen: Int, vLen: Int) extends Bundle {

  /** Scalar to Vector Datapath.
    * at last stage of Core, [[request.valid]] is asserted.
    * [[request.ready]] is back-pressured from vector issue queue.
    */
  val request: DecoupledIO[VectorRequest] = Flipped(Decoupled(new VectorRequest(xLen, log2Ceil(vLen))))

  /** Vector to Scalar Datapath.
    * when [[response.valid]], a vector instruction should assert.
    */
  val response: ValidIO[VectorResponse] = Valid(new VectorResponse(xLen))


  /** Memory hazard resolving IO. */
  val control = Flipped(new VectorControl)
}

/** The hierarchy under [[BaseTile]]. */
abstract class LazyVector()(implicit p: Parameters) extends LazyModule {
  val module: LazyVectorModuleImp
  val banks: Int
  val uarchName: String
  val sourceIdSize: Int
  def banksMapping(bank: Int): AddressSet

  val vectorMasterNode = TLClientNode(Seq.tabulate(banks)(bank => TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = s"${uarchName}_bank$bank",
      sourceId = IdRange(0, sourceIdSize - 1),
      visibility = Seq(banksMapping(bank)),
    ))
  )))
}

/** This is a vector interface comply to chipsalliance/t1 project.
  * but is should be configurable module for fitting different vector architectures
  */
abstract class LazyVectorModuleImp(outer: LazyVector)(implicit p: Parameters) extends LazyModuleImp(outer) {
  // CPU IO
  val vectorCoreIO = IO(new VectorCoreIO(p(XLen), p(VLen)))

  // Sub Memory IO:
  outer.vectorMasterNode.out
}

trait HasLazyVector { this: BaseTile =>
  val vector: Option[LazyVector] = p(BuildVector).map(_(p))
  val vectorMasterNode: Option[TLClientNode] = vector.map(_.vectorMasterNode)
}

trait HasLazyVectorModule  { this: RocketTileModuleImp =>
  /** when a vector store instruction is issued, in rocket core, aka goes to execute stage,
    * it should acquire an token from from [[vectorLoadStoreCounter]]
    */
  val vectorLoadStoreAcquire = Option.when(usingVector)(WireDefault(false.B))
  /** when a vector instruction is finished, release a token, and let scalar goes down. */
  val vectorLoadStoreRelease = Option.when(usingVector)(WireDefault(false.B))
  /** the memory load store token issuer:
    * it will hazard scalar load store instructions for waiting there is not pending vector load store in the pipeline.
    * TODO: when resolving interrupt, the counter should be 0.
    */
  val vectorLoadStoreCounter = Option.when(usingVector)(TwoWayCounter(vectorLoadStoreAcquire.get, vectorLoadStoreRelease.get, 32))

  core.io.vector.foreach { io =>
    io.request
    io.response
    vectorLoadStoreAcquire.get := io.control.memTokenAcquire
    vectorLoadStoreRelease.get := io.control.memTokenRelease

  }
}