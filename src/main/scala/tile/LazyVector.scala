package freechips.rocketchip.tile

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config._

case object VLen extends Field[Int]
case object BuildVector extends Field[Option[Parameters => LazyVector]](None)
abstract class LazyVector()(implicit p: Parameters) extends LazyModule {
  val module: LazyVectorModuleImp
  val banks: Int
  val uarchName: String
  val sourceIdSize: Int
  def banksMapping(bank: Int): AddressSet
  val clientNode = TLClientNode(Seq.tabulate(banks)(bank => TLMasterPortParameters.v1(
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
  val xLen: Int = p(XLen)
  val vLen: Int = p(VLen)
  val request: DecoupledIO[VectorRequest] = IO(Flipped(Decoupled(new VectorRequest(xLen))))
  val response: ValidIO[VectorResponse] = IO(Valid(new VectorResponse(xLen)))
  val csrInterface: CSRInterface = IO(Input(new CSRInterface(log2Ceil(vLen) + 1)))
  val storeBufferClear: Bool = IO(Input(Bool()))
  // Memory Ports: outer.tlNode.out
}

/** Request from CPU to Vector. */
class VectorRequest(xLen: Int) extends Bundle {

  /** instruction fetched by scalar processor. */
  val instruction: UInt = UInt(32.W)

  /** data read from scalar RF RS1. */
  val rs1Data: UInt = UInt(xLen.W)

  /** data read from scalar RF RS2. */
  val rs2Data: UInt = UInt(xLen.W)
}

/** Response from Vector to CPU. */
class VectorResponse(xLen: Int) extends Bundle {

  /** data write to scalar rd. */
  val data: UInt = UInt(xLen.W)

  /** Vector Fixed-Point Saturation Flag, propagate to vcsr in CSR.
   * This is not maintained in the vector coprocessor since it is not used in the Vector processor.
   */
  val vxsat: Bool = Bool()

  /** assert of [[rd.valid]] indicate vector need to write rd,
   * the [[rd.bits]] is the index of rd
   * TODO: merge [[data]] to rd.
   */
  val rd: ValidIO[UInt] = Valid(UInt(log2Ceil(32).W))

  /** when [[memAccess]] is asserted, indicate one instruction accessed memory.
   * if the vector instruction need to access memory,
   * to maintain the order of memory access:
   * the scalar core maintains a counter of vector memory access,
   * if the counter is not zero, the memory access instruction of scalar core will stall until the counter is zero.
   */
  val memAccess: Bool = Bool()
}

/** CSR Interface from Scalar Core. */
class CSRInterface(vlWidth: Int) extends Bundle {
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
   * subfield of `vtype``
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
   * we always keep the undisturbed behavior, since there is no rename here.
   */
  val vta: Bool = Bool()

  /** Vector Mask Agnostic
   * see [[https://github.com/riscv/riscv-v-spec/blob/8c8a53ccc70519755a25203e14c10068a814d4fd/v-spec.adoc#38-vector-fixed-point-rounding-mode-register-vxrm]]
   *
   * we always keep the undisturbed behavior, since there is no rename here.
   */
  val vma: Bool = Bool()
}

trait HasLazyVector { this: BaseTile =>
  val vector: Option[LazyVector] = p(BuildVector).map(_(p))
  val vectorMemoryNode = vector.map(_.clientNode)
}