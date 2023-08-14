// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import chisel3._
import chisel3.util._
import chisel3.experimental.IntParam
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._

case object BuildVector extends Field[Option[Parameters => Vector]](None)
case object VLen extends Field[Int](0)

class VectorCommand(implicit p: Parameters) extends CoreBundle()(p) {
  val inst = Bits(32.W)
  val rs1 = Bits(xLen.W)
  val rs2 = Bits(xLen.W)
  val vconfig = new VConfig
  val vstart = UInt(log2Ceil(maxVLMax).W)
  val vxrm = UInt(2.W)
}

class VectorResponse(implicit p: Parameters) extends CoreBundle()(p) {
  val rd = Bits(5.W)
  val data = Bits(xLen.W)
  val mem = Bool()
  val vxsat = Bool()
}

class VectorCoreIO(implicit p: Parameters) extends CoreBundle()(p) {
  val cmd = Flipped(Decoupled(new VectorCommand))
  val resp = Decoupled(new VectorResponse)
  // TODO: handle exception in Vector
  // val exception = Input(Bool())
}

/** Base classes for Diplomatic TL2 Vector units **/
abstract class Vector()(implicit p: Parameters) extends LazyModule {
  val module: VectorModuleImp
  val atlNode: TLNode = TLIdentityNode()
  val tlNode: TLNode = TLIdentityNode()

  require(p(VLen) > 0, s"vLen can not be 0 when instantiating Vector module, do you forget to apply WithVector?")
}

class VectorModuleImp(outer: Vector) extends LazyModuleImp(outer) {
  val io = IO(new VectorCoreIO)
}

/** Mixins for including Vector **/

trait HasVector { this: BaseTile =>
  val vector = p(BuildVector).map(_(p))

  vector.map(_.atlNode).foreach { atl => tlMasterXbar.node :=* atl }
  vector.map(_.tlNode).foreach { tl => tlOtherMastersNode :=* tl }
}
