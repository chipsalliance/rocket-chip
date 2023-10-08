// See LICENSE.SiFive for license details.
package freechips.rocketchip.amba.axis

import chisel3._
import chisel3.util._
import freechips.rocketchip.util._

sealed trait AXISKey
case object AXISLast extends ControlKey[Bool]("last") with AXISKey
case object AXISId   extends ControlKey[UInt]("id")   with AXISKey
case object AXISDest extends ControlKey[UInt]("dest") with AXISKey
case object AXISKeep extends DataKey   [UInt]("keep") with AXISKey
case object AXISStrb extends DataKey   [UInt]("strb") with AXISKey
case object AXISData extends DataKey   [UInt]("data") with AXISKey

case class AXISLastField()           extends SimpleBundleField(AXISLast)(Output(Bool()),        true.B)
case class AXISIdField  (width: Int) extends SimpleBundleField(AXISId)  (Output(UInt(width.W)), 0.U)
case class AXISDestField(width: Int) extends SimpleBundleField(AXISDest)(Output(UInt(width.W)), 0.U)
case class AXISKeepField(width: Int) extends SimpleBundleField(AXISKeep)(Output(UInt(width.W)), ~0.U(width.W))
case class AXISStrbField(width: Int) extends SimpleBundleField(AXISStrb)(Output(UInt(width.W)), ~0.U(width.W))
case class AXISDataField(width: Int) extends BundleField[UInt](AXISData, Output(UInt(width.W)), _ := DontCare)

class AXISBundleBits(val params: AXISBundleParameters) extends BundleMap(AXISBundle.keys(params)) {
  def last = if (params.hasLast) apply(AXISLast) else true.B
  def id   = if (params.hasId)   apply(AXISId)   else 0.U
  def dest = if (params.hasDest) apply(AXISDest) else 0.U
  def keep = if (params.hasKeep) apply(AXISKeep) else ~0.U(params.keepBits.W)
  def strb = if (params.hasStrb) apply(AXISStrb) else ~0.U(params.strbBits.W)
  def data = if (params.hasData) apply(AXISData) else 0.U(params.dataBits.W)
}

class AXISBundle(val params: AXISBundleParameters) extends IrrevocableIO(new AXISBundleBits(params)) {
}

object AXISBundle {
  def apply(params: AXISBundleParameters) = new AXISBundle(params)
  def standardKeys(params: AXISBundleParameters) = {
    def maybe(b: Boolean, x: BundleFieldBase): Seq[BundleFieldBase] = if (b) List(x) else Nil
    maybe(params.hasLast, AXISLastField())                ++
    maybe(params.hasId,   AXISIdField  (params.idBits  )) ++
    maybe(params.hasDest, AXISDestField(params.destBits)) ++
    maybe(params.hasKeep, AXISKeepField(params.keepBits)) ++
    maybe(params.hasStrb, AXISStrbField(params.strbBits)) ++
    maybe(params.hasData, AXISDataField(params.dataBits))
  }
  def keys(params: AXISBundleParameters) =
    standardKeys(params) ++ params.userFields
}
