// See LICENSE.SiFive for license details.
package freechips.rocketchip.amba.axis

import chisel3._
import chisel3.util._
import freechips.rocketchip.util._

sealed trait AXISKey
case object AXISLast extends ControlKey[Bool]("last", _ := true.B) with AXISKey
case object AXISId   extends ControlKey[UInt]("id",   _ := 0.U)    with AXISKey
case object AXISDest extends ControlKey[UInt]("dest", _ := 0.U)    with AXISKey
case object AXISKeep extends DataKey   [UInt]("keep", _ := ~0.U)   with AXISKey
case object AXISStrb extends DataKey   [UInt]("strb", _ := ~0.U)   with AXISKey
case object AXISData extends DataKey   [UInt]("data", _ := 0.U)    with AXISKey

class AXISBundleBits(val params: AXISBundleParameters) extends BundleMap(AXISBundle.keys(params)) {
  override def cloneType: this.type = (new AXISBundleBits(params)).asInstanceOf[this.type]
  def last = if (params.hasLast) apply(AXISLast) else true.B
  def id   = if (params.hasId)   apply(AXISId)   else 0.U
  def dest = if (params.hasDest) apply(AXISDest) else 0.U
  def keep = if (params.hasKeep) apply(AXISKeep) else ~0.U(params.keepBits.W)
  def strb = if (params.hasStrb) apply(AXISStrb) else ~0.U(params.strbBits.W)
  def data = if (params.hasData) apply(AXISData) else 0.U(params.dataBits.W)
}

class AXISBundle(val params: AXISBundleParameters) extends DecoupledIO(new AXISBundleBits(params)) {
  override def cloneType: this.type = (new AXISBundle(params)).asInstanceOf[this.type]
}

object AXISBundle {
  def apply(params: AXISBundleParameters) = new AXISBundle(params)
  def standardKeys(params: AXISBundleParameters) = {
    def maybe(b: Boolean, x: BundleField): Seq[BundleField] = if (b) List(x) else Nil
    maybe(params.hasLast, AXISLast(Bool()))                  ++
    maybe(params.hasId,   AXISId  (UInt(params.idBits  .W))) ++
    maybe(params.hasDest, AXISDest(UInt(params.destBits.W))) ++
    maybe(params.hasKeep, AXISKeep(UInt(params.keepBits.W))) ++
    maybe(params.hasStrb, AXISStrb(UInt(params.strbBits.W))) ++
    maybe(params.hasData, AXISData(UInt(params.dataBits.W)))
  }
  def keys(params: AXISBundleParameters) =
    standardKeys(params) ++ params.userFields
}
