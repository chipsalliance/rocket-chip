// See LICENSE for license details.

package freechips.rocketchip.coreplex

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.LazyMultiIOModuleImp
import freechips.rocketchip.tile.ResetVectorBits

/** Allow the reset vector to be overridden in Configs */
case object ResetVectorKey extends Field[BigInt]

/** A single place for all tiles to find out the reset vector */
trait HasGlobalResetVectorWire {
  implicit val p: Parameters
  val resetVectorBits = p(ResetVectorBits)
  val global_reset_vector = Wire(UInt(width = resetVectorBits))
}

/** Global reset vector is hardwired to a constant */
trait HasHardwiredResetVectorModuleImp extends LazyMultiIOModuleImp
    with HasGlobalResetVectorWire {
  global_reset_vector := UInt(p(ResetVectorKey), width = resetVectorBits)
}

/** Manage an externally driven reset vector */
trait HasExternalResetVectorBundle {
  implicit val p: Parameters

  val reset_vector: Option[UInt]

  def driveExternalResetVector(dummy: Int = 1) {
    reset_vector.foreach { _ := UInt(p(ResetVectorKey))}
  }
}

/** Global reset vector is driven externally */
trait HasExternalResetVectorModuleImp extends LazyMultiIOModuleImp
    with HasGlobalResetVectorWire
    with HasExternalResetVectorBundle {
  val reset_vector = Some(IO(UInt(INPUT, width = resetVectorBits)))
  global_reset_vector := reset_vector.get
}
