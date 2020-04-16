// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

trait HasBuiltInDeviceParams {
  val zeroDevice: Option[AddressSet]
  val errorDevice: Option[DevNullParams]
}

sealed trait BuiltInDevices {
  def errorOpt: Option[TLError]
  def zeroOpt: Option[TLZero]
}

object BuiltInDevices {
  def none = new BuiltInDevices {
    val errorOpt = None
    val zeroOpt = None
  }

  def attach(
    params: HasBuiltInDeviceParams with HasTLBusParams,
    outwardNode: TLOutwardNode)(implicit p: Parameters) = new BuiltInDevices {
    val errorOpt = params.errorDevice.map { dnp => LazyScope("wrapped_error_device") {
      val error = LazyModule(new TLError(
        params = dnp,
        beatBytes = params.beatBytes))

      error.node := TLBuffer() := outwardNode
      error
    }}

    val zeroOpt = params.zeroDevice.map { addr => LazyScope("wrapped_zero_device") {
      val zero = LazyModule(new TLZero(
        address = addr,
        beatBytes = params.beatBytes))
      zero.node := TLFragmenter(params.beatBytes, params.blockBytes) := TLBuffer() := outwardNode
      zero
    }}
  }
}

/* Optionally add some built-in devices to a bus wrapper */
trait CanHaveBuiltInDevices {
  def builtInDevices: BuiltInDevices
}

