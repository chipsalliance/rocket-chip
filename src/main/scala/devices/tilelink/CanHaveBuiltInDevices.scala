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

/* Optionally add some built-in devices to a bus wrapper */
trait CanHaveBuiltInDevices { this: TLBusWrapper =>

  def attachBuiltInDevices(params: HasBuiltInDeviceParams): BuiltInDevices = new BuiltInDevices {
    val errorOpt = params.errorDevice.map { dnp => LazyScope("wrapped_error_device") {
      val error = LazyModule(new TLError(
        params = dnp,
        beatBytes = beatBytes))

      error.node := TLBuffer() := outwardNode
      error
    }}

    val zeroOpt = params.zeroDevice.map { addr => LazyScope("wrapped_zero_device") {
      val zero = LazyModule(new TLZero(
        address = addr,
        beatBytes = beatBytes))
      zero.node := TLFragmenter(beatBytes, blockBytes) := TLBuffer() := outwardNode
      zero
    }}
  }
}

