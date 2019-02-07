// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

trait HasBuiltInDeviceParams {
  val zeroDevice: Option[AddressSet]
  val errorDevice: Option[DevNullParams]
}

/* Optionally add some built-in devices to a bus wrapper */
trait CanHaveBuiltInDevices { this: TLBusWrapper =>

  def attachBuiltInDevices(params: HasBuiltInDeviceParams) {
    params.errorDevice.foreach { dnp => LazyScope("wrapped_error_device") {
      val error = LazyModule(new TLError(
        params = dnp,
        beatBytes = beatBytes))
      error.node := TLBuffer() := outwardNode
    }}

    params.zeroDevice.foreach { addr => LazyScope("wrapped_zero_device") {
      val zero = LazyModule(new TLZero(
        address = addr,
        beatBytes = beatBytes))
      zero.node := TLFragmenter(beatBytes, blockBytes) := TLBuffer() := outwardNode
    }}
  }
}

