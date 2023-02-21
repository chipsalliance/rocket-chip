// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

case class BuiltInZeroDeviceParams(
  addr: AddressSet,
  cacheCork: Option[TLCacheCorkParams] = None,
  buffer: Option[BufferParams] = Some(BufferParams.default))

case class BuiltInErrorDeviceParams(
  errorParams: DevNullParams,
  buffer: Option[BufferParams] = Some(BufferParams.default))

trait HasBuiltInDeviceParams {
  val zeroDevice: Option[BuiltInZeroDeviceParams]
  val errorDevice: Option[BuiltInErrorDeviceParams]
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
    val errorOpt = params.errorDevice.map { dnp => LazyScope("wrapped_error_device", "ErrorDeviceWrapper") {
      val error = LazyModule(new TLError(
        params = dnp.errorParams,
        beatBytes = params.beatBytes))

      (error.node
        := dnp.buffer.map { params => TLBuffer(params) }.getOrElse{ TLTempNode() }
        := outwardNode)
      error
    }}

    val zeroOpt = params.zeroDevice.map { zeroParams => LazyScope("wrapped_zero_device", "ZeroDeviceWrapper") {
      val zero = LazyModule(new TLZero(
        address = zeroParams.addr,
        beatBytes = params.beatBytes))
      (zero.node
        := TLFragmenter(params.beatBytes, params.blockBytes)
        := zeroParams.buffer.map { params => TLBuffer(params) }.getOrElse { TLTempNode() }
        := zeroParams.cacheCork.map { params => TLCacheCork(params) }.getOrElse { TLTempNode() }
        := outwardNode)
      zero
    }}
  }
}

/* Optionally add some built-in devices to a bus wrapper */
trait CanHaveBuiltInDevices {
  def builtInDevices: BuiltInDevices
}

