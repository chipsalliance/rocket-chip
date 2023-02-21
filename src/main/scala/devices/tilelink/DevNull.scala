// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

case class DevNullParams(
  address: Seq[AddressSet],
  maxAtomic: Int,
  maxTransfer: Int,
  region: RegionType.T = RegionType.VOLATILE,
  executable: Boolean = true,
  mayDenyGet: Boolean = true,
  mayDenyPut: Boolean = true,
  hint: Boolean = true
) {
  require (maxAtomic <= maxTransfer, s"Atomic transfer size must be <= max transfer (but $maxAtomic > $maxTransfer)")
  require (maxTransfer <= 4096, s"Max transfer size must be <= 4096 (was $maxTransfer)")
  def acquire: Boolean = region == RegionType.TRACKED
}

/** DevNullDevices don't obey standard memory operation semantics.
  * They may discard writes, refuse to respond to requests, issue error responses,
  * or otherwise violate 'expected' memory behavior.
  */
abstract class DevNullDevice(params: DevNullParams, minLatency: Int, beatBytes: Int, protected val device: SimpleDevice)
                            (implicit p: Parameters)
    extends LazyModule with HasClockDomainCrossing {
  val xfer = if (params.maxTransfer > 0) TransferSizes(1, params.maxTransfer) else TransferSizes.none
  val atom = if (params.maxAtomic > 0) TransferSizes(1, params.maxAtomic) else TransferSizes.none
  val acq  = if (params.acquire) xfer else TransferSizes.none
  val hint = if (params.hint) xfer else TransferSizes.none
  val node = TLManagerNode(Seq(TLSlavePortParameters.v1(
    Seq(TLSlaveParameters.v1(
      address            = params.address,
      resources          = device.reg,
      regionType         = params.region,
      executable         = params.executable,
      supportsAcquireT   = acq,
      supportsAcquireB   = acq,
      supportsGet        = xfer,
      supportsPutPartial = xfer,
      supportsPutFull    = xfer,
      supportsArithmetic = atom,
      supportsLogical    = atom,
      supportsHint       = hint,
      fifoId             = Some(0), // requests are handled in order
      mayDenyGet         = params.mayDenyGet,
      mayDenyPut         = params.mayDenyPut,
      alwaysGrantsT      = params.acquire)),
    beatBytes  = beatBytes,
    endSinkId  = if (params.acquire) 1 else 0,
    minLatency = minLatency)))
  val tl_xing = this.crossIn(node)
}
