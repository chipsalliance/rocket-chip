// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

case class DevNullParams(
  address: Seq[AddressSet],
  maxAtomic: Int,
  maxTransfer: Int,
  region: RegionType.T = RegionType.UNCACHEABLE,
  executable: Boolean = true,
  mayDenyGet: Boolean = true,
  mayDenyPut: Boolean = true,
) {
  require (1 <= maxAtomic, s"Atomic transfer size must be > 1 (was $maxAtomic)")
  require (maxAtomic <= maxTransfer, s"Atomic transfer size must be <= max transfer (but $maxAtomic > $maxTransfer)")
  require (maxTransfer <= 4096, s"Max transfer size must be <= 4096 (was $maxTransfer)")
  def acquire: Boolean = region == RegionType.TRACKED
}

/** DevNullDevices don't obey standard memory operation semantics.
  * They may discard writes, refuse to respond to requests, issue error responses,
  * or otherwise violate 'expected' memory behavior.
  */
abstract class DevNullDevice(params: DevNullParams, beatBytes: Int, device: SimpleDevice)
                            (implicit p: Parameters)
    extends LazyModule with HasClockDomainCrossing {
  val xfer = TransferSizes(1, params.maxTransfer)
  val atom = TransferSizes(1, params.maxAtomic)
  val node = TLManagerNode(Seq(TLManagerPortParameters(
    Seq(TLManagerParameters(
      address            = params.address,
      resources          = device.reg,
      regionType         = params.region,
      executable         = params.executable,
      supportsAcquireT   = if (params.acquire) xfer else TransferSizes.none,
      supportsAcquireB   = if (params.acquire) xfer else TransferSizes.none,
      supportsGet        = xfer,
      supportsPutPartial = xfer,
      supportsPutFull    = xfer,
      supportsArithmetic = atom,
      supportsLogical    = atom,
      supportsHint       = xfer,
      fifoId             = Some(0), // requests are handled in order
      mayDenyGet         = params.mayDenyGet,
      mayDenyPut         = params.mayDenyPut,
      alwaysGrantsT      = params.acquire)),
    beatBytes  = beatBytes,
    endSinkId  = if (params.acquire) 1 else 0,
    minLatency = 1))) // no bypass needed for this device
  val tl_xing = this.crossIn(node)
}
