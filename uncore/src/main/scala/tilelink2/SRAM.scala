// See LICENSE for license details.

package uncore.tilelink2

import Chisel._

class TLRAM(address: AddressSet, beatBytes: Int = 4) extends TLSimpleFactory
{
  val node = TLManagerNode(beatBytes, TLManagerParameters(
    address            = List(address),
    regionType         = RegionType.UNCACHED,
    supportsGet        = TransferSizes(1, beatBytes),
    supportsPutPartial = TransferSizes(1, beatBytes),
    supportsPutFull    = TransferSizes(1, beatBytes),
    fifoId             = Some(0))) // requests are handled in order

  lazy val module = Module(new TLModule(this) {
    val io = new Bundle {
      val in = node.bundleIn
    }
    
    // do stuff
  })
}
