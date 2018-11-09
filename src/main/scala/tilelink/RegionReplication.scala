// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

case object MultiChipMaskKey extends Field[BigInt](0)

trait HasRegionReplicatorParams {
  val replicatorMask: BigInt
}

// Replicate all devices below this adapter to multiple addreses.
// If a device was at 0x4000-0x4fff and mask=0x10000, it will now be at 0x04000-0x04fff and 0x14000-0x14fff.
class RegionReplicator(mask: BigInt = 0)(implicit p: Parameters) extends LazyModule {
  def ids = AddressSet.enumerateMask(mask)

  val node = TLAdapterNode(
    clientFn  = { cp => cp },
    managerFn = { mp => mp.copy(managers = mp.managers.map { m => m.copy(
      address = m.address.flatMap { a => ids.map { id => 
        AddressSet(a.base | id, a.mask) } })})})

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in
      out.a.bits.address := ~(~in.a.bits.address | mask.U)

      // We can't support probes; we don't have the required information
      edgeOut.manager.managers.foreach { m =>
        require (m.regionType < RegionType.TRACKED, s"${m.name} has regionType ${m.regionType}, which requires Probe support a RegionReplicator cannot provide")
      }
    }
  }
}

object RegionReplicator {
  def apply(mask: BigInt = 0)(implicit p: Parameters): TLNode = {
    val replicator = LazyModule(new RegionReplicator(mask))
    replicator.node
  }
}
