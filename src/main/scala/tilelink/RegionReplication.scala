// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._

/* Address inside the 'local' space are replicated to fill the 'remote' space.
 */
case class ReplicatedRegion(
  local:  AddressSet,
  region: AddressSet)
{
  require (region.contains(local))
  val replicationMask = region.mask & ~local.mask
  require ((local.base & replicationMask) == 0, s"Local region ${local} not aligned within ${region}")

  def isLegalPrefix(prefix: UInt): Bool = ~(~prefix | replicationMask.U) === 0.U
}

trait HasRegionReplicatorParams {
  def replication: Option[ReplicatedRegion]
}

// Replicate all devices below this adapter that are inside replicationRegion to multiple addreses based on mask.
// If a device was at 0x4000-0x4fff and mask=0x10000, it will now be at 0x04000-0x04fff and 0x14000-0x14fff.
class RegionReplicator(val params: ReplicatedRegion)(implicit p: Parameters) extends LazyModule {
  val node = TLAdapterNode(
    clientFn  = { cp => cp },
    managerFn = { mp => mp.v1copy(managers = mp.managers.map { m =>
      m.v1copy(address = m.address.map { a =>
        if (params.region.contains(a)) { a.widen(params.replicationMask) } else { a }
      })
    })}
  )

  val prefix = BundleBridgeSink[UInt]()

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in

      // Is every slave contained by the replication region?
      val totalContainment = edgeIn.slave.slaves.forall(_.address.forall(params.region contains _))

      // Which address within the mask routes to local devices?
      val local_prefix = RegNext(prefix.bundle)
      assert (params.isLegalPrefix(local_prefix) || !out.b.valid)

      val a_addr = in.a.bits.address
      val a_contained = params.region.contains(a_addr) || totalContainment.B
      out.a.bits.address := Mux(a_contained, ~(~a_addr | params.replicationMask.U), a_addr)

      val b_addr = out.b.bits.address
      val b_contained = params.region.contains(b_addr) || totalContainment.B
      in.b.bits.address := Mux(b_contained, b_addr | (local_prefix & params.replicationMask.U), b_addr)

      val c_addr = in.c.bits.address
      val c_contained = params.region.contains(c_addr) || totalContainment.B
      out.c.bits.address := Mux(c_contained, ~(~c_addr | params.replicationMask.U), c_addr)
    }
  }
}
