// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class TLZero(address: AddressSet, resources: Seq[Resource], executable: Boolean = true, beatBytes: Int = 4)(implicit p: Parameters) extends LazyModule
{
  val node = TLManagerNode(Seq(TLManagerPortParameters(
    Seq(TLManagerParameters(
      address            = List(address),
      resources          = resources,
      regionType         = RegionType.UNCACHED,
      executable         = executable,
      supportsGet        = TransferSizes(1, beatBytes),
      supportsPutPartial = TransferSizes(1, beatBytes),
      supportsPutFull    = TransferSizes(1, beatBytes),
      supportsArithmetic = TransferSizes(1, beatBytes),
      supportsLogical    = TransferSizes(1, beatBytes),
      fifoId             = Some(0))), // requests are handled in order
    beatBytes  = beatBytes,
    minLatency = 1))) // no bypass needed for this device

  lazy val module = new LazyModuleImp(this) {
    val (in, edge) = node.in(0)

    val a = Queue(in.a, 2)

    a.ready := in.d.ready
    in.d.valid := a.valid
    in.d.bits := edge.AccessAck(a.bits)
    in.d.bits.opcode := TLMessages.adResponse(edge.opcode(a.bits))

    // Tie off unused channels
    in.b.valid := Bool(false)
    in.c.ready := Bool(true)
    in.e.ready := Bool(true)
  }
}

/* Specifies the location of the Zero device */
case class ZeroParams(base: Long, size: Long, beatBytes: Int)
case object ZeroParams extends Field[ZeroParams]

class MemoryZeroSlave(address: AddressSet, beatBytes: Int)(implicit p: Parameters)
  extends TLZero(
    address = address,
    resources = new SimpleDevice("rom", Seq("ucbbar,cacheable-zero0")).reg("mem"),
    executable = true,
    beatBytes = beatBytes)

/** Adds a /dev/null slave that generates zero-filled responses to reads */
trait HasMemoryZeroSlave { this: BaseSubsystem =>
  private val params = p(ZeroParams)

  val zeros = memBuses.zipWithIndex.map { case (bus, channel) =>
    val channels = memBuses.size
    val base = AddressSet(params.base, params.size-1)
    val filter = AddressSet(channel * bus.blockBytes, ~((channels-1) * bus.blockBytes))
    val address = base.intersect(filter).get
    val zero = LazyModule(new MemoryZeroSlave(address, beatBytes = params.beatBytes))
    bus.toVariableWidthSlave(Some("Zero")) { zero.node }
    zero
  }
}
