// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import Chisel._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._

class TileInterrupts(implicit p: Parameters) extends CoreBundle()(p) {
  val debug = Bool()
  val mtip = Bool()
  val msip = Bool()
  val meip = Bool()
  val seip = usingVM.option(Bool())
  val lip = Vec(coreParams.nLocalInterrupts, Bool())
}

// Use diplomatic interrupts to external interrupts from the subsystem into the tile
trait HasExternalInterrupts { this: BaseTile =>

  val intInwardNode = intXbar.intnode
  protected val intSinkNode = IntSinkNode(IntSinkPortSimple())
  intSinkNode := intXbar.intnode

  val intcDevice = new Device {
    def describe(resources: ResourceBindings): Description = {
      Description(s"cpus/cpu@$hartId/interrupt-controller", Map(
        "compatible"           -> "riscv,cpu-intc".asProperty,
        "interrupt-controller" -> Nil,
        "#interrupt-cells"     -> 1.asProperty))
    }
  }

  ResourceBinding {
    Resource(intcDevice, "reg").bind(ResourceInt(BigInt(hartId)))

    intSinkNode.edges.in.flatMap(_.source.sources).map { case s =>
      for (i <- s.range.start until s.range.end) {
       csrIntMap.lift(i).foreach { j =>
          s.resources.foreach { r =>
            r.bind(intcDevice, ResourceInt(j))
          }
        }
      }
    }
  }

  // TODO: the order of the following two functions must match, and
  //         also match the order which things are connected to the
  //         per-tile crossbar in subsystem.HasRocketTiles

  // debug, msip, mtip, meip, seip, lip offsets in CSRs
  def csrIntMap: List[Int] = {
    val nlips = tileParams.core.nLocalInterrupts
    val seip = if (usingVM) Seq(9) else Nil
    List(65535, 3, 7, 11) ++ seip ++ List.tabulate(nlips)(_ + 16)
  }

  // go from flat diplomatic Interrupts to bundled TileInterrupts
  def decodeCoreInterrupts(core: TileInterrupts) {
    val async_ips = Seq(core.debug)
    val periph_ips = Seq(
      core.msip,
      core.mtip,
      core.meip)

    val seip = if (core.seip.isDefined) Seq(core.seip.get) else Nil

    val core_ips = core.lip

    val (interrupts, _) = intSinkNode.in(0)
    (async_ips ++ periph_ips ++ seip ++ core_ips).zip(interrupts).foreach { case(c, i) => c := i }
  }
}
