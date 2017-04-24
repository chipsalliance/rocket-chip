// See LICENSE.SiFive for license details.

package tile

import Chisel._
import config.Parameters
import uncore.tilelink2.{IntSinkNode, IntSinkPortSimple}
import util._

class TileInterrupts(implicit p: Parameters) extends CoreBundle()(p) {
  val debug = Bool()
  val mtip = Bool()
  val msip = Bool()
  val meip = Bool()
  val seip = usingVM.option(Bool())
  val lip = Vec(coreParams.nLocalInterrupts, Bool())
}

// Use diplomatic interrupts to external interrupts from the coreplex into the tile
trait HasExternalInterrupts extends HasTileParameters {
  implicit val p: Parameters
  val module: HasExternalInterruptsModule

  val intNode = IntSinkNode(IntSinkPortSimple())

  // TODO: the order of the following two functions must match, and
  //         also match the order which things are connected to the
  //         per-tile crossbar in coreplex.HasRocketTiles

  // debug, msip, mtip, meip, seip, lip offsets in CSRs
  def csrIntMap: List[Int] = {
    val nlips = tileParams.core.nLocalInterrupts
    val seip = if (usingVM) Seq(9) else Nil
    List(65535, 3, 7, 11) ++ seip ++ List.tabulate(nlips)(_ + 16)
  }
}

trait HasExternalInterruptsBundle {
  val outer: HasExternalInterrupts
  val interrupts = outer.intNode.bundleIn
}

trait HasExternalInterruptsModule {
  val outer: HasExternalInterrupts
  val io: HasExternalInterruptsBundle

  // go from flat diplomatic Interrupts to bundled TileInterrupts
  def decodeCoreInterrupts(core: TileInterrupts) {
    val core_ips = Seq(
      core.debug,
      core.msip,
      core.mtip,
      core.meip,
      core.seip.getOrElse(Wire(Bool()))) ++ core.lip
    core_ips.zip(io.interrupts(0)).foreach { case(c, i) => c := i }
  }
}
