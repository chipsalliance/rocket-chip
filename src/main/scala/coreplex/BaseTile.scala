// See LICENSE.SiFive for license details.

package coreplex

import Chisel._
import config._
import diplomacy._
import rocket._
import uncore.tilelink2._
import util.GenericParameterizedBundle

case object SharedMemoryTLEdge extends Field[TLEdgeOut]
case object DataScratchpadSize extends Field[Int]
case object TileKey extends Field[TileParameters]

trait TileParameters {
  val core: CoreParameters
  val icache: ICacheParameters
  val dcache: DCacheParameters
  val rocc: Seq[RoccParameters]
  val btb: BTBParameters
  val dataScratchpadBytes: Int
}

abstract class BareTile(implicit p: Parameters) extends LazyModule

abstract class BareTileBundle[+L <: BareTile](_outer: L) extends GenericParameterizedBundle(_outer) {
  val outer = _outer
  implicit val p = outer.p
}

abstract class BareTileModule[+L <: BareTile, +B <: BareTileBundle[L]](_outer: L, _io: () => B) extends LazyModuleImp(_outer) {
  val outer = _outer
  val io = _io ()
}

// Uses a tile-internal crossbar to provide a single TileLink master port
trait TileNetwork {
  implicit val p: Parameters
  val module: TileNetworkModule
  val l1backend = LazyModule(new TLXbar)
  val masterNodes = List(TLOutputNode())
  masterNodes.head := l1backend.node
}

trait TileNetworkBundle {
  val outer: TileNetwork
  val master = outer.masterNodes.head.bundleOut
}

trait TileNetworkModule {
  val outer: TileNetwork
  val io: TileNetworkBundle
}

abstract class BaseTile(val tileConfig: TileConfig)(implicit p: Parameters) extends BareTile
    with TileNetwork {
  override lazy val module = new BaseTileModule(this, () => new BaseTileBundle(this))
}

class BaseTileBundle[+L <: BaseTile](_outer: L) extends BareTileBundle(_outer)
    with TileNetworkBundle {
  val hartid = UInt(INPUT, p(XLen))
  val interrupts = new TileInterrupts()(p).asInput
  val resetVector = UInt(INPUT, p(XLen))
}

class BaseTileModule[+L <: BaseTile, +B <: BaseTileBundle[L]](_outer: L, _io: () => B) extends BareTileModule(_outer, _io)
    with TileNetworkModule
