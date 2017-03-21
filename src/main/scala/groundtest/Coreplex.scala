// See LICENSE.SiFive for license details.

package groundtest

import Chisel._
import config._
import diplomacy._
import coreplex._
import rocket._
import tile._
import uncore.agents._
import uncore.coherence._
import uncore.devices._
import uncore.tilelink._
import uncore.tilelink2._
import uncore.util._
import scala.math.max

case object TileId extends Field[Int]

class GroundTestCoreplex(implicit p: Parameters) extends BaseCoreplex {
  val tiles = List.tabulate(p(NTiles)) { i =>
    LazyModule(new GroundTestTile()(p.alter { (site, here, up) => {
      case TileId => i
      case CacheBlockOffsetBits => log2Up(site(CacheBlockBytes))
      case AmoAluOperandBits => site(XLen)
      case SharedMemoryTLEdge => l1tol2.node.edgesIn(0)
      case TLId => "L1toL2"
      case TLKey("L1toL2") =>
        TileLinkParameters(
          coherencePolicy = new MESICoherence(new NullRepresentation(site(NTiles))),
          nManagers = site(BankedL2Config).nBanks + 1,
          nCachingClients = 1,
          nCachelessClients = 1,
          maxClientXacts = site(GroundTestKey).map(_.maxXacts).reduce(max(_, _)),
          maxClientsPerPort = site(GroundTestKey).map(_.uncached).sum,
          maxManagerXacts = 8,
          dataBeats = (8 * site(CacheBlockBytes)) / site(XLen),
          dataBits = site(CacheBlockBytes)*8)
    }}))
  }

  val fixer = LazyModule(new TLFIFOFixer)
  l1tol2.node :=* fixer.node
  tiles.foreach { fixer.node :=* _.masterNode }

  val cbusRAM = LazyModule(new TLRAM(AddressSet(testRamAddr, 0xffff), false, cbus_beatBytes))
  cbusRAM.node := TLFragmenter(cbus_beatBytes, cbus_lineBytes)(cbus.node)

  override lazy val module = new GroundTestCoreplexModule(this, () => new GroundTestCoreplexBundle(this))
}

class GroundTestCoreplexBundle[+L <: GroundTestCoreplex](_outer: L) extends BaseCoreplexBundle(_outer) {
  val success = Bool(OUTPUT)
}

class GroundTestCoreplexModule[+L <: GroundTestCoreplex, +B <: GroundTestCoreplexBundle[L]](_outer: L, _io: () => B) extends BaseCoreplexModule(_outer, _io) {
  io.success := outer.tiles.map(_.module.io.success).reduce(_&&_)
}
