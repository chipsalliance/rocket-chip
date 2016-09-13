// See LICENSE for details

package uncore.converters

import Chisel._
import junctions._
import uncore.tilelink._
import cde.Parameters

/** Convert TileLink protocol to Smi protocol */
class SmiIOTileLinkIOConverter(val dataWidth: Int, val addrWidth: Int)
                              (implicit p: Parameters) extends Module {
  val io = new Bundle {
    val tl = (new ClientUncachedTileLinkIO).flip
    val smi = new SmiIO(dataWidth, addrWidth)
  }

  def decoupledNastiConnect(outer: NastiIO, inner: NastiIO) {
    outer.ar <> Queue(inner.ar)
    outer.aw <> Queue(inner.aw)
    outer.w  <> Queue(inner.w)
    inner.r  <> Queue(outer.r)
    inner.b  <> Queue(outer.b)
  }

  val tl2nasti  = Module(new NastiIOTileLinkIOConverter())
  val nasti2smi = Module(new SmiIONastiIOConverter(dataWidth, addrWidth))

  tl2nasti.io.tl <> io.tl
  decoupledNastiConnect(nasti2smi.io.nasti, tl2nasti.io.nasti)
  io.smi <> nasti2smi.io.smi
}

class SmiConverterTest(implicit val p: Parameters) extends unittest.UnitTest
    with HasTileLinkParameters {
  val outermostParams = p.alterPartial({ case TLId => "Outermost" })

  val smiWidth = 32
  val smiDepth = 64
  val tlDepth = (smiWidth * smiDepth) / tlDataBits

  val smimem = Module(new SmiMem(smiWidth, smiDepth))
  val conv = Module(new SmiIOTileLinkIOConverter(
    smiWidth, log2Up(smiDepth))(outermostParams))
  val driver = Module(new DriverSet(
    (driverParams: Parameters) => {
      implicit val p = driverParams
      Seq(
        Module(new PutSweepDriver(tlDepth)),
        Module(new PutMaskDriver(smiWidth / 8)),
        Module(new PutBlockSweepDriver(tlDepth / tlDataBeats)),
        Module(new GetMultiWidthDriver))
    })(outermostParams))

  conv.io.tl <> driver.io.mem
  smimem.io <> conv.io.smi
  driver.io.start := io.start
  io.finished := driver.io.finished
}
