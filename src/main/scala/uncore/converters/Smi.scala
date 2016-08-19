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
