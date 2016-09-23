package uncore.util

import Chisel._
import uncore.tilelink._
import cde.Parameters

/** Struct for describing per-channel queue depths */
case class TileLinkDepths(acq: Int, prb: Int, rel: Int, gnt: Int, fin: Int)
case class UncachedTileLinkDepths(acq: Int, gnt: Int)

/** Optionally enqueues each [[uncore.TileLinkChannel]] individually */
class TileLinkEnqueuer(depths: TileLinkDepths)(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val client = new TileLinkIO().flip
    val manager = new TileLinkIO
  }
  io.manager.acquire <> (if(depths.acq > 0) Queue(io.client.acquire, depths.acq) else io.client.acquire)
  io.client.probe    <> (if(depths.prb > 0) Queue(io.manager.probe,  depths.prb) else io.manager.probe)
  io.manager.release <> (if(depths.rel > 0) Queue(io.client.release, depths.rel) else io.client.release)
  io.client.grant    <> (if(depths.gnt > 0) Queue(io.manager.grant,  depths.gnt) else io.manager.grant)
  io.manager.finish  <> (if(depths.fin > 0) Queue(io.client.finish,  depths.fin) else io.client.finish)
}

object TileLinkEnqueuer {
  def apply(in: TileLinkIO, depths: TileLinkDepths): TileLinkIO = {
    val t = Module(new TileLinkEnqueuer(depths)(in.p))
    t.io.client <> in
    t.io.manager
  }
  def apply(in: TileLinkIO, depth: Int): TileLinkIO = {
    apply(in, TileLinkDepths(depth, depth, depth, depth, depth))
  }

  def apply(in: ClientTileLinkIO, depths: TileLinkDepths): ClientTileLinkIO = {
    val t = Module(new ClientTileLinkEnqueuer(depths)(in.p))
    t.io.inner <> in
    t.io.outer
  }
  def apply(in: ClientTileLinkIO, depth: Int): ClientTileLinkIO = {
    apply(in, TileLinkDepths(depth, depth, depth, depth, depth))
  }
  def apply(in: ClientUncachedTileLinkIO, depths: UncachedTileLinkDepths): ClientUncachedTileLinkIO = {
    val t = Module(new ClientUncachedTileLinkEnqueuer(depths)(in.p))
    t.io.inner <> in
    t.io.outer
  }
  def apply(in: ClientUncachedTileLinkIO, depth: Int): ClientUncachedTileLinkIO = {
    apply(in, UncachedTileLinkDepths(depth, depth))
  }
}

class ClientTileLinkEnqueuer(depths: TileLinkDepths)(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val inner = new ClientTileLinkIO().flip
    val outer = new ClientTileLinkIO
  }

  io.outer.acquire <> (if(depths.acq > 0) Queue(io.inner.acquire, depths.acq) else io.inner.acquire)
  io.inner.probe   <> (if(depths.prb > 0) Queue(io.outer.probe,   depths.prb) else io.outer.probe)
  io.outer.release <> (if(depths.rel > 0) Queue(io.inner.release, depths.rel) else io.inner.release)
  io.inner.grant   <> (if(depths.gnt > 0) Queue(io.outer.grant,   depths.gnt) else io.outer.grant)
  io.outer.finish  <> (if(depths.fin > 0) Queue(io.inner.finish,  depths.fin) else io.inner.finish)
}

class ClientUncachedTileLinkEnqueuer(depths: UncachedTileLinkDepths)(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val inner = new ClientUncachedTileLinkIO().flip
    val outer = new ClientUncachedTileLinkIO
  }

  io.outer.acquire <> (if(depths.acq > 0) Queue(io.inner.acquire, depths.acq) else io.inner.acquire)
  io.inner.grant   <> (if(depths.gnt > 0) Queue(io.outer.grant,   depths.gnt) else io.outer.grant)
}
