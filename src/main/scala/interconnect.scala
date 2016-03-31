package uncore

import Chisel._
import junctions._
import scala.collection.mutable.ArraySeq
import cde.{Parameters, Field}

class ClientUncachedTileLinkIORouter(
    nOuter: Int, routeSel: UInt => UInt)(implicit p: Parameters)
    extends TLModule {

  val io = new Bundle {
    val in = (new ClientUncachedTileLinkIO).flip
    val out =  Vec(nOuter, new ClientUncachedTileLinkIO)
  }

  val acq_route = routeSel(io.in.acquire.bits.full_addr())

  io.in.acquire.ready := Bool(false)

  io.out.zipWithIndex.foreach { case (out, i) =>
    out.acquire.valid := io.in.acquire.valid && acq_route(i)
    out.acquire.bits := io.in.acquire.bits
    when (acq_route(i)) { io.in.acquire.ready := out.acquire.ready }
  }

  val gnt_arb = Module(new LockingRRArbiter(
    new Grant, nOuter, tlDataBeats, Some((gnt: Grant) => gnt.hasMultibeatData())))
  gnt_arb.io.in <> io.out.map(_.grant)
  io.in.grant <> gnt_arb.io.out
}

class TileLinkInterconnectIO(val nInner: Int, val nOuter: Int)
    (implicit p: Parameters) extends Bundle {
  val in = Vec(nInner, new ClientUncachedTileLinkIO).flip
  val out = Vec(nOuter, new ClientUncachedTileLinkIO)
}

class ClientUncachedTileLinkIOCrossbar(
    nInner: Int, nOuter: Int, routeSel: UInt => UInt)
    (implicit p: Parameters) extends TLModule {

  val io = new TileLinkInterconnectIO(nInner, nOuter)

  if (nInner == 1) {
    val router = Module(new ClientUncachedTileLinkIORouter(nOuter, routeSel))
    router.io.in <> io.in.head
    io.out <> router.io.out
  } else {
    val routers = List.fill(nInner) {
      Module(new ClientUncachedTileLinkIORouter(nOuter, routeSel)) }
    val arbiters = List.fill(nOuter) {
      Module(new ClientUncachedTileLinkIOArbiter(nInner)) }

    for (i <- 0 until nInner) {
      routers(i).io.in <> io.in(i)
    }

    for (i <- 0 until nOuter) {
      arbiters(i).io.in <> routers.map(r => r.io.out(i))
      io.out(i) <> arbiters(i).io.out
    }
  }
}

abstract class TileLinkInterconnect(implicit p: Parameters) extends TLModule()(p) {
  val nInner: Int
  val nOuter: Int

  lazy val io = new TileLinkInterconnectIO(nInner, nOuter)
}

class TileLinkRecursiveInterconnect(
    val nInner: Int, val nOuter: Int,
    addrmap: AddrMap, base: BigInt)
    (implicit p: Parameters) extends TileLinkInterconnect()(p) {
  var lastEnd = base
  var outInd = 0
  val levelSize = addrmap.size
  val realAddrMap = new ArraySeq[(BigInt, BigInt)](addrmap.size)

  addrmap.zipWithIndex.foreach { case (AddrMapEntry(name, startOpt, region), i) =>
    val start = startOpt.getOrElse(lastEnd)
    val size = region.size

    require(bigIntPow2(size),
      s"Region $name size $size is not a power of 2")
    require(start % size == 0,
      f"Region $name start address 0x$start%x not divisible by 0x$size%x" )
    require(start >= lastEnd,
      f"Region $name start address 0x$start%x before previous region end")

    realAddrMap(i) = (start, size)
    lastEnd = start + size
  }

  val routeSel = (addr: UInt) => {
    Cat(realAddrMap.map { case (start, size) =>
      addr >= UInt(start) && addr < UInt(start + size)
    }.reverse)
  }

  val xbar = Module(new ClientUncachedTileLinkIOCrossbar(nInner, levelSize, routeSel))
  xbar.io.in <> io.in

  addrmap.zip(realAddrMap).zip(xbar.io.out).zipWithIndex.foreach {
    case (((entry, (start, size)), xbarOut), i) => {
      entry.region match {
        case MemSize(_, _) =>
          io.out(outInd) <> xbarOut
          outInd += 1
        case MemSubmap(_, submap) =>
          if (submap.isEmpty) {
            xbarOut.acquire.ready := Bool(false)
            xbarOut.grant.valid := Bool(false)
          } else {
            val subSlaves = submap.countSlaves
            val outputs = io.out.drop(outInd).take(subSlaves)
            val ic = Module(new TileLinkRecursiveInterconnect(1, subSlaves, submap, start))
            ic.io.in.head <> xbarOut
            for ((o, s) <- outputs zip ic.io.out)
              o <> s
            outInd += subSlaves
          }
      }
    }
  }
}
