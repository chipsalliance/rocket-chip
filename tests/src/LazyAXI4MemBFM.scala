package org.chipsalliance.rocketchip.internal.tests

import chisel3._
import freechips.rocketchip.amba.AMBACorrupt
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters

// In the future, the BFM framework should borrow the idea from CIRCT ESI Dialect providing channel based BFM for smoothly hook any BFM to bus
// For now, we only use this simple BFM memory as test program loader for loading program to test the subsystem.
class LazyAXI4MemBFM(edge: AXI4EdgeParameters, size: BigInt, base: BigInt = 0)(implicit p: Parameters)
    extends SimpleLazyModule {
  val node = AXI4MasterNode(List(edge.master))
  val bfms = AddressSet.misaligned(base, size).map { aSet =>
    LazyModule(
      new AXI4BFM(
        address = aSet,
        beatBytes = edge.bundle.dataBits / 8,
        wcorrupt = edge.slave.requestKeys.contains(AMBACorrupt)
      )
    )
  }
  val xbar = AXI4Xbar()
  bfms.foreach { s => s.node := AXI4Buffer() := xbar }
  xbar := node
  val io_axi4 = InModuleBody { node.makeIOs() }
}

class AXI4BFM(
  address:    AddressSet,
  cacheable:  Boolean = true,
  executable: Boolean = true,
  beatBytes:  Int = 4,
  errors:     Seq[AddressSet] = Nil,
  wcorrupt:   Boolean = true
)(
  implicit p: Parameters)
    extends LazyModule { outer =>
  val node = AXI4SlaveNode(
    Seq(
      AXI4SlavePortParameters(
        Seq(
          AXI4SlaveParameters(
            address = List(address) ++ errors,
            resources = Nil,
            regionType = if (cacheable) RegionType.UNCACHED else RegionType.IDEMPOTENT,
            executable = executable,
            supportsRead = TransferSizes(1, beatBytes),
            supportsWrite = TransferSizes(1, beatBytes),
            interleavedId = Some(0)
          )
        ),
        beatBytes = beatBytes,
        requestKeys = if (wcorrupt) Seq(AMBACorrupt) else Seq(),
        minLatency = 1
      )
    )
  )

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val axi4Bundle: AXI4Bundle = node.in.head._1
    val bundleTpe = axi4Bundle.cloneType
    val dpiGen = Module(new DPIModule {
      override def desiredName = "AXI4BFMDPI"
      override val isImport: Boolean = true
      val clock = dpiTrigger("clock", Input(Bool()))
      val reset = dpiTrigger("reset", Input(Bool()))
      override val trigger: String = s"always@(negedge ${clock.name})"
      override val guard:   String = s"!${reset.name}"

      // ar channel
      val arid = dpi("arid", Input(bundleTpe.ar.bits.id))
      val araddr = dpi("araddr", Input(bundleTpe.ar.bits.addr))
      val arlen = dpi("arlen", Input(bundleTpe.ar.bits.len))
      val arsize = dpi("arsize", Input(bundleTpe.ar.bits.size))
      val arburst = dpi("arburst", Input(bundleTpe.ar.bits.burst))
      val arvalid = dpi("arvalid", Input(bundleTpe.ar.valid))
      val arready = dpi("arready", Output(bundleTpe.ar.ready))

      // r channel
      val rid = dpi("rid", Output(bundleTpe.r.bits.id))
      val rdata = dpi("rdata", Output(bundleTpe.r.bits.data))
      val rlast = dpi("rlast", Output(bundleTpe.r.bits.last))
      val rresp = dpi("rresp", Output(bundleTpe.r.bits.resp))
      val rvalid = dpi("rvalid", Output(bundleTpe.r.valid))
      val rready = dpi("rready", Input(bundleTpe.r.ready))

      // aw channel
      val awid = dpi("awid", Input(bundleTpe.aw.bits.id))
      val awaddr = dpi("awaddr", Input(bundleTpe.aw.bits.addr))
      val awlen = dpi("awlen", Input(bundleTpe.aw.bits.len))
      val awsize = dpi("awsize", Input(bundleTpe.aw.bits.size))
      val awburst = dpi("awburst", Input(bundleTpe.aw.bits.burst))
      val awvalid = dpi("awvalid", Input(bundleTpe.aw.valid))
      val awready = dpi("awready", Output(bundleTpe.aw.ready))

      // w channel
      val wdata = dpi("wdata", Input(bundleTpe.w.bits.data))
      val wlast = dpi("wlast", Input(bundleTpe.w.bits.last))
      val wstrb = dpi("wstrb", Input(bundleTpe.w.bits.strb))
      val wvalid = dpi("wvalid", Input(bundleTpe.w.valid))
      val wready = dpi("wready", Output(bundleTpe.w.ready))

      // b dpi
      val bid = dpi("bid", Output(bundleTpe.b.bits.id))
      val bresp = dpi("bresp", Output(bundleTpe.b.bits.resp))
      val bvalid = dpi("bvalid", Output(bundleTpe.b.valid))
      val bready = dpi("bready", Input(bundleTpe.b.ready))

    })
    dpiGen.clock.ref := clock.asBool
    dpiGen.reset.ref := reset

    // ar connect
    dpiGen.arid.ref := axi4Bundle.ar.bits.id
    dpiGen.araddr.ref := axi4Bundle.ar.bits.addr
    dpiGen.arlen.ref := axi4Bundle.ar.bits.len
    dpiGen.arsize.ref := axi4Bundle.ar.bits.size
    dpiGen.arburst.ref := axi4Bundle.ar.bits.burst
    dpiGen.arvalid.ref := axi4Bundle.ar.valid
    axi4Bundle.ar.ready := dpiGen.arready.ref

    // r connect
    axi4Bundle.r.bits.id := dpiGen.rid.ref
    axi4Bundle.r.bits.data := dpiGen.rdata.ref
    axi4Bundle.r.bits.last := dpiGen.rlast.ref
    axi4Bundle.r.bits.resp := dpiGen.rresp.ref
    axi4Bundle.r.valid := dpiGen.rvalid.ref
    dpiGen.rready.ref := axi4Bundle.r.ready

    // aw connect
    dpiGen.awid.ref := axi4Bundle.aw.bits.id
    dpiGen.awaddr.ref := axi4Bundle.aw.bits.addr
    dpiGen.awlen.ref := axi4Bundle.aw.bits.len
    dpiGen.awsize.ref := axi4Bundle.aw.bits.size
    dpiGen.awburst.ref := axi4Bundle.aw.bits.burst
    dpiGen.awvalid.ref := axi4Bundle.aw.valid
    axi4Bundle.aw.ready := dpiGen.awready.ref

    // w connect
    dpiGen.wdata.ref := axi4Bundle.w.bits.data
    dpiGen.wlast.ref := axi4Bundle.w.bits.last
    dpiGen.wstrb.ref := axi4Bundle.w.bits.strb
    dpiGen.wvalid.ref := axi4Bundle.w.valid
    axi4Bundle.w.ready := dpiGen.wready.ref

    // b connect
    axi4Bundle.b.bits.id := dpiGen.bid.ref
    axi4Bundle.b.bits.resp := dpiGen.bresp.ref
    axi4Bundle.b.valid := dpiGen.bvalid.ref
    dpiGen.bready.ref := axi4Bundle.b.ready
  }
}
