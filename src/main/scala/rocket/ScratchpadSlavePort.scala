// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.coreplex.CacheBlockBytes
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

class ScratchpadSlavePort(address: AddressSet, coreDataBytes: Int, usingAtomics: Boolean)(implicit p: Parameters) extends LazyModule {
  val device = new SimpleDevice("dtim", Seq("sifive,dtim0"))
  val node = TLManagerNode(Seq(TLManagerPortParameters(
    Seq(TLManagerParameters(
      address            = List(address),
      resources          = device.reg("mem"),
      regionType         = RegionType.UNCACHED,
      executable         = true,
      supportsArithmetic = if (usingAtomics) TransferSizes(4, coreDataBytes) else TransferSizes.none,
      supportsLogical    = if (usingAtomics) TransferSizes(4, coreDataBytes) else TransferSizes.none,
      supportsPutPartial = TransferSizes(1, coreDataBytes),
      supportsPutFull    = TransferSizes(1, coreDataBytes),
      supportsGet        = TransferSizes(1, coreDataBytes),
      fifoId             = Some(0))), // requests handled in FIFO order
    beatBytes = coreDataBytes,
    minLatency = 1)))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val dmem = new HellaCacheIO
    })

    val (tl_in, edge) = node.in(0)

    val s_ready :: s_wait :: s_replay :: s_grant :: Nil = Enum(UInt(), 4)
    val state = Reg(init = s_ready)
    when (io.dmem.resp.valid) { state := s_grant }
    when (tl_in.d.fire()) { state := s_ready }
    when (io.dmem.s2_nack) { state := s_replay }
    when (io.dmem.req.fire()) { state := s_wait }

    val acq = Reg(tl_in.a.bits)
    when (io.dmem.resp.valid) { acq.data := io.dmem.resp.bits.data_raw }
    when (tl_in.a.fire()) { acq := tl_in.a.bits }

    def formCacheReq(a: TLBundleA) = {
      val req = Wire(new HellaCacheReq)
      req.cmd := MuxLookup(a.opcode, Wire(M_XRD), Array(
        TLMessages.PutFullData    -> M_XWR,
        TLMessages.PutPartialData -> M_PWR,
        TLMessages.ArithmeticData -> MuxLookup(a.param, Wire(M_XRD), Array(
          TLAtomics.MIN           -> M_XA_MIN,
          TLAtomics.MAX           -> M_XA_MAX,
          TLAtomics.MINU          -> M_XA_MINU,
          TLAtomics.MAXU          -> M_XA_MAXU,
          TLAtomics.ADD           -> M_XA_ADD)),
        TLMessages.LogicalData    -> MuxLookup(a.param, Wire(M_XRD), Array(
          TLAtomics.XOR           -> M_XA_XOR,
          TLAtomics.OR            -> M_XA_OR,
          TLAtomics.AND           -> M_XA_AND,
          TLAtomics.SWAP          -> M_XA_SWAP)),
        TLMessages.Get            -> M_XRD))
      req.typ := a.size
      req.addr := a.address
      req.tag := UInt(0)
      req.phys := true
      req
    }

    val ready = state === s_ready || tl_in.d.fire()
    io.dmem.req.valid := (tl_in.a.valid && ready) || state === s_replay
    tl_in.a.ready := io.dmem.req.ready && ready
    io.dmem.req.bits := formCacheReq(Mux(state === s_replay, acq, tl_in.a.bits))
    io.dmem.s1_data.data := acq.data
    io.dmem.s1_data.mask := acq.mask
    io.dmem.s1_kill := false
    io.dmem.invalidate_lr := false

    tl_in.d.valid := io.dmem.resp.valid || state === s_grant
    tl_in.d.bits := Mux(acq.opcode.isOneOf(TLMessages.PutFullData, TLMessages.PutPartialData),
      edge.AccessAck(acq),
      edge.AccessAck(acq, UInt(0)))
    tl_in.d.bits.data := Mux(io.dmem.resp.valid, io.dmem.resp.bits.data_raw, acq.data)

    // Tie off unused channels
    tl_in.b.valid := Bool(false)
    tl_in.c.ready := Bool(true)
    tl_in.e.ready := Bool(true)
  }
}

/** Mix-ins for constructing tiles that have optional scratchpads */
trait CanHaveScratchpad extends HasHellaCache with HasICacheFrontend {
  val module: CanHaveScratchpadModule
  val cacheBlockBytes = p(CacheBlockBytes)

  val scratch = tileParams.dcache.flatMap { d => d.scratch.map(s =>
    LazyModule(new ScratchpadSlavePort(AddressSet(s, d.dataScratchpadBytes-1), xBytes, tileParams.core.useAtomics)))
  }

  val intOutputNode = tileParams.core.tileControlAddr.map(dummy => IntIdentityNode())
  val busErrorUnit = tileParams.core.tileControlAddr map { a =>
    val beu = LazyModule(new BusErrorUnit(new L1BusErrors, BusErrorUnitParams(a)))
    intOutputNode.get := beu.intNode
    beu
  }

  // connect any combination of ITIM, DTIM, and BusErrorUnit
  val slaveNode = TLIdentityNode()
  DisableMonitors { implicit p =>
    val xbarPorts =
      scratch.map(lm => (lm.node, xBytes)) ++
      busErrorUnit.map(lm => (lm.node, xBytes)) ++
      tileParams.icache.flatMap(icache => icache.itimAddr.map(a => (frontend.slaveNode, tileParams.core.fetchBytes)))

    if (xbarPorts.nonEmpty) {
      val xbar = LazyModule(new TLXbar)
      xbar.node := TLFIFOFixer()(TLFragmenter(xBytes, cacheBlockBytes, earlyAck=true)(slaveNode))
      xbarPorts.foreach { case (port, bytes) =>
        port := (if (bytes == xBytes) xbar.node else TLFragmenter(bytes, xBytes, earlyAck=true)(TLWidthWidget(xBytes)(xbar.node)))
      }
    }
  }

  def findScratchpadFromICache: Option[AddressSet] = scratch.map { s =>
    val finalNode = frontend.masterNode.edges.out.head.manager.managers.find(_.nodePath.last == s.node)
    require (finalNode.isDefined, "Could not find the scratch pad; not reachable via icache?")
    require (finalNode.get.address.size == 1, "Scratchpad address space was fragmented!")
    finalNode.get.address(0)
  }

  nDCachePorts += (scratch.isDefined).toInt
}

trait CanHaveScratchpadBundle extends HasHellaCacheBundle with HasICacheFrontendBundle {
  val outer: CanHaveScratchpad
}

trait CanHaveScratchpadModule extends HasHellaCacheModule with HasICacheFrontendModule {
  val outer: CanHaveScratchpad
  val io: CanHaveScratchpadBundle

  outer.scratch.foreach { lm => dcachePorts += lm.module.io.dmem }
  outer.busErrorUnit.foreach { lm =>
    lm.module.io.errors.dcache := outer.dcache.module.io.errors
    lm.module.io.errors.icache := outer.frontend.module.io.errors
  }
}
