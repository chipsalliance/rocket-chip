// See LICENSE.SiFive for license details.

package rocket

import Chisel._
import Chisel.ImplicitConversions._
import config._
import coreplex.CacheBlockBytes
import diplomacy._
import tile._
import uncore.constants._
import uncore.tilelink2._
import uncore.util._
import util._

class ScratchpadSlavePort(address: AddressSet)(implicit p: Parameters) extends LazyModule
    with HasCoreParameters {
  val device = new MemoryDevice
  val node = TLManagerNode(Seq(TLManagerPortParameters(
    Seq(TLManagerParameters(
      address            = List(address),
      resources          = device.reg,
      regionType         = RegionType.UNCACHED,
      executable         = true,
      supportsArithmetic = if (usingAtomics) TransferSizes(1, coreDataBytes) else TransferSizes.none,
      supportsLogical    = if (usingAtomics) TransferSizes(1, coreDataBytes) else TransferSizes.none,
      supportsPutPartial = TransferSizes(1, coreDataBytes),
      supportsPutFull    = TransferSizes(1, coreDataBytes),
      supportsGet        = TransferSizes(1, coreDataBytes),
      fifoId             = Some(0))), // requests handled in FIFO order
    beatBytes = coreDataBytes,
    minLatency = 1)))

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val tl_in = node.bundleIn
      val dmem = new HellaCacheIO
    }

    val tl_in = io.tl_in(0)
    val edge = node.edgesIn(0)

    val s_ready :: s_wait :: s_replay :: s_grant :: Nil = Enum(UInt(), 4)
    val state = Reg(init = s_ready)
    when (io.dmem.resp.valid) { state := s_grant }
    when (tl_in.d.fire()) { state := s_ready }
    when (io.dmem.s2_nack) { state := s_replay }
    when (io.dmem.req.fire()) { state := s_wait }

    val acq = Reg(tl_in.a.bits)
    when (io.dmem.resp.valid) { acq.data := io.dmem.resp.bits.data }
    when (tl_in.a.fire()) { acq := tl_in.a.bits }

    val isWrite = acq.opcode === TLMessages.PutFullData || acq.opcode === TLMessages.PutPartialData
    val isRead = !edge.hasData(acq)

    def formCacheReq(acq: TLBundleA) = {
      val req = Wire(new HellaCacheReq)
      req.cmd := MuxLookup(acq.opcode, Wire(M_XRD), Array(
        TLMessages.PutFullData    -> M_XWR,
        TLMessages.PutPartialData -> M_XWR,
        TLMessages.ArithmeticData -> MuxLookup(acq.param, Wire(M_XRD), Array(
          TLAtomics.MIN           -> M_XA_MIN,
          TLAtomics.MAX           -> M_XA_MAX,
          TLAtomics.MINU          -> M_XA_MINU,
          TLAtomics.MAXU          -> M_XA_MAXU,
          TLAtomics.ADD           -> M_XA_ADD)),
        TLMessages.LogicalData    -> MuxLookup(acq.param, Wire(M_XRD), Array(
          TLAtomics.XOR           -> M_XA_XOR,
          TLAtomics.OR            -> M_XA_OR,
          TLAtomics.AND           -> M_XA_AND,
          TLAtomics.SWAP          -> M_XA_SWAP)),
        TLMessages.Get            -> M_XRD))
      // treat all loads as full words, so bytes appear in correct lane
      req.typ := Mux(isRead, log2Ceil(coreDataBytes), acq.size)
      req.addr := Mux(isRead, ~(~acq.address | (coreDataBytes-1)), acq.address)
      req.tag := UInt(0)
      req
    }

    val ready = state === s_ready || tl_in.d.fire()
    io.dmem.req.valid := (tl_in.a.valid && ready) || state === s_replay
    tl_in.a.ready := io.dmem.req.ready && ready
    io.dmem.req.bits := formCacheReq(Mux(state === s_replay, acq, tl_in.a.bits))
    // the TL data is already in the correct byte lane, but the D$
    // expects right-justified store data, so that it can steer the bytes.
    io.dmem.s1_data := new LoadGen(acq.size, Bool(false), acq.address(log2Ceil(coreDataBytes)-1,0), acq.data, Bool(false), coreDataBytes).data
    io.dmem.s1_kill := false
    io.dmem.invalidate_lr := false

    // place AMO data in correct word lane
    val minAMOBytes = 4
    val grantData = Mux(io.dmem.resp.valid, io.dmem.resp.bits.data, acq.data)
    val alignedGrantData = Mux(acq.size <= log2Ceil(minAMOBytes), Fill(coreDataBytes/minAMOBytes, grantData(8*minAMOBytes-1, 0)), grantData)

    tl_in.d.valid := io.dmem.resp.valid || state === s_grant
    tl_in.d.bits := Mux(isWrite,
      edge.AccessAck(acq, UInt(0)),
      edge.AccessAck(acq, UInt(0), UInt(0)))
    tl_in.d.bits.data := alignedGrantData

    // Tie off unused channels
    tl_in.b.valid := Bool(false)
    tl_in.c.ready := Bool(true)
    tl_in.e.ready := Bool(true)
  }
}

/** Mix-ins for constructing tiles that have optional scratchpads */
trait CanHaveScratchpad extends HasHellaCache with HasICacheFrontend {
  val module: CanHaveScratchpadModule

  val scratch = tileParams.dcache.flatMap(d => d.scratch.map(s =>
    LazyModule(new ScratchpadSlavePort(AddressSet(s, d.dataScratchpadBytes-1)))))
  val slaveNode = TLInputNode()

  scratch foreach { lm => lm.node := TLFragmenter(p(XLen)/8, p(CacheBlockBytes))(slaveNode) }

  def findScratchpadFromICache: Option[AddressSet] = scratch.map { s =>
    val finalNode = frontend.node.edgesOut(0).manager.managers.find(_.nodePath.last == s.node)
    require (finalNode.isDefined, "Could not find the scratch pad; not reachable via icache?")
    require (finalNode.get.address.size == 1, "Scratchpad address space was fragmented!")
    finalNode.get.address(0)
  }

  nDCachePorts += (scratch.isDefined).toInt
}

trait CanHaveScratchpadBundle extends HasHellaCacheBundle with HasICacheFrontendBundle {
  val outer: CanHaveScratchpad
  val slave = outer.slaveNode.bundleIn
}

trait CanHaveScratchpadModule extends HasHellaCacheModule with HasICacheFrontendModule {
  val outer: CanHaveScratchpad
  val io: CanHaveScratchpadBundle

  outer.scratch.foreach { lm => dcachePorts += lm.module.io.dmem }
}
