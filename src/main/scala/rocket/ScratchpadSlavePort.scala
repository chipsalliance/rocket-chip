// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.model.{OMCaches, OMComponent, OMDCache}
import freechips.rocketchip.subsystem.RocketTilesKey
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

/* This adapter converts between diplomatic TileLink and non-diplomatic HellaCacheIO */
class ScratchpadSlavePort(address: AddressSet, coreDataBytes: Int, usingAtomics: Boolean)(implicit p: Parameters) extends LazyModule {
  val device = new SimpleDevice("dtim", Seq("sifive,dtim0")) {
    def getMemory(p: DCacheParams, resourceBindingsMap: ResourceBindingsMap): OMDCache = {
      val resourceBindings = resourceBindingsMap.map.get(this)
      OMCaches.dcache(p, resourceBindings)
    }
  }

  val node = TLManagerNode(Seq(TLManagerPortParameters(
    Seq(TLManagerParameters(
      address            = List(address),
      resources          = device.reg("mem"),
      regionType         = RegionType.UNCACHEABLE,
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

    val s_ready :: s_wait1 :: s_wait2 :: s_replay :: s_grant :: Nil = Enum(UInt(), 5)
    val state = Reg(init = s_ready)
    val dmem_req_valid = Wire(Bool())
    when (state === s_wait1) { state := s_wait2 }
    when (io.dmem.resp.valid) { state := s_grant }
    when (tl_in.d.fire()) { state := s_ready }
    when (io.dmem.s2_nack) { state := s_replay }
    when (dmem_req_valid && io.dmem.req.ready) { state := s_wait1 }

    val acq = Reg(tl_in.a.bits)
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

    // ready_likely assumes that a valid response in s_wait2 is the vastly
    // common case.  In the uncommon case, we'll erroneously send a request,
    // then s1_kill it the following cycle.
    val ready_likely = state === s_ready || state === s_wait2
    val ready = state === s_ready || state === s_wait2 && io.dmem.resp.valid && tl_in.d.ready
    dmem_req_valid := (tl_in.a.valid && ready) || state === s_replay
    val dmem_req_valid_likely = (tl_in.a.valid && ready_likely) || state === s_replay

    io.dmem.req.valid := dmem_req_valid_likely
    tl_in.a.ready := io.dmem.req.ready && ready
    io.dmem.req.bits := formCacheReq(Mux(state === s_replay, acq, tl_in.a.bits))
    io.dmem.s1_data.data := acq.data
    io.dmem.s1_data.mask := acq.mask
    io.dmem.s1_kill := state =/= s_wait1
    io.dmem.s2_kill := false

    tl_in.d.valid := io.dmem.resp.valid || state === s_grant
    tl_in.d.bits := Mux(acq.opcode.isOneOf(TLMessages.PutFullData, TLMessages.PutPartialData),
      edge.AccessAck(acq),
      edge.AccessAck(acq, UInt(0)))
    tl_in.d.bits.data := io.dmem.resp.bits.data_raw.holdUnless(state === s_wait2)

    // Tie off unused channels
    tl_in.b.valid := Bool(false)
    tl_in.c.ready := Bool(true)
    tl_in.e.ready := Bool(true)
  }
}
