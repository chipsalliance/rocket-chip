// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.diplomacy.{AddressSet, RegionType, TransferSizes}
import freechips.rocketchip.resources.{SimpleDevice}

import freechips.rocketchip.tilelink.{TLManagerNode, TLManagerPortParameters, TLManagerParameters, TLBundleA, TLMessages, TLAtomics}

import freechips.rocketchip.util.UIntIsOneOf
import freechips.rocketchip.util.DataToAugmentedData

/* This adapter converts between diplomatic TileLink and non-diplomatic HellaCacheIO */
class ScratchpadManagerPort(address: Seq[AddressSet], coreDataBytes: Int, usingAtomics: Boolean)(implicit p: Parameters) extends LazyModule {
  def this(address: AddressSet, coreDataBytes: Int, usingAtomics: Boolean)(implicit p: Parameters) = {
    this(Seq(address), coreDataBytes, usingAtomics)
  }

  val device = new SimpleDevice("dtim", Seq("sifive,dtim0"))

  val node = TLManagerNode(Seq(TLManagerPortParameters.v1(
    Seq(TLManagerParameters.v1(
      address            = address,
      resources          = device.reg("mem"),
      regionType         = RegionType.IDEMPOTENT,
      executable         = true,
      supportsArithmetic = if (usingAtomics) TransferSizes(4, coreDataBytes) else TransferSizes.none,
      supportsLogical    = if (usingAtomics) TransferSizes(4, coreDataBytes) else TransferSizes.none,
      supportsPutPartial = TransferSizes(1, coreDataBytes),
      supportsPutFull    = TransferSizes(1, coreDataBytes),
      supportsGet        = TransferSizes(1, coreDataBytes),
      fifoId             = Some(0))), // requests handled in FIFO order
    beatBytes = coreDataBytes,
    minLatency = 1)))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val dmem = new HellaCacheIO
    })

    require(coreDataBytes * 8 == io.dmem.resp.bits.data.getWidth, "ScratchpadManagerPort is misconfigured: coreDataBytes must match D$ data width")

    val (tl_in, edge) = node.in(0)

    val s_ready :: s_wait1 :: s_wait2 :: s_replay :: s_init :: s_grant :: Nil = Enum(6)
    val state = RegInit(s_init)
    val dmem_req_valid = Wire(Bool())
    when (state === s_wait1) { state := s_wait2 }
    when (state === s_init && tl_in.a.valid) { state := s_ready }
    when (io.dmem.resp.valid) { state := s_grant }
    when (tl_in.d.fire) { state := s_ready }
    when (io.dmem.s2_nack) { state := s_replay }
    when (dmem_req_valid && io.dmem.req.ready) { state := s_wait1 }

    val acq = Reg(tl_in.a.bits.cloneType)
    when (tl_in.a.fire) { acq := tl_in.a.bits }

    def formCacheReq(a: TLBundleA) = {
      val req = Wire(new HellaCacheReq)
      req.cmd := MuxLookup(a.opcode, M_XRD)(Array(
        TLMessages.PutFullData    -> M_XWR,
        TLMessages.PutPartialData -> M_PWR,
        TLMessages.ArithmeticData -> MuxLookup(a.param, M_XRD)(Array(
          TLAtomics.MIN           -> M_XA_MIN,
          TLAtomics.MAX           -> M_XA_MAX,
          TLAtomics.MINU          -> M_XA_MINU,
          TLAtomics.MAXU          -> M_XA_MAXU,
          TLAtomics.ADD           -> M_XA_ADD)),
        TLMessages.LogicalData    -> MuxLookup(a.param, M_XRD)(Array(
          TLAtomics.XOR           -> M_XA_XOR,
          TLAtomics.OR            -> M_XA_OR,
          TLAtomics.AND           -> M_XA_AND,
          TLAtomics.SWAP          -> M_XA_SWAP)),
        TLMessages.Get            -> M_XRD))

      // Convert full PutPartial into PutFull to work around RMWs causing X-prop problems.
      // Also prevent cmd becoming X out of reset by checking for s_init.
      val mask_full = {
        val desired_mask = new StoreGen(a.size, a.address, 0.U, coreDataBytes).mask
        (a.mask | ~desired_mask).andR
      }
      when (state === s_init || (a.opcode === TLMessages.PutPartialData && mask_full)) {
        req.cmd := M_XWR
      }

      req.size := a.size
      req.signed := false.B
      req.addr := a.address
      req.tag := 0.U
      req.phys := true.B
      req.no_xcpt := true.B
      req.no_resp := false.B
      req.data := 0.U
      req.no_alloc := false.B
      req.mask := 0.U
      req.dprv := 0.U
      req.dv := false.B
      req
    }

    // ready_likely assumes that a valid response in s_wait2 is the vastly
    // common case.  In the uncommon case, we'll erroneously send a request,
    // then s1_kill it the following cycle.
    val ready_likely = state.isOneOf(s_ready, s_wait2)
    val ready = state === s_ready || state === s_wait2 && io.dmem.resp.valid && tl_in.d.ready
    dmem_req_valid := (tl_in.a.valid && ready) || state === s_replay
    val dmem_req_valid_likely = (tl_in.a.valid && ready_likely) || state === s_replay

    io.dmem.keep_clock_enabled := DontCare
    io.dmem.req.valid := dmem_req_valid_likely
    tl_in.a.ready := io.dmem.req.ready && ready
    io.dmem.req.bits := formCacheReq(Mux(state === s_replay, acq, tl_in.a.bits))
    io.dmem.s1_data.data := acq.data
    io.dmem.s1_data.mask := acq.mask
    io.dmem.s1_kill := state =/= s_wait1
    io.dmem.s2_kill := false.B

    tl_in.d.valid := io.dmem.resp.valid || state === s_grant
    tl_in.d.bits := Mux(acq.opcode.isOneOf(TLMessages.PutFullData, TLMessages.PutPartialData),
      edge.AccessAck(acq),
      edge.AccessAck(acq, 0.U))
    tl_in.d.bits.data := io.dmem.resp.bits.data_raw.holdUnless(state === s_wait2)

    // Tie off unused channels
    tl_in.b.valid := false.B
    tl_in.c.ready := true.B
    tl_in.e.ready := true.B
  }
}
