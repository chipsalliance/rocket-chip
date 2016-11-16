// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import diplomacy._
import cde.Parameters
import uncore.tilelink._
import uncore.constants._

// Instantiate 'val p' before HasTileLinkParameters tries to use it
abstract class LegacyLazyModuleImp(module: LazyModule)(implicit val p: Parameters)
  extends LazyModuleImp(module) with HasTileLinkParameters

class TLLegacy(implicit val p: Parameters) extends LazyModule with HasTileLinkParameters
{
  // TL legacy clients don't support anything fancy
  val node = TLClientNode(TLClientParameters(
    sourceId = IdRange(0, 1 << tlClientXactIdBits)))

  lazy val module = new LegacyLazyModuleImp(this) {
    val io = new Bundle {
      val legacy = new ClientUncachedTileLinkIO()(p).flip
      val out = node.bundleOut
    }

    // TL legacy is dumb. All managers must support it's accesses.
    val edge = node.edgesOut(0)
    require (edge.manager.beatBytes == tlDataBytes)
    edge.manager.managers.foreach { m =>
      // If a slave supports read at all, it must support all TL Legacy requires
      if (m.supportsGet) {
        require (m.supportsGet.contains(TransferSizes(1, tlDataBytes)))
        require (m.supportsGet.contains(TransferSizes(tlDataBeats * tlDataBytes)))
      }
      // Likewise, any put support must mean full put support
      if (m.supportsPutPartial) {
        require (m.supportsPutPartial.contains(TransferSizes(1, tlDataBytes)))
        require (m.supportsPutPartial.contains(TransferSizes(tlDataBeats * tlDataBytes)))
      }
      // Any atomic support => must support 32-bit size
      if (m.supportsArithmetic) { require (m.supportsArithmetic.contains(TransferSizes(4))) }
      if (m.supportsLogical)    { require (m.supportsLogical   .contains(TransferSizes(4))) }
      // We straight-up require hints
      require (edge.manager.allSupportHint)
    }
    // TL legacy will not generate PutFull
    // During conversion from TL Legacy, we won't support Acquire

    val out = io.out(0)
    out.a.valid := io.legacy.acquire.valid
    out.d.ready := io.legacy.grant  .ready
    io.legacy.acquire.ready := out.a.ready
    io.legacy.grant  .valid := out.d.valid

    val source  = io.legacy.acquire.bits.client_xact_id
    val data    = io.legacy.acquire.bits.data
    val wmask   = io.legacy.acquire.bits.wmask()
    val address = io.legacy.acquire.bits.full_addr()

    val block = UInt(log2Ceil(tlDataBytes*tlDataBeats))
    val size = io.legacy.acquire.bits.op_size()

    // Find the operation size and offset from the wmask
    // Returns: (any_1, size, offset)
    def mask_helper(range: UInt): (Bool, UInt, UInt) = {
      val len = range.getWidth
      if (len == 1) {
        (range === UInt(1), UInt(0), UInt(0)) // ugh. offset has one useless bit.
      } else {
        val mid = len / 2
        val lo  = range(mid-1, 0)
        val hi  = range(len-1, mid)
        val (lo_1, lo_s, lo_a) = mask_helper(lo)
        val (hi_1, hi_s, hi_a) = mask_helper(hi)
        val out_1 = lo_1 || hi_1
        val out_s = Mux(lo_1, Mux(hi_1, UInt(log2Up(len)), lo_s), hi_s)
        val out_a = Mux(lo_1, Mux(hi_1, UInt(0), lo_a), Cat(UInt(1), hi_a))
        (out_1, out_s, out_a)
      }
    }
    val (_, wsize, wlow1) = mask_helper(wmask)
    val wlow = wlow1 >> 1

    // Only create atomic messages if TL2 managers support them
    val atomics = if (edge.manager.anySupportLogical) {
      MuxLookup(io.legacy.acquire.bits.op_code(), Wire(new TLBundleA(edge.bundle)), Array(
        MemoryOpConstants.M_XA_SWAP -> edge.Logical(source, address, size, data, TLAtomics.SWAP)._2,
        MemoryOpConstants.M_XA_XOR  -> edge.Logical(source, address, size, data, TLAtomics.XOR) ._2,
        MemoryOpConstants.M_XA_OR   -> edge.Logical(source, address, size, data, TLAtomics.OR)  ._2,
        MemoryOpConstants.M_XA_AND  -> edge.Logical(source, address, size, data, TLAtomics.AND) ._2,
        MemoryOpConstants.M_XA_ADD  -> edge.Arithmetic(source, address, size, data, TLAtomics.ADD)._2,
        MemoryOpConstants.M_XA_MIN  -> edge.Arithmetic(source, address, size, data, TLAtomics.MIN)._2,
        MemoryOpConstants.M_XA_MAX  -> edge.Arithmetic(source, address, size, data, TLAtomics.MAX)._2,
        MemoryOpConstants.M_XA_MINU -> edge.Arithmetic(source, address, size, data, TLAtomics.MINU)._2,
        MemoryOpConstants.M_XA_MAXU -> edge.Arithmetic(source, address, size, data, TLAtomics.MAXU)._2))
    } else {
      // If no managers support atomics, assert fail if TL1 asks for them
      assert (!io.legacy.acquire.valid || io.legacy.acquire.bits.a_type =/= Acquire.putAtomicType)
      Wire(new TLBundleA(edge.bundle))
    }

    val beatMask  = UInt(tlDataBytes-1)
    val blockMask = UInt(tlDataBytes*tlDataBeats-1)
    out.a.bits := MuxLookup(io.legacy.acquire.bits.a_type, Wire(new TLBundleA(edge.bundle)), Array(
      Acquire.getType         -> edge.Get (source, address, size)._2,
      Acquire.getBlockType    -> edge.Get (source, ~(~address|blockMask), block)._2,
      Acquire.putType         -> edge.Put (source, address|wlow, wsize, data, wmask)._2,
      Acquire.putBlockType    -> edge.Put (source, ~(~address|blockMask), block, data, wmask)._2,
      Acquire.getPrefetchType -> edge.Hint(source, ~(~address|blockMask), block, UInt(0))._2,
      Acquire.putPrefetchType -> edge.Hint(source, ~(~address|blockMask), block, UInt(1))._2,
      Acquire.putAtomicType   -> atomics))

    // Get rid of some unneeded muxes
    out.a.bits.source  := source
    out.a.bits.data    := data

    // TL legacy does not support bus errors
    assert (!out.d.valid || !out.d.bits.error)

    // Recreate the beat address counter
    val beatCounter = RegInit(UInt(0, width = tlBeatAddrBits))
    when (out.d.fire() && edge.hasData(out.d.bits) && out.d.bits.size === block) {
      beatCounter := beatCounter + UInt(1)
    }

    val grant = io.legacy.grant.bits
    grant.g_type := MuxLookup(out.d.bits.opcode, Grant.prefetchAckType, Array(
      TLMessages.AccessAck     -> Grant.putAckType,
      TLMessages.AccessAckData -> Mux(out.d.bits.size === block, Grant.getDataBlockType, Grant.getDataBeatType),
      TLMessages.HintAck       -> Grant.prefetchAckType))
    grant.is_builtin_type := Bool(true)
    grant.client_xact_id  := out.d.bits.source
    grant.manager_xact_id := UInt(0)
    grant.data            := out.d.bits.data
    grant.addr_beat       := beatCounter

    // Tie off unused channels
    out.b.ready := Bool(true)
    out.c.valid := Bool(false)
    out.e.valid := Bool(false)
  }
}
