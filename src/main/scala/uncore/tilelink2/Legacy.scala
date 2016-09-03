// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import cde.Parameters
import uncore.tilelink._
import uncore.constants._

class TLLegacy(implicit val p: Parameters) extends LazyModule with HasTileLinkParameters
{
  val outer_p = p
  // TL legacy clients don't support anything fancy
  val node = TLClientNode(TLClientParameters(
    sourceId = IdRange(0, 1 << tlClientXactIdBits)))

  lazy val module = new LazyModuleImp(this) with HasTileLinkParameters {
    val p = outer_p
    val io = new Bundle {
      val legacy = new ClientUncachedTileLinkIO()(p).flip
      val out = node.bundleOut
    }

    // TL legacy is dumb. All managers must support it's accesses.
    val edge = node.edgesOut(0)
    require (edge.manager.beatBytes == tlDataBytes)
    require (edge.manager.allSupportGet       .contains(TransferSizes(tlDataBytes)))
    require (edge.manager.allSupportGet       .contains(TransferSizes(tlDataBeats * tlDataBytes)))
    require (edge.manager.allSupportPutPartial.contains(TransferSizes(tlDataBytes)))
    require (edge.manager.allSupportPutPartial.contains(TransferSizes(tlDataBeats * tlDataBytes)))
    require (edge.manager.allSupportArithmetic.contains(TransferSizes(4, tlDataBytes)))
    require (edge.manager.allSupportLogical   .contains(TransferSizes(4, tlDataBytes)))
    require (edge.manager.allSupportHint)
    // TL legacy will not generate PutFull, Acquire

    // Must be able to fit TL2 sink_id into TL legacy
    require ((1 << tlManagerXactIdBits) >= edge.manager.endSinkId)

    val out = io.out(0)
    out.a.valid := io.legacy.acquire.valid
    out.d.ready := io.legacy.grant  .ready
    io.legacy.acquire.ready := out.a.ready
    io.legacy.grant  .valid := out.d.valid

    val source  = io.legacy.acquire.bits.client_xact_id
    val data    = io.legacy.acquire.bits.data
    val wmask   = io.legacy.acquire.bits.wmask()
    val address = io.legacy.acquire.bits.full_addr()

    val beat  = UInt(log2Ceil(tlDataBytes))
    val block = UInt(log2Ceil(tlDataBytes*tlDataBeats))

    out.a.bits := MuxLookup(io.legacy.acquire.bits.a_type, new TLBundleA(edge.bundle), Array(
      Acquire.getType         -> edge.Get (source, address, beat) ._2,
      Acquire.getBlockType    -> edge.Get (source, address, block)._2,
      Acquire.putType         -> edge.Put (source, address, beat,  data, wmask)._2,
      Acquire.putBlockType    -> edge.Put (source, address, block, data, wmask)._2,
      Acquire.getPrefetchType -> edge.Hint(source, address, block, UInt(0))._2,
      Acquire.putPrefetchType -> edge.Hint(source, address, block, UInt(1))._2,
      Acquire.putAtomicType   -> MuxLookup(io.legacy.acquire.bits.op_code(), new TLBundleA(edge.bundle), Array(
        MemoryOpConstants.M_XA_SWAP -> edge.Logical(source, address, beat, data, TLAtomics.SWAP)._2,
        MemoryOpConstants.M_XA_XOR  -> edge.Logical(source, address, beat, data, TLAtomics.XOR) ._2,
        MemoryOpConstants.M_XA_OR   -> edge.Logical(source, address, beat, data, TLAtomics.OR)  ._2,
        MemoryOpConstants.M_XA_AND  -> edge.Logical(source, address, beat, data, TLAtomics.AND) ._2,
        MemoryOpConstants.M_XA_ADD  -> edge.Arithmetic(source, address, beat, data, TLAtomics.ADD)._2,
        MemoryOpConstants.M_XA_MIN  -> edge.Arithmetic(source, address, beat, data, TLAtomics.MIN)._2,
        MemoryOpConstants.M_XA_MAX  -> edge.Arithmetic(source, address, beat, data, TLAtomics.MAX)._2,
        MemoryOpConstants.M_XA_MINU -> edge.Arithmetic(source, address, beat, data, TLAtomics.MINU)._2,
        MemoryOpConstants.M_XA_MAXU -> edge.Arithmetic(source, address, beat, data, TLAtomics.MAXU)._2))))

    val beatMask  = UInt(tlDataBytes-1)
    val blockMask = UInt(tlDataBytes*tlDataBeats-1)
    val addressMask = MuxLookup(io.legacy.acquire.bits.a_type, beatMask, Array(
      Acquire.getType         -> beatMask,
      Acquire.getBlockType    -> blockMask,
      Acquire.putType         -> beatMask,
      Acquire.putBlockType    -> blockMask,
      Acquire.getPrefetchType -> blockMask,
      Acquire.putPrefetchType -> blockMask,
      Acquire.putAtomicType   -> beatMask))

    // Get rid of some unneeded muxes
    out.a.bits.source  := source
    out.a.bits.data    := data
    out.a.bits.address := address & ~addressMask

    // TL legacy does not support bus errors
    assert (!out.d.bits.error)

    // Recreate the beat address counter
    val beatCounter = RegInit(UInt(0, width = tlBeatAddrBits))
    when (out.d.fire() && out.d.bits.hasData() && out.d.bits.size === block) {
      beatCounter := beatCounter + UInt(1)
    }

    val grant = io.legacy.grant.bits
    grant.g_type := MuxLookup(out.d.bits.opcode, Grant.prefetchAckType, Array(
      TLMessages.AccessAck     -> Grant.putAckType,
      TLMessages.AccessAckData -> Mux(out.d.bits.size === beat, Grant.getDataBeatType, Grant.getDataBlockType),
      TLMessages.HintAck       -> Grant.prefetchAckType))
    grant.is_builtin_type := Bool(true)
    grant.client_xact_id  := out.d.bits.source
    grant.manager_xact_id := out.d.bits.sink
    grant.data            := out.d.bits.data
    grant.addr_beat       := beatCounter
  }
}
