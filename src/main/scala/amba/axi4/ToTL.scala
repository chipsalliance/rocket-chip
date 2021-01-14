// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

class ReorderData(val dataWidth: Int, val respWidth: Int, val userFields: Seq[BundleFieldBase]) extends Bundle {
  val data = UInt(dataWidth.W)
  val resp = UInt(respWidth.W)
  val last = Bool()
  val user = BundleMap(userFields)
}

/** Parameters for [[BaseReservableListBuffer]] and all child classes.
  *
  * @param numEntries Total number of elements that can be stored in the 'data' RAM
  * @param numLists   Maximum number of linked lists
  * @param numBeats   Maximum number of beats per entry
  */
case class ReservableListBufferParameters(numEntries: Int, numLists: Int, numBeats: Int) {
  // Avoid zero-width wires when we call 'log2Ceil'
  val entryBits = if (numEntries == 1) 1 else log2Ceil(numEntries)
  val listBits  = if (numLists == 1) 1 else log2Ceil(numLists)
  val beatBits  = if (numBeats == 1) 1 else log2Ceil(numBeats)
}

case class AXI4ToTLNode(numTlTxns: Int, wcorrupt: Boolean)(implicit valName: ValName)
    extends MixedAdapterNode(AXI4Imp, TLImp)(
      dFn = { case mp =>
        TLMasterPortParameters.v2(
          masters = mp.masters.zipWithIndex.map { case (m, i) =>
            // Support 'numTlTxns' read requests and 'numTlTxns' write requests at once.
            val numSourceIds = numTlTxns * 2
            TLMasterParameters.v2(
              name = m.name,
              sourceId = IdRange(i * numSourceIds, (i + 1) * numSourceIds),
              nodePath = m.nodePath
            )
          },
          echoFields = mp.echoFields,
          requestFields = AMBAProtField() +: mp.requestFields,
          responseKeys = mp.responseKeys
        )
      },
      uFn = { mp =>
        AXI4SlavePortParameters(
          slaves = mp.managers.map { m =>
            val maxXfer = TransferSizes(1, mp.beatBytes * (1 << AXI4Parameters.lenBits))
            AXI4SlaveParameters(
              address = m.address,
              resources = m.resources,
              regionType = m.regionType,
              executable = m.executable,
              nodePath = m.nodePath,
              supportsWrite = m.supportsPutPartial.intersect(maxXfer),
              supportsRead = m.supportsGet.intersect(maxXfer),
              interleavedId = Some(0) // TL2 never interleaves D beats
            )
          },
          beatBytes = mp.beatBytes,
          minLatency = mp.minLatency,
          responseFields = mp.responseFields,
          requestKeys = (if (wcorrupt) Seq(AMBACorrupt) else Seq()) ++ mp.requestKeys.filter(_ != AMBAProt)
        )
      }
    )

class AXI4ToTL(numTlTxns: Int, wcorrupt: Boolean)(implicit p: Parameters) extends LazyModule {
  require(numTlTxns >= 1)
  require(isPow2(numTlTxns), s"Number of TileLink transactions ($numTlTxns) must be a power of 2")

  val node = AXI4ToTLNode(numTlTxns, wcorrupt)

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      edgeIn.master.masters.foreach { m =>
        require(m.aligned, "AXI4ToTL requires aligned requests")
      }

      val numIds      = edgeIn.master.endId
      val beatBytes   = edgeOut.slave.beatBytes
      val maxTransfer = edgeOut.slave.maxTransfer
      val maxBeats    = maxTransfer / beatBytes

      // Look for an Error device to redirect bad requests
      val errorDevs = edgeOut.slave.managers.filter(_.nodePath.last.lazyModule.className == "TLError")
      require(!errorDevs.isEmpty, "There is no TLError reachable from AXI4ToTL. One must be instantiated.")
      val errorDev     = errorDevs.maxBy(_.maxTransfer)
      val errorDevAddr = errorDev.address.head.base
      require(
        errorDev.supportsPutPartial.contains(maxTransfer),
        s"Error device supports ${errorDev.supportsPutPartial} PutPartial but must support $maxTransfer"
      )
      require(
        errorDev.supportsGet.contains(maxTransfer),
        s"Error device supports ${errorDev.supportsGet} Get but must support $maxTransfer"
      )

      // All of the read-response reordering logic.
      val listBufData   = new ReorderData(beatBytes * 8, edgeIn.bundle.respBits, out.d.bits.user.fields)
      val listBufParams = ReservableListBufferParameters(numTlTxns, numIds, maxBeats)
      val listBuffer = if (numTlTxns > 1) {
        Module(new ReservableListBuffer(listBufData, listBufParams))
      } else {
        Module(new PassthroughListBuffer(listBufData, listBufParams))
      }

      // To differentiate between read and write transaction IDs, we will set the MSB of the TileLink 'source' field to
      // 0 for read requests and 1 for write requests.
      val isReadSourceBit  = 0.U(1.W)
      val isWriteSourceBit = 1.U(1.W)

      /* Read request logic */

      val rOut    = Wire(Decoupled(new TLBundleA(edgeOut.bundle)))
      val rBytes1 = in.ar.bits.bytes1()
      val rSize   = OH1ToUInt(rBytes1)
      val rOk     = edgeOut.slave.supportsGetSafe(in.ar.bits.addr, rSize)
      val rId = if (numTlTxns > 1) {
        Cat(isReadSourceBit, listBuffer.ioReservedIndex)
      } else {
        isReadSourceBit
      }
      val rAddr = Mux(rOk, in.ar.bits.addr, errorDevAddr.U | in.ar.bits.addr(log2Ceil(beatBytes) - 1, 0))

      // Indicates if there are still valid TileLink source IDs left to use.
      val canIssueR = listBuffer.ioReserve.ready

      listBuffer.ioReserve.bits  := in.ar.bits.id
      listBuffer.ioReserve.valid := in.ar.valid && rOut.ready
      in.ar.ready                := rOut.ready && canIssueR
      rOut.valid                 := in.ar.valid && canIssueR
      rOut.bits :<= edgeOut.Get(rId, rAddr, rSize)._2

      rOut.bits.user :<= in.ar.bits.user
      rOut.bits.user.lift(AMBAProt).foreach { rProt =>
        rProt.privileged := in.ar.bits.prot(0)
        rProt.secure     := !in.ar.bits.prot(1)
        rProt.fetch      := in.ar.bits.prot(2)
        rProt.bufferable := in.ar.bits.cache(0)
        rProt.modifiable := in.ar.bits.cache(1)
        rProt.readalloc  := in.ar.bits.cache(2)
        rProt.writealloc := in.ar.bits.cache(3)
      }

      /* Write request logic */

      // Strip off the MSB, which identifies the transaction as read vs write.
      val strippedResponseSourceId = if (numTlTxns > 1) {
        out.d.bits.source((out.d.bits.source).getWidth - 2, 0)
      } else {
        // When there's only 1 TileLink transaction allowed for read/write, then this field is always 0.
        0.U(1.W)
      }

      // Track when a write request burst is in progress.
      val writeBurstBusy = RegInit(false.B)
      when(in.w.fire()) {
        writeBurstBusy := !in.w.bits.last
      }

      val usedWriteIds = RegInit(0.U(numTlTxns.W))
      val canIssueW    = !usedWriteIds.andR()

      val usedWriteIdsSet = WireDefault(0.U(numTlTxns.W))
      val usedWriteIdsClr = WireDefault(0.U(numTlTxns.W))

      usedWriteIds := (usedWriteIds & ~usedWriteIdsClr) | usedWriteIdsSet

      // Since write responses can show up in the middle of a write burst, we need to ensure the write burst ID doesn't
      // change mid-burst.
      val freeWriteIdOHRaw = Wire(UInt(numTlTxns.W))
      val freeWriteIdOH    = freeWriteIdOHRaw holdUnless !writeBurstBusy
      val freeWriteIdIndex = OHToUInt(freeWriteIdOH)
      freeWriteIdOHRaw := ~(leftOR(~usedWriteIds) << 1) & ~usedWriteIds

      val wOut    = Wire(Decoupled(new TLBundleA(edgeOut.bundle)))
      val wBytes1 = in.aw.bits.bytes1()
      val wSize   = OH1ToUInt(wBytes1)
      val wOk     = edgeOut.slave.supportsPutPartialSafe(in.aw.bits.addr, wSize)
      val wId = if (numTlTxns > 1) {
        Cat(isWriteSourceBit, freeWriteIdIndex)
      } else {
        isWriteSourceBit
      }
      val wAddr = Mux(wOk, in.aw.bits.addr, errorDevAddr.U | in.aw.bits.addr(log2Ceil(beatBytes) - 1, 0))

      // Here, we're taking advantage of the Irrevocable behavior of AXI4 (once 'valid' is asserted it must remain
      // asserted until the handshake occurs). We will only accept W-channel beats when we have a valid AW beat, but
      // the AW-channel beat won't fire until the final W-channel beat fires. So, we have stable address/size/strb
      // bits during a W-channel burst.
      in.aw.ready := wOut.ready && in.w.valid && in.w.bits.last && canIssueW
      in.w.ready  := wOut.ready && in.aw.valid && canIssueW
      wOut.valid  := in.aw.valid && in.w.valid && canIssueW
      wOut.bits :<= edgeOut.Put(wId, wAddr, wSize, in.w.bits.data, in.w.bits.strb)._2
      in.w.bits.user.lift(AMBACorrupt).foreach { wOut.bits.corrupt := _ }

      wOut.bits.user :<= in.aw.bits.user
      wOut.bits.user.lift(AMBAProt).foreach { wProt =>
        wProt.privileged := in.aw.bits.prot(0)
        wProt.secure     := !in.aw.bits.prot(1)
        wProt.fetch      := in.aw.bits.prot(2)
        wProt.bufferable := in.aw.bits.cache(0)
        wProt.modifiable := in.aw.bits.cache(1)
        wProt.readalloc  := in.aw.bits.cache(2)
        wProt.writealloc := in.aw.bits.cache(3)
      }

      // Merge the AXI4 read/write requests into the TL-A channel.
      TLArbiter(TLArbiter.roundRobin)(out.a, (0.U, rOut), (in.aw.bits.len, wOut))

      /* Read/write response logic */

      val okB = Wire(Irrevocable(new AXI4BundleB(edgeIn.bundle)))
      val okR = Wire(Irrevocable(new AXI4BundleR(edgeIn.bundle)))

      val dResp    = Mux(out.d.bits.denied || out.d.bits.corrupt, AXI4Parameters.RESP_SLVERR, AXI4Parameters.RESP_OKAY)
      val dHasData = edgeOut.hasData(out.d.bits)

      val (_dFirst, dLast, _dDone, dCount) = edgeOut.count(out.d)
      val dNumBeats1                       = edgeOut.numBeats1(out.d.bits)

      out.d.ready                          := Mux(dHasData, listBuffer.ioResponse.ready, okB.ready)
      listBuffer.ioDataOut.ready           := okR.ready
      okR.valid                            := listBuffer.ioDataOut.valid
      okB.valid                            := out.d.valid && !dHasData
      listBuffer.ioResponse.valid          := out.d.valid && dHasData
      listBuffer.ioResponse.bits.index     := strippedResponseSourceId
      listBuffer.ioResponse.bits.data.data := out.d.bits.data
      listBuffer.ioResponse.bits.data.resp := dResp
      listBuffer.ioResponse.bits.data.last := dLast
      listBuffer.ioResponse.bits.data.user :<= out.d.bits.user
      listBuffer.ioResponse.bits.count     := dCount
      listBuffer.ioResponse.bits.numBeats1 := dNumBeats1

      okR.bits.id   := listBuffer.ioDataOut.bits.listIndex
      okR.bits.data := listBuffer.ioDataOut.bits.payload.data
      okR.bits.resp := listBuffer.ioDataOut.bits.payload.resp
      okR.bits.last := listBuffer.ioDataOut.bits.payload.last
      okR.bits.user :<= listBuffer.ioDataOut.bits.payload.user

      // Upon the final beat in a write request, record a mapping from TileLink source ID to AXI write ID. Upon a write
      // response, mark the write transaction as complete.
      val writeIdMap      = Mem(numTlTxns, UInt(log2Ceil(numIds).W))
      val writeResponseId = writeIdMap.read(strippedResponseSourceId)
      when(edgeOut.done(wOut)) {
        writeIdMap.write(freeWriteIdIndex, in.aw.bits.id)
        usedWriteIdsSet := freeWriteIdOH
      }
      when(okB.fire()) {
        usedWriteIdsClr := UIntToOH(strippedResponseSourceId, numTlTxns)
      }

      okB.bits.id   := writeResponseId
      okB.bits.resp := dResp
      okB.bits.user :<= out.d.bits.user

      // AXI4 needs irrevocable behaviour
      in.r :<> Queue.irrevocable(okR, 1, flow = true)
      in.b :<> Queue.irrevocable(okB, 1, flow = true)

      // Unused channels
      out.b.ready := true.B
      out.c.valid := false.B
      out.e.valid := false.B

      /* Alignment constraints. The AXI4Fragmenter should guarantee all of these constraints. */

      def checkRequest[T <: AXI4BundleA](a: IrrevocableIO[T], reqType: String): Unit = {
        val lReqType = reqType.toLowerCase
        when(a.valid) {
          assert(a.bits.len < maxBeats.U, s"$reqType burst length (%d) must be less than $maxBeats", a.bits.len + 1.U)

          // Narrow transfers and FIXED bursts must be single-beat bursts.
          when(a.bits.len =/= 0.U) {
            assert(
              a.bits.size === log2Ceil(beatBytes).U,
              s"Narrow $lReqType transfers (%d < $beatBytes bytes) can't be multi-beat bursts (%d beats)",
              1.U << a.bits.size,
              a.bits.len + 1.U
            )
            assert(
              a.bits.burst =/= AXI4Parameters.BURST_FIXED,
              s"Fixed $lReqType bursts can't be multi-beat bursts (%d beats)",
              a.bits.len + 1.U
            )
          }

          // Furthermore, the transfer size (a.bits.bytes1() + 1.U) must be naturally-aligned to the address (in
          // particular, during both WRAP and INCR bursts), but this constraint is already checked by TileLink
          // Monitors. Note that this alignment requirement means that WRAP bursts are identical to INCR bursts.
        }
      }

      checkRequest(in.ar, "Read")
      checkRequest(in.aw, "Write")
    }
  }
}

object AXI4ToTL {
  def apply(numTlTxns: Int = 1, wcorrupt: Boolean = true)(implicit p: Parameters) = {
    val axi42tl = LazyModule(new AXI4ToTL(numTlTxns, wcorrupt))
    axi42tl.node
  }
}

/* ReservableListBuffer logic, and associated classes. */

class ResponsePayload[T <: Data](val data: T, val params: ReservableListBufferParameters) extends Bundle {
  val index     = UInt(params.entryBits.W)
  val count     = UInt(params.beatBits.W)
  val numBeats1 = UInt(params.beatBits.W)
}

class DataOutPayload[T <: Data](val payload: T, val params: ReservableListBufferParameters) extends Bundle {
  val listIndex = UInt(params.listBits.W)
}

/** Abstract base class to unify [[ReservableListBuffer]] and [[PassthroughListBuffer]]. */
abstract class BaseReservableListBuffer[T <: Data](gen: T, params: ReservableListBufferParameters)
    extends MultiIOModule {
  require(params.numEntries > 0)
  require(params.numLists > 0)

  val ioReserve       = IO(Flipped(Decoupled(UInt(params.listBits.W))))
  val ioReservedIndex = IO(Output(UInt(params.entryBits.W)))
  val ioResponse      = IO(Flipped(Decoupled(new ResponsePayload(gen, params))))
  val ioDataOut       = IO(Decoupled(new DataOutPayload(gen, params)))
}

/** A modified version of 'ListBuffer' from 'sifive/block-inclusivecache-sifive'. This module forces users to reserve
  * linked list entries (through the 'ioReserve' port) before writing data into those linked lists (through the
  * 'ioResponse' port). Each response is tagged to indicate which linked list it is written into. The responses for a
  * given linked list can come back out-of-order, but they will be read out through the 'ioDataOut' port in-order.
  *
  * ==Constructor==
  * @param gen    Chisel type of linked list data element
  * @param params Other parameters
  *
  * ==Module IO==
  * @param ioReserve       Index of list to reserve a new element in
  * @param ioReservedIndex Index of the entry that was reserved in the linked list, valid when 'ioReserve.fire()'
  * @param ioResponse      Payload containing response data and linked-list-entry index
  * @param ioDataOut       Payload containing data read from response linked list and linked list index
  */
class ReservableListBuffer[T <: Data](gen: T, params: ReservableListBufferParameters)
    extends BaseReservableListBuffer(gen, params) {
  val valid         = RegInit(0.U(params.numLists.W))
  val head          = Mem(params.numLists, UInt(params.entryBits.W))
  val tail          = Mem(params.numLists, UInt(params.entryBits.W))
  val used          = RegInit(0.U(params.numEntries.W))
  val next          = Mem(params.numEntries, UInt(params.entryBits.W))
  val map           = Mem(params.numEntries, UInt(params.listBits.W))
  val dataMems      = Seq.fill(params.numBeats) { SyncReadMem(params.numEntries, gen) }
  val dataIsPresent = RegInit(0.U(params.numEntries.W))
  val beats         = Mem(params.numEntries, UInt(params.beatBits.W))

  // The 'data' SRAM should be single-ported (read-or-write), since dual-ported SRAMs are significantly slower.
  val dataMemReadEnable  = WireDefault(false.B)
  val dataMemWriteEnable = WireDefault(false.B)
  assert(!(dataMemReadEnable && dataMemWriteEnable))

  // 'freeOH' has a single bit set, which is the least-significant bit that is cleared in 'used'. So, it's the
  // lowest-index entry in the 'data' RAM which is free.
  val freeOH    = Wire(UInt(params.numEntries.W))
  val freeIndex = OHToUInt(freeOH)
  freeOH          := ~(leftOR(~used) << 1) & ~used
  ioReservedIndex := freeIndex

  val validSet         = WireDefault(0.U(params.numLists.W))
  val validClr         = WireDefault(0.U(params.numLists.W))
  val usedSet          = WireDefault(0.U(params.numEntries.W))
  val usedClr          = WireDefault(0.U(params.numEntries.W))
  val dataIsPresentSet = WireDefault(0.U(params.numEntries.W))
  val dataIsPresentClr = WireDefault(0.U(params.numEntries.W))

  valid         := (valid & ~validClr) | validSet
  used          := (used & ~usedClr) | usedSet
  dataIsPresent := (dataIsPresent & ~dataIsPresentClr) | dataIsPresentSet

  /* Reservation logic signals */

  val reserveTail    = Wire(UInt(params.entryBits.W))
  val reserveIsValid = Wire(Bool())

  /* Response logic signals */

  val responseIndex      = Wire(UInt(params.entryBits.W))
  val responseListIndex  = Wire(UInt(params.listBits.W))
  val responseHead       = Wire(UInt(params.entryBits.W))
  val responseTail       = Wire(UInt(params.entryBits.W))
  val nextResponseHead   = Wire(UInt(params.entryBits.W))
  val nextDataIsPresent  = Wire(Bool())
  val isResponseInOrder  = Wire(Bool())
  val isEndOfList        = Wire(Bool())
  val isLastBeat         = Wire(Bool())
  val isLastResponseBeat = Wire(Bool())
  val isLastUnwindBeat   = Wire(Bool())

  /* Reservation logic */

  reserveTail    := tail.read(ioReserve.bits)
  reserveIsValid := valid(ioReserve.bits)

  ioReserve.ready := !used.andR()

  // When we want to append-to and destroy the same linked list on the same cycle, we need to take special care that we
  // actually start a new list, rather than appending to a list that's about to disappear.
  val reserveResponseSameList = ioReserve.bits === responseListIndex
  val appendToAndDestroyList =
    ioReserve.fire() && ioDataOut.fire() && reserveResponseSameList && isEndOfList && isLastBeat

  when(ioReserve.fire()) {
    validSet := UIntToOH(ioReserve.bits, params.numLists)
    usedSet  := freeOH
    when(reserveIsValid && !appendToAndDestroyList) {
      next.write(reserveTail, freeIndex)
    }.otherwise {
      head.write(ioReserve.bits, freeIndex)
    }
    tail.write(ioReserve.bits, freeIndex)
    map.write(freeIndex, ioReserve.bits)
  }

  /* Response logic */

  // The majority of the response logic (reading from and writing to the various RAMs) is common between the
  // response-from-IO case (ioResponse.fire()) and the response-from-unwind case (unwindDataIsValid).

  // The read from the 'next' RAM should be performed at the address given by 'responseHead'. However, we only use the
  // 'nextResponseHead' signal when 'isResponseInOrder' is asserted (both in the response-from-IO and
  // response-from-unwind cases), which implies that 'responseHead' equals 'responseIndex'. 'responseHead' comes after
  // two back-to-back RAM reads, so indexing into the 'next' RAM with 'responseIndex' is much quicker.

  responseHead      := head.read(responseListIndex)
  responseTail      := tail.read(responseListIndex)
  nextResponseHead  := next.read(responseIndex)
  nextDataIsPresent := dataIsPresent(nextResponseHead)

  // Note that when 'isEndOfList' is asserted, 'nextResponseHead' (and therefore 'nextDataIsPresent') is invalid, since
  // there isn't a next element in the linked list.
  isResponseInOrder := responseHead === responseIndex
  isEndOfList       := responseHead === responseTail

  isLastResponseBeat := ioResponse.bits.count === ioResponse.bits.numBeats1

  // When a response's last beat is sent to the output channel, mark it as completed. This can happen in two
  // situations:
  //  1. We receive an in-order response, which travels straight from 'ioResponse' to 'ioDataOut'. The 'data' SRAM
  //     reservation was never needed.
  //  2. An entry is read out of the 'data' SRAM (within the unwind FSM).
  when(ioDataOut.fire() && isLastBeat) {
    // Mark the reservation as no-longer-used.
    usedClr := UIntToOH(responseIndex, params.numEntries)
    // If the response is in-order, then we're popping an element from this linked list.
    when(isEndOfList) {
      // Once we pop the last element from a linked list, mark it as no-longer-present.
      validClr := UIntToOH(responseListIndex, params.numLists)
    }.otherwise {
      // Move the linked list's head pointer to the new head pointer.
      head.write(responseListIndex, nextResponseHead)
    }
  }

  // If we get an out-of-order response, then stash it in the 'data' SRAM for later unwinding.
  when(ioResponse.fire() && !isResponseInOrder) {
    dataMemWriteEnable := true.B

    when(isLastResponseBeat) {
      dataIsPresentSet := UIntToOH(ioResponse.bits.index, params.numEntries)
      beats.write(ioResponse.bits.index, ioResponse.bits.numBeats1)
    }
  }

  // Use the 'ioResponse.bits.count' index (AKA the beat number) to select which 'data' SRAM to write to.
  val responseCountOH = UIntToOH(ioResponse.bits.count, params.numBeats)
  (responseCountOH.asBools zip dataMems) foreach { case (select, seqMem) =>
    when(select && dataMemWriteEnable) {
      seqMem.write(ioResponse.bits.index, ioResponse.bits.data)
    }
  }

  /* Response unwind logic */

  // Unwind FSM state definitions
  val sIdle :: sUnwinding :: Nil = Enum(2)

  val unwindState   = RegInit(sIdle)
  val busyUnwinding = unwindState === sUnwinding
  val startUnwind   = Wire(Bool())
  val stopUnwind    = Wire(Bool())

  when(startUnwind) {
    unwindState := sUnwinding
  }.elsewhen(stopUnwind) {
    unwindState := sIdle
  }
  assert(!(startUnwind && stopUnwind))

  // Start the unwind FSM when there is an old out-of-order response stored in the 'data' SRAM that is now about to
  // become the next in-order response. As noted previously, when 'isEndOfList' is asserted, 'nextDataIsPresent' is
  // invalid.
  //
  // Note that since an in-order response from 'ioResponse' to 'ioDataOut' starts the unwind FSM, we don't have to
  // worry about overwriting the 'data' SRAM's output when we start the unwind FSM.
  startUnwind := ioResponse.fire() && isResponseInOrder && isLastResponseBeat && !isEndOfList && nextDataIsPresent

  // Stop the unwind FSM when the output channel consumes the final beat of an element from the unwind FSM, and one of
  // two things happens:
  //  1. We're still waiting for the next in-order response for this list (!nextDataIsPresent)
  //  2. There are no more outstanding responses in this list (isEndOfList)
  //
  // Including 'busyUnwinding' ensures this is a single-cycle pulse, and it never fires while in-order transactions are
  // passing from 'ioResponse' to 'ioDataOut'.
  stopUnwind := busyUnwinding && ioDataOut.fire() && isLastUnwindBeat && (!nextDataIsPresent || isEndOfList)

  val isUnwindBurstOver = Wire(Bool())
  val startNewBurst     = startUnwind || (isUnwindBurstOver && dataMemReadEnable)

  // Track the number of beats left to unwind for each list entry. At the start of a new burst, we flop the number of
  // beats in this burst (minus 1) into 'unwindBeats1', and we reset the 'beatCounter' counter. With each beat, we
  // increment 'beatCounter' until it reaches 'unwindBeats1'.
  val unwindBeats1    = Reg(UInt(params.beatBits.W))
  val nextBeatCounter = Wire(UInt(params.beatBits.W))
  val beatCounter     = RegNext(nextBeatCounter)

  isUnwindBurstOver := beatCounter === unwindBeats1

  when(startNewBurst) {
    unwindBeats1    := beats.read(nextResponseHead)
    nextBeatCounter := 0.U
  }.elsewhen(dataMemReadEnable) {
    nextBeatCounter := beatCounter + 1.U
  }.otherwise {
    nextBeatCounter := beatCounter
  }

  // When unwinding, feed the next linked-list head pointer (read out of the 'next' RAM) back so we can unwind the next
  // entry in this linked list. Only update the pointer when we're actually moving to the next 'data' SRAM entry (which
  // happens at the start of reading a new stored burst).
  val unwindResponseIndex = RegEnable(nextResponseHead, startNewBurst)
  responseIndex := Mux(busyUnwinding, unwindResponseIndex, ioResponse.bits.index)

  // Hold 'nextResponseHead' static while we're in the middle of unwinding a multi-beat burst entry. We don't want the
  // SRAM read address to shift while reading beats from a burst. Note that this is identical to 'nextResponseHead
  // holdUnless startNewBurst', but 'unwindResponseIndex' already implements the 'RegEnable' signal in 'holdUnless'.
  val unwindReadAddress = Mux(startNewBurst, nextResponseHead, unwindResponseIndex)

  // The 'data' SRAM's output is valid if we read from the SRAM on the previous cycle. The SRAM's output stays valid
  // until it is consumed by the output channel (and if we don't read from the SRAM again on that same cycle).
  val unwindDataIsValid = RegInit(false.B)
  when(dataMemReadEnable) {
    unwindDataIsValid := true.B
  }.elsewhen(ioDataOut.fire()) {
    unwindDataIsValid := false.B
  }

  isLastUnwindBeat := isUnwindBurstOver && unwindDataIsValid

  // Indicates if this is the last beat for both 'ioResponse'-to-'ioDataOut' and unwind-to-'ioDataOut' beats.
  isLastBeat := Mux(busyUnwinding, isLastUnwindBeat, isLastResponseBeat)

  // Select which SRAM to read from based on the beat counter.
  val dataOutputVec     = Wire(Vec(params.numBeats, gen))
  val nextBeatCounterOH = UIntToOH(nextBeatCounter, params.numBeats)
  (nextBeatCounterOH.asBools zip dataMems).zipWithIndex foreach { case ((select, seqMem), i) =>
    dataOutputVec(i) := seqMem.read(unwindReadAddress, select && dataMemReadEnable)
  }

  // Select the current 'data' SRAM output beat, and save the output in a register in case we're being back-pressured
  // by 'ioDataOut'. This implements the functionality of 'readAndHold', but only on the single SRAM we're reading
  // from.
  val dataOutput = dataOutputVec(beatCounter) holdUnless RegNext(dataMemReadEnable)

  // Mark 'data' burst entries as no-longer-present as they get read out of the SRAM.
  when(dataMemReadEnable) {
    dataIsPresentClr := UIntToOH(unwindReadAddress, params.numEntries)
  }

  // As noted above, when starting the unwind FSM, we know the 'data' SRAM's output isn't valid, so it's safe to issue
  // a read command. Otherwise, only issue an SRAM read when the next 'unwindState' is 'sUnwinding', and if we know
  // we're not going to overwrite the SRAM's current output (the SRAM output is already valid, and it's not going to be
  // consumed by the output channel).
  val dontReadFromDataMem = unwindDataIsValid && !ioDataOut.ready
  dataMemReadEnable := startUnwind || (busyUnwinding && !stopUnwind && !dontReadFromDataMem)

  // While unwinding, prevent new reservations from overwriting the current 'map' entry that we're using. We need
  // 'responseListIndex' to be coherent for the entire unwind process.
  val rawResponseListIndex    = map.read(responseIndex)
  val unwindResponseListIndex = RegEnable(rawResponseListIndex, startNewBurst)
  responseListIndex := Mux(busyUnwinding, unwindResponseListIndex, rawResponseListIndex)

  // Accept responses either when they can be passed through to the output channel, or if they're out-of-order and are
  // just going to be stashed in the 'data' SRAM. Never accept a response payload when we're busy unwinding, since that
  // could result in reading from and writing to the 'data' SRAM in the same cycle, and we want that SRAM to be
  // single-ported.
  ioResponse.ready := (ioDataOut.ready || !isResponseInOrder) && !busyUnwinding

  // Either pass an in-order response to the output channel, or data read from the unwind FSM.
  ioDataOut.valid          := Mux(busyUnwinding, unwindDataIsValid, ioResponse.valid && isResponseInOrder)
  ioDataOut.bits.listIndex := responseListIndex
  ioDataOut.bits.payload   := Mux(busyUnwinding, dataOutput, ioResponse.bits.data)

  // It's an error to get a response that isn't associated with a valid linked list.
  when(ioResponse.fire() || unwindDataIsValid) {
    assert(
      valid(responseListIndex),
      "No linked list exists at index %d, mapped from %d",
      responseListIndex,
      responseIndex
    )
  }

  when(busyUnwinding && dataMemReadEnable) {
    assert(isResponseInOrder, "Unwind FSM must read entries from SRAM in order")
  }
}

/** Specialized version of [[ReservableListBuffer]] for the case of numEntries == 1.
  *
  * Much of the complex logic in [[ReservableListBuffer]] can disappear in this case. For instance, we don't have to
  * reorder any responses, or store any linked lists.
  */
class PassthroughListBuffer[T <: Data](gen: T, params: ReservableListBufferParameters)
    extends BaseReservableListBuffer(gen, params) {
  require(params.numEntries == 1, s"PassthroughListBuffer is only valid when 'numEntries' (${params.numEntries}) is 1")

  val used = RegInit(0.U(params.numEntries.W))
  val map  = Mem(params.numEntries, UInt(params.listBits.W))

  val usedSet = WireDefault(0.U(params.numEntries.W))
  val usedClr = WireDefault(0.U(params.numEntries.W))

  used := (used & ~usedClr) | usedSet

  ioReserve.ready := used === 0.U

  // Store which list index was reserved, we need to return this value when we get a response.
  when(ioReserve.fire()) {
    usedSet := 1.U
    map.write(0.U, ioReserve.bits)
  }

  // There's only one valid linked list entry, which is at index 0.
  ioReservedIndex := 0.U

  val isLastResponseBeat = ioResponse.bits.count === ioResponse.bits.numBeats1

  // Mark the linked list as empty when we get the last beat in a response.
  // Note that 'ioResponse.fire() === ioDataOut.fire()'.
  when(ioResponse.fire() && isLastResponseBeat) {
    usedClr := 1.U
  }

  // Always pass the response data straight through, since we never need to reorder the response data.
  ioDataOut.bits.listIndex := map.read(0.U)
  ioDataOut.bits.payload   := ioResponse.bits.data
  ioDataOut.valid          := ioResponse.valid
  ioResponse.ready         := ioDataOut.ready
}
