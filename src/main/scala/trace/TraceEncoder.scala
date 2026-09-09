// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.trace

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._

case class TraceEncoderParams(
  encoderBaseAddr: BigInt,
  buildEncoder: Parameters => LazyTraceEncoder,
  useArbiterMonitor: Boolean,
  // a seq of functions that takes a parameter and returns a lazymodule and a target id
  buildSinks: Seq[Parameters => (LazyTraceSink, Int)] = Seq.empty[Parameters => (LazyTraceSink, Int)]
)

class LazyTraceEncoder(val coreParams: TraceCoreParams)(implicit p: Parameters) extends LazyModule {
  override lazy val module = new LazyTraceEncoderModule(this)
  override def shouldBeInlined = false
}

class LazyTraceEncoderModule(outer: LazyTraceEncoder) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val control = Input(new TraceEncoderControlInterface())
    val in = Input(new TraceCoreInterface(outer.coreParams))
    val stall = Output(Bool())
    val out = Decoupled(UInt(8.W))
  })

  require(outer.coreParams.nGroups == 1,
    "The minimal E-Trace encoder currently supports one retirement group")

  private val xlen = outer.coreParams.xlen
  private val addrBits = outer.coreParams.iaddrWidth
  private val addrLsb = outer.coreParams.iaddrLsb
  private val encodedAddrBits = addrBits - addrLsb
  // Normal encapsulation reserves five header bits for the payload byte count.
  // This minimal, single-source encoder omits srcID and timestamp, so its
  // payload is consequently limited to 31 bytes (248 bits).
  private val maxPayloadBytes = 31
  private val maxPacketBits = maxPayloadBytes * 8
  require(encodedAddrBits + xlen + 15 <= maxPacketBits,
    "E-Trace trap packet exceeds the Normal Encapsulation payload limit")
  private case class EncodedPacket(payload: UInt, bits: UInt)

  val g = io.in.group(0)
  val retired = g.iretire =/= 0.U
  val isBranch = (g.itype === TraceItype.ITBrTaken) || (g.itype === TraceItype.ITBrNTaken)
  val branchTaken = g.itype === TraceItype.ITBrTaken
  val isUninferable = (g.itype === TraceItype.ITUnCall) ||
    (g.itype === TraceItype.ITUnTail) ||
    (g.itype === TraceItype.ITCoSwap) ||
    (g.itype === TraceItype.ITUnJump) ||
    (g.itype === TraceItype.ITExcReturn)
  val isTrap = (g.itype === TraceItype.ITException) || (g.itype === TraceItype.ITInterrupt)
  // `priv` arrives as Cat(reg_debug, mstatus.prv), so bit 2 marks Debug Mode.
  // Debug ROM execution is not part of the traced program, and tracing it is
  // what previously made backpressure unsafe: the park loop kept the FIFO full
  // while an abstract command needed that same hart to make progress.
  val inDebugMode = io.in.priv(2)
  // Trap records are valid even when the trapping instruction did not retire.
  val traceEvent = (retired || isTrap) && !inDebugMode

  val packetBits = RegInit(0.U(maxPacketBits.W))
  val packetPayloadBytes = RegInit(0.U(6.W))
  val packetPos = RegInit(0.U(6.W))
  val packetBusy = RegInit(false.B)

  // RISC-V Unformatted Trace & Diagnostic Data Packet Encapsulation, Normal
  // Encapsulation Structure. The minimal single-hart profile omits srcID and
  // timestamp and uses flow 0. Payload bytes are sent least-significant first.
  private val encapFlow = 0.U(2.W)
  val payloadBytes = packetBits.asTypeOf(Vec(maxPacketBits / 8, UInt(8.W)))
  val totalBytes = packetPayloadBytes +& 1.U
  io.out.valid := packetBusy
  io.out.bits := Mux(packetPos === 0.U,
    Cat(0.U(1.W), encapFlow, packetPayloadBytes(4, 0)),
    payloadBytes((packetPos - 1.U)(4, 0)))
  // Lossless mode: when the packet transport/FIFO cannot accept the next
  // byte, hold the packet and stop retirement until space is available.
  io.stall := packetBusy && !io.out.ready

  def startPacket(packet: EncodedPacket): Unit = {
    assert(packet.bits =/= 0.U && packet.bits <= maxPacketBits.U,
      "E-Trace payload cannot be encoded by Normal Encapsulation")
    packetBits := packet.payload.pad(maxPacketBits)
    packetPayloadBytes := ((packet.bits + 7.U) >> 3).asUInt
    packetPos := 0.U
    packetBusy := true.B
  }

  when (packetBusy && io.out.fire) {
    when (packetPos + 1.U >= totalBytes) {
      packetBusy := false.B
    }.otherwise {
      packetPos := packetPos + 1.U
    }
  }

  val branchMap = RegInit(0.U(31.W))
  val branchCount = RegInit(0.U(5.W))
  val seenTrace = RegInit(false.B)
  val lastReportedPc = RegInit(0.U(addrBits.W))
  val lastPriv = RegInit(0.U(4.W))
  val resyncCount = RegInit(0.U(32.W))
  val pendingJump = RegInit(false.B)
  val pendingTrap = RegInit(false.B)
  val pendingTrapCause = RegInit(0.U(xlen.W))
  val pendingTrapTval = RegInit(0.U(xlen.W))
  val pendingTrapInterrupt = RegInit(false.B)
  val previousEnable = RegInit(false.B)
  val debugSeen = RegInit(false.B)
  val debugEnterPending = RegInit(false.B)
  val debugResumePending = RegInit(false.B)
  val debugStopSupportPending = RegInit(false.B)
  val stoppedInDebug = RegInit(false.B)
  val resumeSyncPending = RegInit(false.B)
  val lastRetiredPc = RegInit(0.U(addrBits.W))
  val lastRetiredPriv = RegInit(0.U(4.W))

  private val format1 = 1.U(2.W)
  private val format2 = 2.U(2.W)
  private val format3 = 3.U(2.W)
  private val subformatSync = 0.U(2.W)
  private val subformatTrap = 1.U(2.W)
  private val subformatSupport = 3.U(2.W)

  private def alignedAddress(pc: UInt): UInt =
    (pc >> addrLsb)(encodedAddrBits - 1, 0)

  private def differentialAddress(pc: UInt): UInt =
    ((pc - lastReportedPc) >> addrLsb)(encodedAddrBits - 1, 0)

  // With no notify/updiscon/implicit-return event, each flag copies address MSB.
  private def normalAddressFlags(address: UInt): UInt =
    Fill(3, address(encodedAddrBits - 1))

  /** Format 3, subformat 0: full PC synchronization point. */
  private def format3Sync(pc: UInt, priv: UInt, branch: Bool): EncodedPacket = {
    val payload = Cat(alignedAddress(pc), priv(2, 0), branch, subformatSync, format3)
    EncodedPacket(payload, (encodedAddrBits + 8).U)
  }

  /** Format 3, subformat 1: exception/interrupt and first handler PC. */
  private def format3Trap(
    handlerPc: UInt,
    priv: UInt,
    interrupt: Bool,
    cause: UInt,
    tval: UInt,
    branch: Bool
  ): EncodedPacket = {
    val noTvalPayload = Cat(alignedAddress(handlerPc), 1.U(1.W), interrupt,
      cause(5, 0), priv(2, 0), branch, subformatTrap, format3)
    val exceptionPayload = Cat(tval, noTvalPayload)
    EncodedPacket(
      Mux(interrupt, noTvalPayload, exceptionPayload),
      Mux(interrupt, (encodedAddrBits + 16).U, (encodedAddrBits + xlen + 16).U))
  }

  /** Format 3, subformat 3: fixed capabilities of this minimal encoder. */
  private def format3Support(ienable: Bool, qualStatus: UInt): EncodedPacket = {
    val dloss = false.B
    val denable = false.B
    val encoderMode = false.B // branch trace
    val payload = Cat(dloss, denable, qualStatus, encoderMode, ienable, subformatSupport, format3)
    EncodedPacket(payload, 10.U)
  }

  /** Format 2: differential PC with no branch-map payload. */
  private def format2Address(pc: UInt): EncodedPacket = {
    val address = differentialAddress(pc)
    val payload = Cat(normalAddressFlags(address), address, format2)
    EncodedPacket(payload, (encodedAddrBits + 5).U)
  }

  private def format1WithAddress(
    branches: UInt,
    map: UInt,
    mapBits: Int,
    pc: UInt
  ): EncodedPacket = {
    val address = differentialAddress(pc)
    val payload = Cat(normalAddressFlags(address), address, map, branches, format1)
    EncodedPacket(payload, (3 + encodedAddrBits + mapBits + 5 + 2).U)
  }

  /** Format 1: tapered branch map plus a differential PC. */
  private def format1BranchMap(branches: UInt, map: UInt, pc: UInt): EncodedPacket = {
    val payload = Wire(UInt(maxPacketBits.W))
    val bits = Wire(UInt(10.W))
    payload := 0.U
    bits := 0.U
    switch (branches) {
      is (1.U) {
        val packet = format1WithAddress(branches, map(0), 1, pc)
        payload := packet.payload
        bits := packet.bits
      }
      is (2.U, 3.U) {
        val packet = format1WithAddress(branches, map(2, 0), 3, pc)
        payload := packet.payload
        bits := packet.bits
      }
      is (4.U, 5.U, 6.U, 7.U) {
        val packet = format1WithAddress(branches, map(6, 0), 7, pc)
        payload := packet.payload
        bits := packet.bits
      }
      is (8.U, 9.U, 10.U, 11.U, 12.U, 13.U, 14.U, 15.U) {
        val packet = format1WithAddress(branches, map(14, 0), 15, pc)
        payload := packet.payload
        bits := packet.bits
      }
      is (16.U, 17.U, 18.U, 19.U, 20.U, 21.U, 22.U, 23.U,
        24.U, 25.U, 26.U, 27.U, 28.U, 29.U, 30.U, 31.U) {
        val packet = format1WithAddress(branches, map(30, 0), 31, pc)
        payload := packet.payload
        bits := packet.bits
      }
    }
    EncodedPacket(payload, bits)
  }

  /** Format 1: full 31-bit branch map, no address field. */
  private def format1FullBranchMap(map: UInt): EncodedPacket =
    EncodedPacket(Cat(map(30, 0), 0.U(5.W), format1), 38.U)

  when (!io.control.enable) {
    branchMap := 0.U
    branchCount := 0.U
    seenTrace := false.B
    pendingJump := false.B
    pendingTrap := false.B
    resyncCount := 0.U
    previousEnable := false.B
    packetBusy := false.B
    debugSeen := false.B
    debugEnterPending := false.B
    debugResumePending := false.B
    debugStopSupportPending := false.B
    stoppedInDebug := false.B
    resumeSyncPending := false.B
    lastRetiredPc := 0.U
    lastRetiredPriv := 0.U
  }.otherwise {
    // The privilege sideband changes to Debug=4 on entry and back to the
    // previous privilege on dret. Latch both edges so a packet already in
    // flight cannot hide the transition.
    when (inDebugMode && !debugSeen) {
      debugEnterPending := true.B
    }
    when (!inDebugMode && debugSeen) {
      debugResumePending := true.B
    }
    debugSeen := inDebugMode

    when (!previousEnable && !packetBusy) {
      startPacket(format3Support(true.B, 0.U(2.W)))
      previousEnable := true.B
    }.elsewhen (!packetBusy && debugEnterPending && !stoppedInDebug) {
      // A halt is represented by the last program address followed by an
      // ended_rep support packet. A full sync is conservative and remains
      // decodable even when a partial branch map was pending at the halt.
      when (seenTrace) {
        startPacket(format3Sync(lastRetiredPc, lastRetiredPriv, true.B))
        debugStopSupportPending := true.B
      }.otherwise {
        startPacket(format3Support(false.B, 1.U(2.W)))
      }
      debugEnterPending := false.B
      stoppedInDebug := true.B
      branchMap := 0.U
      branchCount := 0.U
      pendingJump := false.B
      pendingTrap := false.B
    }.elsewhen (!packetBusy && debugStopSupportPending) {
      // The address/sync emitted on debug entry has completed; terminate the
      // qualification interval. This branch is intentionally one-shot.
      startPacket(format3Support(false.B, 1.U(2.W)))
      debugStopSupportPending := false.B
    }.elsewhen (!packetBusy && debugResumePending && stoppedInDebug) {
      startPacket(format3Support(true.B, 0.U(2.W)))
      debugResumePending := false.B
      stoppedInDebug := false.B
      resumeSyncPending := true.B
    }.elsewhen (!packetBusy && traceEvent) {
      val privilegeChanged = seenTrace && (io.in.priv =/= lastPriv)

      when (resumeSyncPending) {
        startPacket(format3Sync(g.iaddr, io.in.priv, !branchTaken))
        resumeSyncPending := false.B
        seenTrace := true.B
        lastReportedPc := g.iaddr
        lastPriv := io.in.priv
        resyncCount := 0.U
      }.elsewhen (pendingTrap) {
        val trapPacket = format3Trap(g.iaddr, io.in.priv, pendingTrapInterrupt,
          pendingTrapCause, pendingTrapTval, !branchTaken)
        when (branchCount =/= 0.U) {
          startPacket(format1BranchMap(branchCount, branchMap, g.iaddr))
          pendingTrap := true.B
          branchMap := 0.U
          branchCount := 0.U
          lastReportedPc := g.iaddr
        }.otherwise {
          startPacket(trapPacket)
          pendingTrap := false.B
          lastReportedPc := g.iaddr
        }
      }.elsewhen (pendingJump) {
        when (branchCount =/= 0.U) {
          startPacket(format1BranchMap(branchCount, branchMap, g.iaddr))
          branchMap := 0.U
          branchCount := 0.U
          lastReportedPc := g.iaddr
        }.otherwise {
          startPacket(format2Address(g.iaddr))
          lastReportedPc := g.iaddr
        }
        pendingJump := false.B
      }.elsewhen (!seenTrace || privilegeChanged || resyncCount >= io.control.sync_max) {
        when (branchCount =/= 0.U) {
          startPacket(format1BranchMap(branchCount, branchMap, g.iaddr))
          branchMap := 0.U
          branchCount := 0.U
          lastReportedPc := g.iaddr
        }.otherwise {
          startPacket(format3Sync(g.iaddr, io.in.priv, !branchTaken))
          lastReportedPc := g.iaddr
          resyncCount := 0.U
        }
        seenTrace := true.B
        lastPriv := io.in.priv
        when (isUninferable) { pendingJump := true.B }
        when (isTrap) {
          pendingTrap := true.B
          pendingTrapCause := io.in.cause
          pendingTrapTval := io.in.tval
          pendingTrapInterrupt := g.itype === TraceItype.ITInterrupt
        }
      }.elsewhen (isTrap) {
        when (branchCount =/= 0.U) {
          startPacket(format1BranchMap(branchCount, branchMap, g.iaddr))
          branchMap := 0.U
          branchCount := 0.U
          lastReportedPc := g.iaddr
        }
        pendingTrap := true.B
        pendingTrapCause := io.in.cause
        pendingTrapTval := io.in.tval
        pendingTrapInterrupt := g.itype === TraceItype.ITInterrupt
      }.elsewhen (isUninferable) {
        pendingJump := true.B
      }.elsewhen (isBranch) {
        when (branchCount === 30.U) {
          val nextMap = branchMap | ((!branchTaken).asUInt << 30)
          startPacket(format1FullBranchMap(nextMap))
          branchMap := 0.U
          branchCount := 0.U
        }.otherwise {
          // E-Trace uses 0 for taken and 1 for not-taken.
          branchMap := branchMap | ((!branchTaken).asUInt << branchCount)
          branchCount := branchCount + 1.U
        }
      }

      seenTrace := true.B
      lastPriv := io.in.priv
      resyncCount := resyncCount + 1.U
      lastRetiredPc := g.iaddr
      lastRetiredPriv := io.in.priv
    }
  }
}
