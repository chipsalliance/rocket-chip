// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util.{PriorityEncoder, _}
import chisel3.util.experimental.decode.DecodeBundle
import chisel3.withClock
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property
import org.chipsalliance.rocketcore.decoder.InstructionDecoder

import scala.collection.mutable.ArrayBuffer

case class RocketCoreParams(
  bootFreqHz:              BigInt = 0,
  useVM:                   Boolean = true,
  useUser:                 Boolean = false,
  useSupervisor:           Boolean = false,
  useHypervisor:           Boolean = false,
  useDebug:                Boolean = true,
  useAtomics:              Boolean = true,
  useAtomicsOnlyForIO:     Boolean = false,
  useCompressed:           Boolean = true,
  useRVE:                  Boolean = false,
  useBitManip:             Boolean = false,
  useBitManipCrypto:       Boolean = false,
  useCryptoNIST:           Boolean = false,
  useCryptoSM:             Boolean = false,
  useConditionalZero:      Boolean = false,
  nLocalInterrupts:        Int = 0,
  useNMI:                  Boolean = false,
  nBreakpoints:            Int = 1,
  useBPWatch:              Boolean = false,
  mcontextWidth:           Int = 0,
  scontextWidth:           Int = 0,
  nPMPs:                   Int = 8,
  nPerfCounters:           Int = 0,
  haveBasicCounters:       Boolean = true,
  haveCFlush:              Boolean = false,
  misaWritable:            Boolean = true,
  nL2TLBEntries:           Int = 0,
  nL2TLBWays:              Int = 1,
  nPTECacheEntries:        Int = 8,
  mtvecInit:               Option[BigInt] = Some(BigInt(0)),
  mtvecWritable:           Boolean = true,
  fastLoadWord:            Boolean = true,
  fastLoadByte:            Boolean = false,
  branchPredictionModeCSR: Boolean = false,
  clockGate:               Boolean = false,
  mvendorid:               Int = 0, // 0 means non-commercial implementation
  mimpid:                  Int = 0x20181004, // release date in BCD
  mulDiv:                  Option[MulDivParams] = Some(MulDivParams()),
  fpu:                     Option[FPUParams] = Some(FPUParams()),
  vLen:                    Int = 0,
  vMemDataBits:            Int = 0,
  debugROB:                Boolean = false, // if enabled, uses a C++ debug ROB to generate trace-with-wdata
  haveCease:               Boolean = true, // non-standard CEASE instruction
  haveSimTimeout:          Boolean = true // add plusarg for simulation timeout
) extends CoreParams {
  val lgPauseCycles = 5
  val haveFSDirty = false
  val pmpGranularity: Int = if (useHypervisor) 4096 else 4
  val fetchWidth:     Int = if (useCompressed) 2 else 1
  //  fetchWidth doubled, but coreInstBytes halved, for RVC:
  val decodeWidth:   Int = fetchWidth / (if (useCompressed) 2 else 1)
  val retireWidth:   Int = 1
  val instBits:      Int = if (useCompressed) 16 else 32
  val lrscCycles:    Int = 80 // worst case is 14 mispredicted branches + slop
  val traceHasWdata: Boolean = false // ooo wb, so no wdata in trace
  override val customIsaExt = Option.when(haveCease)("xrocket") // CEASE instruction
  override def minFLen: Int = fpu.map(_.minFLen).getOrElse(32)
  override def customCSRs(implicit p: Parameters) = new RocketCustomCSRs

  // TODO: make me configurable
  override val useVector:   Boolean = false
  override val useVectorT1: Boolean = true
}

trait HasRocketCoreParameters extends HasCoreParameters {
  lazy val rocketParams: RocketCoreParams = tileParams.core.asInstanceOf[RocketCoreParams]

  val fastLoadWord = rocketParams.fastLoadWord
  val fastLoadByte = rocketParams.fastLoadByte

  val mulDivParams = rocketParams.mulDiv.getOrElse(MulDivParams()) // TODO ask andrew about this

  val usingABLU = usingBitManip || usingBitManipCrypto
  val aluFn = if (usingABLU) new ABLUFN else new ALUFN

  require(!fastLoadByte || fastLoadWord)
  require(!rocketParams.haveFSDirty, "rocket doesn't support setting fs dirty from outside, please disable haveFSDirty")
  require(!(usingABLU && usingConditionalZero), "Zicond is not yet implemented in ABLU")
}

class RocketCustomCSRs(implicit p: Parameters) extends CustomCSRs with HasRocketCoreParameters {
  override def bpmCSR = {
    rocketParams.branchPredictionModeCSR.option(CustomCSR(bpmCSRId, BigInt(1), Some(BigInt(0))))
  }

  private def haveDCache = tileParams.dcache.get.scratch.isEmpty

  override def chickenCSR = {
    val mask = BigInt(
      tileParams.dcache.get.clockGate.toInt << 0 |
        rocketParams.clockGate.toInt << 1 |
        rocketParams.clockGate.toInt << 2 |
        1 << 3 | // disableSpeculativeICacheRefill
        haveDCache.toInt << 9 | // suppressCorruptOnGrantData
        tileParams.icache.get.prefetch.toInt << 17
    )
    Some(CustomCSR(chickenCSRId, mask, Some(mask)))
  }

  def disableICachePrefetch = getOrElse(chickenCSR, _.value(17), true.B)

  def marchid = CustomCSR.constant(CSRs.marchid, BigInt(1))

  def mvendorid = CustomCSR.constant(CSRs.mvendorid, BigInt(rocketParams.mvendorid))

  // mimpid encodes a release version in the form of a BCD-encoded datestamp.
  def mimpid = CustomCSR.constant(CSRs.mimpid, BigInt(rocketParams.mimpid))

  override def decls = super.decls :+ marchid :+ mvendorid :+ mimpid
}

class Rocket(tile: RocketTile)(implicit val p: Parameters)
    extends Module
    with HasRocketCoreParameters {
  // Checker
  require(decodeWidth == 1 /* TODO */ && retireWidth == decodeWidth)
  require(!(coreParams.useRVE && coreParams.fpu.nonEmpty), "Can't select both RVE and floating-point")
  require(!(coreParams.useRVE && coreParams.useHypervisor), "Can't select both RVE and Hypervisor")

  // Parameters
  def nTotalRoCCCSRs: Int = tile.roccCSRs.flatten.size
  val pipelinedMul: Boolean = usingMulDiv && mulDivParams.mulUnroll == xLen
  val decoder: InstructionDecoder = new org.chipsalliance.rocketcore.decoder.InstructionDecoder(
    org.chipsalliance.rocketcore.decoder.InstructionDecoderParameter(
      // TODO: configurable
      (org.chipsalliance.rvdecoderdb.fromFile.instructions(os.pwd / "dependencies" / "riscv-opcodes") ++
        // TODO: select rocc instructions via configuration.
        org.chipsalliance.rocketcore.decoder.CustomInstructions.roccSet ++
        org.chipsalliance.rocketcore.decoder.CustomInstructions.rocketSet).filter { i =>
        i.instructionSets.map(_.name) match {
          // I
          case s if s.contains("rv_i") => true
          case s if s.contains("rv32_i") => xLen == 32
          case s if s.contains("rv64_i") => xLen == 64
          // M
          case s if s.contains("rv_m") => usingMulDiv
          case s if s.contains("rv64_m") => (xLen == 64) && usingMulDiv
          // A
          case s if s.contains("rv_a") => usingAtomics
          case s if s.contains("rv64_a") => (xLen == 64) && usingAtomics
          // ZICSR
          case s if s.contains("rv_zicsr") => true
          // ZIFENCEI
          case s if s.contains("rv_zifencei") => true
          // F
          case s if s.contains("rv_f") => !(fLen == 0)
          case s if s.contains("rv64_f") => (xLen == 64) && !(fLen == 0)
          // D
          case s if s.contains("rv_d") => fLen == 64
          case s if s.contains("rv64_d") => (xLen == 64) && (fLen == 64)
          // ZFH
          case s if s.contains("rv_zfh") => minFLen == 16
          case s if s.contains("rv64_zfh") => (xLen == 64) && (minFLen == 16)
          case s if s.contains("rv_d_zfh") => (fLen == 64) && (minFLen == 16)

          // Priv
          case s if s.contains("rv_system") => true
          // Supervisor
          case s if s.contains("rv_s") =>
            i.name match {
              // if support superviosr but don't support virtual memory, raise illinstr.
              case s if s.contains("sfence.vma") => usingVM
              case s if s.contains("sret") => usingSupervisor
            }
          case s if s.contains("rv_smrnmi") => usingNMI
          // Hypervisor
          case s if s.contains("rv_h") => usingHypervisor
          case s if s.contains("rv64_h") => (xLen == 64) && usingHypervisor
          // Debug
          case s if s.contains("rv_sdext") => usingDebug

          // TODO:
          //   Bit Manipulation, RocketChip doesn't provide a fine-grand Bit-Manipulation and Crypto support for now.
          //   We will support it in the future.
          case s if s.contains("rv_zba") => usingBitManip
          case s if s.contains("rv64_zba") => (xLen == 64) && usingBitManip
          case s if s.contains("rv_zbb") => usingBitManip
          case s if s.contains("rv32_zbb") => (xLen == 32) && usingBitManip
          case s if s.contains("rv64_zbb") => (xLen == 64) && usingBitManip
          case s if s.contains("rv_zbc") => usingBitManip
          case s if s.contains("rv_zbs") => usingBitManip
          // Cryptography Extensions
          case s if s.contains("rv_zbkb") => usingBitManipCrypto
          case s if s.contains("rv32_zbkb") => (xLen == 32) && usingBitManipCrypto
          case s if s.contains("rv64_zbkb") => (xLen == 64) && usingBitManipCrypto
          // These two are subset to zbkb
          case s if s.contains("rv_zbkc") => usingBitManipCrypto
          case s if s.contains("rv_zbkx") => usingBitManipCrypto
          // NIST
          case s if s.contains("rv32_zknd") => (xLen == 32) && usingCryptoNIST
          case s if s.contains("rv64_zknd") => (xLen == 64) && usingCryptoNIST
          case s if s.contains("rv32_zkne") => (xLen == 32) && usingCryptoNIST
          case s if s.contains("rv64_zkne") => (xLen == 64) && usingCryptoNIST
          case s if s.contains("rv_zknh") => usingCryptoNIST
          case s if s.contains("rv32_zknh") => (xLen == 32) && usingCryptoNIST
          case s if s.contains("rv64_zknh") => (xLen == 64) && usingCryptoNIST
          case s if s.contains("rv_zkn") => usingCryptoNIST
          case s if s.contains("rv32_zkn") => (xLen == 32) && usingCryptoNIST
          case s if s.contains("rv64_zkn") => (xLen == 64) && usingCryptoNIST
          // SM
          case s if s.contains("rv_zksed") => usingCryptoSM
          case s if s.contains("rv_zksh") => usingCryptoSM
          case s if s.contains("rv_zks") => usingCryptoSM
          case s if s.contains("rv32_zks") => (xLen == 32) && usingCryptoSM
          case s if s.contains("rv64_zks") => (xLen == 64) && usingCryptoSM
          case s if s.contains("rv_zk") => usingCryptoSM && usingCryptoNIST
          // NIST && SM
          case s if s.contains("rv32_zk") => (xLen == 32) && usingCryptoSM && usingCryptoNIST
          case s if s.contains("rv64_zk") => (xLen == 64) && usingCryptoSM && usingCryptoNIST
          // T1 Vector
          case s if s.contains("rv_v") => usingVectorT1
          // unratified but supported.
          case s if s.contains("rv_zicond") => usingConditionalZero
          // custom
          case s if s.contains("rv_rocket") =>
            i.name match {
              case "c.flush.d.l1" => coreParams.haveCFlush
              case "c.discard.d.l1" => coreParams.haveCFlush
              case "cease" => rocketParams.haveCease
            }
          case s if s.contains("rv_rocc") => usingRoCC
          case _ => false
        }
      }.filter {
        // special case for rv32 pseudo from rv64
        case i if i.pseudoFrom.isDefined && Seq("slli", "srli", "srai").contains(i.name) => true
        case i if i.pseudoFrom.isDefined => false
        case _ => true
      }.toSeq.distinct,
      pipelinedMul,
      tile.dcache.flushOnFenceI
    )
  )
  val lgNXRegs: Int = if (coreParams.useRVE) 4 else 5
  val regAddrMask: Int = (1 << lgNXRegs) - 1

  // TODO: extract to MultiIO and remove implicit parameters
  val io = IO(new Bundle {
    val hartid = Input(UInt(hartIdLen.W))
    val resetVector = Input(UInt(resetVectorLen.W))
    val interrupts = Input(new CoreInterrupts())
    val imem = new FrontendIO
    val dmem = new HellaCacheIO
    val ptw = Flipped(new DatapathPTWIO())
    val fpu = Flipped(new FPUCoreIO())
    val rocc = Flipped(new RoCCCoreIO(nTotalRoCCCSRs))
    val trace = Output(new TraceBundle)
    val bpwatch = Output(Vec(coreParams.nBreakpoints, new BPWatch(coreParams.retireWidth)))
    val cease = Output(Bool())
    val wfi = Output(Bool())
    val traceStall = Input(Bool())
  })
  // Interface for T1.
  val t1Request = Option.when(usingVectorT1)(IO(Valid(new VectorRequest(xLen))))
  val t1Response = Option.when(usingVectorT1)(IO(Flipped(Valid(new VectorResponse(xLen)))))
  // logic for T1
  val t1IssueQueueFull = Option.when(usingVectorT1)(IO(Output(Bool())))
  val t1IssueQueueEmpty = Option.when(usingVectorT1)(IO(Output(Bool())))

  // Signal outside from internal clock domain.

  val longLatencyStall = Reg(Bool())
  val idRegPause = Reg(Bool())
  val imemMightRequestReg = Reg(Bool())
  val clockEnable = WireDefault(true.B)
  val clockEnableReg = RegInit(true.B)
  val gatedClock = Option.when(rocketParams.clockGate)(ClockGate(clock, clockEnable, "rocket_clock_gate")).getOrElse(clock)
  // leaving gated-clock domain
  val gatedDomain = withClock(gatedClock)(new Gated)

  class Gated {
    // performance counters
    def pipelineIDToWB[T <: Data](x: T): T = RegEnable(RegEnable(RegEnable(x, !ctrlKilled), exPcValid), memPcValid)
    // TODO: remove it and probe signal to verification modules
    // format: off
    val perfEvents: EventSets = new EventSets(
      Seq(
        new EventSet(
          (mask, hits) => Mux(wbException, mask(0), wbValid && pipelineIDToWB((mask & hits).orR)),
          Seq(
            ("exception", () => false.B),
            // TODO: why no FPU here?
            ("load", () => idDecodeOutput(decoder.mem) && idDecodeOutput(decoder.memCommand) === M_XRD && !Option.when(usingFPU)(idDecodeOutput(decoder.fp)).getOrElse(false.B)),
            ("store", () => idDecodeOutput(decoder.mem) && idDecodeOutput(decoder.memCommand) === M_XWR && !Option.when(usingFPU)(idDecodeOutput(decoder.fp)).getOrElse(false.B)),
            ("system", () => idDecodeOutput(decoder.csr) =/= CSR.N),
            ("arith", () => idDecodeOutput(decoder.wxd) && !( idDecodeOutput(decoder.isJal) || idDecodeOutput(decoder.isJalr) || idDecodeOutput(decoder.mem) || Option.when(usingFPU)(idDecodeOutput(decoder.fp)).getOrElse(false.B) || Option.when(usingMulDiv && pipelinedMul)(idDecodeOutput(decoder.mul)).getOrElse(false.B) || Option.when(usingMulDiv)(idDecodeOutput(decoder.div)).getOrElse(false.B) || idDecodeOutput(decoder.csr) =/= CSR.N )),
            ("branch", () => idDecodeOutput(decoder.isBranch)),
            ("jal", () => idDecodeOutput(decoder.isJal)),
            ("jalr", () => idDecodeOutput(decoder.isJalr))
          ) ++
            Option.when(usingAtomics)(Seq(
              ("amo", () => idDecodeOutput(decoder.mem) && (isAMO(idDecodeOutput(decoder.memCommand)) || idDecodeOutput(decoder.memCommand).isOneOf(M_XLR, M_XSC)))
            )).getOrElse(Seq()) ++
            Option.when(usingMulDiv)(Seq(
              ("mul", () => if (pipelinedMul) idDecodeOutput(decoder.mul) else idDecodeOutput(decoder.div) && (idDecodeOutput(decoder.aluFn) & aluFn.FN_DIV) =/= aluFn.FN_DIV),
              ("div", () => if (pipelinedMul) idDecodeOutput(decoder.div) else idDecodeOutput(decoder.div) && (idDecodeOutput(decoder.aluFn) & aluFn.FN_DIV) === aluFn.FN_DIV)
            )).getOrElse(Seq()) ++
            Option.when(usingFPU)(Seq(
              ("fp load", () => idDecodeOutput(decoder.fp) && io.fpu.dec.ldst && io.fpu.dec.wen),
              ("fp store", () => idDecodeOutput(decoder.fp) && io.fpu.dec.ldst && !io.fpu.dec.wen),
              ("fp add", () => idDecodeOutput(decoder.fp) && io.fpu.dec.fma && io.fpu.dec.swap23),
              ("fp mul", () => idDecodeOutput(decoder.fp) && io.fpu.dec.fma && !io.fpu.dec.swap23 && !io.fpu.dec.ren3),
              ("fp mul-add", () => idDecodeOutput(decoder.fp) && io.fpu.dec.fma && io.fpu.dec.ren3),
              ("fp div/sqrt", () => idDecodeOutput(decoder.fp) && (io.fpu.dec.div || io.fpu.dec.sqrt)),
              ("fp other", () => idDecodeOutput(decoder.fp) && !(io.fpu.dec.ldst || io.fpu.dec.fma || io.fpu.dec.div || io.fpu.dec.sqrt ))
            )).getOrElse(Seq())
          ),
        new EventSet(
          (mask, hits) => (mask & hits).orR,
          Seq(
            ("load-use interlock", () => idExHazard && exRegDecodeOutput(decoder.mem) || idMemHazard && memRegDecodeOutput(decoder.mem) || idWbHazard && wbRegDecodeOutput(decoder.mem)           ),
            ("long-latency interlock", () => idScoreboardHazard),
            ("csr interlock", () => idExHazard && exRegDecodeOutput(decoder.csr) =/= CSR.N || idMemHazard && memRegDecodeOutput(decoder.csr) =/= CSR.N || idWbHazard && wbRegDecodeOutput(decoder.csr) =/= CSR.N),
            ("I$ blocked", () => icacheBlocked),
            ("D$ blocked", () => idDecodeOutput(decoder.mem) && dcacheBlocked),
            ("branch misprediction", () => takePcMem && memDirectionMisprediction),
            ("control-flow target misprediction", () => takePcMem && memMisprediction && memCfi && !memDirectionMisprediction && !icacheBlocked),
            ("flush", () => wbRegFlushPipe),
            ("replay", () => replayWb)
          ) ++
            Option.when(usingMulDiv)(Seq(
              ("mul/div interlock", () => idExHazard && (Option.when(pipelinedMul)(exRegDecodeOutput(decoder.mul)).getOrElse(false.B) || exRegDecodeOutput(decoder.div)) || idMemHazard && (Option.when(pipelinedMul)(memRegDecodeOutput(decoder.mul)).getOrElse(false.B) || memRegDecodeOutput(decoder.div)) || idWbHazard && wbRegDecodeOutput(decoder.div))
            )).getOrElse(Seq()) ++
            Option.when(usingFPU)(Seq(
              ("fp interlock", () => idExHazard && exRegDecodeOutput(decoder.fp) || idMemHazard && memRegDecodeOutput(decoder.fp) || idWbHazard && wbRegDecodeOutput(decoder.fp) || idDecodeOutput(decoder.fp) && idStallFpu)
            )).getOrElse(Seq())
        ),
        new EventSet(
          (mask, hits) => (mask & hits).orR,
          Seq(
            ("I$ miss", () => io.imem.perf.acquire),
            ("D$ miss", () => io.dmem.perf.acquire),
            ("D$ release", () => io.dmem.perf.release),
            ("ITLB miss", () => io.imem.perf.tlbMiss),
            ("DTLB miss", () => io.dmem.perf.tlbMiss),
            ("L2 TLB miss", () => io.ptw.perf.l2miss)
          )
        )
      )
    )
    // format: on

    // Start RTL Here
    // instantiate modules
    // TODO: remove implicit parameter for them.

    val csr: CSRFile = Module(new CSRFile(perfEvents, coreParams.customCSRs.decls, tile.roccCSRs.flatten))

    // TODO: move to Parameter Level or LazyModule level.
    /** Decoder instantiated, input from IF, output to ID. */
    val decoderModule = Module(new RawModule {
      override def desiredName: String = "RocketDecoder"
      val instruction = IO(Input(UInt(32.W)))
      val output = IO(Output(decoder.table.bundle))
      output := decoder.table.decode(instruction)
    })
    val instructionBuffer: IBuf = Module(new IBuf)
    val breakpointUnit: BreakpointUnit = Module(new BreakpointUnit(nBreakpoints))
    val arithmeticLogicUnit: ALU = Module(new ALU())
    val muldiv = Module(new MulDiv(if (pipelinedMul) mulDivParams.copy(mulUnroll = 0) else mulDivParams, width = xLen, aluFn = aluFn)).suggestName(if (pipelinedMul)"div" else "muldiv")
    val mul = pipelinedMul.option(Module(new PipelinedMultiplier(xLen, 2, aluFn = aluFn)))
    // RF is not a Module.
    val rf = new RegFile(regAddrMask, xLen)

    // wire definations.

    val idDecodeOutput: DecodeBundle = Wire(decoder.table.bundle)

    val exRegExceptionInterrupt: Bool = Reg(Bool())
    val exRegException: Bool = Reg(Bool())
    val exRegValid: Bool = Reg(Bool())
    val exRegRVC: Bool = Reg(Bool())
    val exRegBTBResponse: BTBResp = Reg(new BTBResp)
    val exRegFlushPipe: Bool = Reg(Bool())
    val exRegLoadUse: Bool = Reg(Bool())
    val exRegCause: UInt = Reg(UInt())
    val exRegReplay: Bool = Reg(Bool())
    val exRegPC: UInt = Reg(UInt())
    // TODO: add real width here.
    val exRegMemSize: UInt = Reg(UInt())
    // Option.when(usingHypervisor)
    val exRegHLS: Bool = Reg(Bool())
    val exRegInstruction: UInt = Reg(Bits())
    val exRegRawInstruction: UInt = Reg(UInt())
    // TODO: what's this?
    val exRegWphit: Vec[Bool] = Reg(Vec(nBreakpoints, Bool()))
    val exRegDecodeOutput:  DecodeBundle = Reg(decoder.table.bundle)

    val memRegExceptionInterrupt = Reg(Bool())
    val memRegValid = Reg(Bool())
    val memRegRVC = Reg(Bool())
    val memRegBTBResponse = Reg(new BTBResp)
    val memRegException = Reg(Bool())
    val memRegReplay = Reg(Bool())
    val memRegFlushPipe = Reg(Bool())
    val memRegCause = Reg(UInt())
    val memRegSlowBypass = Reg(Bool())
    val memRegLoad = Reg(Bool())
    val memRegStore = Reg(Bool())
    val memRegSfence = Reg(Bool())
    val memRegPc = Reg(UInt())
    val memRegInstruction = Reg(Bits())
    val memRegMemSize = Reg(UInt())
    val memRegDecodeOutput: DecodeBundle = Reg(decoder.table.bundle)
    /** virtualization mode? */
    val memRegHlsOrDv = Reg(Bool())
    val memRegRawInstruction = Reg(UInt())
    val memRegWdata = Reg(Bits())
    val memRegRS2 = Reg(Bits())
    val memBranchTaken = Reg(Bool())
    val takePcMem = Wire(Bool())
    val memRegWphit = Reg(Vec(nBreakpoints, Bool()))

    val wbRegValid = Reg(Bool())
    val wbRegException = Reg(Bool())
    val wbRegReplay = Reg(Bool())
    val wbRegFlushPipe = Reg(Bool())
    val wbRegCause = Reg(UInt())
    val wbRegSfence = Reg(Bool())
    val wbRegPc = Reg(UInt())
    val wbRegDecodeOutput:  DecodeBundle = Reg(decoder.table.bundle)
    val wbRegMemSize = Reg(UInt())
    val wbRegHlsOrDv = Reg(Bool())
    val wbRegHfenceV = Reg(Bool())
    val wbRegHfenceG = Reg(Bool())
    val wbRegInstruction = Reg(Bits())
    val wbRegRawInstruction = Reg(UInt())
    val wbRegWdata = Reg(Bits())
    val wbRegRS2 = Reg(Bits())
    val wbRegWphit = Reg(Vec(nBreakpoints, Bool()))
    val takePcWb = Wire(Bool())

    val takePcMemWb = takePcWb || takePcMem
    val takePc = takePcMemWb

    // From IBUF to ID
    instructionBuffer.io.imem <> io.imem.resp
    val instructionBufferOut: DecoupledIO[Instruction] = instructionBuffer.io.inst.head
    // TODO: does these really has its meaning? I don't think so:(
    val idExpandedInstruction: ExpandedInstruction = instructionBufferOut.bits.inst
    val idRawInstruction: UInt = instructionBufferOut.bits.raw
    val idInstruction: UInt = idExpandedInstruction.bits
    idDecodeOutput := decoderModule.output
    instructionBuffer.io.kill := takePc
    decoderModule.instruction := idInstruction

    def decodeReg(x: UInt): (Bool, UInt) = (x.extract(x.getWidth - 1, lgNXRegs).asBool, x(lgNXRegs - 1, 0))
    val (idRaddr3Illegal: Bool, idRaddr3: UInt) = decodeReg(idExpandedInstruction.rs3)
    val (idRaddr2Illegal: Bool, idRaddr2: UInt) = decodeReg(idExpandedInstruction.rs2)
    val (idRaddr1Illegal: Bool, idRaddr1: UInt) = decodeReg(idExpandedInstruction.rs1)
    val (idWaddrIllegal: Bool, idWaddr: UInt) = decodeReg(idExpandedInstruction.rd)

    val idLoadUse: Bool = Wire(Bool())
    val idRegFence: Bool = RegInit(false.B)
    val idRen: Seq[Bool] = IndexedSeq(idDecodeOutput(decoder.rxs1), idDecodeOutput(decoder.rxs2))
    val idRaddr: Seq[UInt] = IndexedSeq(idRaddr1, idRaddr2)
    val idRs: Seq[UInt] = idRaddr.map(rf.read)
    val ctrlKilled: Bool = Wire(Bool())

    // TODO: additional decode out?
    val idCsrEn: Bool = idDecodeOutput(decoder.csr).isOneOf(CSR.S, CSR.C, CSR.W)
    val idSystemInstruction: Bool = idDecodeOutput(decoder.csr) === CSR.I
    val idCsrRen: Bool = idDecodeOutput(decoder.csr).isOneOf(CSR.S, CSR.C) && idExpandedInstruction.rs1 === 0.U
    val idCsr = Mux(idSystemInstruction && idDecodeOutput(decoder.mem), CSR.N, Mux(idCsrRen, CSR.R, idDecodeOutput(decoder.csr)))
    val idCsrFlush = idSystemInstruction || (idCsrEn && !idCsrRen && csr.io.decode(0).writeFlush)
    val idRfIllegal: Bool =
      idRaddr2Illegal && idDecodeOutput(decoder.rxs2) ||
        idRaddr1Illegal && idDecodeOutput(decoder.rxs1) ||
        idWaddrIllegal && idDecodeOutput(decoder.wxd)
    val idCsrIllegalRW: Bool =
      idCsrEn && (csr.io.decode(0).readIllegal || !idCsrRen && csr.io.decode(0).writeIllegal)
    val idSystemIllegal: Bool = !instructionBufferOut.bits.rvc && (idSystemInstruction && csr.io.decode(0).systemIllegal)

    val idAtomicIllegal: Option[Bool] =
      Option.when(usingAtomics)(idDecodeOutput(decoder.amo) && !csr.io.status.isa('a' - 'a'))
    val idMulDivIllegal: Option[Bool] =
      Option.when(usingMulDiv)(
        Option.when(pipelinedMul)(idDecodeOutput(decoder.mul)).getOrElse(false.B) ||
          idDecodeOutput(decoder.div) && !csr.io.status.isa('m' - 'a')
      )
    val idCompressIllegal: Option[Bool] =
      Option.when(usingCompressed)(instructionBufferOut.bits.rvc && !csr.io.status.isa('c' - 'a'))
    val idFpIllegal: Option[Bool] =
      Option.when(usingFPU)(idDecodeOutput(decoder.fp) && (csr.io.decode(0).fpIllegal || io.fpu.illegal_rm))
    val idDpIllegal: Option[Bool] = Option.when(usingFPU)(idDecodeOutput(decoder.dp) && !csr.io.status.isa('d' - 'a'))
    val idRoCCIllegal: Option[Bool] =
      Option.when(usingRoCC)(idDecodeOutput(decoder.rocc) && csr.io.decode(0).roccIllegal)
    val idRnumIllegal: Option[Bool] = Option.when(usingCryptoNIST)(
      idDecodeOutput(decoder.zkn) && aluFn.isKs1(idDecodeOutput(decoder.abluFn)) && idInstruction(23, 20) > 0xa.U(4.W)
    )

    val idIllegalInstruction: Bool =
      !idDecodeOutput(decoder.isLegal) ||
        idRfIllegal ||
        idCsrIllegalRW ||
        idSystemIllegal ||
        idMulDivIllegal.getOrElse(false.B) ||
        idAtomicIllegal.getOrElse(false.B) ||
        idFpIllegal.getOrElse(false.B) ||
        idDpIllegal.getOrElse(false.B) ||
        idCompressIllegal.getOrElse(false.B) ||
        idRoCCIllegal.getOrElse(false.B) ||
        idRnumIllegal.getOrElse(false.B)
    val idVirtualInstruction: Bool =
      idDecodeOutput(decoder.isLegal) &&
        (
          (idCsrEn &&
            !(!idCsrRen && csr.io.decode(0).writeIllegal) &&
            csr.io.decode(0).virtualAccessIllegal
            ) || (
            !instructionBufferOut.bits.rvc &&
              idSystemInstruction &&
              csr.io.decode(0).virtualSystemIllegal
            )
          )

    // stall decode for fences (now, for AMO.rl; later, for AMO.aq and FENCE)
    val idAmoAquire: Bool = idInstruction(26)
    val idAmoRelease: Bool = idInstruction(25)
    // TODO: what's this?
    val idFenceSucc: UInt = idInstruction(23, 20)
    val idFenceNext: Bool = idDecodeOutput(decoder.fence) || idDecodeOutput(decoder.amo) && idAmoAquire
    val idMemoryBusy: Bool = !io.dmem.ordered || io.dmem.req.valid
    when(!idMemoryBusy) { idRegFence := false.B }
    val idRoccBusy: Option[Bool] = Option.when(usingRoCC)(
      io.rocc.busy ||
        exRegValid && exRegDecodeOutput(decoder.rocc) ||
        memRegValid && memRegDecodeOutput(decoder.rocc) ||
        wbRegValid && wbRegDecodeOutput(decoder.rocc)
    )
    val idDoFence =
      idRoccBusy.getOrElse(false.B) && idDecodeOutput(decoder.fence) ||
        idMemoryBusy && (idDecodeOutput(decoder.amo) && idAmoRelease ||
          idDecodeOutput(decoder.fenceI) ||
          idRegFence && (idDecodeOutput(decoder.mem) ||
            Option.when(usingRoCC)(idDecodeOutput(decoder.rocc)).getOrElse(false.B)))

    breakpointUnit.io.status := csr.io.status
    breakpointUnit.io.bp := csr.io.bp
    breakpointUnit.io.pc := instructionBuffer.io.pc
    breakpointUnit.io.ea := memRegWdata
    breakpointUnit.io.mcontext := csr.io.mcontext
    breakpointUnit.io.scontext := csr.io.scontext

    val idException0 = instructionBufferOut.bits.xcpt0
    val idException1 = instructionBufferOut.bits.xcpt1
    val (idException, idCause) = checkExceptions(
      List(
        (csr.io.interrupt, csr.io.interrupt_cause),
        (breakpointUnit.io.debug_if, CSR.debugTriggerCause.U),
        (breakpointUnit.io.xcpt_if, Causes.breakpoint.U),
        (idException0.pf.inst, Causes.fetch_page_fault.U),
        (idException0.gf.inst, Causes.fetch_guest_page_fault.U),
        (idException0.ae.inst, Causes.fetch_access.U),
        (idException1.pf.inst, Causes.fetch_page_fault.U),
        (idException1.gf.inst, Causes.fetch_guest_page_fault.U),
        (idException1.ae.inst, Causes.fetch_access.U),
        (idVirtualInstruction, Causes.virtual_instruction.U),
        (idIllegalInstruction, Causes.illegal_instruction.U)
      )
    )

    val idCoverCauses: Seq[(Int, String)] = List(
      (CSR.debugTriggerCause, "DEBUG_TRIGGER"),
      (Causes.breakpoint, "BREAKPOINT"),
      (Causes.fetch_access, "FETCH_ACCESS"),
      (Causes.illegal_instruction, "ILLEGAL_INSTRUCTION")
    ) ++ Option.when(usingVM)((Causes.fetch_page_fault, "FETCH_PAGE_FAULT"))
    // TODO: move it to verification module.
    coverExceptions(idException, idCause, "DECODE", idCoverCauses)

    // Bypass signals
    val dcacheBypassData: UInt =
      if (fastLoadByte) io.dmem.resp.bits.data(xLen - 1, 0)
      else if (fastLoadWord) io.dmem.resp.bits.data_word_bypass(xLen - 1, 0)
      else wbRegWdata
    // detect bypass opportunities
    val exWaddr: UInt = exRegInstruction(11, 7) & regAddrMask.U
    val memWaddr: UInt = memRegInstruction(11, 7) & regAddrMask.U
    val wbWaddr: UInt = wbRegInstruction(11, 7) & regAddrMask.U
    val bypassSources: Seq[(Bool, UInt, UInt)] = IndexedSeq(
      (true.B, 0.U, 0.U), // treat reading x0 as a bypass
      (exRegValid && exRegDecodeOutput(decoder.wxd), exWaddr, memRegWdata),
      (memRegValid && memRegDecodeOutput(decoder.wxd) && !memRegDecodeOutput(decoder.mem), memWaddr, wbRegWdata),
      (memRegValid && memRegDecodeOutput(decoder.wxd), memWaddr, dcacheBypassData)
    )
    val idBypassSources: Seq[Seq[Bool]] = idRaddr.map(raddr => bypassSources.map(s => s._1 && s._2 === raddr))

    // execute stage
    val bypassMux: Seq[UInt] = bypassSources.map(_._3)
    val exRegRsBypass: Vec[Bool] = Reg(Vec(idRaddr.size, Bool()))
    val exRegRsLSB: Vec[UInt] = Reg(Vec(idRaddr.size, UInt(log2Ceil(bypassSources.size).W)))
    val exRegRsMSB: Vec[UInt] = Reg(Vec(idRaddr.size, UInt()))
    val exRs: Seq[UInt] = Seq.tabulate(idRaddr.size)(i => Mux(exRegRsBypass(i), bypassMux(exRegRsLSB(i)), Cat(exRegRsMSB(i), exRegRsLSB(i))))
    val exImm: SInt = ImmGen(exRegDecodeOutput(decoder.selImm), exRegInstruction)
    val exOp1: SInt = MuxLookup(exRegDecodeOutput(decoder.selAlu1), 0.S)(Seq(A1_RS1 -> exRs(0).asSInt, A1_PC -> exRegPC.asSInt))
    val exOp2: SInt = MuxLookup(exRegDecodeOutput(decoder.selAlu2), 0.S)(Seq(A2_RS2 -> exRs(1).asSInt, A2_IMM -> exImm, A2_SIZE -> Mux(exRegRVC, 2.S, 4.S)))

    arithmeticLogicUnit.io.dw := exRegDecodeOutput(decoder.aluDoubleWords)
    arithmeticLogicUnit.io.fn := exRegDecodeOutput(decoder.aluFn)
    arithmeticLogicUnit.io.in2 := exOp2.asUInt
    arithmeticLogicUnit.io.in1 := exOp1.asUInt

    // multiplier and divider
    // TODO: waive them if !usingMulDiv
    muldiv.io.req.valid := exRegValid && Option.when(usingMulDiv)(exRegDecodeOutput(decoder.div)).getOrElse(false.B)
    muldiv.io.req.bits.dw := exRegDecodeOutput(decoder.aluDoubleWords)
    muldiv.io.req.bits.fn := exRegDecodeOutput(decoder.aluFn)
    muldiv.io.req.bits.in1 := exRs(0)
    muldiv.io.req.bits.in2 := exRs(1)
    muldiv.io.req.bits.tag := exWaddr
    mul.foreach{ m =>
      m.io.req.valid := exRegValid && exRegDecodeOutput(decoder.mul)
      m.io.req.bits := muldiv.io.req.bits
    }

    exRegValid := !ctrlKilled
    exRegReplay := !takePc && instructionBufferOut.valid && instructionBufferOut.bits.replay
    exRegException := !ctrlKilled && idException
    exRegExceptionInterrupt := !takePc && instructionBufferOut.valid && csr.io.interrupt

    // ID goes to EX
    when(!ctrlKilled) {
      exRegDecodeOutput := idDecodeOutput
      exRegRVC := instructionBufferOut.bits.rvc
      exRegDecodeOutput(decoder.csr) := idCsr
      when(idDecodeOutput(decoder.fence) && idFenceSucc === 0.U) { idRegPause := true.B }
      when(idFenceNext) { idRegFence := true.B }
      when(idException) { // pass PC down ALU writeback pipeline for badaddr
        exRegDecodeOutput(decoder.aluFn) := aluFn.FN_ADD
        exRegDecodeOutput(decoder.aluDoubleWords) := DW_XPR
        exRegDecodeOutput(decoder.selAlu1) := A1_RS1 // badaddr := instruction
        exRegDecodeOutput(decoder.selAlu2) := A2_ZERO
        when(idException1.asUInt.orR) { // badaddr := PC+2
          exRegDecodeOutput(decoder.selAlu1) := A1_PC
          exRegDecodeOutput(decoder.selAlu2) := A2_SIZE
          exRegRVC := true.B
        }
        when(breakpointUnit.io.xcpt_if || idException0.asUInt.orR) { // badaddr := PC
          exRegDecodeOutput(decoder.selAlu1) := A1_PC
          exRegDecodeOutput(decoder.selAlu2) := A2_ZERO
        }
      }
      exRegFlushPipe := idDecodeOutput(decoder.fenceI) || idCsrFlush
      exRegLoadUse := idLoadUse
      exRegHLS :=
        usingHypervisor.B &&
          idSystemInstruction &&
          idDecodeOutput(decoder.memCommand).isOneOf(M_XRD, M_XWR, M_HLVX)
      exRegMemSize := Mux(usingHypervisor.B && idSystemInstruction, idInstruction(27, 26), idInstruction(13, 12))
      when(idDecodeOutput(decoder.memCommand).isOneOf(M_SFENCE, M_HFENCEV, M_HFENCEG, M_FLUSH_ALL)) {
        exRegMemSize := Cat(idRaddr2 =/= 0.U, idRaddr1 =/= 0.U)
      }
      when(idDecodeOutput(decoder.memCommand) === M_SFENCE && csr.io.status.v) {
        exRegDecodeOutput(decoder.memCommand) := M_HFENCEV
      }

      if (tile.dcache.flushOnFenceI) {
        when(idDecodeOutput(decoder.fenceI)) {
          exRegMemSize := 0.U
        }
      }

      Seq.tabulate(idRaddr.size){ i =>
        val doBypass = idBypassSources(i).reduce(_ || _)
        val bypassSource = PriorityEncoder(idBypassSources(i))
        exRegRsBypass(i) := doBypass
        exRegRsLSB(i) := bypassSource
        when(idRen(i) && !doBypass) {
          exRegRsLSB(i) := idRs(i)(log2Ceil(bypassSources.size) - 1, 0)
          exRegRsMSB(i) := idRs(i) >> log2Ceil(bypassSources.size)
        }
      }
      when(idIllegalInstruction || idVirtualInstruction) {
        val inst = Mux(instructionBufferOut.bits.rvc, idRawInstruction(15, 0), idRawInstruction)
        exRegRsBypass(0) := false.B
        exRegRsLSB(0) := inst(log2Ceil(bypassSources.size) - 1, 0)
        exRegRsMSB(0) := inst >> log2Ceil(bypassSources.size)
      }
    }
    // ID goes to EX but with interrupt or replay
    when(!ctrlKilled || csr.io.interrupt || instructionBufferOut.bits.replay) {
      exRegCause := idCause
      exRegInstruction := idInstruction
      exRegRawInstruction := idRawInstruction
      exRegPC := instructionBuffer.io.pc
      exRegBTBResponse := instructionBuffer.io.btb_resp
      exRegWphit := breakpointUnit.io.bpwatch.map { bpw => bpw.ivalid(0) }
    }
    // replay inst in ex stage?
    val exPcValid: Bool = exRegValid || exRegReplay || exRegExceptionInterrupt
    val wbDcacheMiss: Bool = wbRegDecodeOutput(decoder.mem) && !io.dmem.resp.valid
    val replayExStructural: Bool = exRegDecodeOutput(decoder.mem) && !io.dmem.req.ready || Option.when(usingMulDiv)(exRegDecodeOutput(decoder.div)).getOrElse(false.B) && !muldiv.io.req.ready
    val replayExLoadUse: Bool = wbDcacheMiss && exRegLoadUse
    val replayEx: Bool = exRegReplay || (exRegValid && (replayExStructural || replayExLoadUse))
    val ctrlKillx: Bool = takePcMemWb || replayEx || !exRegValid
    // detect 2-cycle load-use delay for LB/LH/SC
    val exSlowBypass: Bool = exRegDecodeOutput(decoder.memCommand) === M_XSC || exRegMemSize < 2.U
    val exSfence: Bool =
      usingVM.B &&
        exRegDecodeOutput(decoder.mem) &&
        (exRegDecodeOutput(decoder.memCommand) === M_SFENCE ||
          exRegDecodeOutput(decoder.memCommand) === M_HFENCEV ||
          exRegDecodeOutput(decoder.memCommand) === M_HFENCEG
          )

    val (exException: Bool, exCause: UInt) = checkExceptions(List((exRegExceptionInterrupt || exRegException, exRegCause)))
    val exCoverCauses: Seq[(Int, String)] = idCoverCauses
    coverExceptions(exException, exCause, "EXECUTE", exCoverCauses)

    // memory stage
    val memPcValid: Bool = memRegValid || memRegReplay || memRegExceptionInterrupt
    val memBranchTarget: SInt = memRegPc.asSInt +
      Mux(
        memRegDecodeOutput(decoder.isBranch) && memBranchTaken,
        ImmGen(IMM_SB, memRegInstruction),
        Mux(
          memRegDecodeOutput(decoder.isJal),
          ImmGen(IMM_UJ, memRegInstruction),
          Mux(memRegRVC, 2.S, 4.S))
      )
    val memNextPC: UInt = (Mux(
      memRegDecodeOutput(decoder.isJalr) || memRegSfence,
      encodeVirtualAddress(memRegWdata, memRegWdata).asSInt,
      memBranchTarget
    ) & (-2).S).asUInt
    val memWrongNpc: Bool =
      Mux(
        exPcValid,
        memNextPC =/= exRegPC,
        Mux(
          instructionBufferOut.valid || instructionBuffer.io.imem.valid,
          memNextPC =/= instructionBuffer.io.pc,
          true.B
        )
      )
    val memNpcMisaligned: Bool = !csr.io.status.isa('c' - 'a') && memNextPC(1) && !memRegSfence
    val memIntWdata: UInt = Mux(
      !memRegException && (memRegDecodeOutput(decoder.isJalr) ^ memNpcMisaligned),
      memBranchTarget,
      memRegWdata.asSInt
    ).asUInt
    val memCfi: Bool = memRegDecodeOutput(decoder.isBranch) || memRegDecodeOutput(decoder.isJalr) || memRegDecodeOutput(decoder.isJal)
    val memCfiTaken: Bool =
      (memRegDecodeOutput(decoder.isBranch) && memBranchTaken) || memRegDecodeOutput(decoder.isJalr) || memRegDecodeOutput(decoder.isJal)
    val memDirectionMisprediction: Bool =
      memRegDecodeOutput(decoder.isBranch) && memBranchTaken =/= (usingBTB.B && memRegBTBResponse.taken)
    val memMisprediction: Bool = if (usingBTB) memWrongNpc else memCfiTaken
    takePcMem := memRegValid && !memRegException && (memMisprediction || memRegSfence)

    memRegValid := !ctrlKillx
    memRegReplay := !takePcMemWb && replayEx
    memRegException := !ctrlKillx && exException
    memRegExceptionInterrupt := !takePcMemWb && exRegExceptionInterrupt

    // on pipeline flushes, cause mem_npc to hold the sequential npc, which
    // will drive the W-stage npc mux
    when(memRegValid && memRegFlushPipe) {
      memRegSfence := false.B
    }.elsewhen(exPcValid) {
      memRegDecodeOutput := exRegDecodeOutput
      memRegRVC := exRegRVC
      memRegLoad := exRegDecodeOutput(decoder.mem) && isRead(exRegDecodeOutput(decoder.memCommand))
      memRegStore := exRegDecodeOutput(decoder.mem) && isWrite(exRegDecodeOutput(decoder.memCommand))
      memRegSfence := exSfence
      memRegBTBResponse := exRegBTBResponse
      memRegFlushPipe := exRegFlushPipe
      memRegSlowBypass := exSlowBypass
      memRegWphit := exRegWphit

      memRegCause := exCause
      memRegInstruction := exRegInstruction
      memRegRawInstruction := exRegRawInstruction
      memRegMemSize := exRegMemSize
      memRegHlsOrDv := io.dmem.req.bits.dv
      memRegPc := exRegPC
      // IDecode ensured they are 1H
      memRegWdata := arithmeticLogicUnit.io.out
      memBranchTaken := arithmeticLogicUnit.io.cmp_out

      when(exRegDecodeOutput(decoder.rxs2) && (exRegDecodeOutput(decoder.mem) || Option.when(usingRoCC)(exRegDecodeOutput(decoder.rocc)).getOrElse(false.B) || exSfence)) {
        val size =
          if (usingRoCC) Mux(exRegDecodeOutput(decoder.rocc), log2Ceil(xLen / 8).U, exRegMemSize)
          else exRegMemSize
        memRegRS2 := new StoreGen(size, 0.U, exRs(1), coreDataBytes).data
      }
      when(exRegDecodeOutput(decoder.isJalr) && csr.io.status.debug) {
        // flush I$ on D-mode JALR to effect uncached fetch without D$ flush
        memRegDecodeOutput(decoder.fenceI) := true.B
        memRegFlushPipe := true.B
      }
    }

    val memBreakpoint = (memRegLoad && breakpointUnit.io.xcpt_ld) || (memRegStore && breakpointUnit.io.xcpt_st)
    val memDebugBreakpoint = (memRegLoad && breakpointUnit.io.debug_ld) || (memRegStore && breakpointUnit.io.debug_st)
    val (memLoadStoreException, memLoadStoreCause) = checkExceptions(
      List((memDebugBreakpoint, CSR.debugTriggerCause.U), (memBreakpoint, Causes.breakpoint.U))
    )

    val (memException, memCause) = checkExceptions(
      List(
        (memRegExceptionInterrupt || memRegException, memRegCause),
        (memRegValid && memNpcMisaligned, Causes.misaligned_fetch.U),
        (memRegValid && memLoadStoreException, memLoadStoreCause)
      )
    )

    val memCoverCauses = (exCoverCauses ++ List(
      (CSR.debugTriggerCause, "DEBUG_TRIGGER"),
      (Causes.breakpoint, "BREAKPOINT"),
      (Causes.misaligned_fetch, "MISALIGNED_FETCH")
    )).distinct
    coverExceptions(memException, memCause, "MEMORY", memCoverCauses)

    val dcacheKillMem =
      memRegValid && memRegDecodeOutput(decoder.wxd) && io.dmem.replay_next // structural hazard on writeback port
    val fpuKillMem = Option.when(usingFPU)(memRegValid && memRegDecodeOutput(decoder.fp) && io.fpu.nack_mem)
    val replayMem = dcacheKillMem || memRegReplay || fpuKillMem.getOrElse(false.B)
    val killmCommon = dcacheKillMem || takePcWb || memRegException || !memRegValid
    muldiv.io.kill := killmCommon && RegNext(muldiv.io.req.fire)
    val ctrlKillm = killmCommon || memException || fpuKillMem.getOrElse(false.B)

    // writeback stage
    wbRegValid := !ctrlKillm
    wbRegReplay := replayMem && !takePcWb
    wbRegException := memException && !takePcWb
    wbRegFlushPipe := !ctrlKillm && memRegFlushPipe
    when(memPcValid) {
      wbRegDecodeOutput := memRegDecodeOutput
      wbRegSfence := memRegSfence
      wbRegWdata := {
        if (usingFPU)
          Mux(
            !memRegException && memRegDecodeOutput(decoder.fp) && memRegDecodeOutput(decoder.wxd),
            io.fpu.toint_data,
            memIntWdata
          )
        else memIntWdata
      }
      when(Option.when(usingRoCC)(memRegDecodeOutput(decoder.rocc)).getOrElse(false.B) || memRegSfence) {
        wbRegRS2 := memRegRS2
      }
      wbRegCause := memCause
      wbRegInstruction := memRegInstruction
      wbRegRawInstruction := memRegRawInstruction
      wbRegMemSize := memRegMemSize
      wbRegHlsOrDv := memRegHlsOrDv
      wbRegHfenceV := memRegDecodeOutput(decoder.memCommand) === M_HFENCEV
      wbRegHfenceG := memRegDecodeOutput(decoder.memCommand) === M_HFENCEG
      wbRegPc := memRegPc
      wbRegWphit := memRegWphit | breakpointUnit.io.bpwatch.map { bpw =>
        (bpw.rvalid(0) && memRegLoad) || (bpw.wvalid(0) && memRegStore)
      }

    }

    val (wbException, wbCause) = checkExceptions(
      List(
        (wbRegException, wbRegCause),
        (wbRegValid && wbRegDecodeOutput(decoder.mem) && io.dmem.s2_xcpt.pf.st, Causes.store_page_fault.U),
        (wbRegValid && wbRegDecodeOutput(decoder.mem) && io.dmem.s2_xcpt.pf.ld, Causes.load_page_fault.U),
        (wbRegValid && wbRegDecodeOutput(decoder.mem) && io.dmem.s2_xcpt.gf.st, Causes.store_guest_page_fault.U),
        (wbRegValid && wbRegDecodeOutput(decoder.mem) && io.dmem.s2_xcpt.gf.ld, Causes.load_guest_page_fault.U),
        (wbRegValid && wbRegDecodeOutput(decoder.mem) && io.dmem.s2_xcpt.ae.st, Causes.store_access.U),
        (wbRegValid && wbRegDecodeOutput(decoder.mem) && io.dmem.s2_xcpt.ae.ld, Causes.load_access.U),
        (wbRegValid && wbRegDecodeOutput(decoder.mem) && io.dmem.s2_xcpt.ma.st, Causes.misaligned_store.U),
        (wbRegValid && wbRegDecodeOutput(decoder.mem) && io.dmem.s2_xcpt.ma.ld, Causes.misaligned_load.U)
      )
    )

    val wbCoverCauses = Seq(
      (Causes.misaligned_store, "MISALIGNED_STORE"),
      (Causes.misaligned_load, "MISALIGNED_LOAD"),
      (Causes.store_access, "STORE_ACCESS"),
      (Causes.load_access, "LOAD_ACCESS")
    ) ++
      Option.when(usingVM)(Seq(
          (Causes.store_page_fault, "STORE_PAGE_FAULT"),
          (Causes.load_page_fault, "LOAD_PAGE_FAULT")
        )
      ).getOrElse(Seq()) ++
      Option.when(usingHypervisor)(
        Seq(
          (Causes.store_guest_page_fault, "STORE_GUEST_PAGE_FAULT"),
          (Causes.load_guest_page_fault, "LOAD_GUEST_PAGE_FAULT")
        )
      ).getOrElse(Seq())
    coverExceptions(wbException, wbCause, "WRITEBACK", wbCoverCauses)

    val wbPcValid: Bool = wbRegValid || wbRegReplay || wbRegException
    val wbWxd: Bool = wbRegValid && wbRegDecodeOutput(decoder.wxd)
    val wbSetSboard: Bool =
        wbDcacheMiss ||
          Option.when(usingMulDiv)(wbRegDecodeOutput(decoder.div)).getOrElse(false.B) ||
          Option.when(usingRoCC)(wbRegDecodeOutput(decoder.rocc)).getOrElse(false.B) ||
          Option.when(usingVectorT1)(wbRegDecodeOutput(decoder.isVector)).getOrElse(false.B)
    val replayWbCommon: Bool = io.dmem.s2_nack || wbRegReplay
    val replayWbRocc: Option[Bool] = Option.when(usingRoCC)(wbRegValid && wbRegDecodeOutput(decoder.rocc) && !io.rocc.cmd.ready)
    val replayWbCsr: Bool = wbRegValid && csr.io.rw_stall
    val replayWb: Bool = replayWbCommon || replayWbCsr || replayWbRocc.getOrElse(false.B)
    takePcWb := replayWb || wbException || csr.io.eret || wbRegFlushPipe

    // writeback arbitration
    val dmemResponseXpu: Bool = !io.dmem.resp.bits.tag(0).asBool
    val dmemResponseFpu: Bool = io.dmem.resp.bits.tag(0).asBool
    val dmemResponseWaddr: UInt = io.dmem.resp.bits.tag(5, 1)
    val dmemResponseValid: Bool = io.dmem.resp.valid && io.dmem.resp.bits.has_data
    val dmemResponseReplay: Bool = dmemResponseValid && io.dmem.resp.bits.replay

    muldiv.io.resp.ready := !wbWxd
    val longlatencyWdata: UInt = WireDefault(muldiv.io.resp.bits.data)
    val longlatencyWaddress: UInt = WireDefault(muldiv.io.resp.bits.tag)
    val longLatencyWenable: Bool = WireDefault(muldiv.io.resp.fire)
    if (usingRoCC) {
      io.rocc.resp.ready := !wbWxd
      when(io.rocc.resp.fire) {
        muldiv.io.resp.ready := false.B
        longlatencyWdata := io.rocc.resp.bits.data
        longlatencyWaddress := io.rocc.resp.bits.rd
        longLatencyWenable := true.B
      }
    } else {
      // tie off RoCC
      io.rocc.resp.ready := false.B
      io.rocc.mem.req.ready := false.B
    }
    // Dont care mem since not all RoCC need accessing memory
    io.rocc.mem := DontCare

    when(dmemResponseReplay && dmemResponseXpu) {
      muldiv.io.resp.ready := false.B
      if (usingRoCC) io.rocc.resp.ready := false.B
      longlatencyWaddress := dmemResponseWaddr
      longLatencyWenable := true.B
    }

    val wbValid = wbRegValid && !replayWb && !wbException
    val wbWen = wbValid && wbRegDecodeOutput(decoder.wxd)
    // RF is at WB stage
    val rfWen = wbWen || longLatencyWenable
    val rfWaddr = Mux(longLatencyWenable, longlatencyWaddress, wbWaddr)
    val rfWdata = Mux(
      dmemResponseValid && dmemResponseXpu,
      io.dmem.resp.bits.data(xLen - 1, 0),
      Mux(
        longLatencyWenable,
        longlatencyWdata,
        Mux(
          wbRegDecodeOutput(decoder.csr) =/= CSR.N,
          csr.io.rw.rdata,
          Mux(
            Option.when(usingMulDiv && pipelinedMul)(wbRegDecodeOutput(decoder.mul)).getOrElse(false.B),
            mul.map(_.io.resp.bits.data).getOrElse(wbRegWdata),
            wbRegWdata
          )
        )
      )
    )
    when(rfWen) { rf.write(rfWaddr, rfWdata) }

    // hook up control/status regfile
    csr.io.ungatedClock := clock
    csr.io.decode(0).inst := idInstruction
    csr.io.exception := wbException
    csr.io.cause := wbCause
    csr.io.retire := wbValid
    csr.io.inst(0) := (
      if (usingCompressed) Cat(Mux(wbRegRawInstruction(1, 0).andR, wbRegInstruction >> 16, 0.U), wbRegRawInstruction(15, 0))
      else wbRegInstruction
      )
    csr.io.interrupts := io.interrupts
    csr.io.hartid := io.hartid
    io.fpu.fcsr_rm := csr.io.fcsr_rm
    csr.io.fcsr_flags := io.fpu.fcsr_flags
    io.fpu.time := csr.io.time(31, 0)
    io.fpu.hartid := io.hartid
    csr.io.rocc_interrupt := io.rocc.interrupt
    csr.io.pc := wbRegPc
    val tvalDmemAddr = !wbRegException
    val tvalAnyAddr = tvalDmemAddr ||
      wbRegCause.isOneOf(
        Causes.breakpoint.U,
        Causes.fetch_access.U,
        Causes.fetch_page_fault.U,
        Causes.fetch_guest_page_fault.U
      )
    val tvalInstruction = wbRegCause === Causes.illegal_instruction.U
    val tvalValid = wbException && (tvalAnyAddr || tvalInstruction)
    csr.io.gva := wbException && (tvalAnyAddr && csr.io.status.v || tvalDmemAddr && wbRegHlsOrDv)
    csr.io.tval := Mux(tvalValid, encodeVirtualAddress(wbRegWdata, wbRegWdata), 0.U)
    csr.io.htval := {
      val htvalValidImem = wbRegException && wbRegCause === Causes.fetch_guest_page_fault.U
      val htvalImem = Mux(htvalValidImem, io.imem.gpa.bits, 0.U)
      assert(!htvalValidImem || io.imem.gpa.valid)

      val htvalValidDmem =
        wbException && tvalDmemAddr && io.dmem.s2_xcpt.gf.asUInt.orR && !io.dmem.s2_xcpt.pf.asUInt.orR
      val htvalDmem = Mux(htvalValidDmem, io.dmem.s2_gpa, 0.U)

      (htvalDmem | htvalImem) >> hypervisorExtraAddrBits
    }
    io.ptw.ptbr := csr.io.ptbr
    io.ptw.hgatp := csr.io.hgatp
    io.ptw.vsatp := csr.io.vsatp
    io.ptw.customCSRs.csrs.zip(csr.io.customCSRs).foreach { case (lhs, rhs) => lhs <> rhs }
    io.ptw.status := csr.io.status
    io.ptw.hstatus := csr.io.hstatus
    io.ptw.gstatus := csr.io.gstatus
    io.ptw.pmp := csr.io.pmp
    csr.io.rw.addr := wbRegInstruction(31, 20)
    csr.io.rw.cmd := CSR.maskCmd(wbRegValid, wbRegDecodeOutput(decoder.csr))
    csr.io.rw.wdata := wbRegWdata
    io.rocc.csrs <> csr.io.roccCSRs
    io.trace.time := csr.io.time
    io.trace.insns := csr.io.trace
    // TODO: move it to verification part.
    if (rocketParams.debugROB) {
      val csrTraceWithWdata = WireInit(csr.io.trace(0))
      csrTraceWithWdata.wdata.get := rfWdata
      DebugROB.pushTrace(
        clock,
        reset,
        io.hartid,
        csrTraceWithWdata,
        Option.when(usingFPU)(wbRegDecodeOutput(decoder.wfd) || (wbRegDecodeOutput(decoder.wxd) && wbWaddr =/= 0.U)).getOrElse(false.B) && !csr.io.trace(0).exception,
        wbRegDecodeOutput(decoder.wxd) && wbWen && !wbSetSboard, wbWaddr + Mux(Option.when(usingFPU)(wbRegDecodeOutput(decoder.wfd)).getOrElse(false.B), 32.U, 0.U)
      )
      io.trace.insns(0) := DebugROB.popTrace(clock, reset, io.hartid)
      DebugROB.pushWb(clock, reset, io.hartid, longLatencyWenable, rfWaddr, rfWdata)
    } else {
      io.trace.insns := csr.io.trace
    }
    io.bpwatch.zip(wbRegWphit).zip(csr.io.bp)
    io.bpwatch.lazyZip(wbRegWphit).lazyZip(csr.io.bp).foreach{case (iobpw, wphit, bp) =>
      iobpw.valid(0) := wphit
      iobpw.action := bp.control.action
      // tie off bpwatch valids
      iobpw.rvalid.foreach(_ := false.B)
      iobpw.wvalid.foreach(_ := false.B)
      iobpw.ivalid.foreach(_ := false.B)
    }

    val hazardTargets = Seq(
      (idDecodeOutput(decoder.rxs1) && idRaddr1 =/= 0.U, idRaddr1),
      (idDecodeOutput(decoder.rxs2) && idRaddr2 =/= 0.U, idRaddr2),
      (idDecodeOutput(decoder.wxd) && idWaddr =/= 0.U, idWaddr)
    )
    val fpHazardTargets = Seq(
      (io.fpu.dec.ren1, idRaddr1),
      (io.fpu.dec.ren2, idRaddr2),
      (io.fpu.dec.ren3, idRaddr3),
      (io.fpu.dec.wen, idWaddr)
    )

    val scoreboard: Scoreboard = new Scoreboard(32, true)
    scoreboard.clear(longLatencyWenable, longlatencyWaddress)
    def idScoreboardClearBypass(r: UInt): Bool = {
      // ll_waddr arrives late when D$ has ECC, so reshuffle the hazard check
      if (tileParams.dcache.get.dataECC.isEmpty) longLatencyWenable && longlatencyWaddress === r
      else muldiv.io.resp.fire && muldiv.io.resp.bits.tag === r || dmemResponseReplay && dmemResponseXpu && dmemResponseWaddr === r
    }
    val idScoreboardHazard: Bool = checkHazards(hazardTargets, rd => scoreboard.read(rd) && !idScoreboardClearBypass(rd))
    scoreboard.set(wbSetSboard && wbWen, wbWaddr)

    // stall for RAW/WAW hazards on CSRs, loads, AMOs, and mul/div in execute stage.
    val exCannotBypass: Bool =
      exRegDecodeOutput(decoder.csr) =/= CSR.N ||
        exRegDecodeOutput(decoder.isJalr) ||
        exRegDecodeOutput(decoder.mem) ||
        Option.when(usingMulDiv && pipelinedMul)(exRegDecodeOutput(decoder.mul)).getOrElse(false.B) ||
        Option.when(usingMulDiv)(exRegDecodeOutput(decoder.div)).getOrElse(false.B) ||
        Option.when(usingFPU)(exRegDecodeOutput(decoder.fp)).getOrElse(false.B) ||
        Option.when(usingRoCC)(exRegDecodeOutput(decoder.rocc)).getOrElse(false.B)
    val dataHazardEx: Bool = exRegDecodeOutput(decoder.wxd) && checkHazards(hazardTargets, _ === exWaddr)
    val fpDataHazardEx: Option[Bool] = Option.when(usingFPU)(idDecodeOutput(decoder.fp) && exRegDecodeOutput(decoder.wfd) && checkHazards(fpHazardTargets, _ === exWaddr))
    val idExHazard: Bool = exRegValid && (dataHazardEx && exCannotBypass || fpDataHazardEx.getOrElse(false.B))

    // stall for RAW/WAW hazards on CSRs, LB/LH, and mul/div in memory stage.
    // TODO: what's BH?
    val memMemCmdBh: Bool =
      if (fastLoadWord) (!fastLoadByte).B && memRegSlowBypass
      else true.B
    val memCannotBypass: Bool =
      memRegDecodeOutput(decoder.csr) =/= CSR.N ||
        memRegDecodeOutput(decoder.mem) && memMemCmdBh ||
        Option.when(usingMulDiv && pipelinedMul)(memRegDecodeOutput(decoder.mul)).getOrElse(false.B) ||
        Option.when(usingMulDiv)(memRegDecodeOutput(decoder.div)).getOrElse(false.B) ||
        Option.when(usingFPU)(memRegDecodeOutput(decoder.fp)).getOrElse(false.B) ||
        Option.when(usingRoCC)(memRegDecodeOutput(decoder.rocc)).getOrElse(false.B)
    val dataHazardMem: Bool = memRegDecodeOutput(decoder.wxd) && checkHazards(hazardTargets, _ === memWaddr)
    val fpDataHazardMem: Option[Bool] = Option.when(usingFPU)(
      idDecodeOutput(decoder.fp) &&
        memRegDecodeOutput(decoder.wfd) &&
        checkHazards(fpHazardTargets, _ === memWaddr)
    )
    val idMemHazard: Bool = memRegValid && (dataHazardMem && memCannotBypass || fpDataHazardMem.getOrElse(false.B))
    idLoadUse := memRegValid && dataHazardMem && memRegDecodeOutput(decoder.mem)
    // stall for RAW/WAW hazards on load/AMO misses and mul/div in writeback.
    val dataHazardWb: Bool = wbRegDecodeOutput(decoder.wxd) && checkHazards(hazardTargets, _ === wbWaddr)
    val fpDataHazardWb: Bool =
      Option.when(usingFPU)(
          idDecodeOutput(decoder.fp) &&
            wbRegDecodeOutput(decoder.wfd) &&
            checkHazards(fpHazardTargets, _ === wbWaddr)
      ).getOrElse(false.B)
    val idWbHazard: Bool = wbRegValid && (dataHazardWb && wbSetSboard || fpDataHazardWb)
    val idStallFpu: Bool =
      Option.when(usingFPU){
        val fpScoreboard = new Scoreboard(32)
        fpScoreboard.set((wbDcacheMiss && wbRegDecodeOutput(decoder.wfd) || io.fpu.sboard_set) && wbValid, wbWaddr)
        fpScoreboard.clear(dmemResponseReplay && dmemResponseFpu, dmemResponseWaddr)
        fpScoreboard.clear(io.fpu.sboard_clr, io.fpu.sboard_clra)
        checkHazards(fpHazardTargets, fpScoreboard.read)
      }.getOrElse(false.B)

    val dcacheBlocked: Bool = {
      // speculate that a blocked D$ will unblock the cycle after a Grant
      val blocked = Reg(Bool())
      blocked := !io.dmem.req.ready && io.dmem.clock_enabled && !io.dmem.perf.grant && (blocked || io.dmem.req.valid || io.dmem.s2_nack)
      blocked && !io.dmem.perf.grant
    }
    val roccBlocked: Option[Bool] = Option.when(usingRoCC)(Reg(Bool()))
    roccBlocked.foreach(_ := !wbException && !io.rocc.cmd.ready && (io.rocc.cmd.valid || roccBlocked.get))

    val ctrlStalld: Bool =
      idExHazard || idMemHazard || idWbHazard || idScoreboardHazard || idDoFence || idRegPause ||
        csr.io.csr_stall || csr.io.singleStep && (exRegValid || memRegValid || wbRegValid) ||
        idCsrEn && csr.io.decode(0).fpCsr && !io.fpu.fcsr_rdy || io.traceStall ||
        !clockEnable ||
        Option.when(usingFPU)(idDecodeOutput(decoder.fp) && idStallFpu).getOrElse(false.B) ||
        idDecodeOutput(decoder.mem) && dcacheBlocked || // reduce activity during D$ misses
        Option.when(usingRoCC)(idDecodeOutput(decoder.rocc) && roccBlocked.get).getOrElse(false.B) || // reduce activity while RoCC is busy
        Option.when(usingMulDiv)(idDecodeOutput(decoder.div) && (!(muldiv.io.req.ready || (muldiv.io.resp.valid && !wbWxd)) || muldiv.io.req.valid)).getOrElse(false.B) // reduce odds of replay

    ctrlKilled :=
      !instructionBuffer.io.inst(0).valid ||
        instructionBufferOut.bits.replay ||
        takePcMemWb ||
        ctrlStalld ||
        csr.io.interrupt

    io.imem.req.valid := takePc
    io.imem.req.bits.speculative := !takePcWb
    // flush or branch misprediction
    io.imem.req.bits.pc := Mux(
      wbException || csr.io.eret,
      csr.io.evec, // exception or [m|s]ret
      Mux(
        replayWb,
        wbRegPc, // replay
        memNextPC
      )
    )
    io.imem.flush_icache := wbRegValid && wbRegDecodeOutput(decoder.fenceI) && !io.dmem.s2_nack
    io.imem.might_request := {
      imemMightRequestReg := exPcValid || memPcValid || io.ptw.customCSRs.disableICacheClockGate
      imemMightRequestReg
    }
    io.imem.progress := RegNext(wbRegValid && !replayWbCommon)
    io.imem.sfence.valid := wbRegValid && wbRegSfence
    io.imem.sfence.bits.rs1 := wbRegMemSize(0)
    io.imem.sfence.bits.rs2 := wbRegMemSize(1)
    io.imem.sfence.bits.addr := wbRegWdata
    io.imem.sfence.bits.asid := wbRegRS2
    io.imem.sfence.bits.hv := wbRegHfenceV
    io.imem.sfence.bits.hg := wbRegHfenceG
    io.ptw.sfence := io.imem.sfence

    instructionBufferOut.ready := !ctrlStalld

    io.imem.btb_update.valid := memRegValid && !takePcWb && memWrongNpc && (!memCfi || memCfiTaken)
    io.imem.btb_update.bits.isValid := memCfi
    io.imem.btb_update.bits.cfiType :=
      Mux(
        (memRegDecodeOutput(decoder.isJal) || memRegDecodeOutput(decoder.isJalr)) && memWaddr(0),
        CFIType.call,
        Mux(
          memRegDecodeOutput(decoder.isJalr) && (memRegInstruction(19, 15) & regAddrMask.U) === BitPat("b00?01"),
          CFIType.ret,
          Mux(memRegDecodeOutput(decoder.isJal) || memRegDecodeOutput(decoder.isJalr), CFIType.jump, CFIType.branch)
        )
      )
    io.imem.btb_update.bits.target := io.imem.req.bits.pc
    io.imem.btb_update.bits.br_pc := (if (usingCompressed) memRegPc + Mux(memRegRVC, 0.U, 2.U) else memRegPc)
    io.imem.btb_update.bits.pc := ~(~io.imem.btb_update.bits.br_pc | (coreInstBytes * fetchWidth - 1).U)
    io.imem.btb_update.bits.prediction := memRegBTBResponse
    io.imem.btb_update.bits.taken := DontCare

    io.imem.bht_update.valid := memRegValid && !takePcWb
    io.imem.bht_update.bits.pc := io.imem.btb_update.bits.pc
    io.imem.bht_update.bits.taken := memBranchTaken
    io.imem.bht_update.bits.mispredict := memWrongNpc
    io.imem.bht_update.bits.branch := memRegDecodeOutput(decoder.isBranch)
    io.imem.bht_update.bits.prediction := memRegBTBResponse.bht

    // Connect RAS in Frontend
    io.imem.ras_update := DontCare

    if (usingFPU) {
      io.fpu.valid := !ctrlKilled && idDecodeOutput(decoder.fp)
      io.fpu.killx := ctrlKillx
      io.fpu.killm := killmCommon
      io.fpu.inst := idInstruction
      io.fpu.fromint_data := exRs(0)
      io.fpu.dmem_resp_val := dmemResponseValid && dmemResponseFpu
      io.fpu.dmem_resp_data := (if (minFLen == 32) io.dmem.resp.bits.data_word_bypass else io.dmem.resp.bits.data)
      io.fpu.dmem_resp_type := io.dmem.resp.bits.size
      io.fpu.dmem_resp_tag := dmemResponseWaddr
      io.fpu.keep_clock_enabled := io.ptw.customCSRs.disableCoreClockGate
    } else {
      io.fpu.valid := DontCare
      io.fpu.killx := DontCare
      io.fpu.killm := DontCare
      io.fpu.inst := DontCare
      io.fpu.fromint_data := DontCare
      io.fpu.dmem_resp_val := DontCare
      io.fpu.dmem_resp_data := DontCare
      io.fpu.dmem_resp_type := DontCare
      io.fpu.dmem_resp_tag := DontCare
      io.fpu.keep_clock_enabled := DontCare
    }

    io.dmem.req.valid := exRegValid && exRegDecodeOutput(decoder.mem)
    val ex_dcache_tag = Cat(exWaddr, Option.when(usingFPU)(exRegDecodeOutput(decoder.fp)).getOrElse(false.B))
    require(coreParams.dcacheReqTagBits >= ex_dcache_tag.getWidth)
    io.dmem.req.bits.tag := ex_dcache_tag
    io.dmem.req.bits.cmd := exRegDecodeOutput(decoder.memCommand)
    io.dmem.req.bits.size := exRegMemSize
    io.dmem.req.bits.signed := !Mux(exRegHLS, exRegInstruction(20), exRegInstruction(14))
    io.dmem.req.bits.phys := false.B
    io.dmem.req.bits.addr := encodeVirtualAddress(exRs(0), arithmeticLogicUnit.io.adder_out)
    io.dmem.req.bits.idx.foreach(_ := io.dmem.req.bits.addr)
    io.dmem.req.bits.dprv := Mux(exRegHLS, csr.io.hstatus.spvp, csr.io.status.dprv)
    io.dmem.req.bits.dv := exRegHLS || csr.io.status.dv
    io.dmem.req.bits.no_alloc := DontCare
    io.dmem.req.bits.no_xcpt := DontCare
    io.dmem.req.bits.data := DontCare
    io.dmem.req.bits.mask := DontCare

    io.dmem.s1_data.data := Option.when(usingFPU)(Mux(memRegDecodeOutput(decoder.fp), Fill(xLen.max(fLen) / fLen, io.fpu.store_data), memRegRS2)).getOrElse(memRegRS2)
    io.dmem.s1_data.mask := DontCare

    io.dmem.s1_kill := killmCommon || memLoadStoreException || fpuKillMem.getOrElse(false.B)
    io.dmem.s2_kill := false.B
    // don't let D$ go to sleep if we're probably going to use it soon
    io.dmem.keep_clock_enabled := instructionBufferOut.valid && idDecodeOutput(decoder.mem) && !csr.io.csr_stall

    if (usingRoCC) {
      io.rocc.cmd.valid := wbRegValid && wbRegDecodeOutput(decoder.rocc) && !replayWbCommon
      io.rocc.exception := wbException && csr.io.status.xs.orR
      io.rocc.cmd.bits.status := csr.io.status
      io.rocc.cmd.bits.inst := wbRegInstruction.asTypeOf(new RoCCInstruction())
      io.rocc.cmd.bits.rs1 := wbRegWdata
      io.rocc.cmd.bits.rs2 := wbRegRS2
    } else {
      io.rocc.cmd.valid := DontCare
      io.rocc.exception := DontCare
      io.rocc.cmd.bits.status := DontCare
      io.rocc.cmd.bits.inst := DontCare
      io.rocc.cmd.bits.rs1 := DontCare
      io.rocc.cmd.bits.rs2 := DontCare
    }
    t1Request.foreach { t1 =>
      t1.valid := wbRegValid && !replayWbCommon && wbRegDecodeOutput(decoder.isVector)
      t1.bits.instruction := wbRegInstruction
      t1.bits.rs1Data := wbRegWdata
      t1.bits.rs2Data := wbRegRS2
    }
    t1Response.foreach(_ <> DontCare)
    t1IssueQueueFull.foreach(_ <> DontCare)
    t1IssueQueueEmpty.foreach(_ <> DontCare)

    // gate the clock
    val unpause: Bool = csr.io.time(rocketParams.lgPauseCycles - 1, 0) === 0.U || csr.io.inhibit_cycle || io.dmem.perf.release || takePc
    when(unpause) { idRegPause := false.B }
    io.cease := csr.io.status.cease && !clockEnableReg
    io.wfi := csr.io.status.wfi
    if (rocketParams.clockGate) {
      longLatencyStall := csr.io.csr_stall || io.dmem.perf.blocked || idRegPause && !unpause
      clockEnable := clockEnableReg || exPcValid || (!longLatencyStall && io.imem.resp.valid)
      clockEnableReg :=
        exPcValid || memPcValid || wbPcValid || // instruction in flight
        io.ptw.customCSRs.disableCoreClockGate || // chicken bit
        !muldiv.io.req.ready || // mul/div in flight
        usingFPU.B && !io.fpu.fcsr_rdy || // long-latency FPU in flight
        io.dmem.replay_next || // long-latency load replaying
        (!longLatencyStall && (instructionBufferOut.valid || io.imem.resp.valid)) // instruction pending

      assert(!(exPcValid || memPcValid || wbPcValid) || clockEnable)
    }

    // evaluate performance counters
    val icacheBlocked = !(io.imem.resp.valid || RegNext(io.imem.resp.valid))
    csr.io.counters.foreach { c => c.inc := RegNext(perfEvents.evaluate(c.eventSel)) }

    // TODO: move to Probe Module
    val coreMonitorBundle: CoreMonitorBundle = Wire(new CoreMonitorBundle(xLen, fLen))
    coreMonitorBundle.clock := clock
    coreMonitorBundle.reset := reset
    coreMonitorBundle.hartid := io.hartid
    coreMonitorBundle.timer := csr.io.time(31, 0)
    coreMonitorBundle.valid := csr.io.trace(0).valid && !csr.io.trace(0).exception
    coreMonitorBundle.pc := csr.io.trace(0).iaddr(vaddrBitsExtended - 1, 0).sextTo(xLen)
    coreMonitorBundle.wrenx := wbWen && !wbSetSboard
    coreMonitorBundle.wrenf := false.B
    coreMonitorBundle.wrdst := wbWaddr
    coreMonitorBundle.wrdata := rfWdata
    coreMonitorBundle.rd0src := wbRegInstruction(19, 15)
    coreMonitorBundle.rd0val := RegNext(RegNext(exRs(0)))
    coreMonitorBundle.rd1src := wbRegInstruction(24, 20)
    coreMonitorBundle.rd1val := RegNext(RegNext(exRs(1)))
    coreMonitorBundle.inst := csr.io.trace(0).insn
    coreMonitorBundle.excpt := csr.io.trace(0).exception
    coreMonitorBundle.priv_mode := csr.io.trace(0).priv
    if (enableCommitLog) {
      val t = csr.io.trace(0)
      val rd = wbWaddr
      val wfd = Option.when(usingFPU)(wbRegDecodeOutput(decoder.wfd)).getOrElse(false.B)
      val wxd = wbRegDecodeOutput(decoder.wxd)
      val has_data = wbWen && !wbSetSboard

      when(t.valid && !t.exception) {
        when(wfd) {
          printf("%d 0x%x (0x%x) f%d p%d 0xXXXXXXXXXXXXXXXX\n", t.priv, t.iaddr, t.insn, rd, rd + 32.U)
        }
          .elsewhen(wxd && rd =/= 0.U && has_data) {
            printf("%d 0x%x (0x%x) x%d 0x%x\n", t.priv, t.iaddr, t.insn, rd, rfWdata)
          }
          .elsewhen(wxd && rd =/= 0.U && !has_data) {
            printf("%d 0x%x (0x%x) x%d p%d 0xXXXXXXXXXXXXXXXX\n", t.priv, t.iaddr, t.insn, rd, rd)
          }
          .otherwise {
            printf("%d 0x%x (0x%x)\n", t.priv, t.iaddr, t.insn)
          }
      }

      when(longLatencyWenable && rfWaddr =/= 0.U) {
        printf("x%d p%d 0x%x\n", rfWaddr, rfWaddr, rfWdata)
      }
    } else {
      when(csr.io.trace(0).valid) {
        printf(
          "C%d: %d [%d] pc=[%x] W[r%d=%x][%d] R[r%d=%x] R[r%d=%x] inst=[%x] DASM(%x)\n",
          io.hartid,
          coreMonitorBundle.timer,
          coreMonitorBundle.valid,
          coreMonitorBundle.pc,
          Mux(
            wbRegDecodeOutput(decoder.wxd) || Option.when(usingFPU)(wbRegDecodeOutput(decoder.wfd)).getOrElse(false.B),
            coreMonitorBundle.wrdst,
            0.U
          ),
          Mux(coreMonitorBundle.wrenx, coreMonitorBundle.wrdata, 0.U),
          coreMonitorBundle.wrenx,
          Mux(
            wbRegDecodeOutput(decoder.rxs1) || Option.when(usingFPU)(wbRegDecodeOutput(decoder.rfs1)).getOrElse(false.B),
            coreMonitorBundle.rd0src,
            0.U
          ),
          Mux(
            wbRegDecodeOutput(decoder.rxs1) || Option.when(usingFPU)(wbRegDecodeOutput(decoder.rfs1)).getOrElse(false.B),
            coreMonitorBundle.rd0val,
            0.U
          ),
          Mux(
            wbRegDecodeOutput(decoder.rxs2) || Option.when(usingFPU)(wbRegDecodeOutput(decoder.rfs2)).getOrElse(false.B),
            coreMonitorBundle.rd1src,
            0.U
          ),
          Mux(
            wbRegDecodeOutput(decoder.rxs2) || Option.when(usingFPU)(wbRegDecodeOutput(decoder.rfs2)).getOrElse(false.B),
            coreMonitorBundle.rd1val,
            0.U
          ),
          coreMonitorBundle.inst,
          coreMonitorBundle.inst
        )
      }
    }

    // CoreMonitorBundle for late latency writes
    val xrfWriteBundle = Wire(new CoreMonitorBundle(xLen, fLen))
    xrfWriteBundle.clock := clock
    xrfWriteBundle.reset := reset
    xrfWriteBundle.hartid := io.hartid
    xrfWriteBundle.timer := csr.io.time(31, 0)
    xrfWriteBundle.valid := false.B
    xrfWriteBundle.pc := 0.U
    xrfWriteBundle.wrdst := rfWaddr
    xrfWriteBundle.wrenx := rfWen && !(csr.io.trace(0).valid && wbWen && (wbWaddr === rfWaddr))
    xrfWriteBundle.wrenf := false.B
    xrfWriteBundle.wrdata := rfWdata
    xrfWriteBundle.rd0src := 0.U
    xrfWriteBundle.rd0val := 0.U
    xrfWriteBundle.rd1src := 0.U
    xrfWriteBundle.rd1val := 0.U
    xrfWriteBundle.inst := 0.U
    xrfWriteBundle.excpt := false.B
    xrfWriteBundle.priv_mode := csr.io.trace(0).priv
    if (rocketParams.haveSimTimeout)
      PlusArg.timeout(
        name = "max_core_cycles",
        docstring = "Kill the emulation after INT rdtime cycles. Off if 0."
      )(csr.io.time)
  }

  def checkExceptions(x: Seq[(Bool, UInt)]) =
    (x.map(_._1).reduce(_ || _), PriorityMux(x))

  def coverExceptions(
    exceptionValid:    Bool,
    cause:             UInt,
    labelPrefix:       String,
    coverCausesLabels: Seq[(Int, String)]
  ): Unit = {
    for ((coverCause, label) <- coverCausesLabels) {
      property.cover(exceptionValid && (cause === coverCause.U), s"${labelPrefix}_${label}")
    }
  }

  def checkHazards(targets: Seq[(Bool, UInt)], cond: UInt => Bool) =
    targets.map(h => h._1 && cond(h._2)).reduce(_ || _)

  def encodeVirtualAddress(a0: UInt, ea: UInt) = if (vaddrBitsExtended == vaddrBits) ea
  else {
    // efficient means to compress 64-bit VA into vaddrBits+1 bits
    // (VA is bad if VA(vaddrBits) != VA(vaddrBits-1))
    val b = vaddrBitsExtended - 1
    val a = (a0 >> b).asSInt
    val msb = Mux(a === 0.S || a === -1.S, ea(b), !ea(b - 1))
    Cat(msb, ea(b - 1, 0))
  }

  class Scoreboard(n: Int, zero: Boolean = false) {
    def set(en:            Bool, addr: UInt): Unit = update(en, _next | mask(en, addr))
    def clear(en:          Bool, addr: UInt): Unit = update(en, _next & ~mask(en, addr))
    def read(addr:         UInt): Bool = r(addr)
    def readBypassed(addr: UInt):      Bool = _next(addr)

    private val _r = RegInit(0.U(n.W))
    private val r = if (zero) (_r >> 1 << 1) else _r
    private var _next = r
    private var ens = false.B
    private def mask(en: Bool, addr: UInt) = Mux(en, 1.U << addr, 0.U)
    private def update(en: Bool, update: UInt) = {
      _next = update
      ens = ens || en
      when(ens) { _r := _next }
    }
  }
}

class RegFile(n: Int, w: Int, zero: Boolean = false) {
  val rf = Mem(n, UInt(w.W))
  private def access(addr: UInt) = rf(~addr(log2Up(n) - 1, 0))
  private val reads = ArrayBuffer[(UInt, UInt)]()
  private var canRead = true
  def read(addr: UInt) = {
    require(canRead)
    reads += addr -> Wire(UInt())
    reads.last._2 := Mux(zero.B && addr === 0.U, 0.U, access(addr))
    reads.last._2
  }
  def write(addr: UInt, data: UInt) = {
    canRead = false
    when(addr =/= 0.U) {
      access(addr) := data
      for ((raddr, rdata) <- reads)
        when(addr === raddr) { rdata := data }
    }
  }
}

object ImmGen {
  def apply(sel: UInt, inst: UInt) = {
    val sign = Mux(sel === IMM_Z, 0.S, inst(31).asSInt)
    val b30_20 = Mux(sel === IMM_U, inst(30, 20).asSInt, sign)
    val b19_12 = Mux(sel =/= IMM_U && sel =/= IMM_UJ, sign, inst(19, 12).asSInt)
    val b11 = Mux(
      sel === IMM_U || sel === IMM_Z,
      0.S,
      Mux(sel === IMM_UJ, inst(20).asSInt, Mux(sel === IMM_SB, inst(7).asSInt, sign))
    )
    val b10_5 = Mux(sel === IMM_U || sel === IMM_Z, 0.U, inst(30, 25))
    val b4_1 = Mux(
      sel === IMM_U,
      0.U,
      Mux(sel === IMM_S || sel === IMM_SB, inst(11, 8), Mux(sel === IMM_Z, inst(19, 16), inst(24, 21)))
    )
    val b0 = Mux(sel === IMM_S, inst(7), Mux(sel === IMM_I, inst(20), Mux(sel === IMM_Z, inst(15), 0.U)))

    Cat(sign, b30_20, b19_12, b11, b10_5, b4_1, b0).asSInt
  }
}
