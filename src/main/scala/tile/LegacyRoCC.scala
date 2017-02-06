// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package tile

import Chisel._
import Chisel.ImplicitConversions._
import config._
import coreplex._
import diplomacy._
import rocket._
import uncore.constants._
import uncore.agents._
import uncore.coherence._
import uncore.devices._
import uncore.tilelink._
import uncore.tilelink2._
import uncore.util._
import util._

case object RoccMaxTaggedMemXacts extends Field[Int]
case object RoccNMemChannels extends Field[Int]
case object RoccNPTWPorts extends Field[Int]
case object BuildRoCC extends Field[Seq[RoCCParams]]

trait CanHaveLegacyRoccs extends CanHaveSharedFPU with CanHavePTW with TileNetwork {
  val module: CanHaveLegacyRoccsModule
  val legacyRocc = if (p(BuildRoCC).isEmpty) None
    else Some(LazyModule(new LegacyRoccComplex()(p.alter { (site, here, up) => {
        case CacheBlockOffsetBits => log2Up(site(CacheBlockBytes))
        case AmoAluOperandBits => site(XLen)
        case RoccNMemChannels => site(BuildRoCC).map(_.nMemChannels).foldLeft(0)(_ + _)
        case RoccNPTWPorts => site(BuildRoCC).map(_.nPTWPorts).foldLeft(0)(_ + _)
        case TLId => "L1toL2"
        case TLKey("L1toL2") =>
          TileLinkParameters(
            coherencePolicy = new MESICoherence(new NullRepresentation(site(NTiles))),
            nManagers = site(BankedL2Config).nBanks + 1 /* MMIO */,
            nCachingClients = 1,
            nCachelessClients = 1,
            maxClientXacts = List(
                tileParams.dcache.nMSHRs + 1 /* IOMSHR */,
                if (site(BuildRoCC).isEmpty) 1 else site(RoccMaxTaggedMemXacts)).max,
            maxClientsPerPort = if (site(BuildRoCC).isEmpty) 1 else 2,
            maxManagerXacts = 8,
            dataBeats = (8 * site(CacheBlockBytes)) / site(XLen),
            dataBits = site(CacheBlockBytes)*8)
    }})))

  // TODO for now, all legacy rocc mem ports mapped to one external node
  legacyRocc foreach { lr =>
    lr.masterNodes.foreach { l1backend.node := _ }
    nPTWPorts += lr.nPTWPorts
    nDCachePorts += lr.nRocc
  }
}

trait CanHaveLegacyRoccsModule extends CanHaveSharedFPUModule with CanHavePTWModule with TileNetworkModule {
  val outer: CanHaveLegacyRoccs

  fpuOpt foreach { fpu =>
    outer.legacyRocc.orElse {
      fpu.io.cp_req.valid := Bool(false)
      fpu.io.cp_resp.ready := Bool(false)
      None
    } foreach { lr =>
      fpu.io.cp_req <> lr.module.io.fpu.cp_req
      fpu.io.cp_resp <> lr.module.io.fpu.cp_resp
    }
  }

  outer.legacyRocc foreach { lr =>
    ptwPorts ++= lr.module.io.ptw
    dcachePorts ++= lr.module.io.dcache
  }

}

class LegacyRoccComplex(implicit p: Parameters) extends LazyModule {
  val buildRocc = p(BuildRoCC)
  val usingRocc = !buildRocc.isEmpty
  val nRocc = buildRocc.size
  val nFPUPorts = buildRocc.filter(_.useFPU).size
  val nMemChannels = buildRocc.map(_.nMemChannels).sum + nRocc
  val nPTWPorts = buildRocc.map(_.nPTWPorts).sum
  val roccOpcodes = buildRocc.map(_.opcodes)

  val legacies = List.fill(nMemChannels) { LazyModule(new TLLegacy()(p.alterPartial({ case PAddrBits => 32 }))) }
  val masterNodes = legacies.map(_ => TLOutputNode())
  legacies.zip(masterNodes).foreach { case(l,m) => m := TLHintHandler()(l.node) }

  lazy val module = new LazyModuleImp(this) with HasCoreParameters {
    val io = new Bundle {
      val tl = masterNodes.map(_.bundleOut)
      val dcache = Vec(nRocc, new HellaCacheIO)
      val fpu = new Bundle {
        val cp_req = Decoupled(new FPInput())
        val cp_resp = Decoupled(new FPResult()).flip
      }
      val ptw = Vec(nPTWPorts, new TLBPTWIO)
      val core = new Bundle {
        val cmd = Decoupled(new RoCCCommand).flip
        val resp = Decoupled(new RoCCResponse)
        val busy = Bool(OUTPUT)
        val interrupt = Bool(OUTPUT)
        val exception = Bool(INPUT)
      }
    }

    val respArb = Module(new RRArbiter(new RoCCResponse, nRocc))
    io.core.resp <> respArb.io.out

    val cmdRouter = Module(new RoccCommandRouter(roccOpcodes))
    cmdRouter.io.in <> io.core.cmd

    val roccs = buildRocc.zipWithIndex.map { case (accelParams, i) =>
      val rocc = accelParams.generator(p.alterPartial({
        case RoccNMemChannels => accelParams.nMemChannels
        case RoccNPTWPorts => accelParams.nPTWPorts
      }))
      val dcIF = Module(new SimpleHellaCacheIF)
      rocc.io.cmd <> cmdRouter.io.out(i)
      rocc.io.exception := io.core.exception
      dcIF.io.requestor <> rocc.io.mem
      io.dcache(i) := dcIF.io.cache
      legacies(i).module.io.legacy <> rocc.io.autl
      respArb.io.in(i) <> Queue(rocc.io.resp)
      rocc
    }

    (nRocc until legacies.size) zip roccs.map(_.io.utl) foreach { case(i, utl) =>
      legacies(i).module.io.legacy <> utl
    }
    io.core.busy := cmdRouter.io.busy || roccs.map(_.io.busy).reduce(_ || _)
    io.core.interrupt := roccs.map(_.io.interrupt).reduce(_ || _)

    if (usingFPU && nFPUPorts > 0) {
      val fpArb = Module(new InOrderArbiter(new FPInput, new FPResult, nFPUPorts))
      val fp_rocc_ios = roccs.zip(buildRocc)
        .filter { case (_, params) => params.useFPU }
        .map { case (rocc, _) => rocc.io }
      fpArb.io.in_req <> fp_rocc_ios.map(_.fpu_req)
      fp_rocc_ios.zip(fpArb.io.in_resp).foreach {
        case (rocc, arb) => rocc.fpu_resp <> arb
      }
      io.fpu.cp_req <> fpArb.io.out_req
      fpArb.io.out_resp <> io.fpu.cp_resp
    } else {
      io.fpu.cp_req.valid := Bool(false)
      io.fpu.cp_resp.ready := Bool(false)
    }
  }
}

case class RoCCParams(
  opcodes: OpcodeSet,
  generator: Parameters => RoCC,
  nMemChannels: Int = 0,
  nPTWPorts : Int = 0,
  useFPU: Boolean = false)

class RoCCInstruction extends Bundle
{
  val funct = Bits(width = 7)
  val rs2 = Bits(width = 5)
  val rs1 = Bits(width = 5)
  val xd = Bool()
  val xs1 = Bool()
  val xs2 = Bool()
  val rd = Bits(width = 5)
  val opcode = Bits(width = 7)
}

class RoCCCommand(implicit p: Parameters) extends CoreBundle()(p) {
  val inst = new RoCCInstruction
  val rs1 = Bits(width = xLen)
  val rs2 = Bits(width = xLen)
  val status = new MStatus
}

class RoCCResponse(implicit p: Parameters) extends CoreBundle()(p) {
  val rd = Bits(width = 5)
  val data = Bits(width = xLen)
}

class RoCCCoreIO(implicit p: Parameters) extends CoreBundle()(p) {
  val cmd = Decoupled(new RoCCCommand).flip
  val resp = Decoupled(new RoCCResponse)
  val mem = new HellaCacheIO
  val busy = Bool(OUTPUT)
  val interrupt = Bool(OUTPUT)
  val exception = Bool(INPUT)

  override def cloneType = new RoCCCoreIO()(p).asInstanceOf[this.type]
}

class RoCCIO(implicit p: Parameters) extends RoCCCoreIO()(p) {
  // These should be handled differently, eventually
  val autl = new ClientUncachedTileLinkIO
  val utl = Vec(p(RoccNMemChannels), new ClientUncachedTileLinkIO)
  val ptw = Vec(p(RoccNPTWPorts), new TLBPTWIO)
  val fpu_req = Decoupled(new FPInput)
  val fpu_resp = Decoupled(new FPResult).flip

  override def cloneType = new RoCCIO()(p).asInstanceOf[this.type]
}

abstract class RoCC(implicit p: Parameters) extends CoreModule()(p) {
  val io = new RoCCIO
  io.mem.req.bits.phys := Bool(true) // don't perform address translation
  io.mem.invalidate_lr := Bool(false) // don't mess with LR/SC
}

class AccumulatorExample(n: Int = 4)(implicit p: Parameters) extends RoCC()(p) {
  val regfile = Mem(n, UInt(width = xLen))
  val busy = Reg(init = Vec.fill(n){Bool(false)})

  val cmd = Queue(io.cmd)
  val funct = cmd.bits.inst.funct
  val addr = cmd.bits.rs2(log2Up(n)-1,0)
  val doWrite = funct === UInt(0)
  val doRead = funct === UInt(1)
  val doLoad = funct === UInt(2)
  val doAccum = funct === UInt(3)
  val memRespTag = io.mem.resp.bits.tag(log2Up(n)-1,0)

  // datapath
  val addend = cmd.bits.rs1
  val accum = regfile(addr)
  val wdata = Mux(doWrite, addend, accum + addend)

  when (cmd.fire() && (doWrite || doAccum)) {
    regfile(addr) := wdata
  }

  when (io.mem.resp.valid) {
    regfile(memRespTag) := io.mem.resp.bits.data
    busy(memRespTag) := Bool(false)
  }

  // control
  when (io.mem.req.fire()) {
    busy(addr) := Bool(true)
  }

  val doResp = cmd.bits.inst.xd
  val stallReg = busy(addr)
  val stallLoad = doLoad && !io.mem.req.ready
  val stallResp = doResp && !io.resp.ready

  cmd.ready := !stallReg && !stallLoad && !stallResp
    // command resolved if no stalls AND not issuing a load that will need a request

  // PROC RESPONSE INTERFACE
  io.resp.valid := cmd.valid && doResp && !stallReg && !stallLoad
    // valid response if valid command, need a response, and no stalls
  io.resp.bits.rd := cmd.bits.inst.rd
    // Must respond with the appropriate tag or undefined behavior
  io.resp.bits.data := accum
    // Semantics is to always send out prior accumulator register value

  io.busy := cmd.valid || busy.reduce(_||_)
    // Be busy when have pending memory requests or committed possibility of pending requests
  io.interrupt := Bool(false)
    // Set this true to trigger an interrupt on the processor (please refer to supervisor documentation)

  // MEMORY REQUEST INTERFACE
  io.mem.req.valid := cmd.valid && doLoad && !stallReg && !stallResp
  io.mem.req.bits.addr := addend
  io.mem.req.bits.tag := addr
  io.mem.req.bits.cmd := M_XRD // perform a load (M_XWR for stores)
  io.mem.req.bits.typ := MT_D // D = 8 bytes, W = 4, H = 2, B = 1
  io.mem.req.bits.data := Bits(0) // we're not performing any stores...

  io.autl.acquire.valid := false
  io.autl.grant.ready := false
}

class TranslatorExample(implicit p: Parameters) extends RoCC()(p) {
  val req_addr = Reg(UInt(width = coreMaxAddrBits))
  val req_rd = Reg(io.resp.bits.rd)
  val req_offset = req_addr(pgIdxBits - 1, 0)
  val req_vpn = req_addr(coreMaxAddrBits - 1, pgIdxBits)
  val pte = Reg(new PTE)

  val s_idle :: s_ptw_req :: s_ptw_resp :: s_resp :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  io.cmd.ready := (state === s_idle)

  when (io.cmd.fire()) {
    req_rd := io.cmd.bits.inst.rd
    req_addr := io.cmd.bits.rs1
    state := s_ptw_req
  }

  private val ptw = io.ptw(0)

  when (ptw.req.fire()) { state := s_ptw_resp }

  when (state === s_ptw_resp && ptw.resp.valid) {
    pte := ptw.resp.bits.pte
    state := s_resp
  }

  when (io.resp.fire()) { state := s_idle }

  ptw.req.valid := (state === s_ptw_req)
  ptw.req.bits.addr := req_vpn
  ptw.req.bits.store := Bool(false)
  ptw.req.bits.fetch := Bool(false)

  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := req_rd
  io.resp.bits.data := Mux(pte.leaf(), Cat(pte.ppn, req_offset), SInt(-1, xLen).asUInt)

  io.busy := (state =/= s_idle)
  io.interrupt := Bool(false)
  io.mem.req.valid := Bool(false)
  io.autl.acquire.valid := Bool(false)
  io.autl.grant.ready := Bool(false)
}

class CharacterCountExample(implicit p: Parameters) extends RoCC()(p)
    with HasTileLinkParameters {

  private val blockOffset = tlBeatAddrBits + tlByteAddrBits

  val needle = Reg(UInt(width = 8))
  val addr = Reg(UInt(width = coreMaxAddrBits))
  val count = Reg(UInt(width = xLen))
  val resp_rd = Reg(io.resp.bits.rd)

  val addr_block = addr(coreMaxAddrBits - 1, blockOffset)
  val offset = addr(blockOffset - 1, 0)
  val next_addr = (addr_block + UInt(1)) << UInt(blockOffset)

  val s_idle :: s_acq :: s_gnt :: s_check :: s_resp :: Nil = Enum(Bits(), 5)
  val state = Reg(init = s_idle)

  val gnt = io.autl.grant.bits
  val recv_data = Reg(UInt(width = tlDataBits))
  val recv_beat = Reg(UInt(width = tlBeatAddrBits))

  val data_bytes = Vec.tabulate(tlDataBytes) { i => recv_data(8 * (i + 1) - 1, 8 * i) }
  val zero_match = data_bytes.map(_ === UInt(0))
  val needle_match = data_bytes.map(_ === needle)
  val first_zero = PriorityEncoder(zero_match)

  val chars_found = PopCount(needle_match.zipWithIndex.map {
    case (matches, i) =>
      val idx = Cat(recv_beat, UInt(i, tlByteAddrBits))
      matches && idx >= offset && UInt(i) <= first_zero
  })
  val zero_found = zero_match.reduce(_ || _)
  val finished = Reg(Bool())

  io.cmd.ready := (state === s_idle)
  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := resp_rd
  io.resp.bits.data := count
  io.autl.acquire.valid := (state === s_acq)
  io.autl.acquire.bits := GetBlock(addr_block = addr_block)
  io.autl.grant.ready := (state === s_gnt)

  when (io.cmd.fire()) {
    addr := io.cmd.bits.rs1
    needle := io.cmd.bits.rs2
    resp_rd := io.cmd.bits.inst.rd
    count := UInt(0)
    finished := Bool(false)
    state := s_acq
  }

  when (io.autl.acquire.fire()) { state := s_gnt }

  when (io.autl.grant.fire()) {
    recv_beat := gnt.addr_beat
    recv_data := gnt.data
    state := s_check
  }

  when (state === s_check) {
    when (!finished) {
      count := count + chars_found
    }
    when (zero_found) { finished := Bool(true) }
    when (recv_beat === UInt(tlDataBeats - 1)) {
      addr := next_addr
      state := Mux(zero_found || finished, s_resp, s_acq)
    } .otherwise {
      state := s_gnt
    }
  }

  when (io.resp.fire()) { state := s_idle }

  io.busy := (state =/= s_idle)
  io.interrupt := Bool(false)
  io.mem.req.valid := Bool(false)
}

class OpcodeSet(val opcodes: Seq[UInt]) {
  def |(set: OpcodeSet) =
    new OpcodeSet(this.opcodes ++ set.opcodes)

  def matches(oc: UInt) = opcodes.map(_ === oc).reduce(_ || _)
}

object OpcodeSet {
  def custom0 = new OpcodeSet(Seq(Bits("b0001011")))
  def custom1 = new OpcodeSet(Seq(Bits("b0101011")))
  def custom2 = new OpcodeSet(Seq(Bits("b1011011")))
  def custom3 = new OpcodeSet(Seq(Bits("b1111011")))
  def all = custom0 | custom1 | custom2 | custom3
}

class RoccCommandRouter(opcodes: Seq[OpcodeSet])(implicit p: Parameters)
    extends CoreModule()(p) {
  val io = new Bundle {
    val in = Decoupled(new RoCCCommand).flip
    val out = Vec(opcodes.size, Decoupled(new RoCCCommand))
    val busy = Bool(OUTPUT)
  }

  val cmd = Queue(io.in)
  val cmdReadys = io.out.zip(opcodes).map { case (out, opcode) =>
    val me = opcode.matches(cmd.bits.inst.opcode)
    out.valid := cmd.valid && me
    out.bits := cmd.bits
    out.ready && me
  }
  cmd.ready := cmdReadys.reduce(_ || _)
  io.busy := cmd.valid

  assert(PopCount(cmdReadys) <= UInt(1),
    "Custom opcode matched for more than one accelerator")
}
