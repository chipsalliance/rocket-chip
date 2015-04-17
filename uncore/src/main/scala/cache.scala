// See LICENSE for license details.

package uncore
import Chisel._
import scala.reflect.ClassTag

case object CacheName extends Field[String]
case object NSets extends Field[Int]
case object NWays extends Field[Int]
case object RowBits extends Field[Int]
case object Replacer extends Field[() => ReplacementPolicy]
case object AmoAluOperandBits extends Field[Int]
case object L2DirectoryRepresentation extends Field[DirectoryRepresentation]
case object NPrimaryMisses extends Field[Int]
case object NSecondaryMisses extends Field[Int]
case object CacheBlockBytes extends Field[Int]
case object CacheBlockOffsetBits extends Field[Int]
case object ECCCode extends Field[Option[Code]]

abstract trait CacheParameters extends UsesParameters {
  val nSets = params(NSets)
  val blockOffBits = params(CacheBlockOffsetBits)
  val idxBits = log2Up(nSets)
  val untagBits = blockOffBits + idxBits
  val tagBits = params(PAddrBits) - untagBits
  val nWays = params(NWays)
  val wayBits = log2Up(nWays)
  val isDM = nWays == 1
  val rowBits = params(RowBits)
  val rowBytes = rowBits/8
  val rowOffBits = log2Up(rowBytes)
  val code = params(ECCCode).getOrElse(new IdentityCode)
}

abstract class CacheBundle extends Bundle with CacheParameters
abstract class CacheModule extends Module with CacheParameters

class StoreGen(typ: Bits, addr: Bits, dat: Bits) {
  val byte = typ === MT_B || typ === MT_BU
  val half = typ === MT_H || typ === MT_HU
  val word = typ === MT_W || typ === MT_WU
  def mask =
    Mux(byte, Bits(  1) <<     addr(2,0),
    Mux(half, Bits(  3) << Cat(addr(2,1), Bits(0,1)),
    Mux(word, Bits( 15) << Cat(addr(2),   Bits(0,2)),
              Bits(255))))
  def data =
    Mux(byte, Fill(8, dat( 7,0)),
    Mux(half, Fill(4, dat(15,0)),
                      wordData))
  lazy val wordData =
    Mux(word, Fill(2, dat(31,0)),
                      dat)
}

class LoadGen(typ: Bits, addr: Bits, dat: Bits, zero: Bool) {
  val t = new StoreGen(typ, addr, dat)
  val sign = typ === MT_B || typ === MT_H || typ === MT_W || typ === MT_D

  val wordShift = Mux(addr(2), dat(63,32), dat(31,0))
  val word = Cat(Mux(t.word, Fill(32, sign && wordShift(31)), dat(63,32)), wordShift)
  val halfShift = Mux(addr(1), word(31,16), word(15,0))
  val half = Cat(Mux(t.half, Fill(48, sign && halfShift(15)), word(63,16)), halfShift)
  val byteShift = Mux(zero, UInt(0), Mux(addr(0), half(15,8), half(7,0)))
  val byte = Cat(Mux(zero || t.byte, Fill(56, sign && byteShift(7)), half(63,8)), byteShift)
}

class AMOALU extends CacheModule {
  val operandBits = params(AmoAluOperandBits)
  require(operandBits == 64)
  val io = new Bundle {
    val addr = Bits(INPUT, blockOffBits)
    val cmd = Bits(INPUT, M_SZ)
    val typ = Bits(INPUT, MT_SZ)
    val lhs = Bits(INPUT, operandBits)
    val rhs = Bits(INPUT, operandBits)
    val out = Bits(OUTPUT, operandBits)
  }

  val storegen = new StoreGen(io.typ, io.addr, io.rhs)
  val rhs = storegen.wordData
  
  val sgned = io.cmd === M_XA_MIN || io.cmd === M_XA_MAX
  val max = io.cmd === M_XA_MAX || io.cmd === M_XA_MAXU
  val min = io.cmd === M_XA_MIN || io.cmd === M_XA_MINU
  val word = io.typ === MT_W || io.typ === MT_WU || // Logic minimization:
               io.typ === MT_B || io.typ === MT_BU

  val mask = SInt(-1,64) ^ (io.addr(2) << UInt(31))
  val adder_out = (io.lhs & mask).toUInt + (rhs & mask)

  val cmp_lhs  = Mux(word && !io.addr(2), io.lhs(31), io.lhs(63))
  val cmp_rhs  = Mux(word && !io.addr(2), rhs(31), rhs(63))
  val lt_lo = io.lhs(31,0) < rhs(31,0)
  val lt_hi = io.lhs(63,32) < rhs(63,32)
  val eq_hi = io.lhs(63,32) === rhs(63,32)
  val lt = Mux(word, Mux(io.addr(2), lt_hi, lt_lo), lt_hi || eq_hi && lt_lo)
  val less = Mux(cmp_lhs === cmp_rhs, lt, Mux(sgned, cmp_lhs, cmp_rhs))

  val out = Mux(io.cmd === M_XA_ADD, adder_out,
            Mux(io.cmd === M_XA_AND, io.lhs & rhs,
            Mux(io.cmd === M_XA_OR,  io.lhs | rhs,
            Mux(io.cmd === M_XA_XOR, io.lhs ^ rhs,
            Mux(Mux(less, min, max), io.lhs,
            storegen.data)))))

  val wmask = FillInterleaved(8, storegen.mask)
  io.out := wmask & out | ~wmask & io.lhs
}

abstract class ReplacementPolicy {
  def way: UInt
  def miss: Unit
  def hit: Unit
}

class RandomReplacement(ways: Int) extends ReplacementPolicy {
  private val replace = Bool()
  replace := Bool(false)
  val lfsr = LFSR16(replace)

  def way = if(ways == 1) UInt(0) else lfsr(log2Up(ways)-1,0)
  def miss = replace := Bool(true)
  def hit = {}
}

abstract class Metadata extends CacheBundle {
  val tag = Bits(width = tagBits)
  val coh: CoherenceMetadata
}

class MetaReadReq extends CacheBundle {
  val idx  = Bits(width = idxBits)
}

class MetaWriteReq[T <: Metadata](gen: T) extends MetaReadReq {
  val way_en = Bits(width = nWays)
  val data = gen.clone
  override def clone = new MetaWriteReq(gen).asInstanceOf[this.type]
}

class MetadataArray[T <: Metadata](makeRstVal: () => T) extends CacheModule {
  val rstVal = makeRstVal()
  val io = new Bundle {
    val read = Decoupled(new MetaReadReq).flip
    val write = Decoupled(new MetaWriteReq(rstVal.clone)).flip
    val resp = Vec.fill(nWays){rstVal.clone.asOutput}
  }
  val rst_cnt = Reg(init=UInt(0, log2Up(nSets+1)))
  val rst = rst_cnt < UInt(nSets)
  val waddr = Mux(rst, rst_cnt, io.write.bits.idx)
  val wdata = Mux(rst, rstVal, io.write.bits.data).toBits
  val wmask = Mux(rst, SInt(-1), io.write.bits.way_en).toUInt
  when (rst) { rst_cnt := rst_cnt+UInt(1) }

  val metabits = rstVal.getWidth
  val tag_arr = Mem(UInt(width = metabits*nWays), nSets, seqRead = true)
  when (rst || io.write.valid) {
    tag_arr.write(waddr, Fill(nWays, wdata), FillInterleaved(metabits, wmask))
  }

  val tags = tag_arr(RegEnable(io.read.bits.idx, io.read.valid))
  io.resp := io.resp.fromBits(tags)
  io.read.ready := !rst && !io.write.valid // so really this could be a 6T RAM
  io.write.ready := !rst
}

abstract trait L2HellaCacheParameters extends CacheParameters with CoherenceAgentParameters {
  val idxMSB = idxBits-1
  val idxLSB = 0
  val blockAddrBits = params(TLBlockAddrBits)
  val refillCyclesPerBeat = outerDataBits/rowBits
  val refillCycles = refillCyclesPerBeat*outerDataBeats
  val internalDataBeats = params(CacheBlockBytes)*8/rowBits
  require(refillCyclesPerBeat == 1)
  val amoAluOperandBits = params(AmoAluOperandBits)
  require(amoAluOperandBits <= innerDataBits)
  require(rowBits == innerDataBits) // TODO: relax this by improving s_data_* states
  val nSecondaryMisses = params(NSecondaryMisses)
  val isLastLevelCache = true
  val ignoresWriteMask = !params(ECCCode).isEmpty
}

abstract class L2HellaCacheBundle extends Bundle with L2HellaCacheParameters
abstract class L2HellaCacheModule extends Module with L2HellaCacheParameters {
  def doInternalOutputArbitration[T <: Data : ClassTag](
      out: DecoupledIO[T],
      ins: Seq[DecoupledIO[T]]) {
    val arb = Module(new RRArbiter(out.bits.clone, ins.size))
    out <> arb.io.out
    arb.io.in <> ins 
  }

  def doInternalInputRouting[T <: HasL2Id](in: ValidIO[T], outs: Seq[ValidIO[T]]) {
    val idx = in.bits.id
    outs.map(_.bits := in.bits)
    outs.zipWithIndex.map { case (o,i) => o.valid := in.valid && idx === UInt(i) }
  }
}

trait HasL2Id extends Bundle with CoherenceAgentParameters {
  val id = UInt(width  = log2Up(nTransactors + 1))
}

trait HasL2InternalRequestState extends L2HellaCacheBundle {
  val tag_match = Bool()
  val meta = new L2Metadata
  val way_en = Bits(width = nWays)
}

trait HasL2BeatAddr extends L2HellaCacheBundle {
  val addr_beat = UInt(width = log2Up(refillCycles))
}

trait HasL2Data extends L2HellaCacheBundle 
    with HasL2BeatAddr {
  val data = UInt(width = rowBits)
  def hasData(dummy: Int = 0) = Bool(true)
  def hasMultibeatData(dummy: Int = 0) = Bool(refillCycles > 1)
}

class L2Metadata extends Metadata with L2HellaCacheParameters {
  val coh = new HierarchicalMetadata
}

object L2Metadata {
  def apply(tag: Bits, coh: HierarchicalMetadata) = {
    val meta = new L2Metadata
    meta.tag := tag
    meta.coh := coh
    meta
  }
}

class L2MetaReadReq extends MetaReadReq with HasL2Id {
  val tag = Bits(width = tagBits)
}

class L2MetaWriteReq extends MetaWriteReq[L2Metadata](new L2Metadata)
    with HasL2Id {
  override def clone = new L2MetaWriteReq().asInstanceOf[this.type]
}

class L2MetaResp extends L2HellaCacheBundle
  with HasL2Id 
  with HasL2InternalRequestState

trait HasL2MetaReadIO extends L2HellaCacheBundle {
  val read = Decoupled(new L2MetaReadReq)
  val resp = Valid(new L2MetaResp).flip
}

trait HasL2MetaWriteIO extends L2HellaCacheBundle {
  val write = Decoupled(new L2MetaWriteReq)
}

class L2MetaRWIO extends L2HellaCacheBundle with HasL2MetaReadIO with HasL2MetaWriteIO

class L2MetadataArray extends L2HellaCacheModule {
  val io = new L2MetaRWIO().flip

  def onReset = L2Metadata(UInt(0), HierarchicalMetadata.onReset)
  val meta = Module(new MetadataArray(onReset _))
  meta.io.read <> io.read
  meta.io.write <> io.write
  
  val s1_tag = RegEnable(io.read.bits.tag, io.read.valid)
  val s1_id = RegEnable(io.read.bits.id, io.read.valid)
  def wayMap[T <: Data](f: Int => T) = Vec((0 until nWays).map(f))
  val s1_clk_en = Reg(next = io.read.fire())
  val s1_tag_eq_way = wayMap((w: Int) => meta.io.resp(w).tag === s1_tag)
  val s1_tag_match_way = wayMap((w: Int) => s1_tag_eq_way(w) && meta.io.resp(w).coh.outer.isValid()).toBits
  val s2_tag_match_way = RegEnable(s1_tag_match_way, s1_clk_en)
  val s2_tag_match = s2_tag_match_way.orR
  val s2_hit_coh = Mux1H(s2_tag_match_way, wayMap((w: Int) => RegEnable(meta.io.resp(w).coh, s1_clk_en)))

  val replacer = params(Replacer)()
  val s1_replaced_way_en = UIntToOH(replacer.way)
  val s2_replaced_way_en = UIntToOH(RegEnable(replacer.way, s1_clk_en))
  val s2_repl_meta = Mux1H(s2_replaced_way_en, wayMap((w: Int) => 
    RegEnable(meta.io.resp(w), s1_clk_en && s1_replaced_way_en(w))).toSeq)
  when(!s2_tag_match) { replacer.miss }

  io.resp.valid := Reg(next = s1_clk_en)
  io.resp.bits.id := RegEnable(s1_id, s1_clk_en)
  io.resp.bits.tag_match := s2_tag_match
  io.resp.bits.meta := Mux(s2_tag_match, 
    L2Metadata(s2_repl_meta.tag, s2_hit_coh), 
    s2_repl_meta)
  io.resp.bits.way_en := Mux(s2_tag_match, s2_tag_match_way, s2_replaced_way_en)
}

class L2DataReadReq extends L2HellaCacheBundle 
    with HasL2BeatAddr 
    with HasL2Id {
  val addr_idx = UInt(width = idxBits)
  val way_en = Bits(width = nWays)
}

class L2DataWriteReq extends L2DataReadReq 
    with HasL2Data {
  val wmask  = Bits(width = rowBits/8)
}

class L2DataResp extends L2HellaCacheBundle with HasL2Id with HasL2Data

trait HasL2DataReadIO extends L2HellaCacheBundle { 
  val read = Decoupled(new L2DataReadReq)
  val resp = Valid(new L2DataResp).flip
}

trait HasL2DataWriteIO extends L2HellaCacheBundle { 
  val write = Decoupled(new L2DataWriteReq)
}

class L2DataRWIO extends L2HellaCacheBundle with HasL2DataReadIO with HasL2DataWriteIO

class L2DataArray(delay: Int) extends L2HellaCacheModule {
  val io = new L2DataRWIO().flip

  val wmask = FillInterleaved(8, io.write.bits.wmask)
  val reg_raddr = Reg(UInt())
  val array = Mem(Bits(width=rowBits), nWays*nSets*refillCycles, seqRead = true)
  val waddr = Cat(OHToUInt(io.write.bits.way_en), io.write.bits.addr_idx, io.write.bits.addr_beat)
  val raddr = Cat(OHToUInt(io.read.bits.way_en), io.read.bits.addr_idx, io.read.bits.addr_beat)

  when (io.write.bits.way_en.orR && io.write.valid) {
    array.write(waddr, io.write.bits.data, wmask)
  }.elsewhen (io.read.bits.way_en.orR && io.read.valid) {
    reg_raddr := raddr
  }

  io.resp.valid := ShiftRegister(io.read.fire(), delay+1)
  io.resp.bits.id := ShiftRegister(io.read.bits.id, delay+1)
  io.resp.bits.addr_beat := ShiftRegister(io.read.bits.addr_beat, delay+1)
  io.resp.bits.data := ShiftRegister(array(reg_raddr), delay)
  io.read.ready := !io.write.valid
  io.write.ready := Bool(true)
}

class L2SecondaryMissInfo extends TLBundle
    with HasTileLinkBeatId
    with HasClientTransactionId

class L2HellaCacheBank extends HierarchicalCoherenceAgent
    with L2HellaCacheParameters {
  require(isPow2(nSets))
  require(isPow2(nWays)) 

  val meta = Module(new L2MetadataArray) // TODO: add delay knob
  val data = Module(new L2DataArray(1))
  val tshrfile = Module(new TSHRFile)
  tshrfile.io.inner <> io.inner
  io.outer <> tshrfile.io.outer
  io.incoherent <> tshrfile.io.incoherent
  tshrfile.io.meta <> meta.io
  tshrfile.io.data <> data.io
}

class TSHRFileIO extends HierarchicalTLIO {
  val meta = new L2MetaRWIO
  val data = new L2DataRWIO
}

class TSHRFile extends L2HellaCacheModule 
    with HasCoherenceAgentWiringHelpers {
  val io = new TSHRFileIO

  // Create TSHRs for outstanding transactions
  val trackerList = (0 until nReleaseTransactors).map(id => Module(new L2VoluntaryReleaseTracker(id))) ++
    (nReleaseTransactors until nTransactors).map(id => Module(new L2AcquireTracker(id)))
  
  // WritebackUnit evicts data from L2, including invalidating L1s
  val wb = Module(new L2WritebackUnit(nTransactors))
  doInternalOutputArbitration(wb.io.wb.req, trackerList.map(_.io.wb.req))
  doInternalInputRouting(wb.io.wb.resp, trackerList.map(_.io.wb.resp))

  // Propagate incoherence flags
  (trackerList.map(_.io.incoherent) :+ wb.io.incoherent) foreach { _ := io.incoherent.toBits }

  // Handle acquire transaction initiation
  val trackerAcquireIOs = trackerList.map(_.io.inner.acquire)
  val acquireConflicts = Vec(trackerList.map(_.io.has_acquire_conflict)).toBits
  val acquireMatches = Vec(trackerList.map(_.io.has_acquire_match)).toBits
  val acquireReadys = Vec(trackerAcquireIOs.map(_.ready)).toBits
  val acquire_idx = Mux(acquireMatches.orR,
                      PriorityEncoder(acquireMatches),
                      PriorityEncoder(acquireReadys))

  val block_acquires = acquireConflicts.orR
  io.inner.acquire.ready := acquireReadys.orR && !block_acquires
  trackerAcquireIOs.zipWithIndex.foreach {
    case(tracker, i) =>
      tracker.bits := io.inner.acquire.bits
      tracker.valid := io.inner.acquire.valid && !block_acquires && (acquire_idx === UInt(i))
  }

  // Wire releases from clients
  val trackerReleaseIOs = trackerList.map(_.io.inner.release) :+ wb.io.inner.release
  val releaseReadys = Vec(trackerReleaseIOs.map(_.ready)).toBits
  val releaseMatches = Vec(trackerList.map(_.io.has_release_match) :+ wb.io.has_release_match).toBits
  val release_idx = PriorityEncoder(releaseMatches)
  io.inner.release.ready := releaseReadys(release_idx)
  trackerReleaseIOs.zipWithIndex.foreach {
    case(tracker, i) =>
      tracker.bits := io.inner.release.bits
      tracker.valid := io.inner.release.valid && (release_idx === UInt(i))
  }
  assert(!(io.inner.release.valid && !releaseMatches.orR),
    "Non-voluntary release should always have a Tracker waiting for it.")

  // Wire probe requests and grant reply to clients, finish acks from clients
  doOutputArbitration(io.inner.probe, trackerList.map(_.io.inner.probe) :+ wb.io.inner.probe)
  doOutputArbitration(io.inner.grant, trackerList.map(_.io.inner.grant))
  doInputRouting(io.inner.finish, trackerList.map(_.io.inner.finish))

  // Create an arbiter for the one memory port
  val outerList = trackerList.map(_.io.outer) :+ wb.io.outer
  val outer_arb = Module(new ClientTileLinkIOArbiter(outerList.size))(outerTLParams)
  outer_arb.io.in <> outerList
  io.outer <> outer_arb.io.out

  // Wire local memory arrays
  doInternalOutputArbitration(io.meta.read, trackerList.map(_.io.meta.read))
  doInternalOutputArbitration(io.meta.write, trackerList.map(_.io.meta.write))
  doInternalOutputArbitration(io.data.read, trackerList.map(_.io.data.read) :+ wb.io.data.read)
  doInternalOutputArbitration(io.data.write, trackerList.map(_.io.data.write))
  doInternalInputRouting(io.meta.resp, trackerList.map(_.io.meta.resp))
  doInternalInputRouting(io.data.resp, trackerList.map(_.io.data.resp) :+ wb.io.data.resp)
}


class L2XactTrackerIO extends HierarchicalXactTrackerIO {
  val data = new L2DataRWIO
  val meta = new L2MetaRWIO
  val wb = new L2WritebackIO
}

abstract class L2XactTracker extends XactTracker with L2HellaCacheParameters {
  class CacheBlockBuffer {
    val buffer = Reg(Bits(width = params(CacheBlockBytes)*8))

    def internal = Vec.fill(internalDataBeats){ Bits(width = rowBits) }.fromBits(buffer)
    def inner = Vec.fill(innerDataBeats){ Bits(width = innerDataBits) }.fromBits(buffer)
    def outer = Vec.fill(outerDataBeats){ Bits(width = outerDataBits) }.fromBits(buffer)
  }

  def connectDataBeatCounter[S <: L2HellaCacheBundle](inc: Bool, data: S, beat: UInt, full_block: Bool) = {
    if(data.refillCycles > 1) {
      val (multi_cnt, multi_done) = Counter(full_block && inc, data.refillCycles)
      (Mux(!full_block, beat, multi_cnt), Mux(!full_block, inc, multi_done))
    } else { (UInt(0), inc) }
  }

  def connectInternalDataBeatCounter[T <: HasL2BeatAddr](
      in: DecoupledIO[T],
      beat: UInt = UInt(0),
      full_block: Bool = Bool(true)) = {
    connectDataBeatCounter(in.fire(), in.bits, beat, full_block)
  }

  def connectInternalDataBeatCounter[T <: HasL2Data](
      in: ValidIO[T],
      full_block: Bool = Bool(true)) = {
    connectDataBeatCounter(in.valid, in.bits, UInt(0), full_block)._2
  }

  def addPendingBitInternal[T <: HasL2BeatAddr](in: DecoupledIO[T]) =
    Fill(in.bits.refillCycles, in.fire()) & UIntToOH(in.bits.addr_beat)

  def addPendingBitInternal[T <: HasL2BeatAddr](in: ValidIO[T]) =
    Fill(in.bits.refillCycles, in.valid) & UIntToOH(in.bits.addr_beat)

  def dropPendingBit[T <: HasL2BeatAddr] (in: DecoupledIO[T]) =
    ~Fill(in.bits.refillCycles, in.fire()) | ~UIntToOH(in.bits.addr_beat)

  def dropPendingBitInternal[T <: HasL2BeatAddr] (in: ValidIO[T]) =
    ~Fill(in.bits.refillCycles, in.valid) | ~UIntToOH(in.bits.addr_beat)

  def addPendingBitWhenBeatHasPartialWritemask(in: DecoupledIO[AcquireFromSrc]): UInt = {
    val a = in.bits
    val isPartial = a.wmask() != Acquire.fullWriteMask
    addPendingBitWhenBeat(in.fire() && isPartial && Bool(ignoresWriteMask), a)
  }
}

class L2VoluntaryReleaseTracker(trackerId: Int) extends L2XactTracker {
  val io = new L2XactTrackerIO

  val s_idle :: s_meta_read :: s_meta_resp :: s_data_write :: s_meta_write :: s_inner_grant :: s_inner_finish :: Nil = Enum(UInt(), 7)
  val state = Reg(init=s_idle)

  val xact = Reg(Bundle(new ReleaseFromSrc, { case TLId => params(InnerTLId); case TLDataBits => 0 }))
  val xact_tag_match = Reg{ Bool() }
  val xact_old_meta = Reg{ new L2Metadata }
  val xact_way_en = Reg{ Bits(width = nWays) }
  val data_buffer = Vec.fill(innerDataBeats){ Reg(io.irel().data.clone) }
  val coh = xact_old_meta.coh

  val collect_irel_data = Reg(init=Bool(false))
  val irel_data_valid = Reg(init=Bits(0, width = innerDataBeats))
  val irel_data_done = connectIncomingDataBeatCounter(io.inner.release)
  val (write_data_cnt, write_data_done) = connectInternalDataBeatCounter(io.data.write)

  io.has_acquire_conflict := Bool(false)
  io.has_acquire_match := Bool(false)
  io.has_release_match := io.irel().isVoluntary()

  io.outer.acquire.valid := Bool(false)
  io.outer.probe.ready := Bool(false)
  io.outer.release.valid := Bool(false)
  io.outer.grant.ready := Bool(false)
  io.inner.acquire.ready := Bool(false)
  io.inner.probe.valid := Bool(false)
  io.inner.release.ready := Bool(false)
  io.inner.grant.valid := Bool(false)
  io.inner.finish.ready := Bool(false)

  io.inner.grant.bits := coh.inner.makeGrant(xact, UInt(trackerId))

  io.data.read.valid := Bool(false)
  io.data.write.valid := Bool(false)
  io.data.write.bits.id := UInt(trackerId)
  io.data.write.bits.way_en := xact_way_en
  io.data.write.bits.addr_idx := xact.addr_block(idxMSB,idxLSB)
  io.data.write.bits.addr_beat := write_data_cnt
  io.data.write.bits.wmask := SInt(-1)
  io.data.write.bits.data := data_buffer(write_data_cnt)
  io.meta.read.valid := Bool(false)
  io.meta.read.bits.id := UInt(trackerId)
  io.meta.read.bits.idx := xact.addr_block(idxMSB,idxLSB)
  io.meta.read.bits.tag := xact.addr_block >> UInt(idxBits)
  io.meta.write.valid := Bool(false)
  io.meta.write.bits.id := UInt(trackerId)
  io.meta.write.bits.idx := xact.addr_block(idxMSB,idxLSB)
  io.meta.write.bits.way_en := xact_way_en
  io.meta.write.bits.data.tag := xact.addr_block >> UInt(idxBits)
  io.meta.write.bits.data.coh.inner := xact_old_meta.coh.inner.onRelease(xact)
  io.meta.write.bits.data.coh.outer := xact_old_meta.coh.outer.onHit(M_XWR) // WB is a write
  io.wb.req.valid := Bool(false)

  when(collect_irel_data) {
    io.inner.release.ready := Bool(true)
    when(io.inner.release.valid) {
      data_buffer(io.irel().addr_beat) := io.irel().data
      irel_data_valid(io.irel().addr_beat) := Bool(true)
    }
    when(irel_data_done) { collect_irel_data := Bool(false) }
  }

  switch (state) {
    is(s_idle) {
      io.inner.release.ready := Bool(true)
      when( io.inner.release.valid ) {
        xact := io.irel()
        data_buffer(io.irel().addr_beat) := io.irel().data
        collect_irel_data := io.irel().hasMultibeatData()
        irel_data_valid := io.irel().hasData() << io.irel().addr_beat
        state := s_meta_read
      }
    }
    is(s_meta_read) {
      io.meta.read.valid := Bool(true)
      when(io.meta.read.ready) { state := s_meta_resp }
    }
    is(s_meta_resp) {
      when(io.meta.resp.valid) {
        xact_tag_match := io.meta.resp.bits.tag_match
        xact_old_meta := io.meta.resp.bits.meta
        xact_way_en := io.meta.resp.bits.way_en
        state := Mux(io.meta.resp.bits.tag_match, 
                   Mux(xact.hasData(), s_data_write, s_meta_write),
                   Mux(xact.requiresAck(), s_inner_grant, s_idle))
      }
    }
    is(s_data_write) {
      io.data.write.valid := !collect_irel_data || irel_data_valid(write_data_cnt)
      when(write_data_done) { state := s_meta_write }
    }
    is(s_meta_write) {
      io.meta.write.valid := Bool(true)
      when(io.meta.write.ready) { 
        state := Mux(xact.requiresAck(), s_inner_grant, s_idle) // Need a Grant.voluntaryAck?
      }
    }
    is(s_inner_grant) {
      io.inner.grant.valid := !collect_irel_data
      when(io.inner.grant.ready) { 
        state := Mux(io.ignt().requiresAck(), s_inner_finish, s_idle)
      }
    }
    is(s_inner_finish) {
      io.inner.finish.ready := Bool(true)
      when(io.inner.finish.valid) { state := s_idle }
    }
  }

  // Checks for illegal behavior
  assert(!(state === s_meta_resp && io.meta.resp.valid && !io.meta.resp.bits.tag_match),
    "VoluntaryReleaseTracker accepted Release for a block not resident in this cache!")
}


class L2AcquireTracker(trackerId: Int) extends L2XactTracker {
  val io = new L2XactTrackerIO

  val s_idle :: s_meta_read :: s_meta_resp :: s_wb_req :: s_wb_resp :: s_inner_probe :: s_outer_acquire :: s_busy :: s_meta_write :: Nil = Enum(UInt(), 9)
  val state = Reg(init=s_idle)

  // State holding transaction metadata
  val xact = Reg(Bundle(new AcquireFromSrc, { case TLId => params(InnerTLId) }))
  val data_buffer = Vec.fill(innerDataBeats){ Reg(init=UInt(0, width = innerDataBits)) }
  val wmask_buffer = Vec.fill(innerDataBeats){ Reg(init=UInt(0,width = innerDataBits/8)) }
  val xact_tag_match = Reg{ Bool() }
  val xact_way_en = Reg{ Bits(width = nWays) }
  val xact_old_meta = Reg{ new L2Metadata }
  val pending_coh = Reg{ xact_old_meta.coh.clone }

  // Secondary miss queue
  val ignt_q = Module(new Queue(new L2SecondaryMissInfo, nSecondaryMisses))(innerTLParams)
  ignt_q.io.enq.bits.client_xact_id := io.iacq().client_xact_id
  ignt_q.io.enq.bits.addr_beat := io.iacq().addr_beat
  // TODO add ignt.dst <- iacq.src

  // State holding progress made on processing this transaction
  val iacq_data_done = connectIncomingDataBeatCounter(io.inner.acquire)
  val pending_irels = connectTwoWayBeatCounter(
    max = io.inner.tlNCoherentClients,
    up = io.inner.probe,
    down = io.inner.release)._1
  val (pending_ognts, oacq_data_idx, oacq_data_done, ognt_data_idx, ognt_data_done) =
    connectTwoWayBeatCounter(
      max = 1,
      up = io.outer.acquire,
      down = io.outer.grant,
      beat = xact.addr_beat)
  val (ignt_data_idx, ignt_data_done) = connectOutgoingDataBeatCounter(io.inner.grant, ignt_q.io.deq.bits.addr_beat)
  val pending_ifins = connectTwoWayBeatCounter(
    max = nSecondaryMisses,
    up = io.inner.grant,
    down = io.inner.finish,
    track = (g: Grant) => g.requiresAck())._1
  val pending_puts = Reg(init=Bits(0, width = io.inner.tlDataBeats))
  val pending_iprbs = Reg(init = Bits(0, width = io.inner.tlNCoherentClients))
  val pending_reads = Reg(init=Bits(0, width = io.inner.tlDataBeats))
  val pending_writes = Reg(init=Bits(0, width = io.inner.tlDataBeats))
  val pending_resps = Reg(init=Bits(0, width = io.inner.tlDataBeats))
  val pending_ignt_data = Reg(init=Bits(0, width = io.inner.tlDataBeats))
  val pending_meta_write = Reg{ Bool() }

  val all_pending_done =
    !(pending_reads.orR ||
      pending_writes.orR ||
      pending_resps.orR ||
      pending_puts.orR ||
      pending_ognts ||
      ignt_q.io.count > UInt(0) ||
      //pending_meta_write || // Has own state: s_meta_write
      pending_ifins)

  // Provide a single ALU per tracker to merge Puts and AMOs with data being
  // refilled, written back, or extant in the cache
  val amoalu = Module(new AMOALU)
  amoalu.io.addr := xact.addr()
  amoalu.io.cmd := xact.op_code()
  amoalu.io.typ := xact.op_size()
  amoalu.io.lhs := io.data.resp.bits.data // default, overwritten by calls to mergeData
  amoalu.io.rhs := data_buffer.head       // default, overwritten by calls to mergeData
  val amo_result = xact.data // Reuse xact buffer space to store AMO result

  // Utility functions for updating the data and metadata that will be kept in
  // the cache or granted to the original requestor after this transaction:

  def updatePendingCohWhen(flag: Bool, next: HierarchicalMetadata) {
    when(flag && pending_coh != next) {
      pending_meta_write := Bool(true)
      pending_coh := next
    }
  }

  def mergeData(dataBits: Int)(beat: UInt, incoming: UInt) {
    val old_data = incoming     // Refilled, written back, or de-cached data
    val new_data = data_buffer(beat) // Newly Put data is already in the buffer
    amoalu.io.lhs := old_data >> xact.amo_shift_bits()
    amoalu.io.rhs := new_data >> xact.amo_shift_bits()
    val wmask = FillInterleaved(8, wmask_buffer(beat))
    data_buffer(beat) := ~wmask & old_data |
                          wmask & Mux(xact.isBuiltInType(Acquire.putAtomicType),
                                        amoalu.io.out << xact.amo_shift_bits(),
                                        new_data)
    wmask_buffer(beat) := SInt(-1)
    when(xact.is(Acquire.putAtomicType) && xact.addr_beat === beat) { amo_result := old_data }
  }
  def mergeDataInternal[T <: HasL2Data with HasL2BeatAddr](in: ValidIO[T]) {
    when(in.valid) { mergeData(rowBits)(in.bits.addr_beat, in.bits.data) }
  }
  def mergeDataInner[T <: HasTileLinkData with HasTileLinkBeatId](in: DecoupledIO[T]) {
    when(in.fire() && in.bits.hasData()) { 
      mergeData(innerDataBits)(in.bits.addr_beat, in.bits.data)
    }
  }
  def mergeDataOuter[T <: HasTileLinkData with HasTileLinkBeatId](in: DecoupledIO[T]) {
    when(in.fire() && in.bits.hasData()) { 
      mergeData(outerDataBits)(in.bits.addr_beat, in.bits.data)
    }
  }

  // Actual transaction processing logic begins here:
  //
  // First, take care of accpeting new requires or secondary misses
  // For now, the only allowed secondary miss types are Gets-under-Get
  // and Puts-under-Put from the same client
  val can_merge_iacq_get = (xact.isBuiltInType(Acquire.getType) &&
                               io.iacq().isBuiltInType(Acquire.getType)) &&
                           xact.client_id === io.iacq().client_id && //TODO remove
                           xact.conflicts(io.iacq()) &&
                           state != s_idle && state != s_meta_write &&
                           !all_pending_done &&
                           xact.allocate() &&
                           !io.inner.release.fire() &&
                           !io.outer.grant.fire() &&
                           !io.data.resp.valid &&
                           ignt_q.io.enq.ready

  // This logic also allows the tail beats of a PutBlock to be merged in
  val can_merge_iacq_put = ((xact.isBuiltInType(Acquire.putType) &&
                               io.iacq().isBuiltInType(Acquire.putType)) ||
                             (xact.isBuiltInType(Acquire.putBlockType) &&
                               io.iacq().isBuiltInType(Acquire.putBlockType))) &&
                           xact.client_id === io.iacq().client_id && //TODO remove
                           xact.conflicts(io.iacq()) &&
                           state != s_idle && state != s_meta_write &&
                           !all_pending_done &&
                           xact.allocate() &&
                           !io.inner.release.fire() &&
                           !io.outer.grant.fire() &&
                           !io.data.resp.valid &&
                           ignt_q.io.enq.ready

  io.inner.acquire.ready := state === s_idle ||
                              can_merge_iacq_put ||
                              can_merge_iacq_get

  // Track whether any beats are missing from a PutBlock
  pending_puts := (pending_puts & dropPendingBitWhenBeatHasData(io.inner.acquire))

  // Track which clients yet need to be probed and make Probe message
  pending_iprbs := pending_iprbs & dropPendingBitAtDest(io.inner.probe)
  val curr_probe_dst = PriorityEncoder(pending_iprbs)
  io.inner.probe.valid := state === s_inner_probe && pending_iprbs.orR
  io.inner.probe.bits := pending_coh.inner.makeProbe(curr_probe_dst, xact)

  // Handle incoming releases from clients, which may reduce sharer counts
  // and/or write back dirty data
  io.inner.release.ready := state === s_inner_probe
  val pending_coh_on_irel = HierarchicalMetadata(
                              pending_coh.inner.onRelease(io.irel()), // Drop sharer
                              Mux(io.irel().hasData(),     // Dirty writeback
                                pending_coh.outer.onHit(M_XWR),
                                pending_coh.outer))
  updatePendingCohWhen(io.inner.release.fire(), pending_coh_on_irel)
  mergeDataInner(io.inner.release)

  // Handle misses or coherence permission upgrades by initiating a new transaction in the outer memory:
  //
  // If we're allocating in this cache, we can use the current metadata
  // to make an appropriate custom Acquire, otherwise we copy over the
  // built-in Acquire from the inner TL to the outer TL
  io.outer.acquire.valid := state === s_outer_acquire
  io.outer.acquire.bits := Mux(
      xact.allocate(),
      xact_old_meta.coh.outer.makeAcquire(
        client_xact_id = UInt(0),
        addr_block = xact.addr_block,
        op_code = xact.op_code()),
      Bundle(Acquire(xact))(outerTLParams))

  // Probes from the outer memory system are handled in the WritebackUnit
  io.outer.probe.ready := Bool(false)
  io.outer.release.valid := Bool(false)

  io.outer.grant.ready := state === s_busy
  val pending_coh_on_ognt = HierarchicalMetadata(
                              ManagerMetadata.onReset,
                              pending_coh.outer.onGrant(io.outer.grant.bits, xact.op_code()))
  updatePendingCohWhen(ognt_data_done, pending_coh_on_ognt)
  mergeDataOuter(io.outer.grant)

  // Going back to the original inner transaction, we can issue a Grant as
  // soon as the data is released, granted, put, or read from the cache
  pending_ignt_data := pending_ignt_data |
                       addPendingBitWhenBeatHasData(io.inner.release) |
                       addPendingBitWhenBeatHasData(io.outer.grant) |
                       addPendingBitInternal(io.data.resp)
  ignt_q.io.enq.valid := iacq_data_done
  ignt_q.io.deq.ready := ignt_data_done
  // Make the Grant message using the data stored in the secondary miss queue
  io.inner.grant.valid := state === s_busy &&
                          ignt_q.io.deq.valid &&
                          (!io.ignt().hasData() ||
                            pending_ignt_data(ignt_data_idx))
  io.inner.grant.bits := pending_coh.inner.makeGrant(
                           dst = xact.client_id, // TODO: ignt_q.io.deq.bits.src 
                           acq = xact,
                           client_xact_id = ignt_q.io.deq.bits.client_xact_id,
                           manager_xact_id = UInt(trackerId), 
                           addr_beat = ignt_data_idx,
                           data = Mux(xact.is(Acquire.putAtomicType),
                                    amo_result,
                                    data_buffer(ignt_data_idx)))

  val pending_coh_on_ignt = HierarchicalMetadata(
                              pending_coh.inner.onGrant(io.ignt()),
                              Mux(ognt_data_done,
                                pending_coh_on_ognt.outer,
                                pending_coh.outer))
  updatePendingCohWhen(io.inner.grant.fire(), pending_coh_on_ignt)

  // We must wait for as many Finishes as we sent Grants
  io.inner.finish.ready := state === s_busy

  // We read from the the cache at this level if data wasn't written back or refilled.
  // We may merge Gets, requiring further beats to be read.
  // If ECC requires a full writemask, we'll read out data on partial writes as well.
  pending_reads := (pending_reads &
                       dropPendingBit(io.data.read) &
                       dropPendingBitWhenBeatHasData(io.inner.release) &
                       dropPendingBitWhenBeatHasData(io.outer.grant)) |
                     addPendingBitWhenBeatIsGetOrAtomic(io.inner.acquire) |
                     addPendingBitWhenBeatHasPartialWritemask(io.inner.acquire)
  val curr_read_beat = PriorityEncoder(pending_reads)
  io.data.read.valid := state === s_busy &&
                          pending_reads.orR &&
                          !pending_ognts
  io.data.read.bits.id := UInt(trackerId)
  io.data.read.bits.way_en := xact_way_en
  io.data.read.bits.addr_idx := xact.addr_block(idxMSB,idxLSB)
  io.data.read.bits.addr_beat := curr_read_beat

  pending_resps := (pending_resps & dropPendingBitInternal(io.data.resp)) |
                     addPendingBitInternal(io.data.read)
  mergeDataInternal(io.data.resp)

  // We write data to the cache at this level if it was Put here with allocate flag,
  // written back dirty, or refilled from outer memory.
  pending_writes := (pending_writes & dropPendingBit(io.data.write)) |
                      addPendingBitWhenBeatHasData(io.inner.acquire) |
                      addPendingBitWhenBeatHasData(io.inner.release) |
                      addPendingBitWhenBeatHasData(io.outer.grant)
  val curr_write_beat = PriorityEncoder(pending_writes)
  io.data.write.valid := state === s_busy &&
                           pending_writes.orR &&
                           !pending_ognts &&
                           !pending_reads(curr_write_beat) &&
                           !pending_resps(curr_write_beat)
  io.data.write.bits.id := UInt(trackerId)
  io.data.write.bits.way_en := xact_way_en
  io.data.write.bits.addr_idx := xact.addr_block(idxMSB,idxLSB)
  io.data.write.bits.addr_beat := curr_write_beat
  io.data.write.bits.wmask := wmask_buffer(curr_write_beat)
  io.data.write.bits.data := data_buffer(curr_write_beat)

  io.meta.read.valid := state === s_meta_read
  io.meta.read.bits.id := UInt(trackerId)
  io.meta.read.bits.idx := xact.addr_block(idxMSB,idxLSB)
  io.meta.read.bits.tag := xact.addr_block >> UInt(idxBits)

  io.meta.write.valid := state === s_meta_write
  io.meta.write.bits.id := UInt(trackerId)
  io.meta.write.bits.idx := xact.addr_block(idxMSB,idxLSB)
  io.meta.write.bits.way_en := xact_way_en
  io.meta.write.bits.data.tag := xact.addr_block >> UInt(idxBits)
  io.meta.write.bits.data.coh := pending_coh
                                        
  io.wb.req.valid := state === s_wb_req
  io.wb.req.bits.addr_block := Cat(xact_old_meta.tag, xact.addr_block(idxMSB,idxLSB))
  io.wb.req.bits.coh := xact_old_meta.coh
  io.wb.req.bits.way_en := xact_way_en
  io.wb.req.bits.id := UInt(trackerId)

  // Handling of secondary misses (Gets and Puts only for now)
  when(io.inner.acquire.fire() && io.iacq().hasData()) { // state <= s_meta_wrtie
    val beat = io.iacq().addr_beat
    val wmask = io.iacq().wmask()
    val full = FillInterleaved(8, wmask)
    data_buffer(beat) := (~full & data_buffer(beat)) | (full & io.iacq().data)
    wmask_buffer(beat) := wmask | Mux(state === s_idle, Bits(0), wmask_buffer(beat))
  }

  // Defined here because of Chisel default wire demands, used in s_meta_resp
  val pending_coh_on_hit = HierarchicalMetadata(
    io.meta.resp.bits.meta.coh.inner,
    io.meta.resp.bits.meta.coh.outer.onHit(xact.op_code()))

  // State machine updates and transaction handler metadata intialization
  when(state === s_idle && io.inner.acquire.valid) {
    xact := io.iacq()
    xact.data := UInt(0)
    pending_puts := Mux( // Make sure to collect all data from a PutBlock
      io.iacq().isBuiltInType(Acquire.putBlockType),
      dropPendingBitWhenBeatHasData(io.inner.acquire),
      UInt(0))
    pending_reads := Mux( // GetBlocks and custom types read all beats
      io.iacq().isBuiltInType(Acquire.getBlockType) || !io.iacq().isBuiltInType(),
      SInt(-1, width = innerDataBeats),
      (addPendingBitWhenBeatIsGetOrAtomic(io.inner.acquire) | 
        addPendingBitWhenBeatHasPartialWritemask(io.inner.acquire)).toUInt)
    pending_writes := addPendingBitWhenBeatHasData(io.inner.acquire)
    pending_resps := UInt(0)
    pending_ignt_data := UInt(0)
    pending_meta_write := UInt(0)
    state := s_meta_read
  }
  when(state === s_meta_read && io.meta.read.ready) { state := s_meta_resp }
  when(state === s_meta_resp && io.meta.resp.valid) {
    xact_tag_match := io.meta.resp.bits.tag_match
    xact_old_meta := io.meta.resp.bits.meta
    xact_way_en := io.meta.resp.bits.way_en
    val coh = io.meta.resp.bits.meta.coh
    val tag_match = io.meta.resp.bits.tag_match
    val is_hit = (if(!isLastLevelCache) tag_match && coh.outer.isHit(xact.op_code())
                  else xact.isBuiltInType(Acquire.putBlockType) ||
                       tag_match && coh.outer.isValid())
    val needs_writeback = !tag_match &&
                          xact.allocate() && 
                          (coh.outer.requiresVoluntaryWriteback() ||
                             coh.inner.requiresProbesOnVoluntaryWriteback())
    val needs_inner_probes = tag_match && coh.inner.requiresProbes(xact)
    when(!tag_match || is_hit && pending_coh_on_hit != coh) { pending_meta_write := Bool(true) }
    pending_coh := Mux(is_hit, pending_coh_on_hit, coh)
    when(needs_inner_probes) {
      val full_sharers = coh.inner.full()
      val mask_self = Mux(
        xact.requiresSelfProbe(),
        coh.inner.full() | UIntToOH(xact.client_id),
        coh.inner.full() & ~UIntToOH(xact.client_id))
      val mask_incoherent = mask_self & ~io.incoherent.toBits
      pending_iprbs := mask_incoherent
    } 
    state := Mux(needs_writeback, s_wb_req,
               Mux(needs_inner_probes, s_inner_probe,
                  Mux(!is_hit, s_outer_acquire, s_busy)))
  }
  when(state === s_wb_req && io.wb.req.ready) { state := s_wb_resp }
  when(state === s_wb_resp && io.wb.resp.valid) {
    // If we're overwriting the whole block in a last level cache we can
    // just do it without fetching any data from memory
    val skip_outer_acquire = Bool(isLastLevelCache) && xact.isBuiltInType(Acquire.putBlockType)
    state := Mux(!skip_outer_acquire, s_outer_acquire, s_busy)
  }
  when(state === s_inner_probe && !(pending_iprbs.orR || pending_irels)) {
    // Tag matches, so if this is the last level cache we can use the data without upgrading permissions
    val skip_outer_acquire = 
      (if(!isLastLevelCache) xact_old_meta.coh.outer.isHit(xact.op_code())
       else xact.isBuiltInType(Acquire.putBlockType) || xact_old_meta.coh.outer.isValid())
    state := Mux(!skip_outer_acquire, s_outer_acquire, s_busy)
  }
  when(state === s_outer_acquire && oacq_data_done) { state := s_busy }
  when(state === s_busy && all_pending_done) { state := s_meta_write  }
  when(state === s_meta_write && (io.meta.write.ready || !pending_meta_write)) {
    wmask_buffer.foreach { w => w := UInt(0) }
    state := s_idle
  }

  // These IOs are used for routing in the parent
  val in_same_set = xact.addr_block(idxMSB,idxLSB) === io.iacq().addr_block(idxMSB,idxLSB)
  io.has_release_match := xact.conflicts(io.irel()) &&
                          !io.irel().isVoluntary() &&
                          (state === s_inner_probe)
  io.has_acquire_match := can_merge_iacq_put || can_merge_iacq_get
  io.has_acquire_conflict := in_same_set && (state != s_idle) && !io.has_acquire_match
  //TODO: relax from in_same_set to xact.conflicts(io.iacq())?

  // Checks for illegal behavior
  assert(!(state != s_idle && io.inner.acquire.fire() &&
    io.inner.acquire.bits.client_id != xact.client_id),
    "AcquireTracker accepted data beat from different network source than initial request.")
}

class L2WritebackReq extends L2HellaCacheBundle
    with HasL2Id {
  val addr_block = UInt(width = blockAddrBits) // TODO: assumes same block size
  val coh = new HierarchicalMetadata
  val way_en = Bits(width = nWays)
}

class L2WritebackResp extends L2HellaCacheBundle with HasL2Id

class L2WritebackIO extends L2HellaCacheBundle {
  val req = Decoupled(new L2WritebackReq)
  val resp = Valid(new L2WritebackResp).flip
}

class L2WritebackUnitIO extends HierarchicalXactTrackerIO {
  val wb = new L2WritebackIO().flip
  val data = new L2DataRWIO
}

class L2WritebackUnit(trackerId: Int) extends L2XactTracker {
  val io = new L2WritebackUnitIO

  val s_idle :: s_inner_probe :: s_data_read :: s_data_resp :: s_outer_release :: s_outer_grant :: s_wb_resp :: Nil = Enum(UInt(), 7)
  val state = Reg(init=s_idle)

  val xact_addr_block = Reg(io.wb.req.bits.addr_block.clone)
  val xact_coh = Reg{ new HierarchicalMetadata }
  val xact_way_en = Reg{ Bits(width = nWays) }
  val data_buffer = Vec.fill(innerDataBeats){ Reg(io.irel().data.clone) }
  val xact_id = Reg{ UInt() }

  val irel_had_data = Reg(init = Bool(false))
  val irel_cnt = Reg(init = UInt(0, width = log2Up(io.inner.tlNCoherentClients+1)))
  val pending_probes = Reg(init = Bits(0, width = io.inner.tlNCoherentClients))
  val curr_probe_dst = PriorityEncoder(pending_probes)
  val full_sharers = io.wb.req.bits.coh.inner.full()
  val mask_incoherent = full_sharers & ~io.incoherent.toBits

  val irel_data_done = connectIncomingDataBeatCounter(io.inner.release)
  val (orel_data_cnt, orel_data_done) = connectOutgoingDataBeatCounter(io.outer.release)
  val (read_data_cnt, read_data_done) = connectInternalDataBeatCounter(io.data.read)
  val resp_data_done = connectInternalDataBeatCounter(io.data.resp)

  val pending_icoh_on_irel = xact_coh.inner.onRelease(io.irel())
  val pending_ocoh_on_irel = xact_coh.outer.onHit(M_XWR) // WB is a write
  val pending_ofin_on_ognt = io.ognt().makeFinish()

  io.has_acquire_conflict := Bool(false)
  io.has_acquire_match := Bool(false)
  io.has_release_match := !io.irel().isVoluntary() &&
                            io.irel().conflicts(xact_addr_block) &&
                            (state === s_inner_probe)

  io.outer.acquire.valid := Bool(false)
  io.outer.probe.ready := Bool(false)
  io.outer.release.valid := Bool(false) // default
  io.outer.release.bits := xact_coh.outer.makeVoluntaryWriteback(
                             client_xact_id = UInt(trackerId),
                             addr_block = xact_addr_block,
                             addr_beat = orel_data_cnt,
                             data = data_buffer(orel_data_cnt))
  io.outer.grant.ready := Bool(false) // default

  io.inner.probe.valid := Bool(false)
  io.inner.probe.bits := xact_coh.inner.makeProbeForVoluntaryWriteback(curr_probe_dst, xact_addr_block)

  io.inner.grant.valid := Bool(false)
  io.inner.acquire.ready := Bool(false)
  io.inner.release.ready := Bool(false)
  io.inner.finish.ready := Bool(false)

  io.data.read.valid := Bool(false)
  io.data.read.bits.id := UInt(trackerId)
  io.data.read.bits.way_en := xact_way_en
  io.data.read.bits.addr_idx := xact_addr_block(idxMSB,idxLSB)
  io.data.read.bits.addr_beat := read_data_cnt
  io.data.write.valid := Bool(false)

  io.wb.req.ready := Bool(false)
  io.wb.resp.valid := Bool(false)
  io.wb.resp.bits.id := xact_id

  switch (state) {
    is(s_idle) {
      io.wb.req.ready := Bool(true)
      when(io.wb.req.valid) {
        xact_addr_block := io.wb.req.bits.addr_block
        xact_coh := io.wb.req.bits.coh
        xact_way_en := io.wb.req.bits.way_en
        xact_id := io.wb.req.bits.id
        irel_had_data := Bool(false)
        val needs_inner_probes = io.wb.req.bits.coh.inner.requiresProbesOnVoluntaryWriteback()
        when(needs_inner_probes) {
          pending_probes := mask_incoherent
          irel_cnt := PopCount(mask_incoherent)
        }
        state := Mux(needs_inner_probes, s_inner_probe, s_data_read)
      }
    }
    is(s_inner_probe) {
      // Send probes
      io.inner.probe.valid := pending_probes != UInt(0)
      when(io.inner.probe.ready) {
        pending_probes := pending_probes & ~UIntToOH(curr_probe_dst)
      }
      // Handle releases, which may have data being written back
      io.inner.release.ready := Bool(true)
      when(io.inner.release.valid) {
        xact_coh.inner := pending_icoh_on_irel
        // Handle released dirty data
        when(io.irel().hasData()) {
          irel_had_data := Bool(true)
          xact_coh.outer := pending_ocoh_on_irel
          data_buffer(io.irel().addr_beat) := io.irel().data
        }
        // We don't decrement irel_cnt until we've received all the data beats.
        when(!io.irel().hasData() || irel_data_done) {
          irel_cnt := irel_cnt - UInt(1)
        }
      }
      when(irel_cnt === UInt(0)) {
        state := Mux(irel_had_data, // If someone released a dirty block
                   s_outer_release, // write that block back, otherwise
                   Mux(xact_coh.outer.requiresVoluntaryWriteback(),
                     s_data_read,   // write extant dirty data back, or just
                     s_wb_resp))    // drop a clean block after collecting acks
      }
    }
    is(s_data_read) {
      io.data.read.valid := Bool(true)
      when(io.data.resp.valid) { data_buffer(io.data.resp.bits.addr_beat) := io.data.resp.bits.data }
      when(read_data_done) { state := s_data_resp }
    }
    is(s_data_resp) {
      when(io.data.resp.valid) { data_buffer(io.data.resp.bits.addr_beat) := io.data.resp.bits.data }
      when(resp_data_done) { state := s_outer_release }
    }
    is(s_outer_release) {
      io.outer.release.valid := Bool(true)
      when(orel_data_done) {
        state := Mux(io.orel().requiresAck(), s_outer_grant, s_wb_resp)
      }
    }
    is(s_outer_grant) {
      io.outer.grant.ready := Bool(true)
      when(io.outer.grant.valid) { 
        state := s_wb_resp
      }
    }
    is(s_wb_resp) {
      io.wb.resp.valid := Bool(true)
      state := s_idle
    }
  }
}
