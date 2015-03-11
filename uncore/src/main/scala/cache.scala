// See LICENSE for license details.

package uncore
import Chisel._

case object CacheName extends Field[String]
case object NSets extends Field[Int]
case object NWays extends Field[Int]
case object RowBits extends Field[Int]
case object Replacer extends Field[() => ReplacementPolicy]
case object AmoAluOperandBits extends Field[Int]
case object L2DirectoryRepresentation extends Field[DirectoryRepresentation]
case object CacheBlockBytes extends Field[Int]
case object CacheBlockOffsetBits extends Field[Int]

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
}

abstract class L2HellaCacheBundle extends Bundle with L2HellaCacheParameters
abstract class L2HellaCacheModule extends Module with L2HellaCacheParameters

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

class L2HellaCacheBank(bankId: Int) extends HierarchicalCoherenceAgent
    with L2HellaCacheParameters {
  require(isPow2(nSets))
  require(isPow2(nWays)) 

  val tshrfile = Module(new TSHRFile(bankId))
  val meta = Module(new L2MetadataArray)
  val data = Module(new L2DataArray(1))

  tshrfile.io.inner <> io.inner
  tshrfile.io.meta <> meta.io
  tshrfile.io.data <> data.io
  io.outer <> tshrfile.io.outer
  io.incoherent <> tshrfile.io.incoherent
}

class TSHRFileIO extends HierarchicalTLIO {
  val meta = new L2MetaRWIO
  val data = new L2DataRWIO
}

class TSHRFile(bankId: Int) extends L2HellaCacheModule 
    with HasCoherenceAgentWiringHelpers {
  val io = new TSHRFileIO

  // Create TSHRs for outstanding transactions
  val trackerList = (0 until nReleaseTransactors).map { id => 
    Module(new L2VoluntaryReleaseTracker(id, bankId)) 
  } ++ (nReleaseTransactors until nTransactors).map { id => 
    Module(new L2AcquireTracker(id, bankId))
  }
  
  val wb = Module(new L2WritebackUnit(nTransactors, bankId))
  doOutputArbitration(wb.io.wb.req, trackerList.map(_.io.wb.req))
  doInputRouting(wb.io.wb.resp, trackerList.map(_.io.wb.resp))

  // Propagate incoherence flags
  (trackerList.map(_.io.incoherent) :+ wb.io.incoherent).map( _ := io.incoherent.toBits)

  // Handle acquire transaction initiation
  val acquire = io.inner.acquire
  val alloc_arb = Module(new Arbiter(Bool(), trackerList.size))
  val acquireList =  trackerList.map(_.io.inner.acquire)
  val acquireMatchList = trackerList.map(_.io.has_acquire_match)
  val any_acquire_matches = acquireMatchList.reduce(_||_)
  val alloc_idx = Vec(alloc_arb.io.in.map(_.ready)).lastIndexWhere{b: Bool => b}
  val match_idx = Vec(acquireMatchList).indexWhere{b: Bool => b}
  val acquire_idx = Mux(any_acquire_matches, match_idx, alloc_idx)
  acquireList.zip(alloc_arb.io.in).zipWithIndex.map { case((acq, arb), i) =>
    arb.valid := acq.ready
    acq.bits := acquire.bits
    acq.valid := arb.ready && (acquire_idx === UInt(i))
  }
  val block_acquires = trackerList.map(_.io.has_acquire_conflict).reduce(_||_)
  acquire.ready := acquireList.map(_.ready).reduce(_||_) && !block_acquires
  alloc_arb.io.out.ready := acquire.valid && !block_acquires

  // Wire releases from clients
  val release = io.inner.release
  val release_idx = Vec(trackerList.map(_.io.has_release_match) :+
                          wb.io.has_release_match).indexWhere{b: Bool => b}
  val releaseList = trackerList.map(_.io.inner.release) :+ wb.io.inner.release
  releaseList.zipWithIndex.map { case(r, i) =>
    r.bits := release.bits
    r.valid := release.valid && (release_idx === UInt(i))
  }
  release.ready := Vec(releaseList.map(_.ready)).read(release_idx)

  // Wire probe requests and grant reply to clients, finish acks from clients
  doOutputArbitration(io.inner.probe, trackerList.map(_.io.inner.probe) :+ wb.io.inner.probe)
  doOutputArbitration(io.inner.grant, trackerList.map(_.io.inner.grant))
  doInputRouting(io.inner.finish, trackerList.map(_.io.inner.finish))

  // Create an arbiter for the one memory port
  val outerList = trackerList.map(_.io.outer) :+ wb.io.outer
  val outer_arb = Module(new TileLinkIOArbiterThatPassesId(outerList.size))(outerTLParams)
  outerList zip outer_arb.io.in map { case(out, arb) => out <> arb }
  io.outer <> outer_arb.io.out

  // Wire local memories
  doOutputArbitration(io.meta.read, trackerList.map(_.io.meta.read))
  doOutputArbitration(io.meta.write, trackerList.map(_.io.meta.write))
  doOutputArbitration(io.data.read, trackerList.map(_.io.data.read) :+ wb.io.data.read)
  doOutputArbitration(io.data.write, trackerList.map(_.io.data.write))
  doInputRouting(io.meta.resp, trackerList.map(_.io.meta.resp))
  doInputRouting(io.data.resp, trackerList.map(_.io.data.resp) :+ wb.io.data.resp)
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
}

class L2VoluntaryReleaseTracker(trackerId: Int, bankId: Int) extends L2XactTracker {
  val io = new L2XactTrackerIO

  val s_idle :: s_meta_read :: s_meta_resp :: s_data_write :: s_meta_write :: s_inner_grant :: s_inner_finish :: Nil = Enum(UInt(), 7)
  val state = Reg(init=s_idle)

  val xact_src = Reg(io.inner.release.bits.header.src.clone)
  val xact = Reg(Bundle(new Release, { case TLId => params(InnerTLId); case TLDataBits => 0 }))
  val xact_tag_match = Reg{ Bool() }
  val xact_meta = Reg{ new L2Metadata }
  val xact_way_en = Reg{ Bits(width = nWays) }
  val data_buffer = Vec.fill(innerDataBeats){ Reg(io.irel().data.clone) }
  val coh = xact_meta.coh

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
  io.outer.finish.valid := Bool(false)
  io.inner.acquire.ready := Bool(false)
  io.inner.probe.valid := Bool(false)
  io.inner.release.ready := Bool(false)
  io.inner.grant.valid := Bool(false)
  io.inner.finish.ready := Bool(false)

  io.inner.grant.bits.header.src := UInt(bankId)
  io.inner.grant.bits.header.dst := xact_src
  io.inner.grant.bits.payload := coh.inner.makeGrant(xact, UInt(trackerId))

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
  io.meta.write.bits.data.coh.inner := xact_meta.coh.inner.onRelease(xact, xact_src)
  io.meta.write.bits.data.coh.outer := xact_meta.coh.outer.onHit(M_XWR) // WB is a write
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
        xact_src := io.inner.release.bits.header.src
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
        xact_meta := io.meta.resp.bits.meta
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
      io.inner.grant.valid := Bool(true)
      when(io.inner.grant.ready) { 
        state := Mux(io.ignt().requiresAck(), s_inner_finish, s_idle)
      }
    }
    is(s_inner_finish) {
      io.inner.finish.ready := Bool(true)
      when(io.inner.finish.valid) { state := s_idle }
    }
  }
}

class L2AcquireTracker(trackerId: Int, bankId: Int) extends L2XactTracker {
  val io = new L2XactTrackerIO

  val s_idle :: s_meta_read :: s_meta_resp :: s_wb_req :: s_wb_resp :: s_probe :: s_outer_acquire :: s_outer_grant :: s_outer_finish :: s_data_read :: s_data_resp :: s_data_write :: s_inner_grant :: s_meta_write :: s_inner_finish :: Nil = Enum(UInt(), 15)
  val state = Reg(init=s_idle)

  val xact_src = Reg(io.inner.acquire.bits.header.src.clone)
  val xact = Reg(Bundle(new Acquire, { case TLId => params(InnerTLId); case TLDataBits => 0 }))
  val data_buffer = Vec.fill(innerDataBeats) {
    Reg(io.iacq().data.clone) 
  }
  val amo_result = Reg(io.iacq().data.clone)
  val xact_tag_match = Reg{ Bool() }
  val xact_meta = Reg{ new L2Metadata }
  val xact_way_en = Reg{ Bits(width = nWays) }
  val pending_coh = Reg{ xact_meta.coh.clone }
  val pending_finish = Reg{ io.outer.finish.bits.clone }

  val is_hit = xact_tag_match && xact_meta.coh.outer.isHit(xact.op_code())
  val do_allocate = xact.allocate()
  val needs_writeback = !xact_tag_match && do_allocate && 
                          (xact_meta.coh.outer.requiresVoluntaryWriteback() ||
                           xact_meta.coh.inner.requiresProbesOnVoluntaryWriteback())

  val pending_coh_on_hit = HierarchicalMetadata(
                              io.meta.resp.bits.meta.coh.inner,
                              io.meta.resp.bits.meta.coh.outer.onHit(xact.op_code()))
  val pending_icoh_on_irel = pending_coh.inner.onRelease(
                                incoming = io.irel(), 
                                src = io.inner.release.bits.header.src)
  val pending_ocoh_on_irel = pending_coh.outer.onHit(M_XWR) // WB is a write
  val pending_coh_on_ognt = HierarchicalMetadata(
                              ManagerMetadata.onReset,
                              pending_coh.outer.onGrant(io.ognt(), xact.op_code()))
  val pending_coh_on_ignt = HierarchicalMetadata(
                              pending_coh.inner.onGrant(
                                outgoing = io.ignt(),
                                dst = io.inner.grant.bits.header.dst),
                              pending_coh.outer)

  val release_count = Reg(init = UInt(0, width = log2Up(nClients+1)))
  val pending_probes = Reg(init = Bits(0, width = nClients))
  val curr_p_id = PriorityEncoder(pending_probes)
  val full_sharers = io.meta.resp.bits.meta.coh.inner.full()
  val mask_self = Mux(xact.requiresSelfProbe(),
                    full_sharers | (UInt(1) << xact_src),
                    full_sharers & ~UInt(UInt(1) << xact_src, width = nClients))
  val mask_incoherent = mask_self & ~io.incoherent.toBits

  val collect_iacq_data = Reg(init=Bool(false))
  val iacq_data_valid = Reg(init=Bits(0, width = innerDataBeats))
  val irel_had_data = Reg(init = Bool(false))
  val ognt_had_data = Reg(init = Bool(false))
  val iacq_data_done = connectIncomingDataBeatCounter(io.inner.acquire)
  val irel_data_done = connectIncomingDataBeatCounter(io.inner.release)
  val ognt_data_done = connectIncomingDataBeatCounter(io.outer.grant)
  val (ignt_data_cnt, ignt_data_done) = connectOutgoingDataBeatCounter(io.inner.grant, xact.addr_beat)
  val (oacq_data_cnt, oacq_data_done) = connectOutgoingDataBeatCounter(io.outer.acquire, xact.addr_beat)
  val (read_data_cnt, read_data_done) = connectInternalDataBeatCounter(io.data.read, xact.addr_beat, !xact.isSubBlockType())
  val (write_data_cnt, write_data_done) = connectInternalDataBeatCounter(io.data.write, xact.addr_beat, !xact.isSubBlockType() || ognt_had_data || irel_had_data)
  val resp_data_done = connectInternalDataBeatCounter(io.data.resp, !xact.isSubBlockType())

  val amoalu = Module(new AMOALU)
  amoalu.io.addr := xact.addr()
  amoalu.io.cmd := xact.op_code()
  amoalu.io.typ := xact.op_size()
  amoalu.io.lhs := io.data.resp.bits.data //default
  amoalu.io.rhs := data_buffer.head // default

  def mergeData[T <: HasTileLinkData]
      (byteAddrBits: Int, dataBits: Int)
      (buffer: Vec[UInt], beat: UInt, incoming: UInt) {
    val old_data = incoming
    val new_data = buffer(beat)
    val amoOpSz = UInt(amoAluOperandBits)
    val offset = xact.addr_byte()(byteAddrBits-1, log2Up(amoAluOperandBits/8))
    amoalu.io.lhs := old_data >> offset*amoOpSz
    amoalu.io.rhs := new_data >> offset*amoOpSz
    val valid_beat = xact.addr_beat === beat
    val wmask = Fill(dataBits, valid_beat) &
      Mux(xact.is(Acquire.putAtomicType), 
        FillInterleaved(amoAluOperandBits, UIntToOH(offset)),
        Mux(xact.is(Acquire.putBlockType) || xact.is(Acquire.putType),
          FillInterleaved(8, xact.write_mask()), 
          UInt(0, width = dataBits)))
    buffer(beat) := ~wmask & old_data | wmask & 
      Mux(xact.is(Acquire.putAtomicType), amoalu.io.out << offset*amoOpSz, new_data)
    when(xact.is(Acquire.putAtomicType) && valid_beat) { amo_result := old_data }
  }
  val mergeDataInternal = mergeData(log2Up(rowBits/8), rowBits) _
  val mergeDataInner = mergeData(innerByteAddrBits, innerDataBits) _
  val mergeDataOuter = mergeData(outerByteAddrBits, outerDataBits) _

  //TODO: Allow hit under miss for stores
  val in_same_set = xact.addr_block(idxMSB,idxLSB) === 
                      io.iacq().addr_block(idxMSB,idxLSB)
  io.has_acquire_conflict := (xact.conflicts(io.iacq()) || in_same_set) &&
                              (state != s_idle) &&
                              !collect_iacq_data
  io.has_acquire_match := xact.conflicts(io.iacq()) &&
                              collect_iacq_data
  io.has_release_match := !io.irel().isVoluntary() &&
                            (xact.addr_block === io.irel().addr_block) &&
                            (state === s_probe)

  // If we're allocating in this cache, we can use the current metadata
  // to make an appropriate custom Acquire, otherwise we copy over the
  // built-in Acquire from the inner TL to the outer TL
  io.outer.acquire.valid := Bool(false)
  io.outer.acquire.bits.payload := Mux(do_allocate,
                                    xact_meta.coh.outer.makeAcquire(
                                      client_xact_id = UInt(trackerId),
                                      addr_block = xact.addr_block,
                                      op_code = xact.op_code()),
                                    Bundle(Acquire(xact))(outerTLParams))
  io.outer.acquire.bits.header.src := UInt(bankId)
  io.outer.probe.ready := Bool(false)
  io.outer.release.valid := Bool(false)
  io.outer.grant.ready := Bool(false)
  io.outer.finish.valid := Bool(false)
  io.outer.finish.bits := pending_finish
  val pending_finish_on_ognt = io.ognt().makeFinish()

  io.inner.probe.valid := Bool(false)
  io.inner.probe.bits.header.src := UInt(bankId)
  io.inner.probe.bits.header.dst := curr_p_id
  io.inner.probe.bits.payload := pending_coh.inner.makeProbe(xact)

  io.inner.grant.valid := Bool(false)
  io.inner.grant.bits.header.src := UInt(bankId)
  io.inner.grant.bits.header.dst := xact_src
  io.inner.grant.bits.payload := pending_coh.inner.makeGrant(
                                    acq = xact,
                                    manager_xact_id = UInt(trackerId), 
                                    addr_beat = ignt_data_cnt,
                                    data = Mux(xact.is(Acquire.putAtomicType),
                                            amo_result,
                                            data_buffer(ignt_data_cnt)))

  io.inner.acquire.ready := Bool(false)
  io.inner.release.ready := Bool(false)
  io.inner.finish.ready := Bool(false)

  io.data.read.valid := Bool(false)
  io.data.read.bits.id := UInt(trackerId)
  io.data.read.bits.way_en := xact_way_en
  io.data.read.bits.addr_idx := xact.addr_block(idxMSB,idxLSB)
  io.data.read.bits.addr_beat := read_data_cnt
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
  io.meta.write.bits.data.coh := pending_coh
                                        
  io.wb.req.valid := Bool(false)
  io.wb.req.bits.addr_block := Cat(xact_meta.tag, xact.addr_block(idxMSB,idxLSB))
  io.wb.req.bits.coh := xact_meta.coh
  io.wb.req.bits.way_en := xact_way_en
  io.wb.req.bits.id := UInt(trackerId)

  when(collect_iacq_data) {
    io.inner.acquire.ready := Bool(true)
    when(io.inner.acquire.valid) {
      data_buffer(io.iacq().addr_beat) := io.iacq().data
      iacq_data_valid(io.iacq().addr_beat) := Bool(true)
    }
    when(iacq_data_done) { collect_iacq_data := Bool(false) }
  }

  switch (state) {
    is(s_idle) {
      io.inner.acquire.ready := Bool(true)
      when(io.inner.acquire.valid) {
        xact_src := io.inner.acquire.bits.header.src
        xact := io.iacq()
        data_buffer(io.iacq().addr_beat) := io.iacq().data
        collect_iacq_data := io.iacq().hasMultibeatData()
        iacq_data_valid := io.iacq().hasData() << io.iacq().addr_beat
        irel_had_data := Bool(false)
        ognt_had_data := Bool(false)
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
        xact_meta := io.meta.resp.bits.meta
        xact_way_en := io.meta.resp.bits.way_en
        pending_coh := io.meta.resp.bits.meta.coh
        val _coh = io.meta.resp.bits.meta.coh
        val _tag_match = io.meta.resp.bits.tag_match
        val _is_hit = _tag_match && _coh.outer.isHit(xact.op_code())
        val _needs_writeback = !_tag_match && do_allocate && 
                                  (_coh.outer.requiresVoluntaryWriteback() ||
                                   _coh.inner.requiresProbesOnVoluntaryWriteback())
        val _needs_probes = _tag_match && _coh.inner.requiresProbes(xact)
        when(_is_hit) { pending_coh := pending_coh_on_hit }
        when(_needs_probes) {
          pending_probes := mask_incoherent(nCoherentClients-1,0)
          release_count := PopCount(mask_incoherent(nCoherentClients-1,0))
        } 
        state := Mux(_tag_match,
                   Mux(_needs_probes, s_probe, Mux(_is_hit, s_data_read, s_outer_acquire)), // Probe, hit or upgrade
                   Mux(_needs_writeback, s_wb_req, s_outer_acquire)) // Evict ifneedbe
      }
    }
    is(s_wb_req) {
      io.wb.req.valid := Bool(true)
      when(io.wb.req.ready) { state := s_wb_resp }
    }
    is(s_wb_resp) {
      when(io.wb.resp.valid) { state := s_outer_acquire }
    }
    is(s_probe) {
      // Send probes
      io.inner.probe.valid := pending_probes != UInt(0)
      when(io.inner.probe.ready) {
        pending_probes := pending_probes & ~UIntToOH(curr_p_id)
      }
      // Handle releases, which may have data being written back
      io.inner.release.ready := Bool(true)
      when(io.inner.release.valid) {
        pending_coh.inner := pending_icoh_on_irel
        // Handle released dirty data
        //TODO: make sure cacq data is actually present before accpeting
        //      release data to merge!
        when(io.irel().hasData()) {
          irel_had_data := Bool(true)
          pending_coh.outer := pending_ocoh_on_irel
          mergeDataInner(data_buffer, io.irel().addr_beat, io.irel().data)
        }
        // We don't decrement release_count until we've received all the data beats.
        when(!io.irel().hasMultibeatData() || irel_data_done) {
          release_count := release_count - UInt(1)
        }
      }
      when(release_count === UInt(0)) {
        state := Mux(is_hit, Mux(irel_had_data, s_data_write, s_data_read), s_outer_acquire)
      }
    }
    is(s_outer_acquire) {
      io.outer.acquire.valid := !iacq_data_done // collect all data before refilling
      when(oacq_data_done) {
        state := s_outer_grant
      }
    }
    is(s_outer_grant) {
      io.outer.grant.ready := Bool(true)
      when(io.outer.grant.valid) {
        when(io.ognt().hasData()) { 
          mergeDataOuter(data_buffer, io.ognt().addr_beat, io.ognt().data)
          ognt_had_data := Bool(true)
        }
        when(ognt_data_done) { 
          pending_coh := pending_coh_on_ognt
          when(io.ognt().requiresAck()) {
            pending_finish.payload := pending_finish_on_ognt
            pending_finish.header.dst := io.outer.grant.bits.header.src
            pending_finish.header.src := UInt(bankId)
            state := s_outer_finish
          }.otherwise {
            state := Mux(!do_allocate, s_inner_grant,
                       Mux(io.ognt().hasData(), s_data_write, s_data_read))
          }
        }
      }
    }
    is(s_outer_finish) {
      io.outer.finish.valid := Bool(true)
      when(io.outer.finish.ready) {
        state := Mux(!do_allocate, s_inner_grant,
                   Mux(ognt_had_data, s_data_write, s_data_read))
      }
    }
    is(s_data_read) {
      io.data.read.valid := !collect_iacq_data || iacq_data_valid(read_data_cnt)
      when(io.data.resp.valid) {
        mergeDataInternal(data_buffer, io.data.resp.bits.addr_beat, io.data.resp.bits.data)
      }
      when(read_data_done) { state := s_data_resp }
    }
    is(s_data_resp) {
      when(io.data.resp.valid) {
        mergeDataInternal(data_buffer, io.data.resp.bits.addr_beat, io.data.resp.bits.data)
      }
      when(resp_data_done) {
        state := Mux(xact.hasData(), s_data_write, s_inner_grant)
      }
    }
    is(s_data_write) {
      io.data.write.valid := Bool(true)
      when(write_data_done) { state := s_inner_grant }
    }
    is(s_inner_grant) {
      io.inner.grant.valid := Bool(true)
      when(ignt_data_done) {
        val meta = pending_coh_on_ignt != xact_meta.coh
        when(meta) { pending_coh := pending_coh_on_ignt }
        state := Mux(meta, s_meta_write,
                   Mux(io.ignt().requiresAck(), s_inner_finish, s_idle))
      }
    }
    is(s_meta_write) {
      io.meta.write.valid := Bool(true)
      when(io.meta.write.ready) {
        state := Mux(io.ignt().requiresAck(), s_inner_finish, s_idle)
      }
    }
    is(s_inner_finish) {
      io.inner.finish.ready := Bool(true)
      when(io.inner.finish.valid) { state := s_idle }
    }
  }
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

class L2WritebackUnit(trackerId: Int, bankId: Int) extends L2XactTracker {
  val io = new L2WritebackUnitIO

  val s_idle :: s_probe :: s_data_read :: s_data_resp :: s_outer_release :: s_outer_grant :: s_outer_finish :: s_wb_resp :: Nil = Enum(UInt(), 8)
  val state = Reg(init=s_idle)

  val xact_addr_block = Reg(io.wb.req.bits.addr_block.clone)
  val xact_coh = Reg{ new HierarchicalMetadata }
  val xact_way_en = Reg{ Bits(width = nWays) }
  val data_buffer = Vec.fill(innerDataBeats){ Reg(io.irel().data.clone) }
  val xact_id = Reg{ UInt() }
  val pending_finish = Reg{ io.outer.finish.bits.clone }

  val irel_had_data = Reg(init = Bool(false))
  val release_count = Reg(init = UInt(0, width = log2Up(nClients+1)))
  val pending_probes = Reg(init = Bits(0, width = nClients))
  val curr_p_id = PriorityEncoder(pending_probes)

  val irel_data_done = connectIncomingDataBeatCounter(io.inner.release)
  val (orel_data_cnt, orel_data_done) = connectOutgoingDataBeatCounter(io.outer.release)
  val (read_data_cnt, read_data_done) = connectInternalDataBeatCounter(io.data.read)
  val resp_data_done = connectInternalDataBeatCounter(io.data.resp)

  val pending_icoh_on_irel = xact_coh.inner.onRelease(
                               incoming = io.irel(),
                               src = io.inner.release.bits.header.src)
  val pending_ocoh_on_irel = xact_coh.outer.onHit(M_XWR) // WB is a write

  io.has_acquire_conflict := Bool(false)
  io.has_acquire_match := Bool(false)
  io.has_release_match := !io.irel().isVoluntary() &&
                            io.irel().conflicts(xact_addr_block) &&
                            (state === s_probe)

  io.outer.acquire.valid := Bool(false)
  io.outer.probe.ready := Bool(false)
  io.outer.release.valid := Bool(false) // default
  io.outer.release.bits.payload := xact_coh.outer.makeVoluntaryWriteback(
                                            client_xact_id = UInt(trackerId),
                                            addr_block = xact_addr_block,
                                            addr_beat = orel_data_cnt,
                                            data = data_buffer(orel_data_cnt))
  io.outer.release.bits.header.src := UInt(bankId)
  io.outer.grant.ready := Bool(false) // default
  io.outer.finish.valid := Bool(false) // default
  io.outer.finish.bits := pending_finish
  val pending_finish_on_ognt = io.ognt().makeFinish()

  io.inner.probe.valid := Bool(false)
  io.inner.probe.bits.header.src := UInt(bankId)
  io.inner.probe.bits.header.dst := curr_p_id
  io.inner.probe.bits.payload :=
    xact_coh.inner.makeProbeForVoluntaryWriteback(xact_addr_block)

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
        val coh = io.wb.req.bits.coh
        val needs_probes = coh.inner.requiresProbesOnVoluntaryWriteback()
        when(needs_probes) {
          val mask_incoherent = coh.inner.full() & ~io.incoherent.toBits
          pending_probes := mask_incoherent(nCoherentClients-1,0)
          release_count := PopCount(mask_incoherent(nCoherentClients-1,0))
        }
        state := Mux(needs_probes, s_probe, s_data_read)
      }
    }
    is(s_probe) {
      // Send probes
      io.inner.probe.valid := pending_probes != UInt(0)
      when(io.inner.probe.ready) {
        pending_probes := pending_probes & ~UIntToOH(curr_p_id)
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
        // We don't decrement release_count until we've received all the data beats.
        when(!io.irel().hasData() || irel_data_done) {
          release_count := release_count - UInt(1)
        }
      }
      when(release_count === UInt(0)) {
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
        when(io.ognt().requiresAck()) {
          pending_finish.payload := pending_finish_on_ognt
          pending_finish.header.dst := io.outer.grant.bits.header.src
          state :=  s_outer_finish
        }.otherwise { 
          state := s_wb_resp
        }
      }
    }
    is(s_outer_finish) {
      io.outer.finish.valid := Bool(true)
      when(io.outer.finish.ready) { state := s_wb_resp }
    }
    is(s_wb_resp) {
      io.wb.resp.valid := Bool(true)
      state := s_idle
    }
  }
}
