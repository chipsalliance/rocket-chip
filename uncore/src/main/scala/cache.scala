// See LICENSE for license details.

package uncore
import Chisel._
import scala.reflect.ClassTag

case object CacheName extends Field[String]
case object NSets extends Field[Int]
case object NWays extends Field[Int]
case object BlockOffBits extends Field[Int]
case object RowBits extends Field[Int]
case object Replacer extends Field[() => ReplacementPolicy]
case object AmoAluOperandBits extends Field[Int]

abstract trait CacheParameters extends UsesParameters {
  val nSets = params(NSets)
  val blockOffBits = params(BlockOffBits)
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
  val metabits = rstVal.getWidth
  val rst_cnt = Reg(init=UInt(0, log2Up(nSets+1)))
  val rst = rst_cnt < UInt(nSets)
  val waddr = Mux(rst, rst_cnt, io.write.bits.idx)
  val wdata = Mux(rst, rstVal, io.write.bits.data).toBits
  val wmask = Mux(rst, SInt(-1), io.write.bits.way_en).toUInt
  when (rst) { rst_cnt := rst_cnt+UInt(1) }

  val tag_arr = Mem(UInt(width = metabits*nWays), nSets, seqRead = true)
  when (rst || io.write.valid) {
    tag_arr.write(waddr, Fill(nWays, wdata), FillInterleaved(metabits, wmask))
  }

  val tags = tag_arr(RegEnable(io.read.bits.idx, io.read.valid))
  for (w <- 0 until nWays) {
    val m = tags(metabits*(w+1)-1, metabits*w)
    io.resp(w) := rstVal.clone.fromBits(m)
  }

  io.read.ready := !rst && !io.write.valid // so really this could be a 6T RAM
  io.write.ready := !rst
}

abstract trait L2HellaCacheParameters extends CacheParameters with CoherenceAgentParameters {
  val idxMSB = idxBits-1
  val idxLSB = 0
  val refillCyclesPerBeat = tlDataBits/rowBits
  val refillCycles = refillCyclesPerBeat*tlDataBeats
  require(refillCyclesPerBeat == 1)
  val amoAluOperandBits = params(AmoAluOperandBits)
  require(amoAluOperandBits <= tlDataBits)
}

abstract class L2HellaCacheBundle extends TLBundle with L2HellaCacheParameters

abstract class L2HellaCacheModule extends TLModule with L2HellaCacheParameters {
  def connectDataBeatCounter[S <: HasTileLinkData](inc: Bool, data: S) = {
    val (cnt, cnt_done) =
      Counter(inc && data.hasMultibeatData(), tlDataBeats)
    val done = (inc && !data.hasMultibeatData()) || cnt_done
    (cnt, done)
  }
  def connectOutgoingDataBeatCounter[T <: HasTileLinkData : ClassTag](in: DecoupledIO[LogicalNetworkIO[T]]) = {
    connectDataBeatCounter(in.fire(), in.bits.payload)
  }
  def connectIncomingDataBeatCounter[T <: HasTileLinkData](in: DecoupledIO[LogicalNetworkIO[T]]) = {
    connectDataBeatCounter(in.fire(), in.bits.payload)._2
  }
  def connectOutgoingDataBeatCounter[T <: HasTileLinkData](in: DecoupledIO[T]) = {
    connectDataBeatCounter(in.fire(), in.bits)
  }
  def connectIncomingDataBeatCounter[T <: HasTileLinkData](in: ValidIO[T]) = {
    connectDataBeatCounter(in.valid, in.bits)._2
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

trait HasL2Data extends HasTileLinkData {
  def hasData(dummy: Int = 0) = Bool(true)
  def hasMultibeatData(dummy: Int = 0) = Bool(tlDataBeats > 1)
}

object L2Metadata {
  def apply(tag: Bits, coh: ManagerMetadata) = {
    val meta = new L2Metadata
    meta.tag := tag
    meta.coh := coh
    meta
  }
}
class L2Metadata extends Metadata with L2HellaCacheParameters {
  val coh = new ManagerMetadata
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

  val meta = Module(new MetadataArray(() => L2Metadata(UInt(0), co.managerMetadataOnFlush)))
  meta.io.read <> io.read
  meta.io.write <> io.write
  
  val s1_tag = RegEnable(io.read.bits.tag, io.read.valid)
  val s1_id = RegEnable(io.read.bits.id, io.read.valid)
  def wayMap[T <: Data](f: Int => T) = Vec((0 until nWays).map(f))
  val s1_clk_en = Reg(next = io.read.fire())
  val s1_tag_eq_way = wayMap((w: Int) => meta.io.resp(w).tag === s1_tag)
  val s1_tag_match_way = wayMap((w: Int) => s1_tag_eq_way(w) && co.isValid(meta.io.resp(w).coh)).toBits
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
    with HasCacheBlockAddress
    with HasTileLinkBeatId
    with HasL2Id {
  val way_en = Bits(width = nWays)
}

class L2DataWriteReq extends L2DataReadReq 
    with HasL2Data {
  val wmask  = Bits(width = tlWriteMaskBits)
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

class L2DataArray extends L2HellaCacheModule {
  val io = new L2DataRWIO().flip

  val wmask = FillInterleaved(8, io.write.bits.wmask)
  val reg_raddr = Reg(UInt())
  val array = Mem(Bits(width=rowBits), nWays*nSets*refillCycles, seqRead = true)
  val waddr = Cat(OHToUInt(io.write.bits.way_en), io.write.bits.addr_block, io.write.bits.addr_beat)
  val raddr = Cat(OHToUInt(io.read.bits.way_en), io.read.bits.addr_block, io.read.bits.addr_beat)

  when (io.write.bits.way_en.orR && io.write.valid) {
    array.write(waddr, io.write.bits.data, wmask)
  }.elsewhen (io.read.bits.way_en.orR && io.read.valid) {
    reg_raddr := raddr
  }

  io.resp.valid := ShiftRegister(io.read.fire(), 1)
  io.resp.bits.id := ShiftRegister(io.read.bits.id, 1)
  io.resp.bits.addr_beat := ShiftRegister(io.read.bits.addr_beat, 1)
  io.resp.bits.data := array(reg_raddr)
  io.read.ready := !io.write.valid
  io.write.ready := Bool(true)
}

class L2HellaCache(bankId: Int, innerId: String, outerId: String) extends 
    CoherenceAgent(innerId, outerId) with L2HellaCacheParameters {

  require(isPow2(nSets))
  require(isPow2(nWays)) 

  val tshrfile = Module(new TSHRFile(bankId, innerId, outerId))
  val meta = Module(new L2MetadataArray)
  val data = Module(new L2DataArray)

  tshrfile.io.inner <> io.inner
  tshrfile.io.meta <> meta.io
  tshrfile.io.data <> data.io
  io.outer <> tshrfile.io.outer
  io.incoherent <> tshrfile.io.incoherent
}


class TSHRFile(bankId: Int, innerId: String, outerId: String) extends L2HellaCacheModule {
  val io = new Bundle {
    val inner = Bundle(new TileLinkIO, {case TLId => innerId}).flip
    val outer = Bundle(new UncachedTileLinkIO, {case TLId => outerId})
    val incoherent = Vec.fill(nClients){Bool()}.asInput
    val meta = new L2MetaRWIO
    val data = new L2DataRWIO
  }

  // Wiring helper funcs
  def doOutputArbitration[T <: Data](
      out: DecoupledIO[T],
      ins: Seq[DecoupledIO[T]],
      count: Int = 1,
      lock: T => Bool = (a: T) => Bool(true)) {
    val arb = Module(new LockingRRArbiter(out.bits.clone, ins.size, count, lock))
    out <> arb.io.out
    arb.io.in zip ins map { case (a, in) => a <> in }
  }

  def doInputRouting[T <: HasL2Id](in: ValidIO[T], outs: Seq[ValidIO[T]]) {
    outs.map(_.bits := in.bits)
    outs.zipWithIndex.map { case (o, i) => o.valid := in.valid && (UInt(i) === in.bits.id) }
  }

  // Create TSHRs for outstanding transactions
  val trackerList = (0 until nReleaseTransactors).map { id => 
    Module(new L2VoluntaryReleaseTracker(id, bankId, innerId, outerId)) 
  } ++ (nReleaseTransactors until nTransactors).map { id => 
    Module(new L2AcquireTracker(id, bankId, innerId, outerId))
  }
  
  val wb = Module(new L2WritebackUnit(nTransactors, bankId, innerId, outerId))
  doOutputArbitration(wb.io.wb.req, trackerList.map(_.io.wb.req))
  doInputRouting(wb.io.wb.resp, trackerList.map(_.io.wb.resp))

  // Propagate incoherence flags
  (trackerList.map(_.io.tile_incoherent) :+ wb.io.tile_incoherent).map( _ := io.incoherent.toBits)

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
    acq.valid := acquire.valid && (acquire_idx === UInt(i))
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

  // Wire finished transaction acks
  val finish = io.inner.finish
  val finish_idx = finish.bits.payload.manager_xact_id
  trackerList.zipWithIndex.map { case (t, i) => 
    t.io.inner.finish.valid := finish.valid && finish_idx === UInt(i)
  }
  trackerList.map(_.io.inner.finish.bits := finish.bits)
  finish.ready := Vec(trackerList.map(_.io.inner.finish.ready)).read(finish_idx)

  // Wire probe requests to clients
  doOutputArbitration(io.inner.probe, trackerList.map(_.io.inner.probe) :+ wb.io.inner.probe)

  // Wire grant reply to initiating client
  doOutputArbitration(
    io.inner.grant,
    trackerList.map(_.io.inner.grant), 
    tlDataBeats,
    (m: LogicalNetworkIO[Grant]) => m.payload.hasMultibeatData())

  // Create an arbiter for the one memory port
  val outerList = trackerList.map(_.io.outer) :+ wb.io.outer
  val outer_arb = Module(new UncachedTileLinkIOArbiterThatPassesId(outerList.size),
                         {case TLId => outerId})
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

class L2WritebackReq extends L2HellaCacheBundle
    with HasL2Id {
  val addr_block = UInt(width = tlBlockAddrBits)
  val coh = new ManagerMetadata
  val way_en = Bits(width = nWays)
}

class L2WritebackResp extends L2HellaCacheBundle with HasL2Id

class L2WritebackIO extends L2HellaCacheBundle {
  val req = Decoupled(new L2WritebackReq)
  val resp = Valid(new L2WritebackResp).flip
}

class L2WritebackUnit(trackerId: Int, bankId: Int, innerId: String, outerId: String) extends L2HellaCacheModule {
  val io = new Bundle {
    val wb = new L2WritebackIO().flip
    val inner = Bundle(new TileLinkIO, {case TLId => innerId}).flip
    val outer = Bundle(new UncachedTileLinkIO, {case TLId => outerId})
    val tile_incoherent = Bits(INPUT, nClients)
    val has_release_match = Bool(OUTPUT)
    val data = new L2DataRWIO
  }
  val cacq = io.inner.acquire.bits
  val crel = io.inner.release.bits
  val cgnt = io.inner.grant.bits
  val c_ack = io.inner.finish.bits
  val mgnt = io.outer.grant.bits

  val s_idle :: s_probe :: s_data_read :: s_data_resp :: s_outer_write :: Nil = Enum(UInt(), 5)
  val state = Reg(init=s_idle)

  val xact_addr_block = Reg(io.inner.acquire.bits.payload.addr_block.clone)
  val xact_coh = Reg{ new ManagerMetadata }
  val xact_way_en = Reg{ Bits(width = nWays) }
  val xact_data = Vec.fill(tlDataBeats){ Reg(io.inner.acquire.bits.payload.data.clone) }
  val xact_id = Reg{ UInt() }

  val crel_had_data = Reg(init = Bool(false))
  val release_count = Reg(init = UInt(0, width = log2Up(nClients+1)))
  val pending_probes = Reg(init = co.dir.flush)
  val curr_p_id = co.dir.next(pending_probes)

  val crel_data_done = connectIncomingDataBeatCounter(io.inner.release)
  val (macq_data_cnt, macq_data_done) = connectOutgoingDataBeatCounter(io.outer.acquire)
  val (read_data_cnt, read_data_done) = Counter(io.data.read.fire(), tlDataBeats)
  val resp_data_done = connectIncomingDataBeatCounter(io.data.resp)

  io.has_release_match := !crel.payload.isVoluntary() &&
                            co.isCoherenceConflict(xact_addr_block, crel.payload.addr_block) &&
                            (state === s_probe)

  val next_coh_on_rel = co.managerMetadataOnRelease(crel.payload, xact_coh, crel.header.src)

  io.outer.acquire.valid := Bool(false)
  io.outer.acquire.bits.payload := Bundle(UncachedWriteBlock(
                                            client_xact_id = UInt(trackerId),
                                            addr_block = xact_addr_block,
                                            addr_beat = macq_data_cnt,
                                            data = xact_data(macq_data_cnt)),
                                     { case TLId => outerId })
  io.outer.grant.ready := Bool(false) // Never gets mgnts

  io.inner.probe.valid := Bool(false)
  io.inner.probe.bits.header.src := UInt(bankId)
  io.inner.probe.bits.header.dst := curr_p_id
  io.inner.probe.bits.payload := Probe.onVoluntaryWriteback(xact_coh, xact_addr_block)

  io.inner.grant.valid := Bool(false)
  io.inner.acquire.ready := Bool(false)
  io.inner.release.ready := Bool(false)
  io.inner.finish.ready := Bool(false)

  io.data.read.valid := Bool(false)
  io.data.read.bits.id := UInt(trackerId)
  io.data.read.bits.way_en := xact_way_en
  io.data.read.bits.addr_block := xact_addr_block
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
        val coh = io.wb.req.bits.coh
        val needs_probes = co.requiresProbesOnVoluntaryWriteback(coh)
        when(needs_probes) {
          val mask_incoherent = co.dir.full(coh.sharers) & ~io.tile_incoherent
          pending_probes := mask_incoherent
          release_count := co.dir.count(mask_incoherent)
          crel_had_data := Bool(false)
        }
        state := Mux(needs_probes, s_probe, s_data_read)
      }
    }
    is(s_probe) {
      // Send probes
      io.inner.probe.valid := !co.dir.none(pending_probes)
      when(io.inner.probe.ready) {
        pending_probes := co.dir.pop(pending_probes, curr_p_id)
      }
      // Handle releases, which may have data being written back
      io.inner.release.ready := Bool(true)
      when(io.inner.release.valid) {
        xact_coh := next_coh_on_rel
        // Handle released dirty data
        when(crel.payload.hasData()) {
          crel_had_data := Bool(true)
          xact_data(crel.payload.addr_beat) := crel.payload.data
        }
        // We don't decrement release_count until we've received all the data beats.
        when(!crel.payload.hasData() || crel_data_done) {
          release_count := release_count - UInt(1)
        }
      }
      when(release_count === UInt(0)) {
        state := Mux(crel_had_data, s_outer_write, s_data_read)
      }
    }
    is(s_data_read) {
      io.data.read.valid := Bool(true)
      when(io.data.resp.valid) { xact_data(io.data.resp.bits.addr_beat) := io.data.resp.bits.data }
      when(read_data_done) { state := s_data_resp }
    }
    is(s_data_resp) {
      when(io.data.resp.valid) { xact_data(io.data.resp.bits.addr_beat) := io.data.resp.bits.data }
      when(resp_data_done) { state := s_outer_write }
    }
    is(s_outer_write) {
      io.outer.acquire.valid := Bool(true)
      when(macq_data_done) {
        io.wb.resp.valid := Bool(true)
        state := s_idle
      }
    }
  }
}

abstract class L2XactTracker(innerId: String, outerId: String) extends L2HellaCacheModule {
  val io = new Bundle {
    val inner = Bundle(new TileLinkIO, {case TLId => innerId}).flip
    val outer = Bundle(new UncachedTileLinkIO, {case TLId => outerId})
    val tile_incoherent = Bits(INPUT, nClients)
    val has_acquire_conflict = Bool(OUTPUT)
    val has_acquire_match = Bool(OUTPUT)
    val has_release_match = Bool(OUTPUT)
    val data = new L2DataRWIO
    val meta = new L2MetaRWIO
    val wb = new L2WritebackIO
  }

  val cacq = io.inner.acquire.bits
  val crel = io.inner.release.bits
  val cgnt = io.inner.grant.bits
  val cack = io.inner.finish.bits
  val mgnt = io.outer.grant.bits
}

class L2VoluntaryReleaseTracker(trackerId: Int, bankId: Int, innerId: String, outerId: String) extends L2XactTracker(innerId, outerId) {
  val s_idle :: s_meta_read :: s_meta_resp :: s_data_write :: s_meta_write :: s_grant :: s_ack :: Nil = Enum(UInt(), 7)
  val state = Reg(init=s_idle)

  val xact_src = Reg(io.inner.release.bits.header.src.clone)
  val xact_r_type = Reg(io.inner.release.bits.payload.r_type)
  val xact_addr_block = Reg(io.inner.release.bits.payload.addr_block.clone)
  val xact_addr_beat = Reg(io.inner.release.bits.payload.addr_beat.clone)
  val xact_client_xact_id = Reg(io.inner.release.bits.payload.client_xact_id.clone)
  val xact_data = Vec.fill(tlDataBeats){ Reg(io.inner.release.bits.payload.data.clone) }
  val xact_tag_match = Reg{ Bool() }
  val xact_meta = Reg{ new L2Metadata }
  val xact_way_en = Reg{ Bits(width = nWays) }
  val xact = Release(
    voluntary = Bool(true),
    r_type = xact_r_type, 
    client_xact_id = xact_client_xact_id,
    addr_block = xact_addr_block)

  val collect_crel_data = Reg(init=Bool(false))
  val crel_data_valid = Reg(init=Bits(0, width = tlDataBeats))
  val crel_data_done = connectIncomingDataBeatCounter(io.inner.release)
  val (write_data_cnt, write_data_done) = connectOutgoingDataBeatCounter(io.data.write)

  io.has_acquire_conflict := Bool(false)
  io.has_acquire_match := Bool(false)
  io.has_release_match := crel.payload.isVoluntary()

  io.outer.grant.ready := Bool(false)
  io.outer.acquire.valid := Bool(false)
  io.inner.acquire.ready := Bool(false)
  io.inner.probe.valid := Bool(false)
  io.inner.release.ready := Bool(false)
  io.inner.grant.valid := Bool(false)
  io.inner.finish.ready := Bool(false)

  io.inner.grant.bits.header.src := UInt(bankId)
  io.inner.grant.bits.header.dst := xact_src
  io.inner.grant.bits.payload := xact.makeGrant(UInt(trackerId), xact_meta.coh)

  io.data.read.valid := Bool(false)
  io.data.write.valid := Bool(false)
  io.data.write.bits.id := UInt(trackerId)
  io.data.write.bits.way_en := xact_way_en
  io.data.write.bits.addr_block := xact_addr_block
  io.data.write.bits.addr_beat := write_data_cnt
  io.data.write.bits.wmask := SInt(-1)
  io.data.write.bits.data := xact_data(write_data_cnt)
  io.meta.read.valid := Bool(false)
  io.meta.read.bits.id := UInt(trackerId)
  io.meta.read.bits.idx := xact_addr_block(idxMSB,idxLSB)
  io.meta.read.bits.tag := xact_addr_block >> UInt(idxBits)
  io.meta.write.valid := Bool(false)
  io.meta.write.bits.id := UInt(trackerId)
  io.meta.write.bits.idx := xact_addr_block(idxMSB,idxLSB)
  io.meta.write.bits.way_en := xact_way_en
  io.meta.write.bits.data.tag := xact_addr_block >> UInt(idxBits)
  io.meta.write.bits.data.coh := co.managerMetadataOnRelease(xact, 
                                                            xact_meta.coh, 
                                                            xact_src)
  io.wb.req.valid := Bool(false)

  when(collect_crel_data) {
    io.inner.release.ready := Bool(true)
    when(io.inner.release.valid) {
      xact_data(crel.payload.addr_beat) := crel.payload.data
      crel_data_valid(crel.payload.addr_beat) := Bool(true)
    }
    when(crel_data_done) { collect_crel_data := Bool(false) }
  }

  switch (state) {
    is(s_idle) {
      io.inner.release.ready := Bool(true)
      when( io.inner.release.valid ) {
        xact_src := crel.header.src
        xact_r_type := crel.payload.r_type
        xact_addr_block := crel.payload.addr_block
        xact_addr_beat := crel.payload.addr_beat
        xact_client_xact_id := crel.payload.client_xact_id
        xact_data(UInt(0)) := crel.payload.data
        collect_crel_data := crel.payload.hasMultibeatData()
        crel_data_valid := Bits(1)
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
                   Mux(xact.requiresAck(), s_grant, s_idle))
      }
    }
    is(s_data_write) {
      io.data.write.valid := !collect_crel_data || crel_data_valid(write_data_cnt)
      when(write_data_done) { state := s_meta_write }
    }
    is(s_meta_write) {
      io.meta.write.valid := Bool(true)
      when(io.meta.write.ready) { 
        state := Mux(xact.requiresAck(), s_grant, s_idle) // Need a Grant.voluntaryAck?
      }
    }
    is(s_grant) {
      io.inner.grant.valid := Bool(true)
      when(io.inner.grant.ready) { 
        state := Mux(cgnt.payload.requiresAck(), s_ack, s_idle)
      }
    }
    is(s_ack) {
      // TODO: This state is unnecessary if no client will ever issue the
      // pending Acquire that caused this writeback until it receives the 
      // Grant.voluntaryAck for this writeback
      io.inner.finish.ready := Bool(true)
      when(io.inner.finish.valid) { state := s_idle }
    }
  }
}


class L2AcquireTracker(trackerId: Int, bankId: Int, innerId: String, outerId: String) extends L2XactTracker(innerId, outerId) {
  val s_idle :: s_meta_read :: s_meta_resp :: s_wb_req :: s_wb_resp :: s_probe :: s_outer_read :: s_outer_resp :: s_data_read :: s_data_resp :: s_data_write :: s_meta_write :: s_grant :: s_ack :: Nil = Enum(UInt(), 14)
  val state = Reg(init=s_idle)

  val xact_src = Reg(io.inner.acquire.bits.header.src.clone)
  val xact_builtin_type = Reg(io.inner.acquire.bits.payload.builtin_type.clone)
  val xact_a_type = Reg(io.inner.acquire.bits.payload.a_type.clone)
  val xact_addr_block = Reg(io.inner.acquire.bits.payload.addr_block.clone)
  val xact_addr_beat = Reg(io.inner.acquire.bits.payload.addr_beat.clone)
  val xact_client_xact_id = Reg(io.inner.acquire.bits.payload.client_xact_id.clone)
  val xact_subblock = Reg(io.inner.acquire.bits.payload.subblock.clone)
  val xact_data = Vec.fill(tlDataBeats+1) { // Extra entry holds AMO result
    Reg(io.inner.acquire.bits.payload.data.clone) 
  } 
  val xact_tag_match = Reg{ Bool() }
  val xact_meta = Reg{ new L2Metadata }
  val xact_way_en = Reg{ Bits(width = nWays) }
  val xact = Acquire(
              builtin_type = xact_builtin_type,
              a_type = xact_a_type,
              client_xact_id = xact_client_xact_id,
              addr_block = xact_addr_block,
              addr_beat = xact_addr_beat,
              data = UInt(0),
              subblock = xact_subblock)

  val collect_cacq_data = Reg(init=Bool(false))
  val cacq_data_valid = Reg(init=Bits(0, width = tlDataBeats))
  val crel_had_data = Reg(init = Bool(false))
  val release_count = Reg(init = UInt(0, width = log2Up(nClients+1)))
  val pending_probes = Reg(init = UInt(0, width = nCoherentClients))
  val curr_p_id = co.dir.next(pending_probes)
  val full_sharers = co.dir.full(io.meta.resp.bits.meta.coh.sharers)
  val mask_self = Mux(xact.requiresSelfProbe(),
                    full_sharers | (UInt(1) << xact_src),
                    full_sharers & ~UInt(UInt(1) << xact_src, width = nClients))
  val mask_incoherent = mask_self & ~io.tile_incoherent

  val cacq_data_done = connectIncomingDataBeatCounter(io.inner.acquire)
  val crel_data_done = connectIncomingDataBeatCounter(io.inner.release)
  val (macq_data_cnt, macq_data_done) = connectOutgoingDataBeatCounter(io.outer.acquire)
  val mgnt_data_done = connectIncomingDataBeatCounter(io.outer.grant)
  val cgnt_data_cnt = Reg(init = UInt(0, width = tlBeatAddrBits+1))
  val cgnt_data_max = Reg(init = UInt(0, width = tlBeatAddrBits+1))
  val read_data_cnt = Reg(init = UInt(0, width = log2Up(refillCycles)+1))
  val read_data_max = Reg(init = UInt(0, width = log2Up(refillCycles)+1)) 
  val write_data_cnt = Reg(init = UInt(0, width = log2Up(refillCycles)+1))
  val write_data_max = Reg(init = UInt(0, width = log2Up(refillCycles)+1)) 
  val resp_data_cnt = Reg(init = UInt(0, width = log2Up(refillCycles)+1))
  val resp_data_max = Reg(init = UInt(0, width = log2Up(refillCycles)+1)) 

  val needs_writeback = !xact_tag_match && co.isValid(xact_meta.coh) // TODO: dirty bit
  val is_hit = xact_tag_match && co.isHit(xact, xact_meta.coh)
  val needs_probes = co.requiresProbes(xact, xact_meta.coh)
  //val do_allocate = !xact_builtin_type || xact.allocate()

  val amoalu = Module(new AMOALU)
  amoalu.io.addr := xact.addr()
  amoalu.io.cmd := xact.op_code()
  amoalu.io.typ := xact.op_size()
  amoalu.io.lhs := io.data.resp.bits.data //default
  amoalu.io.rhs := xact.data(0) // default

  def mergeData[T <: HasTileLinkData](buffer: Vec[UInt], incoming: T) {
    val old_data = incoming.data
    val new_data = buffer(incoming.addr_beat)
    val amoOpSz = UInt(amoAluOperandBits)
    val offset = xact.addr_byte()(tlByteAddrBits-1, log2Up(amoAluOperandBits/8))
    amoalu.io.lhs := old_data >> offset*amoOpSz
    amoalu.io.rhs := new_data >> offset*amoOpSz
    val wmask = 
      Mux(xact.is(Acquire.uncachedAtomic), 
        FillInterleaved(amoAluOperandBits, UIntToOH(offset)),
        Mux(xact.is(Acquire.uncachedWriteBlock) || xact.is(Acquire.uncachedWrite),
          FillInterleaved(8, xact.write_mask()), 
          UInt(0, width = tlDataBits)))
    buffer(incoming.addr_beat) := ~wmask & old_data | wmask & 
      Mux(xact.is(Acquire.uncachedAtomic), amoalu.io.out << offset*amoOpSz, new_data)
    when(xact.is(Acquire.uncachedAtomic)) { buffer(tlDataBeats) := old_data } // For AMO result 
  }

  //TODO: Allow hit under miss for stores
  io.has_acquire_conflict := (co.isCoherenceConflict(xact.addr_block, cacq.payload.addr_block) ||
                              xact.addr_block(idxMSB,idxLSB) === cacq.payload.addr_block(idxMSB,idxLSB)) &&
                              (state != s_idle) &&
                              !collect_cacq_data
  io.has_acquire_match := xact.hasMultibeatData() &&
                            (xact.addr_block === cacq.payload.addr_block) &&
                              collect_cacq_data
  io.has_release_match := !crel.payload.isVoluntary() &&
                            (xact.addr_block === crel.payload.addr_block) &&
                            (state === s_probe)

  val next_coh_on_rel = co.managerMetadataOnRelease(
                          incoming = crel.payload, 
                          meta = xact_meta.coh, 
                          src = crel.header.src)
  val next_coh_on_gnt = co.managerMetadataOnGrant(
                          outgoing = cgnt.payload, 
                          meta = xact_meta.coh,
                          dst = cgnt.header.dst)

  val outer_write = Bundle(UncachedWriteBlock(
                              client_xact_id = UInt(trackerId),
                              addr_block = xact_addr_block,
                              addr_beat = macq_data_cnt,
                              data = xact_data(macq_data_cnt)),
                            { case TLId => outerId })
  val outer_read = Bundle(UncachedReadBlock(
                              client_xact_id = UInt(trackerId),
                              addr_block = xact_addr_block),
                            { case TLId => outerId })

  io.outer.acquire.valid := Bool(false)
  io.outer.acquire.bits.payload := outer_read //default
  io.outer.grant.ready := Bool(true) //grant.data -> xact.data

  io.inner.probe.valid := Bool(false)
  io.inner.probe.bits.header.src := UInt(bankId)
  io.inner.probe.bits.header.dst := curr_p_id
  io.inner.probe.bits.payload := xact.makeProbe(xact_meta.coh)

  io.inner.grant.valid := Bool(false)
  io.inner.grant.bits.header.src := UInt(bankId)
  io.inner.grant.bits.header.dst := xact_src
  io.inner.grant.bits.payload := xact.makeGrant(
                                    manager_xact_id = UInt(trackerId), 
                                    meta = xact_meta.coh, 
                                    addr_beat = cgnt_data_cnt,
                                    data = Mux(xact.is(Acquire.uncachedAtomic),
                                            xact_data(tlDataBeats),
                                            xact_data(cgnt_data_cnt)))

  io.inner.acquire.ready := Bool(false)
  io.inner.release.ready := Bool(false)
  io.inner.finish.ready := Bool(false)

  io.data.read.valid := Bool(false)
  io.data.read.bits.id := UInt(trackerId)
  io.data.read.bits.way_en := xact_way_en
  io.data.read.bits.addr_block := xact_addr_block
  io.data.read.bits.addr_beat := read_data_cnt
  io.data.write.valid := Bool(false)
  io.data.write.bits.id := UInt(trackerId)
  io.data.write.bits.way_en := xact_way_en
  io.data.write.bits.addr_block := xact_addr_block
  io.data.write.bits.addr_beat := write_data_cnt
  io.data.write.bits.wmask := SInt(-1)
  io.data.write.bits.data := xact_data(write_data_cnt)
  io.meta.read.valid := Bool(false)
  io.meta.read.bits.id := UInt(trackerId)
  io.meta.read.bits.idx := xact_addr_block(idxMSB,idxLSB)
  io.meta.read.bits.tag := xact_addr_block >> UInt(idxBits)
  io.meta.write.valid := Bool(false)
  io.meta.write.bits.id := UInt(trackerId)
  io.meta.write.bits.idx := xact_addr_block(idxMSB,idxLSB)
  io.meta.write.bits.way_en := xact_way_en
  io.meta.write.bits.data.tag := xact_addr_block >> UInt(idxBits)
  io.meta.write.bits.data.coh := next_coh_on_gnt

  io.wb.req.valid := Bool(false)
  io.wb.req.bits.addr_block := Cat(xact_meta.tag, xact_addr_block(idxMSB,idxLSB))
  io.wb.req.bits.coh := xact_meta.coh
  io.wb.req.bits.way_en := xact_way_en
  io.wb.req.bits.id := UInt(trackerId)

  when(collect_cacq_data) {
    io.inner.acquire.ready := Bool(true)
    when(io.inner.acquire.valid) {
      xact_data(cacq.payload.addr_beat) := cacq.payload.data
      cacq_data_valid(cacq.payload.addr_beat) := Bool(true)
    }
    when(cacq_data_done) { collect_cacq_data := Bool(false) }
  }

  switch (state) {
    is(s_idle) {
      io.inner.acquire.ready := Bool(true)
      when( io.inner.acquire.valid ) {
        xact_builtin_type := cacq.payload.builtin_type
        xact_a_type := cacq.payload.a_type
        xact_addr_block := cacq.payload.addr_block
        xact_addr_beat := cacq.payload.addr_beat
        xact_client_xact_id := cacq.payload.client_xact_id
        xact_data(UInt(0)) := cacq.payload.data
        xact_subblock := cacq.payload.subblock
        xact_src := cacq.header.src
        collect_cacq_data := cacq.payload.hasMultibeatData()
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
        val coh = io.meta.resp.bits.meta.coh
        val _tag_match = io.meta.resp.bits.tag_match
        val _needs_writeback = !_tag_match && co.isValid(coh) //TODO: dirty bit
        val _needs_probes = _tag_match && co.requiresProbes(xact, coh)
        val _is_hit = _tag_match && co.isHit(xact, coh)
        val full_block = !xact.builtin_type || 
                            xact.hasMultibeatData() ||
                            cgnt.payload.hasMultibeatData()
        read_data_cnt := Mux(full_block, UInt(0), xact_addr_beat)
        read_data_max := Mux(full_block, UInt(refillCycles-1), xact_addr_beat)
        write_data_cnt := Mux(full_block || !_is_hit, UInt(0), xact_addr_beat)
        write_data_max := Mux(full_block || !_is_hit, UInt(refillCycles-1), xact_addr_beat)
        resp_data_cnt := Mux(full_block, UInt(0), xact_addr_beat)
        resp_data_max := Mux(full_block, UInt(refillCycles-1), xact_addr_beat)
        cgnt_data_cnt := Mux(full_block, UInt(0), xact_addr_beat)
        cgnt_data_max := Mux(full_block, UInt(tlDataBeats-1), xact_addr_beat)
        when(_needs_probes) {
          pending_probes := mask_incoherent(nCoherentClients-1,0)
          release_count := co.dir.count(mask_incoherent)
          crel_had_data := Bool(false)
        } 
        state := Mux(_tag_match,
                   Mux(_needs_probes, s_probe, Mux(_is_hit, s_data_read, s_outer_read)), // Probe, hit or upgrade
                   Mux(_needs_writeback, s_wb_req, s_outer_read)) // Evict ifneedbe
      }
    }
    is(s_wb_req) {
      io.wb.req.valid := Bool(true)
      when(io.wb.req.ready) { state := s_wb_resp }
    }
    is(s_wb_resp) {
      when(io.wb.resp.valid) { state := s_outer_read }
    }
    is(s_probe) {
      // Send probes
      io.inner.probe.valid := !co.dir.none(pending_probes)
      when(io.inner.probe.ready) {
        pending_probes := co.dir.pop(pending_probes, curr_p_id)
      }
      // Handle releases, which may have data being written back
        //TODO: make sure cacq data is actually present before accpeting
        //      release data to merge!
      io.inner.release.ready := Bool(true)
      when(io.inner.release.valid) {
        xact_meta.coh := next_coh_on_rel
        // Handle released dirty data
        when(crel.payload.hasData()) {
          crel_had_data := Bool(true)
          mergeData(xact_data, crel.payload)
        }
        // We don't decrement release_count until we've received all the data beats.
        when(!crel.payload.hasMultibeatData() || crel_data_done) {
          release_count := release_count - UInt(1)
        }
      }
      when(release_count === UInt(0)) {
        state := Mux(is_hit, Mux(crel_had_data, s_data_write, s_data_read), s_outer_read)
      }
    }
    is(s_outer_read) {
      io.outer.acquire.valid := Bool(true)
      io.outer.acquire.bits.payload := outer_read
      when(io.outer.acquire.ready) {
        state := s_outer_resp
      }
    }
    is(s_outer_resp) {
      io.outer.grant.ready := Bool(true)
      when(io.outer.grant.valid) {
        //TODO make sure cacq data is actually present before merging
        mergeData(xact_data, mgnt.payload)
        when(mgnt_data_done) { 
          state := Mux(mgnt.payload.hasData(), s_data_write, s_data_read)
        }
      }
    }
    is(s_data_read) {
      io.data.read.valid := !collect_cacq_data || cacq_data_valid(read_data_cnt)
      when(io.data.resp.valid) {
        mergeData(xact_data, io.data.resp.bits)
        resp_data_cnt := resp_data_cnt + UInt(1)
      }
      when(io.data.read.ready) { 
        read_data_cnt := read_data_cnt + UInt(1)
        when(read_data_cnt === read_data_max) { state := s_data_resp }
      }
    }
    is(s_data_resp) {
      when(io.data.resp.valid) {
        mergeData(xact_data, io.data.resp.bits)
        resp_data_cnt := resp_data_cnt + UInt(1)
      }
      when(resp_data_cnt === resp_data_max) {
          state := Mux(xact.hasData(), s_data_write, s_meta_write)
      }
    }
    is(s_data_write) {
      io.data.write.valid := Bool(true)
      when(io.data.write.ready) {
        write_data_cnt := write_data_cnt + UInt(1)
        when(write_data_cnt === write_data_max) {
          state := s_meta_write
        }
      }
    }
    is(s_meta_write) {
      io.meta.write.valid := Bool(true)
      when(io.meta.write.ready) { state := s_grant }
    }
    is(s_grant) {
      io.inner.grant.valid := Bool(true)
      when(io.inner.grant.ready) {
        cgnt_data_cnt := cgnt_data_cnt + UInt(1)
        when(cgnt_data_cnt === cgnt_data_max) { 
          state := Mux(cgnt.payload.requiresAck(), s_ack, s_idle) 
        }
      }
    }
    is(s_ack) {
      io.inner.finish.ready := Bool(true)
      when(io.inner.finish.valid) { state := s_idle }
    }
  }
}
