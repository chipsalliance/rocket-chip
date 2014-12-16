// See LICENSE for license details.

package uncore
import Chisel._

case object CacheName extends Field[String]
case object NSets extends Field[Int]
case object NWays extends Field[Int]
case object BlockOffBits extends Field[Int]
case object RowBits extends Field[Int]
case object WordBits extends Field[Int]
case object Replacer extends Field[() => ReplacementPolicy]

abstract trait CacheParameters extends UsesParameters {
  val paddrBits = params(PAddrBits)
  val vaddrBits = params(VAddrBits)
  val pgIdxBits = params(PgIdxBits) 
  val nSets = params(NSets)
  val blockOffBits = params(BlockOffBits)
  val idxBits = log2Up(nSets)
  val untagBits = blockOffBits + idxBits
  val tagBits = paddrBits - untagBits
  val nWays = params(NWays)
  val wayBits = log2Up(nWays)
  val isDM = nWays == 1
  val wordBits = params(WordBits)
  val wordBytes = wordBits/8
  val wordOffBits = log2Up(wordBytes)
  val rowBits = params(RowBits)
  val rowWords = rowBits/wordBits 
  val rowBytes = rowBits/8
  val rowOffBits = log2Up(rowBytes)
  val refillCyclesPerBeat = params(TLDataBits)/rowBits
  val refillCycles = refillCyclesPerBeat*params(TLDataBeats)
}

abstract class CacheBundle extends Bundle with CacheParameters
abstract class CacheModule extends Module with CacheParameters

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
  val wmask = Mux(rst, SInt(-1), io.write.bits.way_en)
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

abstract trait L2HellaCacheParameters extends CacheParameters 
  with CoherenceAgentParameters

abstract class L2HellaCacheBundle extends Bundle with L2HellaCacheParameters
abstract class L2HellaCacheModule extends Module with L2HellaCacheParameters

trait HasL2Id extends Bundle with CoherenceAgentParameters {
  val id = UInt(width  = log2Up(nTransactors))
}

trait HasL2InternalRequestState extends L2HellaCacheBundle {
  val tag_match = Bool()
  val meta = new L2Metadata
  val way_en = Bits(width = nWays)
}

object L2Metadata {
  def apply(tag: Bits, coh: MasterMetadata) = {
    val meta = new L2Metadata
    meta.tag := tag
    meta.coh := coh
    meta
  }
}
class L2Metadata extends Metadata with L2HellaCacheParameters {
  val coh = new MasterMetadata()(co) //co.masterMetadataOnFlush.clone
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

  val meta = Module(new MetadataArray(() => L2Metadata(UInt(0), co.masterMetadataOnFlush)))
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

class L2DataReadReq extends L2HellaCacheBundle with HasL2Id {
  val way_en = Bits(width = nWays)
  val addr   = Bits(width = tlAddrBits)
}

class L2DataWriteReq extends L2DataReadReq {
  val wmask  = Bits(width = tlWriteMaskBits)
  val data   = Bits(width = tlDataBits)
}

class L2DataResp extends Bundle with HasL2Id with TileLinkParameters {
  val data   = Bits(width = tlDataBits)
}

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

  val waddr = io.write.bits.addr
  val raddr = io.read.bits.addr
  val wmask = FillInterleaved(8, io.write.bits.wmask)
  val resp = (0 until nWays).map { w =>
    val array = Mem(Bits(width=rowBits), nSets*refillCycles, seqRead = true)
    val reg_raddr = Reg(UInt())
    when (io.write.bits.way_en(w) && io.write.valid) {
      array.write(waddr, io.write.bits.data, wmask)
    }.elsewhen (io.read.bits.way_en(w) && io.read.valid) {
      reg_raddr := raddr
    }
    array(reg_raddr)
  }
  io.resp.valid := ShiftRegister(io.read.fire(), 1)
  io.resp.bits.id := ShiftRegister(io.read.bits.id, 1)
  io.resp.bits.data := Mux1H(ShiftRegister(io.read.bits.way_en, 1), resp)

  io.read.ready := !io.write.valid // TODO 1R/W vs 1R1W?
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
  def doOutputArbitration[T <: Data](out: DecoupledIO[T], 
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
  
  // Propagate incoherence flags
  trackerList.map(_.io.tile_incoherent := io.incoherent.toBits)

  // Handle acquire transaction initiation
  val acquire = io.inner.acquire
  val any_acquire_conflict = trackerList.map(_.io.has_acquire_conflict).reduce(_||_)
  val block_acquires = any_acquire_conflict
  val alloc_arb = Module(new Arbiter(Bool(), trackerList.size))
  for( i <- 0 until trackerList.size ) {
    val t = trackerList(i).io.inner
    alloc_arb.io.in(i).valid := t.acquire.ready
    t.acquire.bits := acquire.bits
    t.acquire.valid := alloc_arb.io.in(i).ready
  }
  acquire.ready := trackerList.map(_.io.inner.acquire.ready).reduce(_||_) && !block_acquires
  alloc_arb.io.out.ready := acquire.valid && !block_acquires

  // Handle releases, which might be voluntary and might have data
  val release = io.inner.release
  val voluntary = co.isVoluntary(release.bits.payload)
  val any_release_conflict = trackerList.tail.map(_.io.has_release_conflict).reduce(_||_)
  val block_releases = Bool(false)
  val conflict_idx = Vec(trackerList.map(_.io.has_release_conflict)).lastIndexWhere{b: Bool => b}
  val release_idx = Mux(voluntary, Mux(any_release_conflict, conflict_idx, UInt(0)), conflict_idx)
  for( i <- 0 until trackerList.size ) {
    val t = trackerList(i).io.inner
    t.release.bits := release.bits 
    t.release.valid := release.valid && (release_idx === UInt(i)) && !block_releases
  }
  release.ready := Vec(trackerList.map(_.io.inner.release.ready)).read(release_idx) && !block_releases

  // Wire finished transaction acks
  val finish = io.inner.finish
  val finish_idx = finish.bits.payload.master_xact_id
  trackerList.zipWithIndex.map { case (t, i) => 
    t.io.inner.finish.valid := finish.valid && finish_idx === UInt(i)
  }
  trackerList.map(_.io.inner.finish.bits := finish.bits)
  finish.ready := Vec(trackerList.map(_.io.inner.finish.ready)).read(finish_idx)

  // Wire probe requests to clients
  doOutputArbitration(io.inner.probe, trackerList.map(_.io.inner.probe))

  // Wire grant reply to initiating client
  def hasData(m: LogicalNetworkIO[Grant]) = co.messageHasData(m.payload)
  doOutputArbitration(io.inner.grant, trackerList.map(_.io.inner.grant), tlDataBeats, hasData _)

  // Create an arbiter for the one memory port
  val outer_arb = Module(new UncachedTileLinkIOArbiterThatPassesId(trackerList.size),
                         {case TLId => outerId})
  outer_arb.io.in zip  trackerList map { case(arb, t) => arb <> t.io.outer }
  io.outer <> outer_arb.io.out

  // Wire local memories
  doOutputArbitration(io.meta.read, trackerList.map(_.io.meta.read))
  doOutputArbitration(io.meta.write, trackerList.map(_.io.meta.write))
  doOutputArbitration(io.data.read, trackerList.map(_.io.data.read))
  doOutputArbitration(io.data.write, trackerList.map(_.io.data.write))
  doInputRouting(io.meta.resp, trackerList.map(_.io.meta.resp))
  doInputRouting(io.data.resp, trackerList.map(_.io.data.resp))
}


abstract class L2XactTracker(innerId: String, outerId: String) extends L2HellaCacheModule {
  val io = new Bundle {
    val inner = Bundle(new TileLinkIO, {case TLId => innerId}).flip
    val outer = Bundle(new UncachedTileLinkIO, {case TLId => outerId})
    val tile_incoherent = Bits(INPUT, nClients)
    val has_acquire_conflict = Bool(OUTPUT)
    val has_release_conflict = Bool(OUTPUT)
    val data = new L2DataRWIO
    val meta = new L2MetaRWIO
  }

  val c_acq = io.inner.acquire.bits
  val c_rel = io.inner.release.bits
  val c_gnt = io.inner.grant.bits
  val c_ack = io.inner.finish.bits
  val m_gnt = io.outer.grant.bits

  def mergeData(acq: Acquire, old_data: UInt, new_data: UInt): UInt = {
    //TODO apply acq's write mask
    Mux(co.messageHasData(acq), old_data, new_data)
  }
}

class L2VoluntaryReleaseTracker(trackerId: Int, bankId: Int, innerId: String, outerId: String) extends L2XactTracker(innerId, outerId) {
  val s_idle :: s_meta_read :: s_meta_resp :: s_data_write :: s_meta_write :: s_grant :: s_busy :: Nil = Enum(UInt(), 7)
  val state = Reg(init=s_idle)

  val xact_src = Reg(io.inner.release.bits.header.src.clone)
  val xact_r_type = Reg(io.inner.release.bits.payload.r_type)
  val xact_addr = Reg(io.inner.release.bits.payload.addr.clone)
  val xact_client_xact_id = Reg(io.inner.release.bits.payload.client_xact_id.clone)
  val xact_data = Vec.fill(tlDataBeats){ Reg(io.inner.release.bits.payload.data.clone) }
  val xact_tag_match = Reg{ Bool() }
  val xact_meta = Reg{ new L2Metadata }
  val xact_way_en = Reg{ Bits(width = nWays) }
  val xact = Release(xact_r_type, xact_addr, xact_client_xact_id)

  val collect_inner_data = Reg(init=Bool(false))
  val (inner_data_cnt, inner_data_done) =
    Counter(io.inner.release.fire() && co.messageHasData(io.inner.release.bits.payload), tlDataBeats)
  val (local_data_cnt, local_data_done) =
    Counter(io.data.write.fire(), tlDataBeats)

  io.has_acquire_conflict := Bool(false)
  io.has_release_conflict := co.isCoherenceConflict(xact_addr, c_rel.payload.addr) && 
                               (state != s_idle)

  io.outer.grant.ready := Bool(false)
  io.outer.acquire.valid := Bool(false)
  io.inner.acquire.ready := Bool(false)
  io.inner.probe.valid := Bool(false)
  io.inner.release.ready := Bool(false)
  io.inner.grant.valid := Bool(false)
  io.inner.finish.ready := Bool(false)

  io.inner.grant.bits.header.src := UInt(bankId)
  io.inner.grant.bits.header.dst := xact_src
  io.inner.grant.bits.payload := Grant(Bool(false),
                                        co.getGrantTypeOnVoluntaryWriteback(xact_meta.coh),
                                        xact_client_xact_id,
                                        UInt(trackerId))

  io.data.read.valid := Bool(false)
  io.data.write.valid := Bool(false)
  io.data.write.bits.id := UInt(trackerId)
  io.data.write.bits.way_en := xact_way_en
  io.data.write.bits.addr := Cat(xact_addr, local_data_cnt)
  io.data.write.bits.wmask := SInt(-1)
  io.data.write.bits.data := xact_data(local_data_cnt)
  io.meta.read.valid := Bool(false)
  io.meta.read.bits.id := UInt(trackerId)
  io.meta.read.bits.idx := xact_addr(untagBits-1,blockOffBits)
  io.meta.read.bits.tag := xact_addr >> UInt(untagBits)
  io.meta.write.valid := Bool(false)
  io.meta.write.bits.id := UInt(trackerId)
  io.meta.write.bits.idx := xact_addr(untagBits-1,blockOffBits)
  io.meta.write.bits.way_en := xact_way_en
  io.meta.write.bits.data.tag := xact_addr >> UInt(untagBits)
  io.meta.write.bits.data.coh := co.masterMetadataOnRelease(xact, 
                                                            xact_meta.coh, 
                                                            xact_src)

  when(collect_inner_data) {
    io.inner.release.ready := Bool(true)
    when(io.inner.release.valid) {
      xact_data(inner_data_cnt) := c_rel.payload.data
    }
    when(inner_data_done) { collect_inner_data := Bool(false) }
  }

  switch (state) {
    is(s_idle) {
      io.inner.release.ready := Bool(true)
      when( io.inner.release.valid ) {
        xact_src := c_rel.header.src
        xact_r_type := c_rel.payload.r_type
        xact_addr := c_rel.payload.addr
        xact_client_xact_id := c_rel.payload.client_xact_id
        xact_data(UInt(0)) := c_rel.payload.data
        collect_inner_data := co.messageHasData(c_rel.payload)
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
                   Mux(co.messageHasData(xact), s_data_write, s_meta_write),
                   s_grant)
      }
    }
    is(s_data_write) {
      io.data.write.valid := (if(tlDataBeats == 1) Bool(true) 
                                  else !collect_inner_data || (local_data_cnt < inner_data_cnt))
      when(local_data_done) { state := s_meta_write }
    }
    is(s_meta_write) {
      io.meta.write.valid := Bool(true)
      when(io.meta.write.ready) { state := s_grant }
    }
    is(s_grant) {
      io.inner.grant.valid := Bool(true)
      when(io.inner.grant.ready) { 
        state := Mux(co.requiresAckForGrant(c_gnt.payload),
          s_busy, s_idle) 
      }
    }
    is(s_busy) {
      io.inner.finish.ready := Bool(true)
      when(io.inner.finish.valid) { state := s_idle }
    }
  }
}

class L2AcquireTracker(trackerId: Int, bankId: Int, innerId: String, outerId: String) extends L2XactTracker(innerId, outerId) {
  val s_idle :: s_meta_read :: s_meta_resp :: s_probe :: s_data_read_wb :: s_data_resp_wb :: s_outer_write_wb :: s_outer_read :: s_outer_resp :: s_data_read_hit :: s_data_resp_hit :: s_data_write :: s_outer_write_acq :: s_meta_write :: s_grant :: s_busy :: Nil = Enum(UInt(), 16)
  val state = Reg(init=s_idle)

  val xact_src = Reg(io.inner.acquire.bits.header.src.clone)
  val xact_uncached = Reg(io.inner.acquire.bits.payload.uncached.clone)
  val xact_a_type = Reg(io.inner.acquire.bits.payload.a_type.clone)
  val xact_addr = Reg(io.inner.acquire.bits.payload.addr.clone)
  val xact_client_xact_id = Reg(io.inner.acquire.bits.payload.client_xact_id.clone)
  val xact_subblock = Reg(io.inner.acquire.bits.payload.subblock.clone)
  val xact_data = Vec.fill(tlDataBeats){ Reg(io.inner.acquire.bits.payload.data.clone) }
  val xact_tag_match = Reg{ Bool() }
  val xact_meta = Reg{ new L2Metadata }
  val xact_way_en = Reg{ Bits(width = nWays) }
  val xact = Acquire(xact_uncached, xact_a_type, xact_addr, xact_client_xact_id, UInt(0), xact_subblock)

  val crel_had_data = Reg(init = Bool(false))
  val crel_was_voluntary = Reg(init = Bool(false))
  val crel_wb_src = Reg(init = UInt(0, width = log2Up(nClients)))
  val crel_wb_g_type = Reg(init = UInt(0, width = co.grantTypeWidth))
  val wb_buffer = Vec.fill(tlDataBeats){ Reg(io.inner.acquire.bits.payload.data.clone) }
  val wb_addr = Cat(xact_meta.tag, xact_addr(untagBits-1,blockOffBits))

  val collect_cacq_data = Reg(init=Bool(false))
  //TODO: zero width wires
  val (cacq_data_cnt, cacq_data_done) = 
    Counter(io.inner.acquire.fire() && co.messageHasData(io.inner.acquire.bits.payload), tlDataBeats)
  val (crel_data_cnt, crel_data_done) = 
    Counter(io.inner.release.fire() && co.messageHasData(io.inner.release.bits.payload), tlDataBeats)
  val (cgnt_data_cnt, cgnt_data_done) = 
    Counter(io.inner.grant.fire() && co.messageHasData(io.inner.grant.bits.payload), tlDataBeats)
  val (outer_data_write_cnt, outer_data_write_done) = 
    Counter(io.outer.acquire.fire() && co.messageHasData(io.outer.acquire.bits.payload), tlDataBeats)
  val (outer_data_resp_cnt, outer_data_resp_done) = 
    Counter(io.outer.grant.fire() && co.messageHasData(io.outer.grant.bits.payload), tlDataBeats)
  val (local_data_read_cnt, local_data_read_done) = Counter(io.data.read.fire(), tlDataBeats)
  val (local_data_write_cnt, local_data_write_done) = Counter(io.data.write.fire(), tlDataBeats)
  val (local_data_resp_cnt, local_data_resp_done) = Counter(io.data.resp.valid, tlDataBeats)

  val release_count = Reg(init = UInt(0, width = log2Up(nClients+1)))
  val pending_probes = Reg(init = co.dir().flush)
  val curr_p_id = co.dir().next(pending_probes)
  
  val needs_writeback = !xact_tag_match && co.needsWriteback(xact_meta.coh)
  val is_hit = xact_tag_match && co.isHit(xact, xact_meta.coh)
  val needs_probes = co.requiresProbes(xact, xact_meta.coh)
  //TODO: uncached does or does not allocate

  //TODO: Are there any races between lines with the same idx?
  //TODO: Allow hit under miss for stores
  io.has_acquire_conflict := co.isCoherenceConflict(xact.addr, c_acq.payload.addr) &&
                              xact.addr(untagBits-1,0) === c_acq.payload.addr(untagBits-1,0) &&
                              (state != s_idle) &&
                              !collect_cacq_data
  io.has_release_conflict := (co.isCoherenceConflict(xact.addr, c_rel.payload.addr) ||
                               co.isCoherenceConflict(wb_addr, c_rel.payload.addr)) &&
                               (state != s_idle)

  val next_coh_on_release = co.masterMetadataOnRelease(
                                    c_rel.payload, 
                                    xact_meta.coh,
                                    c_rel.header.src)
  val next_coh_on_grant = co.masterMetadataOnGrant(
                                    c_gnt.payload, 
                                    xact_meta.coh,
                                    c_gnt.header.dst)

  val outer_write_acq = Bundle(UncachedWrite(xact_addr, UInt(trackerId), xact_data(outer_data_write_cnt)), 
                          { case TLId => outerId })
  val outer_write_wb = Bundle(UncachedWrite(wb_addr, UInt(trackerId), wb_buffer(outer_data_write_cnt)),
                          { case TLId => outerId })
  val outer_read = Bundle(UncachedRead( xact_addr, UInt(trackerId)), { case TLId => outerId })

  io.outer.acquire.valid := Bool(false)
  io.outer.acquire.bits.payload := outer_read //default
  io.outer.acquire.bits.header.src := UInt(bankId)
  io.outer.grant.ready := Bool(true) //grant.data -> xact.data

  val cprb_for_cacq = Probe(co.getProbeType(xact, xact_meta.coh), xact_addr)
  val cprb_for_mvwb = Probe(co.getProbeTypeOnVoluntaryWriteback, wb_addr)
  //TODO cprb_for_mprb
  io.inner.probe.valid := Bool(false)
  io.inner.probe.bits.header.src := UInt(bankId)
  io.inner.probe.bits.header.dst := curr_p_id
  io.inner.probe.bits.payload := Mux(!xact_tag_match && needs_writeback, 
                                      cprb_for_mvwb,
                                      cprb_for_cacq)

  val cgnt_for_cacq = Grant(xact_uncached, co.getGrantType(xact, xact_meta.coh),
                              xact_client_xact_id,
                              UInt(trackerId),
                              xact_data(cgnt_data_cnt))
  val cgnt_for_cvwb = Grant(Bool(false), crel_wb_g_type, UInt(0), UInt(trackerId), UInt(0))
  io.inner.grant.valid := Bool(false)
  io.inner.grant.bits.header.src := UInt(bankId)
  io.inner.grant.bits.header.dst := Mux(crel_was_voluntary, crel_wb_src, xact_src)
  io.inner.grant.bits.payload := Mux(crel_was_voluntary, cgnt_for_cvwb, cgnt_for_cacq)

  io.inner.acquire.ready := Bool(false)
  io.inner.release.ready := Bool(false)
  io.inner.finish.ready := Bool(false)

  io.data.read.valid := Bool(false)
  io.data.read.bits.id := UInt(trackerId)
  io.data.read.bits.way_en := xact_way_en
  io.data.read.bits.addr := Cat(xact_addr, local_data_read_cnt)
  io.data.write.valid := Bool(false)
  io.data.write.bits.id := UInt(trackerId)
  io.data.write.bits.way_en := xact_way_en
  io.data.write.bits.addr := Cat(xact_addr, local_data_write_cnt)
  io.data.write.bits.wmask := SInt(-1)
  io.data.write.bits.data := xact_data(local_data_write_cnt)
  io.meta.read.valid := Bool(false)
  io.meta.read.bits.id := UInt(trackerId)
  io.meta.read.bits.idx := xact_addr(untagBits-1,blockOffBits)
  io.meta.read.bits.tag := xact_addr >> UInt(untagBits)
  io.meta.write.valid := Bool(false)
  io.meta.write.bits.id := UInt(trackerId)
  io.meta.write.bits.idx := xact_addr(untagBits-1,blockOffBits)
  io.meta.write.bits.way_en := xact_way_en
  io.meta.write.bits.data.tag := xact_addr >> UInt(untagBits)
  io.meta.write.bits.data.coh := next_coh_on_grant

  when(collect_cacq_data) {
    io.inner.acquire.ready := Bool(true)
    when(io.inner.acquire.valid) {
      xact_data(cacq_data_cnt) := c_acq.payload.data
    }
    when(cacq_data_done) { collect_cacq_data := Bool(false) }
  }

  switch (state) {
    is(s_idle) {
      io.inner.acquire.ready := Bool(true)
      when( io.inner.acquire.valid ) {
        xact_uncached := c_acq.payload.uncached
        xact_a_type := c_acq.payload.a_type
        xact_addr := c_acq.payload.addr
        xact_client_xact_id := c_acq.payload.client_xact_id
        xact_data(UInt(0)) := c_acq.payload.data
        xact_subblock := c_acq.payload.subblock
        xact_src := c_acq.header.src
        collect_cacq_data := co.messageHasData(c_acq.payload)
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
        val _needs_writeback = !_tag_match && co.needsWriteback(coh)
        val _is_hit = _tag_match && co.isHit(xact, coh)
        val _needs_probes = co.requiresProbes(xact, coh)
        when(_needs_probes) {
          val mask_incoherent = co.dir().full(coh.sharers) & ~io.tile_incoherent
          val mask_self = mask_incoherent & 
                            ~(!(co.requiresSelfProbe(xact) || _needs_writeback) << xact_src)
          pending_probes := mask_self
          release_count := co.dir().count(mask_self)
          crel_had_data := Bool(false)
          crel_was_voluntary := Bool(false)
        } 
        state := Mux(_tag_match,
                  Mux(_is_hit,
                    Mux(_needs_probes, s_probe, s_data_read_hit),
                    Mux(_needs_probes, s_probe, s_outer_read)),
                  Mux(_needs_writeback,
                    Mux(_needs_probes, s_probe, s_data_read_wb),
                    s_outer_read))
      }
    }
    is(s_probe) {
      io.inner.probe.valid := !co.dir().none(pending_probes)
      when(io.inner.probe.ready) {
        pending_probes := co.dir().pop(pending_probes, curr_p_id)
      }

      // Handle releases, which may have data being written back
      io.inner.release.ready := Bool(true)
      when(io.inner.release.valid) {
        xact_meta.coh := next_coh_on_release

        // Handle released dirty data
        when(co.messageHasData(c_rel.payload)) {
          crel_had_data := Bool(true)
          when(xact_tag_match) { // Hit, so merge new write with released data
            //TODO make sure cacq data is actually present before merging
            xact_data(crel_data_cnt) := mergeData(xact,
                                                  xact_data(crel_data_cnt),
                                                  io.inner.release.bits.payload.data)
          } .otherwise {         // Miss, we're voluntarily evicting this data
            wb_buffer(crel_data_cnt) := io.inner.release.bits.payload.data
          }
        }

        // Voluntary releases don't count against the release countdown
        // because we will get a further release ack from that client in
        // response to our probe. We don't send a grant acknowledging
        // a writeback or decrement release_count until we've received 
        // all the data beats.
        when(co.isVoluntary(c_rel.payload)) {
          when(!co.messageHasData(c_rel.payload) || crel_data_done) {
            crel_was_voluntary := Bool(true)
            crel_wb_src := c_rel.header.src
            crel_wb_g_type := co.getGrantTypeOnVoluntaryWriteback(xact_meta.coh)
          }
        } .otherwise {
          when(!co.messageHasData(c_rel.payload) || crel_data_done) {
            release_count := release_count - UInt(1)
          }
        }
      }

      // If we saw a voluntary writeback, we need to send an extra grant
      // to acknowledge it.
      when(crel_was_voluntary) { 
        io.inner.grant.valid := Bool(true)
        when(io.inner.grant.ready) { 
          crel_was_voluntary := Bool(false)
        }
      }

      when(release_count === UInt(0) && !crel_was_voluntary) {
        state := Mux(xact_tag_match,
                  Mux(is_hit,
                    Mux(crel_had_data, s_data_write, s_data_read_hit),
                    s_outer_read),
                  Mux(crel_had_data, s_outer_write_wb, s_data_read_wb))
      }
    }
    is(s_data_read_wb) {
      io.data.read.valid := Bool(true)
      when(local_data_read_done) { state := s_data_resp_wb }
      when(io.data.resp.valid) {
        wb_buffer(local_data_resp_cnt) := io.data.resp.bits.data
      }
    }
    is(s_data_resp_wb) {
      when(io.data.resp.valid) { wb_buffer(local_data_resp_cnt) := io.data.resp.bits.data }
      when(local_data_resp_done) { state := s_outer_write_wb }
    }
    is(s_outer_write_wb) {
      io.outer.acquire.valid := Bool(true)
      io.outer.acquire.bits.payload := outer_write_wb
      when(outer_data_write_done) { 
        state := s_outer_read
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
        xact_data(outer_data_resp_cnt) := mergeData(xact, xact_data(outer_data_resp_cnt), 
                                                      io.outer.grant.bits.payload.data)
        //TODO: set pending client state in xact_meta.coh
        when(outer_data_resp_done) { 
          state := Mux(co.messageHasData(io.outer.grant.bits.payload),
                                          s_data_write, s_data_read_hit)
        }
      }
    }
    is(s_data_read_hit) {
      io.data.read.valid := Bool(true)
      when(io.data.resp.valid) {
        //TODO make sure cacq data is actually present before merging
        xact_data(local_data_resp_cnt) := mergeData(xact, xact_data(local_data_resp_cnt), 
                                                      io.data.resp.bits.data)
      }
      when(local_data_read_done) { state := s_data_resp_hit }
    }
    is(s_data_resp_hit) {
      when(io.data.resp.valid) {
        xact_data(local_data_resp_cnt) := mergeData(xact, xact_data(local_data_resp_cnt), 
                                                      io.data.resp.bits.data)
      }
      when(local_data_resp_done) { state := s_meta_write }
    }
    is(s_data_write) {
      io.data.write.valid := Bool(true)
      when(local_data_write_done) {
        state := s_meta_write
      }
    }
    is(s_meta_write) {
      io.meta.write.valid := Bool(true)
      when(io.meta.write.ready) { state := s_grant }
    }
    is(s_grant) {
      io.inner.grant.valid := Bool(true)
      when(!co.messageHasData(c_gnt.payload) || cgnt_data_done) { 
        state := Mux(co.requiresAckForGrant(c_gnt.payload),
          s_busy, s_idle) 
      }
    }
    is(s_busy) {
      io.inner.finish.ready := Bool(true)
      when(io.inner.finish.valid) { state := s_idle }
    }
  }
}
