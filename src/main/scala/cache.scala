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
  val refillCycles = params(TLDataBits)/rowBits
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

class L2MetadataArray extends L2HellaCacheModule {
  val io = new Bundle {
    val read = Decoupled(new L2MetaReadReq).flip
    val write = Decoupled(new L2MetaWriteReq).flip
    val resp = Valid(new L2MetaResp)
  }

  val meta = Module(new MetadataArray(() => L2Metadata(UInt(0), co.masterMetadataOnFlush)))
  meta.io.read <> io.read
  meta.io.write <> io.write
  
  val s1_clk_en = Reg(next = io.read.fire())
  val s1_tag = RegEnable(io.read.bits.tag, io.read.valid)
  val s1_id = RegEnable(io.read.bits.id, io.read.valid)
  def wayMap[T <: Data](f: Int => T) = Vec((0 until nWays).map(f))
  val s1_tag_eq_way = wayMap((w: Int) => meta.io.resp(w).tag === s1_tag)
  val s1_tag_match_way = wayMap((w: Int) => s1_tag_eq_way(w) && co.isValid(meta.io.resp(w).coh)).toBits
  val s2_tag_match_way = RegEnable(s1_tag_match_way, s1_clk_en)
  val s2_tag_match = s2_tag_match_way.orR
  val s2_hit_coh = Mux1H(s2_tag_match_way, wayMap((w: Int) => RegEnable(meta.io.resp(w).coh, s1_clk_en)))
  //val s2_hit = s2_tag_match && tl.co.isHit(s2_req.cmd, s2_hit_state) && s2_hit_state === tl.co.newStateOnHit(s2_req.cmd, s2_hit_state)

  val replacer = params(Replacer)()
  val s1_replaced_way_en = UIntToOH(replacer.way)
  val s2_replaced_way_en = UIntToOH(RegEnable(replacer.way, s1_clk_en))
  val s2_repl_meta = Mux1H(s2_replaced_way_en, wayMap((w: Int) => 
    RegEnable(meta.io.resp(w), s1_clk_en && s1_replaced_way_en(w))).toSeq)

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
  val addr   = Bits(width = params(TLAddrBits))
}

class L2DataWriteReq extends L2DataReadReq {
  val wmask  = Bits(width = params(TLWriteMaskBits))
  val data   = Bits(width = params(TLDataBits))
}

class L2DataResp extends Bundle with HasL2Id {
  val data   = Bits(width = params(TLDataBits))
}

class L2DataArray extends L2HellaCacheModule {
  val io = new Bundle {
    val read = Decoupled(new L2DataReadReq).flip
    val write = Decoupled(new L2DataWriteReq).flip
    val resp = Valid(new L2DataResp)
  }

  val waddr = io.write.bits.addr
  val raddr = io.read.bits.addr
  val wmask = FillInterleaved(wordBits, io.write.bits.wmask)
  val resp = (0 until nWays).map { w =>
    val array = Mem(Bits(width=params(RowBits)), nSets*refillCycles, seqRead = true)
    when (io.write.bits.way_en(w) && io.write.valid) {
      array.write(waddr, io.write.bits.data, wmask)
    }
    array(RegEnable(raddr, io.read.bits.way_en(w) && io.read.valid))
  }
  io.resp.valid := ShiftRegister(io.read.valid, 2)
  io.resp.bits.id := ShiftRegister(io.read.bits.id, 2)
  io.resp.bits.data := Mux1H(ShiftRegister(io.read.bits.way_en, 2), resp)

  io.read.ready := Bool(true)
  io.write.ready := Bool(true)
}

class L2HellaCache(bankId: Int, innerId: String, outerId: String) extends 
    CoherenceAgent(innerId, outerId) with L2HellaCacheParameters {

  require(isPow2(nSets))
  require(isPow2(nWays)) 
  require(refillCycles == 1)

  val tshrfile = Module(new TSHRFile(bankId, innerId, outerId))
  val meta = Module(new L2MetadataArray)
  val data = Module(new L2DataArray)

  tshrfile.io.inner <> io.inner
  tshrfile.io.meta_read <> meta.io.read
  tshrfile.io.meta_write <> meta.io.write
  tshrfile.io.meta_resp <> meta.io.resp
  tshrfile.io.data_read <> data.io.read
  tshrfile.io.data_write <> data.io.write
  tshrfile.io.data_resp <> data.io.resp
  io.outer <> tshrfile.io.outer
  io.incoherent <> tshrfile.io.incoherent
}


class TSHRFile(bankId: Int, innerId: String, outerId: String) extends L2HellaCacheModule {
  val io = new Bundle {
    val inner = Bundle(new TileLinkIO, {case TLId => innerId}).flip
    val outer = Bundle(new UncachedTileLinkIO, {case TLId => outerId})
    val incoherent = Vec.fill(nClients){Bool()}.asInput
    val meta_read = Decoupled(new L2MetaReadReq)
    val meta_write = Decoupled(new L2MetaWriteReq)
    val meta_resp = Valid(new L2MetaResp).flip
    val data_read = Decoupled(new L2DataReadReq)
    val data_write = Decoupled(new L2DataWriteReq)
    val data_resp = Valid(new L2DataResp).flip
  }

  // Wiring helper funcs
  def doOutputArbitration[T <: Data](out: DecoupledIO[T], ins: Seq[DecoupledIO[T]]) {
    val arb = Module(new RRArbiter(out.bits.clone, ins.size))
    out <> arb.io.out
    arb.io.in zip ins map { case (a, in) => a <> in }
  }

  def doInputRouting[T <: HasL2Id](in: ValidIO[T], outs: Seq[ValidIO[T]]) {
    outs.map(_.bits := in.bits)
    outs.zipWithIndex.map { case (o, i) => o.valid := UInt(i) === in.bits.id }
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

  // Handle probe requests
  doOutputArbitration(io.inner.probe, trackerList.map(_.io.inner.probe))

  // Handle releases, which might be voluntary and might have data
  val release = io.inner.release
  val voluntary = co.isVoluntary(release.bits.payload)
  val any_release_conflict = trackerList.tail.map(_.io.has_release_conflict).reduce(_||_)
  val block_releases = Bool(false)
  val conflict_idx = Vec(trackerList.map(_.io.has_release_conflict)).lastIndexWhere{b: Bool => b}
  //val release_idx = Mux(voluntary, Mux(any_release_conflict, conflict_idx, UInt(0)), release.bits.payload.master_xact_id) // TODO: Add merging logic to allow allocated AcquireTracker to handle conflicts, send all necessary grants, use first sufficient response
  val release_idx = Mux(voluntary, UInt(0), release.bits.payload.master_xact_id)
  for( i <- 0 until trackerList.size ) {
    val t = trackerList(i).io.inner
    t.release.bits := release.bits 
    t.release.valid := release.valid && (release_idx === UInt(i)) && !block_releases
  }
  release.ready := Vec(trackerList.map(_.io.inner.release.ready)).read(release_idx) && !block_releases

  // Reply to initial requestor
  doOutputArbitration(io.inner.grant, trackerList.map(_.io.inner.grant))

  // Free finished transactions on ack
  val finish = io.inner.finish
  val finish_idx = finish.bits.payload.master_xact_id
  trackerList.zipWithIndex.map { case (t, i) => 
    t.io.inner.finish.valid := finish.valid && finish_idx === UInt(i)
  }
  trackerList.map(_.io.inner.finish.bits := finish.bits)
  finish.ready := Vec(trackerList.map(_.io.inner.finish.ready)).read(finish_idx)

  // Arbitrate for the outer memory port
  val outer_arb = Module(new UncachedTileLinkIOArbiterThatPassesId(trackerList.size),
                         {case TLId => outerId})
  outer_arb.io.in zip  trackerList map { case(arb, t) => arb <> t.io.outer }
  io.outer <> outer_arb.io.out

  // Local memory
  doOutputArbitration(io.meta_read, trackerList.map(_.io.meta_read))
  doOutputArbitration(io.meta_write, trackerList.map(_.io.meta_write))
  doOutputArbitration(io.data_read, trackerList.map(_.io.data_read))
  doOutputArbitration(io.data_write, trackerList.map(_.io.data_write))
  doInputRouting(io.meta_resp, trackerList.map(_.io.meta_resp))
  doInputRouting(io.data_resp, trackerList.map(_.io.data_resp))

}


abstract class L2XactTracker(innerId: String, outerId: String) extends L2HellaCacheModule {
  val io = new Bundle {
    val inner = Bundle(new TileLinkIO, {case TLId => innerId}).flip
    val outer = Bundle(new UncachedTileLinkIO, {case TLId => outerId})
    val tile_incoherent = Bits(INPUT, nClients)
    val has_acquire_conflict = Bool(OUTPUT)
    val has_release_conflict = Bool(OUTPUT)
    val meta_read = Decoupled(new L2MetaReadReq)
    val meta_write = Decoupled(new L2MetaWriteReq)
    val meta_resp = Valid(new L2MetaResp).flip
    val data_read = Decoupled(new L2DataReadReq)
    val data_write = Decoupled(new L2DataWriteReq)
    val data_resp = Valid(new L2DataResp).flip
  }

  val c_acq = io.inner.acquire.bits
  val c_rel = io.inner.release.bits
  val c_gnt = io.inner.grant.bits
  val c_ack = io.inner.finish.bits
  val m_gnt = io.outer.grant.bits

  def mergeData(acq: Acquire, data: UInt): UInt = {
    //TODO wite mask
    Mux(co.messageHasData(acq), acq.data, data)
  }
}

class L2VoluntaryReleaseTracker(trackerId: Int, bankId: Int, innerId: String, outerId: String) extends L2XactTracker(innerId, outerId) {
  val s_idle :: s_meta_read :: s_meta_resp :: s_meta_write :: s_data_write :: s_grant :: s_busy :: Nil = Enum(UInt(), 7)
  val state = Reg(init=s_idle)
  val xact  = Reg{ new Release }
  val xact_internal = Reg{ new L2MetaResp }
  val init_client_id = Reg(init=UInt(0, width = log2Up(nClients)))

  io.has_acquire_conflict := Bool(false)
  io.has_release_conflict := co.isCoherenceConflict(xact.addr, c_rel.payload.addr) && 
                               (state != s_idle)

  io.outer.grant.ready := Bool(false)
  io.outer.acquire.valid := Bool(false)

  io.inner.acquire.ready := Bool(false)
  io.inner.probe.valid := Bool(false)
  io.inner.release.ready := Bool(false)
  io.inner.grant.valid := Bool(false)
  io.inner.grant.bits.header.src := UInt(bankId)
  io.inner.grant.bits.header.dst := init_client_id
  io.inner.grant.bits.payload := Grant(co.getGrantType(xact, xact_internal.meta.coh),
                                        xact.client_xact_id,
                                        UInt(trackerId))

  io.data_read.valid := Bool(false)
  io.data_write.valid := Bool(false)
  io.data_write.bits.id := UInt(trackerId)
  io.data_write.bits.way_en := xact_internal.way_en
  io.data_write.bits.addr := xact.addr
  io.data_write.bits.wmask := SInt(-1)
  io.data_write.bits.data := xact.data
  io.meta_read.valid := Bool(false)
  io.meta_read.bits.id := UInt(trackerId)
  io.meta_read.bits.idx := xact.addr(untagBits-1,blockOffBits)
  io.meta_read.bits.tag := xact.addr >> UInt(untagBits)
  io.meta_write.valid := Bool(false)
  io.meta_write.bits.id := UInt(trackerId)
  io.meta_write.bits.idx := xact.addr(untagBits-1,blockOffBits)
  io.meta_write.bits.way_en := xact_internal.way_en
  io.meta_write.bits.data := xact_internal.meta

  switch (state) {
    is(s_idle) {
      io.inner.release.ready := Bool(true)
      when( io.inner.release.valid ) {
        xact := c_rel.payload
        init_client_id := c_rel.header.src
        state := s_meta_read
      }
    }
    is(s_meta_read) {
      io.meta_read.valid := Bool(true)
      when(io.meta_read.ready) { state := s_meta_resp }
    }
    is(s_meta_resp) {
      when(io.meta_resp.valid) {
        xact_internal := co.masterMetadataOnRelease(xact, 
                                                    io.meta_resp.bits.meta.coh, 
                                                    init_client_id)
        state := Mux(io.meta_resp.bits.tag_match, 
                   Mux(co.messageHasData(xact), s_data_write, s_meta_write),
                   s_grant)
      }
    }
    is(s_data_write) {
      io.data_write.valid := Bool(true)
      when(io.data_write.ready) { state := s_meta_write }
    }
    is(s_meta_write) {
      io.meta_write.valid := Bool(true)
      when(io.meta_write.ready) { state := s_grant }
    }
    is(s_grant) {
      io.inner.grant.valid := Bool(true)
      when(io.inner.grant.ready) { 
        state := Mux(co.requiresAckForGrant(c_gnt.payload.g_type),
          s_busy, s_idle) 
      }
    }
    is(s_busy) {
      when(io.inner.finish.valid) { state := s_idle }
    }
  }
}

class L2AcquireTracker(trackerId: Int, bankId: Int, innerId: String, outerId: String) extends L2XactTracker(innerId, outerId) {
  val s_idle :: s_meta_read :: s_meta_resp :: s_probe :: s_data_read_wb :: s_data_resp_wb :: s_outer_write_wb :: s_outer_read :: s_outer_resp :: s_data_read_hit :: s_data_resp_hit :: s_data_write :: s_outer_write_acq :: s_meta_write :: s_grant :: s_busy :: Nil = Enum(UInt(), 16)
  val state = Reg(init=s_idle)
  val xact  = Reg{ new Acquire }
  val xact_internal = Reg{ new L2MetaResp }
  val test = Reg{UInt()}
  val init_client_id = Reg(init=UInt(0, width = log2Up(nClients)))
  //TODO: Will need id reg for merged release xacts
  

  val release_count = Reg(init = UInt(0, width = log2Up(nClients)))
  val pending_probes = Reg(init = co.dir().flush)
  val curr_p_id = co.dir().next(pending_probes)
  
  val is_uncached = co.messageIsUncached(xact)
  val tag_match = xact_internal.tag_match
  val needs_writeback = co.needsWriteback(xact_internal.meta.coh)
  val is_hit = co.isHit(xact, xact_internal.meta.coh)
  val needs_probes = co.requiresProbes(xact.a_type, xact_internal.meta.coh)
  val c_rel_had_data = Reg(init = Bool(false))
  val c_rel_was_voluntary = Reg(init = Bool(false))
  val wb_buffer = Reg{xact.data.clone}

  io.has_acquire_conflict := co.isCoherenceConflict(xact.addr, c_acq.payload.addr) && 
                              (state != s_idle) //TODO: Also indexes
  io.has_release_conflict := co.isCoherenceConflict(xact.addr, c_rel.payload.addr) && 
                              (state != s_idle) //TODO: Also indexes?

  val outer_write_acq = Bundle(Acquire(co.getUncachedWriteAcquireType, 
                                        xact.addr, 
                                        UInt(trackerId), 
                                        xact.data), { case TLId => outerId })
  val outer_write_wb = Bundle(Acquire(co.getUncachedWriteAcquireType, 
                                        Cat(xact_internal.meta.tag, 
                                            xact.addr(untagBits-1,blockOffBits)), 
                                        UInt(trackerId), 
                                        wb_buffer), { case TLId => outerId })
  val outer_read = Bundle(Acquire(co.getUncachedReadAcquireType, 
                                        xact.addr, 
                                        UInt(trackerId)), { case TLId => outerId })
  io.outer.acquire.valid := Bool(false)
  io.outer.acquire.bits.payload := outer_read //default
  io.outer.acquire.bits.header.src := UInt(bankId)
  io.outer.grant.ready := Bool(true) //grant.data -> xact.data

  val inner_probe_cacq = Probe(co.getProbeType(xact, xact_internal.meta.coh),
                                xact.addr,
                                UInt(trackerId))
  val inner_probe_wb = Probe(co.getProbeTypeOnVoluntaryWriteback,
                                xact.addr,
                                UInt(trackerId))
  //TODO inner_probe_mprb
  io.inner.probe.valid := Bool(false)
  io.inner.probe.bits.header.src := UInt(bankId)
  io.inner.probe.bits.header.dst := curr_p_id
  io.inner.probe.bits.payload := Mux(needs_writeback, 
                                      inner_probe_wb,
                                      inner_probe_cacq)

  val grant_type = co.getGrantType(xact, xact_internal.meta.coh)
  io.inner.grant.valid := Bool(false)
  io.inner.grant.bits.header.src := UInt(bankId)
  io.inner.grant.bits.header.dst := init_client_id
  io.inner.grant.bits.payload := Grant(grant_type,
                                        xact.client_xact_id,
                                        UInt(trackerId),
                                        xact.data)

  io.inner.acquire.ready := Bool(false)
  io.inner.release.ready := Bool(false)

  io.data_read.valid := Bool(false)
  io.data_read.bits.id := UInt(trackerId)
  io.data_read.bits.way_en := xact_internal.way_en
  io.data_read.bits.addr := xact.addr 
  io.data_write.valid := Bool(false)
  io.data_write.bits.id := UInt(trackerId)
  io.data_write.bits.way_en := xact_internal.way_en
  io.data_write.bits.addr := xact.addr
  io.data_write.bits.wmask := SInt(-1)
  io.data_write.bits.data := xact.data
  io.meta_read.valid := Bool(false)
  io.meta_read.bits.id := UInt(trackerId)
  io.meta_read.bits.idx := xact.addr(untagBits-1,blockOffBits)
  io.meta_read.bits.tag := xact.addr >> UInt(untagBits)
  io.meta_write.valid := Bool(false)
  io.meta_write.bits.id := UInt(trackerId)
  io.meta_write.bits.idx := xact.addr(untagBits-1,blockOffBits)
  io.meta_write.bits.way_en := xact_internal.way_en
  io.meta_write.bits.data.tag := xact.addr >> UInt(untagBits)
  io.meta_write.bits.data.coh := xact_internal.meta.coh

  switch (state) {
    is(s_idle) {
      io.inner.acquire.ready := Bool(true)
      when( io.inner.acquire.valid ) {
        xact := c_acq.payload
        init_client_id := c_acq.header.src
        state := s_meta_read
      }
    }
    is(s_meta_read) {
      io.meta_read.valid := Bool(true)
      when(io.meta_read.ready) { state := s_meta_resp }
    }
    is(s_meta_resp) {
      when(io.meta_resp.valid) {
        val coh = io.meta_resp.bits.meta.coh
        val _tag_match = io.meta_resp.bits.tag_match
        val _needs_writeback = co.needsWriteback(coh)
        val _is_hit = co.isHit(xact, coh)
        val _needs_probes = co.requiresProbes(xact.a_type, coh)
        xact_internal := io.meta_resp.bits
        test := UInt(0)
        when(!_needs_writeback) {
//          xact_internal.meta.coh := co.masterMetadataOnFlush
          test := UInt(12)
        }
        when(_needs_probes) {
          pending_probes := coh.sharers
          release_count := co.dir().count(coh.sharers)
          c_rel_had_data := Bool(false)
          c_rel_was_voluntary := Bool(false)
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
      val skip = io.tile_incoherent(curr_p_id) ||
                  ((curr_p_id === init_client_id) && 
                    !co.requiresSelfProbe(xact.a_type))
      io.inner.probe.valid := !(co.dir().none(pending_probes) || skip)
      when(io.inner.probe.ready || skip) {
        co.dir().pop(pending_probes, curr_p_id)
      }
      when(skip) { release_count := release_count - UInt(1) }

      // Handle releases, which may have data being written back
      io.inner.release.ready := Bool(true)
      when(io.inner.release.valid) {
/*
        xact_internal.meta.coh := co.masterMetadataOnRelease(
                                    c_rel.payload, 
                                    xact_internal.meta.coh,
                                    c_rel.header.src)
*/
        when(co.messageHasData(c_rel.payload)) {
          c_rel_had_data := Bool(true)
          when(tag_match) {
            xact.data := mergeData(xact, io.inner.release.bits.payload.data)
          } .otherwise {
            wb_buffer := io.inner.release.bits.payload.data
          }
        }
        when(co.isVoluntary(c_rel.payload)) {
          c_rel_was_voluntary := Bool(true)
        }
        when(!co.isVoluntary(c_rel.payload)) {
          release_count := release_count - Mux(skip, UInt(2), UInt(1))
          when(release_count === UInt(1)) {
              state := Mux(tag_match,
                        Mux(is_hit,
                          Mux(c_rel_had_data, s_meta_write, s_data_read_hit),
                          s_outer_read),
                        Mux(c_rel_had_data, s_outer_write_wb, s_data_read_wb))
          }
        }
      }
    }
    is(s_data_read_wb) {
      io.data_read.valid := Bool(true)
      when(io.data_read.ready) {
        state := s_data_resp_wb
      }
    }
    is(s_data_resp_wb) {
      when(io.data_resp.valid) {
        wb_buffer := io.data_resp.bits.data
        state := s_outer_write_wb
      }
    }
    is(s_outer_write_wb) {
      io.outer.acquire.valid := Bool(true)
      io.outer.acquire.bits.payload := outer_write_wb
      when(io.outer.acquire.ready) { 
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
        xact.data := mergeData(xact, io.outer.grant.bits.payload.data)
        //TODO: set pending client state in xact_internal.meta.coh
        state := Mux(co.messageHasData(io.outer.grant.bits.payload),
                      s_data_write, s_data_read_hit)
      }
    }
    is(s_data_read_hit) {
      io.data_read.valid := Bool(true)
      when(io.data_read.ready) {
        state := s_data_resp_hit
      }
    }
    is(s_data_resp_hit) {
      when(io.data_resp.valid) {
        xact.data := mergeData(xact, io.data_resp.bits.data)
        state := s_meta_write
      }
    }
    is(s_data_write) {
      io.data_write.valid := Bool(true)
      when(io.data_write.ready) {
        state := s_meta_write
      }
    }
    is(s_meta_write) {
      io.meta_write.valid := Bool(true)
      when(io.meta_write.ready) { state := s_grant }
    }
    is(s_grant) {
      io.inner.grant.valid := Bool(true)
      when(io.inner.grant.ready) { 
        state := Mux(co.requiresAckForGrant(c_gnt.payload.g_type),
          s_busy, s_idle) 
      }
    }
    is(s_busy) {
      when(io.inner.finish.valid) { state := s_idle }
    }
  }
}
