package uncore
import Chisel._

trait CacheConfig {
  def sets: Int
  def ways: Int
  def tl: TileLinkConfiguration
  def as: AddressSpaceConfiguration
  def dm: Boolean
  def states: Int
  def lines: Int
  def tagbits: Int
  def idxbits: Int
  def offbits: Int
  def untagbits: Int
  def rowbits: Int
}

case class L2CacheConfig(
    val sets: Int, val ways: Int,
    val nrpq: Int, val nsdq: Int,
    val nReleaseTransactions: Int, 
    val nAcquireTransactions: Int,
    val tl: TileLinkConfiguration,
    val as: AddressSpaceConfiguration) 
  extends CoherenceAgentConfiguration
  with CacheConfig
{
  def states = tl.co.nMasterStates
  def lines = sets*ways
  def dm = ways == 1
  def offbits = 0
  def lineaddrbits = tl.addrBits
  def idxbits = log2Up(sets)
  def waybits = log2Up(ways)
  def untagbits = offbits + idxbits
  def tagbits = lineaddrbits - idxbits
  def wordbits = 64
  def wordbytes = wordbits/8
  def wordoffbits = log2Up(wordbytes)
  def rowbits = tl.dataBits
  def rowbytes = rowbits/8
  def rowoffbits = log2Up(rowbytes)
  def refillcycles = tl.dataBits/(rowbits)
  def statebits = log2Up(states)
  def nTSHRs = nReleaseTransactions + nAcquireTransactions

  require(states > 0)
  require(isPow2(sets))
  require(isPow2(ways)) // TODO: relax this
  require(refillcycles == 1) //TODO: relax this?
}

abstract trait CacheBundle extends Bundle {
  implicit val cacheconf: CacheConfig
  override def clone = this.getClass.getConstructors.head.newInstance(cacheconf).asInstanceOf[this.type]
}

abstract trait L2CacheBundle extends Bundle {
  implicit val l2cacheconf: L2CacheConfig
  override def clone = this.getClass.getConstructors.head.newInstance(l2cacheconf).asInstanceOf[this.type]
}

trait HasL2Id extends L2CacheBundle {
  val id = UInt(width  = log2Up(l2cacheconf.nTSHRs))
}

trait HasL2InternalRequestState extends L2CacheBundle {
  val tag_match = Bool()
  val old_meta = new L2MetaData
  val way_en = Bits(width = l2cacheconf.ways)
}

abstract class ReplacementPolicy {
  def way: UInt
  def miss: Unit
  def hit: Unit
}

class RandomReplacement(implicit val cacheconf: CacheConfig) extends ReplacementPolicy {
  private val replace = Bool()
  replace := Bool(false)
  val lfsr = LFSR16(replace)

  def way = if(cacheconf.dm) UInt(0) else lfsr(log2Up(cacheconf.ways)-1,0)
  def miss = replace := Bool(true)
  def hit = {}
}

abstract class MetaData(implicit val cacheconf: CacheConfig) extends CacheBundle {
  val tag = Bits(width = cacheconf.tagbits)
}

class MetaReadReq(implicit val cacheconf: CacheConfig) extends CacheBundle {
  val idx  = Bits(width = cacheconf.idxbits)
}

class MetaWriteReq[T <: MetaData](gen: T)(implicit conf: CacheConfig) extends MetaReadReq {
  val way_en = Bits(width = conf.ways)
  val data = gen.clone
  override def clone = new MetaWriteReq(gen)(conf).asInstanceOf[this.type]
}

class MetaDataArray[T <: MetaData](gen: () => T)(implicit conf: CacheConfig) extends Module {
  implicit val tl = conf.tl
  val rstVal = gen()
  val io = new Bundle {
    val read = Decoupled(new MetaReadReq).flip
    val write = Decoupled(new MetaWriteReq(rstVal.clone)).flip
    val resp = Vec.fill(conf.ways){rstVal.clone.asOutput}
  }

  val metabits = rstVal.getWidth
  val rst_cnt = Reg(init=UInt(0, log2Up(conf.sets+1)))
  val rst = rst_cnt < UInt(conf.sets)
  when (rst) { rst_cnt := rst_cnt+UInt(1) }

  val tags = Mem(UInt(width = metabits*conf.ways), conf.sets, seqRead = true)

  when (rst || io.write.valid) {
    val addr = Mux(rst, rst_cnt, io.write.bits.idx)
    val data = Mux(rst, rstVal, io.write.bits.data).toBits
    val mask = Mux(rst, SInt(-1), io.write.bits.way_en)
    tags.write(addr, Fill(conf.ways, data), FillInterleaved(metabits, mask))
  }
  val tag = tags(RegEnable(io.read.bits.idx, io.read.valid))

  for (w <- 0 until conf.ways) {
    val m = tag(metabits*(w+1)-1, metabits*w)
    io.resp(w) := rstVal.clone.fromBits(m)
  }

  io.read.ready := !rst && !io.write.valid // so really this could be a 6T RAM
  io.write.ready := !rst
}

object L2MetaData {
  def apply(tag: Bits, state: UInt, sharers: UInt)(implicit conf: L2CacheConfig) = {
    val meta = new L2MetaData
    meta.tag := tag
    meta.state := state
    meta.sharers := sharers
    meta
  }
}
class L2MetaData(implicit val l2cacheconf: L2CacheConfig) extends MetaData
  with L2CacheBundle {
  val state = UInt(width = l2cacheconf.statebits)
  val sharers = Bits(width = l2cacheconf.tl.ln.nClients)
}

/*
class L3MetaData(implicit conf: L3CacheConfig) extends MetaData()(conf) {
  val cstate = UInt(width = cacheconf.cstatebits)
  val mstate = UInt(width = cacheconf.mstatebits)
  val sharers = Bits(width = cacheconf.tl.ln.nClients)
}
*/

class L2MetaReadReq(implicit val l2cacheconf: L2CacheConfig) extends MetaReadReq
  with HasL2Id {
  val tag = Bits(width = l2cacheconf.tagbits)
}

class L2MetaWriteReq(implicit val l2cacheconf: L2CacheConfig) extends MetaWriteReq[L2MetaData](new L2MetaData)
  with HasL2Id

class L2MetaResp(implicit val l2cacheconf: L2CacheConfig) extends L2CacheBundle
  with HasL2Id 
  with HasL2InternalRequestState

class L2MetaDataArray(implicit conf: L2CacheConfig) extends Module {
  implicit val tl = conf.tl
  val io = new Bundle {
    val read = Decoupled(new L2MetaReadReq).flip
    val write = Decoupled(new L2MetaWriteReq).flip
    val resp = Valid(new L2MetaResp)
  }

  val meta = Module(new MetaDataArray(() => L2MetaData(UInt(0), tl.co.newStateOnFlush, UInt(0))))
  meta.io.read <> io.read
  meta.io.write <> io.write
  
  val s1_clk_en = Reg(next = io.read.fire())
  val s1_tag = RegEnable(io.read.bits.tag, io.read.valid)
  val s1_id = RegEnable(io.read.bits.id, io.read.valid)
  def wayMap[T <: Data](f: Int => T) = Vec((0 until conf.ways).map(f))
  val s1_tag_eq_way = wayMap((w: Int) => meta.io.resp(w).tag === s1_tag)
  val s1_tag_match_way = wayMap((w: Int) => s1_tag_eq_way(w) && tl.co.isValid(meta.io.resp(w).state)).toBits
  val s2_tag_match_way = RegEnable(s1_tag_match_way, s1_clk_en)
  val s2_tag_match = s2_tag_match_way.orR
  val s2_hit_state = Mux1H(s2_tag_match_way, wayMap((w: Int) => RegEnable(meta.io.resp(w).state, s1_clk_en)))
  val s2_hit_sharers = Mux1H(s2_tag_match_way, wayMap((w: Int) => RegEnable(meta.io.resp(w).sharers, s1_clk_en)))

  val replacer = new RandomReplacement
  val s1_replaced_way_en = UIntToOH(replacer.way)
  val s2_replaced_way_en = UIntToOH(RegEnable(replacer.way, s1_clk_en))
  val s2_repl_meta = Mux1H(s2_replaced_way_en, wayMap((w: Int) => 
    RegEnable(meta.io.resp(w), s1_clk_en && s1_replaced_way_en(w))).toSeq)

  io.resp.valid := Reg(next = s1_clk_en)
  io.resp.bits.id := RegEnable(s1_id, s1_clk_en)
  io.resp.bits.tag_match := s2_tag_match
  io.resp.bits.old_meta := Mux(s2_tag_match, 
    L2MetaData(s2_repl_meta.tag, s2_hit_state, s2_hit_sharers), 
    s2_repl_meta)
  io.resp.bits.way_en := Mux(s2_tag_match, s2_tag_match_way, s2_replaced_way_en)
}

class L2DataReadReq(implicit val l2cacheconf: L2CacheConfig) extends L2CacheBundle 
  with HasL2Id {
  val way_en = Bits(width = l2cacheconf.ways)
  val addr   = Bits(width = l2cacheconf.tl.addrBits)
}

class L2DataWriteReq(implicit l2cacheconf: L2CacheConfig) extends L2DataReadReq {
  val wmask  = Bits(width = l2cacheconf.tl.writeMaskBits)
  val data   = Bits(width = l2cacheconf.tl.dataBits)
}

class L2DataResp(implicit val l2cacheconf: L2CacheConfig) extends L2CacheBundle 
  with HasL2Id {
  val data   = Bits(width = l2cacheconf.tl.dataBits)
}

class L2DataArray(implicit conf: L2CacheConfig) extends Module {
  val io = new Bundle {
    val read = Decoupled(new L2DataReadReq).flip
    val write = Decoupled(new L2DataWriteReq).flip
    val resp = Valid(new L2DataResp)
  }

  val waddr = io.write.bits.addr
  val raddr = io.read.bits.addr
  val wmask = FillInterleaved(conf.wordbits, io.write.bits.wmask)
  val resp = (0 until conf.ways).map { w =>
    val array = Mem(Bits(width=conf.rowbits), conf.sets*conf.refillcycles, seqRead = true)
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

class L2InternalAcquire(implicit val l2cacheconf: L2CacheConfig) extends Acquire()(l2cacheconf.tl) 
  with HasL2InternalRequestState

class L2InternalRelease(implicit val l2cacheconf: L2CacheConfig) extends Release()(l2cacheconf.tl) 
  with HasL2InternalRequestState

class InternalTileLinkIO(implicit val l2cacheconf: L2CacheConfig) extends L2CacheBundle {
  implicit val (tl, ln) = (l2cacheconf.tl, l2cacheconf.tl.ln)
  val acquire   = new DecoupledIO(new LogicalNetworkIO(new L2InternalAcquire))
  val probe     = new DecoupledIO(new LogicalNetworkIO(new Probe)).flip
  val release   = new DecoupledIO(new LogicalNetworkIO(new L2InternalRelease))
  val grant     = new DecoupledIO(new LogicalNetworkIO(new Grant)).flip
  val finish = new DecoupledIO(new LogicalNetworkIO(new Finish))
}

class L2HellaCache(bankId: Int)(implicit conf: L2CacheConfig) extends CoherenceAgent {
  implicit val (tl, ln, co) = (conf.tl, conf.tl.ln, conf.tl.co)

  val tshrfile = Module(new TSHRFile(bankId))
  val meta = Module(new L2MetaDataArray)
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


class TSHRFile(bankId: Int)(implicit conf: L2CacheConfig) extends Module {
  implicit val (tl, ln, co) = (conf.tl, conf.tl.ln, conf.tl.co)
  val io = new Bundle {
    val inner = (new TileLinkIO).flip
    val outer = new UncachedTileLinkIO
    val incoherent = Vec.fill(ln.nClients){Bool()}.asInput
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
    arb.io.in zip ins map { case (a, i) => a <> i }
  }

  // Create TSHRs for outstanding transactions
  val nTrackers = conf.nReleaseTransactions + conf.nAcquireTransactions
  val trackerList = (0 until conf.nReleaseTransactions).map { id => 
    Module(new L2VoluntaryReleaseTracker(id, bankId)) 
  } ++ (conf.nReleaseTransactions until nTrackers).map { id => 
    Module(new L2AcquireTracker(id, bankId))
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
  //val probe_arb = Module(new Arbiter(new LogicalNetworkIO(new Probe), trackerList.size))
  //io.inner.probe <> probe_arb.io.out
  //probe_arb.io.in zip trackerList map { case (arb, t) => arb <> t.io.inner.probe }

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
  //val grant_arb = Module(new Arbiter(new LogicalNetworkIO(new Grant), trackerList.size))
  //io.inner.grant <> grant_arb.io.out
  //grant_arb.io.in zip trackerList map { case (arb, t) => arb <> t.io.inner.grant }

  // Free finished transactions
  val ack = io.inner.finish
  trackerList.map(_.io.inner.finish.valid := ack.valid)
  trackerList.map(_.io.inner.finish.bits := ack.bits)
  ack.ready := Bool(true)

  // Arbitrate for the outer memory port
  val outer_arb = Module(new UncachedTileLinkIOArbiterThatPassesId(trackerList.size))
  outer_arb.io.in zip  trackerList map { case(arb, t) => arb <> t.io.outer }
  io.outer <> outer_arb.io.out

  // Local memory
  doOutputArbitration(io.meta_read, trackerList.map(_.io.meta_read))
  doOutputArbitration(io.meta_write, trackerList.map(_.io.meta_write))
  doOutputArbitration(io.data_read, trackerList.map(_.io.data_read))
  doOutputArbitration(io.data_write, trackerList.map(_.io.data_write))


}


abstract class L2XactTracker()(implicit conf: L2CacheConfig) extends Module {
  implicit val (tl, ln, co) = (conf.tl, conf.tl.ln, conf.tl.co)
  val io = new Bundle {
    val inner = (new TileLinkIO).flip
    val outer = new UncachedTileLinkIO
    val tile_incoherent = Bits(INPUT, ln.nClients)
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

}

class L2VoluntaryReleaseTracker(trackerId: Int, bankId: Int)(implicit conf: L2CacheConfig) extends L2XactTracker()(conf) {
  val s_idle :: s_mem :: s_ack :: s_busy :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_idle)
  val xact  = Reg{ new Release }
  val init_client_id = Reg(init=UInt(0, width = log2Up(ln.nClients)))
  val incoming_rel = io.inner.release.bits

  io.has_acquire_conflict := Bool(false)
  io.has_release_conflict := co.isCoherenceConflict(xact.addr, incoming_rel.payload.addr) && 
                               (state != s_idle)

  io.outer.grant.ready := Bool(false)
  io.outer.acquire.valid := Bool(false)
  io.outer.acquire.bits.header.src := UInt(bankId) 
  //io.outer.acquire.bits.header.dst TODO
  io.outer.acquire.bits.payload := Acquire(co.getUncachedWriteAcquireType,
                                            xact.addr,
                                            UInt(trackerId),
                                            xact.data)
  io.inner.acquire.ready := Bool(false)
  io.inner.probe.valid := Bool(false)
  io.inner.release.ready := Bool(false)
  io.inner.grant.valid := Bool(false)
  io.inner.grant.bits.header.src := UInt(bankId)
  io.inner.grant.bits.header.dst := init_client_id
  io.inner.grant.bits.payload := Grant(co.getGrantType(xact, UInt(0)),
                                        xact.client_xact_id,
                                        UInt(trackerId))

  switch (state) {
    is(s_idle) {
      io.inner.release.ready := Bool(true)
      when( io.inner.release.valid ) {
        xact := incoming_rel.payload
        init_client_id := incoming_rel.header.src
        state := Mux(co.messageHasData(incoming_rel.payload), s_mem, s_ack)
      }
    }
    is(s_mem) {
      io.outer.acquire.valid := Bool(true)
      when(io.outer.acquire.ready) { state := s_ack }
    }
    is(s_ack) {
      io.inner.grant.valid := Bool(true)
      when(io.inner.grant.ready) { state := s_idle }
    }
  }
}

class L2AcquireTracker(trackerId: Int, bankId: Int)(implicit conf: L2CacheConfig) extends L2XactTracker()(conf) {
  val s_idle :: s_probe :: s_mem_read :: s_mem_write :: s_make_grant :: s_busy :: Nil = Enum(UInt(), 6)
  val state = Reg(init=s_idle)
  val xact  = Reg{ new Acquire }
  val init_client_id = Reg(init=UInt(0, width = log2Up(ln.nClients)))
  //TODO: Will need id reg for merged release xacts

  val init_sharer_cnt = Reg(init=UInt(0, width = log2Up(ln.nClients)))
  val release_count = if (ln.nClients == 1) UInt(0) else Reg(init=UInt(0, width = log2Up(ln.nClients)))
  val probe_flags = Reg(init=Bits(0, width = ln.nClients))
  val curr_p_id = PriorityEncoder(probe_flags)

  val pending_outer_write = co.messageHasData(xact)
  val pending_outer_read = co.needsOuterRead(xact.a_type, UInt(0))
  val outer_write_acq = Acquire(co.getUncachedWriteAcquireType, 
                                       xact.addr, UInt(trackerId), xact.data)
  val outer_write_rel = Acquire(co.getUncachedWriteAcquireType, 
                                       xact.addr, UInt(trackerId), c_rel.payload.data)
  val outer_read = Acquire(co.getUncachedReadAcquireType, xact.addr, UInt(trackerId))

  val probe_initial_flags = Bits(width = ln.nClients)
  probe_initial_flags := Bits(0)
  if (ln.nClients > 1) {
    // issue self-probes for uncached read xacts to facilitate I$ coherence
    val probe_self = Bool(true) //co.needsSelfProbe(io.inner.acquire.bits.payload)
    val myflag = Mux(probe_self, Bits(0), UIntToOH(c_acq.header.src(log2Up(ln.nClients)-1,0)))
    probe_initial_flags := ~(io.tile_incoherent | myflag)
  }

  io.has_acquire_conflict := co.isCoherenceConflict(xact.addr, c_acq.payload.addr) && (state != s_idle)
  io.has_release_conflict := co.isCoherenceConflict(xact.addr, c_rel.payload.addr) && (state != s_idle)

  io.outer.acquire.valid := Bool(false)
  io.outer.acquire.bits.header.src := UInt(bankId)
  //io.outer.acquire.bits.header.dst TODO
  io.outer.acquire.bits.payload := outer_read 
  io.outer.grant.ready := io.inner.grant.ready

  io.inner.probe.valid := Bool(false)
  io.inner.probe.bits.header.src := UInt(bankId)
  io.inner.probe.bits.header.dst := curr_p_id
  io.inner.probe.bits.payload := Probe(co.getProbeType(xact.a_type, UInt(0)),
                                               xact.addr,
                                               UInt(trackerId))

  val grant_type = co.getGrantType(xact.a_type, init_sharer_cnt)
  io.inner.grant.valid := Bool(false)
  io.inner.grant.bits.header.src := UInt(bankId)
  io.inner.grant.bits.header.dst := init_client_id
  io.inner.grant.bits.payload := Grant(grant_type,
                                        xact.client_xact_id,
                                        UInt(trackerId),
                                        m_gnt.payload.data)

  io.inner.acquire.ready := Bool(false)
  io.inner.release.ready := Bool(false)

  switch (state) {
    is(s_idle) {
      io.inner.acquire.ready := Bool(true)
      val needs_outer_write = co.messageHasData(c_acq.payload)
      val needs_outer_read = co.needsOuterRead(c_acq.payload.a_type, UInt(0))
      when( io.inner.acquire.valid ) {
        xact := c_acq.payload
        init_client_id := c_acq.header.src
        init_sharer_cnt := UInt(ln.nClients) // TODO: Broadcast only
        probe_flags := probe_initial_flags
        if(ln.nClients > 1) {
          release_count := PopCount(probe_initial_flags)
          state := Mux(probe_initial_flags.orR, s_probe,
                    Mux(needs_outer_write, s_mem_write,
                      Mux(needs_outer_read, s_mem_read, s_make_grant)))
        } else state := Mux(needs_outer_write, s_mem_write,
                        Mux(needs_outer_read, s_mem_read, s_make_grant))
      }
    }
    is(s_probe) {
      // Generate probes
      io.inner.probe.valid := probe_flags.orR
      when(io.inner.probe.ready) {
        probe_flags := probe_flags & ~(UIntToOH(curr_p_id))
      }

      // Handle releases, which may have data to be written back
      when(io.inner.release.valid) {
        when(co.messageHasData(c_rel.payload)) {
          io.outer.acquire.valid := Bool(true)
          io.outer.acquire.bits.payload := outer_write_rel
          when(io.outer.acquire.ready) {
            io.inner.release.ready := Bool(true)
            if(ln.nClients > 1) release_count := release_count - UInt(1)
            when(release_count === UInt(1)) {
              state := Mux(pending_outer_write, s_mem_write,
                        Mux(pending_outer_read, s_mem_read, s_make_grant))
            }
          }
        } .otherwise {
          io.inner.release.ready := Bool(true)
          if(ln.nClients > 1) release_count := release_count - UInt(1)
          when(release_count === UInt(1)) {
            state := Mux(pending_outer_write, s_mem_write, 
                      Mux(pending_outer_read, s_mem_read, s_make_grant))
          }
        }
      }
    }
    is(s_mem_read) {
      io.outer.acquire.valid := Bool(true)
      io.outer.acquire.bits.payload := outer_read
      when(io.outer.acquire.ready) {
        state := Mux(co.requiresAckForGrant(grant_type), s_busy, s_idle)
      }
    }
    is(s_mem_write) {
      io.outer.acquire.valid := Bool(true)
      io.outer.acquire.bits.payload := outer_write_acq
      when(io.outer.acquire.ready) { 
        state := Mux(pending_outer_read, s_mem_read, s_make_grant)
      }
    }
    is(s_make_grant) {
      io.inner.grant.valid := Bool(true)
      when(io.inner.grant.ready) { 
        state := Mux(co.requiresAckForGrant(grant_type), s_busy, s_idle)
      }
    }
    is(s_busy) { // Nothing left to do but wait for transaction to complete
      when(io.outer.grant.valid && m_gnt.payload.client_xact_id === UInt(trackerId)) {
        io.inner.grant.valid := Bool(true)
      }
      when(io.inner.finish.valid && c_ack.payload.master_xact_id === UInt(trackerId)) {
        state := s_idle
      }
    }
  }
}
