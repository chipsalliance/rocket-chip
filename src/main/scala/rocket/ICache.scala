// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util.{Cat, Decoupled, Mux1H, OHToUInt, RegEnable, Valid, isPow2, log2Ceil, log2Up, PopCount}
import freechips.rocketchip.amba._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{DescribedSRAM, _}
import freechips.rocketchip.util.property
import chisel3.experimental.SourceInfo
import chisel3.dontTouch
import chisel3.util.random.LFSR

/** Parameter of [[ICache]].
  *
  * @param nSets number of sets.
  * @param nWays number of ways.
  * @param rowBits L1Cache parameter
  * @param nTLBSets TLB sets
  * @param nTLBWays TLB ways
  * @param nTLBBasePageSectors TLB BasePageSectors
  * @param nTLBSuperpages TLB Superpages
  * @param tagECC tag ECC, will be parsed to [[freechips.rocketchip.util.Code]].
  * @param dataECC data ECC, will be parsed to [[freechips.rocketchip.util.Code]].
  * @param itimAddr optional base ITIM address,
  *                 if None, ITIM won't be generated,
  *                 if Some, ITIM will be generated, with itimAddr as ITIM base address.
  * @param prefetch if set, will send next-line[[TLEdgeOut.Hint]] to manger.
  * @param blockBytes size of a cacheline, calculates in byte.
  * @param latency latency of a instruction fetch, 1 or 2 are available
  * @param fetchBytes byte size fetched by CPU for each cycle.
  */
case class ICacheParams(
    nSets: Int = 64,
    nWays: Int = 4,
    rowBits: Int = 128,
    nTLBSets: Int = 1,
    nTLBWays: Int = 32,
    nTLBBasePageSectors: Int = 4,
    nTLBSuperpages: Int = 4,
    cacheIdBits: Int = 0,
    tagECC: Option[String] = None,
    dataECC: Option[String] = None,
    itimAddr: Option[BigInt] = None,
    prefetch: Boolean = false,
    blockBytes: Int = 64,
    latency: Int = 2,
    fetchBytes: Int = 4) extends L1CacheParams {
  def tagCode: Code = Code.fromString(tagECC)
  def dataCode: Code = Code.fromString(dataECC)
  def replacement = new RandomReplacement(nWays)
}

trait HasL1ICacheParameters extends HasL1CacheParameters with HasCoreParameters {
  val cacheParams = tileParams.icache.get
}

class ICacheReq(implicit p: Parameters) extends CoreBundle()(p) with HasL1ICacheParameters {
  val addr = UInt(vaddrBits.W)
}

class ICacheErrors(implicit p: Parameters) extends CoreBundle()(p)
    with HasL1ICacheParameters
    with CanHaveErrors {
  val correctable = (cacheParams.tagCode.canDetect || cacheParams.dataCode.canDetect).option(Valid(UInt(paddrBits.W)))
  val uncorrectable = (cacheParams.itimAddr.nonEmpty && cacheParams.dataCode.canDetect).option(Valid(UInt(paddrBits.W)))
  val bus = Valid(UInt(paddrBits.W))
}

/** [[ICache]] is a set associated cache I$(Instruction Cache) of Rocket.
 * {{{
  * Keywords: Set-associated
  *           3 stage pipeline
  *           Virtually-Indexed Physically-Tagged (VIPT)
  *           Parallel access to tag and data SRAM
  *           Random replacement algorithm
  * Optional Features:
  *           Prefetch
  *           ECC
  *           Instruction Tightly Integrated Memory(ITIM)}}}
  *{{{
  * PipeLine:
  *   Stage 0 : access data and tag SRAM in parallel
  *   Stage 1 : receive paddr from CPU
  *             compare tag and paddr when the entry is valid
  *             if hit : pick up the target instruction
  *             if miss : start refilling in stage 2
  *   Stage 2 : respond to CPU or start a refill}}}
  *{{{
  * Note: Page size = 4KB thus paddr[11:0] = vaddr[11:0]
  *       considering sets = 64, cachelineBytes =64
  *       use vaddr[11:6] to access tag_array
  *       use vaddr[11:2] to access data_array}}}
  *{{{
  * ITIM:
  * │          tag         │    set    │offset│
  *                    ├way┘                    → indicate way location
  *                    │    line       │ }}}
  *   if `way` == b11 (last way), deallocate
  *   if write to ITIM all I$ will be invalidate
  *
  * The optional dynamic configurable ITIM sharing SRAM with I$ is set by  [[icacheParams.itimAddr]].
  * if PutFullData/PutPartialData to the ITIM address, it will dynamically allocate base address to the address of this accessing from SRAM.
  * if access to last way of ITIM, it set will change back to I$.
  *
  * If ITIM is configured:
  *   set: if address to access is not to be configured to ITIM yet,
  *        a memory accessing to ITIM address range will modify `scratchpadMax`,
  *        from ITIM base to `scratchpadMax` will be used as ITIM.
  *   unset: @todo
  *
  * There will always be one way(the last way) used for I$, which cannot be allocated to ITIM.
  *
  * @param icacheParams parameter to this I$.
  * @param staticIdForMetadataUseOnly metadata used for hart id.
  */
class ICache(val icacheParams: ICacheParams, val staticIdForMetadataUseOnly: Int)(implicit p: Parameters) extends LazyModule {
  lazy val module = new ICacheModule(this)

  /** Diplomatic hartid bundle used for ITIM. */
  val hartIdSinkNodeOpt = icacheParams.itimAddr.map(_ => BundleBridgeSink[UInt]())
  /** @todo base address offset for ITIM? */
  val mmioAddressPrefixSinkNodeOpt = icacheParams.itimAddr.map(_ => BundleBridgeSink[UInt]())

  /** Rocket configuration has virtual memory.
    *
    * This only affect [[masterNode]] AMBA ports only:
    * AMBA privileged, secure will be set as true while others set as false.
    * see [[freechips.rocketchip.amba.AMBAProt]] for more informations.
    */
  val useVM = p(TileKey).core.useVM

  /** [[TLClientNode]] of I$.
    *
    * source Id range:
    * 0: use [[TLEdgeOut.Get]] to get instruction.
    * 1: use [[TLEdgeOut.Hint]] to hint next level memory device fetching next cache line, if configured [[icacheParams.prefetch]].
    *
    * @todo why if no [[useVM]], will have AMBAProtField in requestFields?
    */
  val masterNode = TLClientNode(Seq(TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      sourceId = IdRange(0, 1 + icacheParams.prefetch.toInt), // 0=refill, 1=hint
      name = s"Core ${staticIdForMetadataUseOnly} ICache")),
    requestFields = useVM.option(Seq()).getOrElse(Seq(AMBAProtField())))))

  /** size of [[ICache]], count in byte. */
  val size = icacheParams.nSets * icacheParams.nWays * icacheParams.blockBytes

  /** last way will be configured to control offest, access it will deallocate an entire set to I$. */
  val itim_control_offset = size - icacheParams.nSets * icacheParams.blockBytes

  val device = new SimpleDevice("itim", Seq("sifive,itim0")) {
    override def describe(resources: ResourceBindings): Description = {
     val Description(name, mapping) = super.describe(resources)
     val Seq(Binding(_, ResourceAddress(address, perms))) = resources("reg/mem")
     val base_address = address.head.base
     val mem_part = AddressSet.misaligned(base_address, itim_control_offset)
     val control_part = AddressSet.misaligned(base_address + itim_control_offset, size - itim_control_offset)
     val extra = Map(
       "reg-names" -> Seq(ResourceString("mem"), ResourceString("control")),
       "reg" -> Seq(ResourceAddress(mem_part, perms), ResourceAddress(control_part, perms)))
     Description(name, mapping ++ extra)
    }
  }

  def itimProperty: Option[Seq[ResourceValue]] = icacheParams.itimAddr.map(_ => device.asProperty)

  /** @todo why [[wordBytes]] is defined by [[icacheParams.fetchBytes]], rather than 32 directly? */
  private val wordBytes = icacheParams.fetchBytes

  /** Instruction Tightly Integrated Memory node. */
  val slaveNode =
    TLManagerNode(icacheParams.itimAddr.toSeq.map { itimAddr => TLSlavePortParameters.v1(
      Seq(TLSlaveParameters.v1(
        address         = Seq(AddressSet(itimAddr, size-1)),
        resources       = device.reg("mem"),
        regionType      = RegionType.IDEMPOTENT,
        executable      = true,
        supportsPutFull = TransferSizes(1, wordBytes),
        supportsPutPartial = TransferSizes(1, wordBytes),
        supportsGet     = TransferSizes(1, wordBytes),
        fifoId          = Some(0))), // requests handled in FIFO order
      beatBytes = wordBytes,
      minLatency = 1)})
}

class ICacheResp(outer: ICache) extends Bundle {
  /** data to CPU. */
  val data = UInt((outer.icacheParams.fetchBytes*8).W)
  /** ask CPU to replay fetch when tag or data ECC error happened. */
  val replay = Bool()
  /** access exception:
    * indicate CPU an tag ECC error happened.
    * if [[outer.icacheParams.latency]] is 1, tie 0.
    */
  val ae = Bool()

}

class ICachePerfEvents extends Bundle {
  val acquire = Bool()
}

/** IO from CPU to ICache. */
class ICacheBundle(val outer: ICache) extends CoreBundle()(outer.p) {
  /** first cycle requested from CPU. */
  val req = Flipped(Decoupled(new ICacheReq))
  val s1_paddr = Input(UInt(paddrBits.W)) // delayed one cycle w.r.t. req
  val s2_vaddr = Input(UInt(vaddrBits.W)) // delayed two cycles w.r.t. req
  val s1_kill = Input(Bool()) // delayed one cycle w.r.t. req
  val s2_kill = Input(Bool()) // delayed two cycles; prevents I$ miss emission
  val s2_cacheable = Input(Bool()) // should L2 cache line on a miss?
  val s2_prefetch = Input(Bool()) // should I$ prefetch next line on a miss?
  /** response to CPU. */
  val resp = Valid(new ICacheResp(outer))

  /** flush L1 cache from CPU.
    * TODO: IIRC, SFENCE.I
    */
  val invalidate = Input(Bool())

  /** I$ has error, notify to bus.
    * TODO: send to BPU.
    */
  val errors = new ICacheErrors

  /** for performance counting. */
  val perf = Output(new ICachePerfEvents())

  /** enable clock. */
  val clock_enabled = Input(Bool())

  /** I$ miss or ITIM access will still enable clock even [[ICache]] is asked to be gated. */
  val keep_clock_enabled = Output(Bool())
}

class ICacheModule(outer: ICache) extends LazyModuleImp(outer)
    with HasL1ICacheParameters {
  override val cacheParams = outer.icacheParams // Use the local parameters

  /** IO between Core and ICache. */
  val io = IO(new ICacheBundle(outer))

  /** TileLink port to memory. */
  val (tl_out, edge_out) = outer.masterNode.out(0)

  /** TileLink port as ITIM memory.
    * if [[outer.slaveNode]] is not connected [[outer.slaveNode.in]] will be empty.
    *
    * wes: Option.unzip does not exist :-(
    */
  val (tl_in, edge_in) = outer.slaveNode.in.headOption.unzip

  val tECC = cacheParams.tagCode
  val dECC = cacheParams.dataCode

  require(isPow2(nSets) && isPow2(nWays))
  require(!usingVM || outer.icacheParams.itimAddr.isEmpty || pgIdxBits >= untagBits,
    s"When VM and ITIM are enabled, I$$ set size must not exceed ${1<<(pgIdxBits-10)} KiB; got ${(outer.size/nWays)>>10} KiB")

  /** if this ICache can be used as ITIM, which hart it belongs to. */
  val io_hartid = outer.hartIdSinkNodeOpt.map(_.bundle)
  /** @todo tile Memory mapping I/O base address? */
  val io_mmio_address_prefix = outer.mmioAddressPrefixSinkNodeOpt.map(_.bundle)
  /** register indicates wheather ITIM is enabled. */
  val scratchpadOn = RegInit(false.B)
  /** a cut point to SRAM, indicates which SRAM will be used as SRAM or Cache. */
  val scratchpadMax = tl_in.map(tl => Reg(UInt(log2Ceil(nSets * (nWays - 1)).W)))

  /** Check if a line is in the scratchpad.
    *
    * line is a minimal granularity accessing to SRAM, calculated by [[scratchpadLine]]
    */
  def lineInScratchpad(line: UInt) = scratchpadMax.map(scratchpadOn && line <= _).getOrElse(false.B)
  /** scratchpad base address, if exist [[ICacheParams.itimAddr]], add [[ReplicatedRegion]] to base.
    * @todo seem [[io_hartid]] is not connected?
    *       maybe when implementing itim, LookupByHartId should be changed to [[]]?
    */
  val scratchpadBase = outer.icacheParams.itimAddr.map { dummy =>
    p(LookupByHartId)(_.icache.flatMap(_.itimAddr.map(_.U)), io_hartid.get) | io_mmio_address_prefix.get
  }

  /** check an address in the scratchpad address range. */
  def addrMaybeInScratchpad(addr: UInt) = scratchpadBase.map(base => addr >= base && addr < base + outer.size.U).getOrElse(false.B)
  /** check property this address(paddr) exists in scratchpad.
    * @todo seems duplicated in `addrMaybeInScratchpad(addr)` between `lineInScratchpad(addr(untagBits+log2Ceil(nWays)-1, blockOffBits))`?
    */
  def addrInScratchpad(addr: UInt) = addrMaybeInScratchpad(addr) && lineInScratchpad(addr(untagBits+log2Ceil(nWays)-1, blockOffBits))

  /** return the way which will be used as scratchpad for accessing address
    * {{{
    * │          tag         │    set    │offset│
    *                    └way┘
    * }}}
    * @param addr address to be found.
    */
  def scratchpadWay(addr: UInt) = addr.extract(untagBits+log2Ceil(nWays)-1, untagBits)

  /** check if the selected way is legal.
    * note: the last way should be reserved to ICache.
    */
  def scratchpadWayValid(way: UInt) = way < (nWays - 1).U

  /** return the cacheline which will be used as scratchpad for accessing address
    * {{{
    * │          tag         │    set    │offset│
    *                    ├way┘                    → indicate way location
    *                    │    line       │
    * }}}
    * @param addr address to be found.
   *             applied to slave_addr
    */
  def scratchpadLine(addr: UInt) = addr(untagBits+log2Ceil(nWays)-1, blockOffBits)

  /** scratchpad access valid in stage N*/
  val s0_slaveValid = tl_in.map(_.a.fire).getOrElse(false.B)
  val s1_slaveValid = RegNext(s0_slaveValid, false.B)
  val s2_slaveValid = RegNext(s1_slaveValid, false.B)
  val s3_slaveValid = RegNext(false.B)

  /** valid signal for CPU accessing cache in stage 0. */
  val s0_valid = io.req.fire
  /** virtual address from CPU in stage 0. */
  val s0_vaddr = io.req.bits.addr

  /** valid signal for stage 1, drived by s0_valid.*/
  val s1_valid = RegInit(false.B)
  /** virtual address from CPU in stage 1. */
  val s1_vaddr = RegEnable(s0_vaddr, s0_valid)
  /** tag hit vector to indicate hit which way. */
  val s1_tag_hit = Wire(Vec(nWays, Bool()))
  /** CPU I$ Hit in stage 1.
    *
    * @note
    * for logic in `Mux(s1_slaveValid, true.B, addrMaybeInScratchpad(io.s1_paddr))`,
    * there are two different types based on latency:
    *
    * if latency is 1: `s1_slaveValid === false.B` and `addrMaybeInScratchpad(io.s1_paddr) === false.B` ,
    *                   since in this case, ITIM must be empty.
    *
    * if latency is 2: if `s1_slaveValid` is true, this SRAM accessing is coming from [[tl_in]], so it will hit.
    *                  if `s1_slaveValid` is false, but CPU is accessing memory range in scratchpad address, it will hit by default.
    *                  Hardware won't guarantee this access will access to a data which have been written in ITIM.
    *
    * @todo seem CPU access are both processed by `s1_tag_hit` and `Mux(s1_slaveValid, true.B, addrMaybeInScratchpad(io.s1_paddr))`?
    */
  val s1_hit = s1_tag_hit.reduce(_||_) || Mux(s1_slaveValid, true.B, addrMaybeInScratchpad(io.s1_paddr))
  dontTouch(s1_hit)
  val s2_valid = RegNext(s1_valid && !io.s1_kill, false.B)
  val s2_hit = RegNext(s1_hit)

  /** status register to indicate a cache flush. */
  val invalidated = Reg(Bool())
  val refill_valid = RegInit(false.B)
  /** register to indicate [[tl_out]] is performing a hint.
   *  prefetch only happens after refilling
   * */
  val send_hint = RegInit(false.B)
  /** indicate [[tl_out]] is performing a refill. */
  val refill_fire = tl_out.a.fire && !send_hint
  /** register to indicate there is a outstanding hint. */
  val hint_outstanding = RegInit(false.B)
  /** [[io]] access L1 I$ miss. */
  val s2_miss = s2_valid && !s2_hit && !io.s2_kill
  /** forward signal to stage 1, permit stage 1 refill. */
  val s1_can_request_refill = !(s2_miss || refill_valid)
  /** real refill signal, stage 2 miss, and was permit to refill in stage 1.
    * Since a miss will trigger burst.
    * miss under miss won't trigger another burst.
    */
  val s2_request_refill = s2_miss && RegNext(s1_can_request_refill)
  val refill_paddr = RegEnable(io.s1_paddr, s1_valid && s1_can_request_refill)
  val refill_vaddr = RegEnable(s1_vaddr, s1_valid && s1_can_request_refill)
  val refill_tag = refill_paddr >> pgUntagBits
  val refill_idx = index(refill_vaddr, refill_paddr)
  /** AccessAckData, is refilling I$, it will block request from CPU. */
  val refill_one_beat = tl_out.d.fire && edge_out.hasData(tl_out.d.bits)

  /** block request from CPU when refill or scratch pad access. */
  io.req.ready := !(refill_one_beat || s0_slaveValid || s3_slaveValid)
  s1_valid := s0_valid

  val (_, _, d_done, refill_cnt) = edge_out.count(tl_out.d)
  /** at last beat of `tl_out.d.fire`, finish refill. */
  val refill_done = refill_one_beat && d_done
  /** scratchpad is writing data. block refill. */
  tl_out.d.ready := !s3_slaveValid
  require (edge_out.manager.minLatency > 0)

  /** way to be replaced, implemented with a hardcoded random replacement algorithm */
  val repl_way = if (isDM) 0.U else {
    // pick a way that is not used by the scratchpad
    val v0 = LFSR(16, refill_fire)(log2Up(nWays)-1,0)
    var v = v0
    for (i <- log2Ceil(nWays) - 1 to 0 by -1) {
      val mask = nWays - (BigInt(1) << (i + 1))
      v = v | (lineInScratchpad(Cat(v0 | mask.U, refill_idx)) << i)
    }
    assert(!lineInScratchpad(Cat(v, refill_idx)))
    v
  }

/**  Tag SRAM, indexed with virtual memory,
 *   content with `refillError ## tag[19:0]` after ECC
 * */
  val tag_array  = DescribedSRAM(
    name = "tag_array",
    desc = "ICache Tag Array",
    size = nSets,
    data = Vec(nWays, UInt(tECC.width(1 + tagBits).W))
  )
  val tag_rdata = tag_array.read(s0_vaddr(untagBits-1,blockOffBits), !refill_done && s0_valid)
  /** register indicates the ongoing GetAckData transaction is corrupted. */
  val accruedRefillError = Reg(Bool())
  /** wire indicates the ongoing GetAckData transaction is corrupted. */
  val refillError = tl_out.d.bits.corrupt || (refill_cnt > 0.U && accruedRefillError)
  when (refill_done) {
    // For AccessAckData, denied => corrupt
    /** data written to [[tag_array]].
     *  ECC encoded `refillError ## refill_tag`*/
    val enc_tag = tECC.encode(Cat(refillError, refill_tag))
    tag_array.write(refill_idx, VecInit(Seq.fill(nWays){enc_tag}), Seq.tabulate(nWays)(repl_way === _.U))

    ccover(refillError, "D_CORRUPT", "I$ D-channel corrupt")
  }
  // notify CPU, I$ has corrupt.
  io.errors.bus.valid := tl_out.d.fire && (tl_out.d.bits.denied || tl_out.d.bits.corrupt)
  io.errors.bus.bits  := (refill_paddr >> blockOffBits) << blockOffBits

  /** true indicate this cacheline is valid,
    * indexed by (wayIndex ## setIndex)
    * after refill_done and not FENCE.I, (repl_way ## refill_idx) set to true.
    */
  val vb_array = RegInit(0.U((nSets*nWays).W))
  when (refill_one_beat) {
    accruedRefillError := refillError
    // clear bit when refill starts so hit-under-miss doesn't fetch bad data
    vb_array := vb_array.bitSet(Cat(repl_way, refill_idx), refill_done && !invalidated)
  }

  /** flush cache when invalidate is true. */
  val invalidate = WireDefault(io.invalidate)
  when (invalidate) {
    vb_array := 0.U
    invalidated := true.B
  }

  /** wire indicates that tag is correctable or uncorrectable.
    * will trigger CPU to replay and I$ invalidating, if correctable.
    */
  val s1_tag_disparity = Wire(Vec(nWays, Bool()))
  /** wire indicates that bus has an uncorrectable error.
    * respond to CPU [[io.resp.bits.ae]], cause [[Causes.fetch_access]].
    */
  val s1_tl_error = Wire(Vec(nWays, Bool()))
  /** how many bits will be fetched by CPU for each fetch. */
  val wordBits = outer.icacheParams.fetchBytes*8
  /** a set of raw data read from [[data_arrays]]. */
  val s1_dout = Wire(Vec(nWays, UInt(dECC.width(wordBits).W)))
  s1_dout := DontCare

  /** address accessed by [[tl_in]] for ITIM. */
  val s0_slaveAddr = tl_in.map(_.a.bits.address).getOrElse(0.U)
  /** address used at stage 1 and 3.
    * {{{
    * In stage 1, it caches TileLink data, store in stage 2 if ECC passed.
    * In stage 3, it caches corrected data from stage 2, and store in stage 4.}}}
    */
  val s1s3_slaveAddr = Reg(UInt(log2Ceil(outer.size).W))
  /** data used at stage 1 and 3.
    * {{{
    * In stage 1, it caches TileLink data, store in stage 2.
    * In stage 3, it caches corrected data from data ram, and return to d channel.}}}
    */
  val s1s3_slaveData = Reg(UInt(wordBits.W))

  for (i <- 0 until nWays) {
    val s1_idx = index(s1_vaddr, io.s1_paddr)
    val s1_tag = io.s1_paddr >> pgUntagBits
    /** this way is used by scratchpad.
      * [[tag_array]] corrupted.
      */
    val scratchpadHit = scratchpadWayValid(i.U) &&
      Mux(s1_slaveValid,
        // scratchpad accessing form [[tl_in]].
        // @todo I think XBar will guarantee there won't be an illegal access on the bus?
        //       so why did have this check `lineInScratchpad(scratchpadLine(s1s3_slaveAddr))`?
        //       I think it will always be true.
        lineInScratchpad(scratchpadLine(s1s3_slaveAddr)) && scratchpadWay(s1s3_slaveAddr) === i.U,
        // scratchpad accessing from [[io]].
        // @todo Accessing ITIM correspond address will be able to read cacheline?
        //       is this desired behavior?
        addrInScratchpad(io.s1_paddr) && scratchpadWay(io.s1_paddr) === i.U)
    val s1_vb = vb_array(Cat(i.U, s1_idx)) && !s1_slaveValid
    val enc_tag = tECC.decode(tag_rdata(i))
    /** [[tl_error]] ECC error bit.
      * [[tag]] of [[tag_array]] access.
      */
    val (tl_error, tag) = Split(enc_tag.uncorrected, tagBits)
    val tagMatch = s1_vb && tag === s1_tag
    /** tag error happens. */
    s1_tag_disparity(i) := s1_vb && enc_tag.error
    /** if tag matched but ecc checking failed, this access will trigger [[Causes.fetch_access]] exception.*/
    s1_tl_error(i) := tagMatch && tl_error.asBool
    s1_tag_hit(i) := tagMatch || scratchpadHit
  }
  assert(!(s1_valid || s1_slaveValid) || PopCount(s1_tag_hit zip s1_tag_disparity map { case (h, d) => h && !d }) <= 1.U)

  require(tl_out.d.bits.data.getWidth % wordBits == 0)

  /** Data SRAM
    *
    * banked with TileLink beat bytes / CPU fetch bytes,
    * indexed with [[index]] and multi-beats cycle,
    * content with `eccError ## wordBits` after ECC.
    * {{{
    * │                          │xx│xxxxxx│xxx│x│xx│
    *                                            ↑word
    *                                          ↑bank
    *                            ↑way
    *                               └─set──┴─offset─┘
    *                               └────row───┘
    *}}}
    * Note:
    *  Data SRAM is indexed with virtual memory(vaddr[11:2]),
    *  - vaddr[11:3]->row,
    *  - vaddr[2]->bank=i
    *  - Cache line size = refillCycels(8) * bank(2) * datasize(4 bytes) = 64 bytes
    *  - data width = 32
    *
    *  read:
    *      read happens in stage 0
    *
    *  write:
    *    It takes 8 beats to refill 16 instruction in each refilling cycle.
    *    Data_array receives data[63:0](2 instructions) at once,they will be allocated in deferent bank according to vaddr[2]
    */
  val data_arrays = Seq.tabulate(tl_out.d.bits.data.getWidth / wordBits) {
    i =>
      DescribedSRAM(
        name = s"data_arrays_${i}",
        desc = "ICache Data Array",
        size = nSets * refillCycles,
        data = Vec(nWays, UInt(dECC.width(wordBits).W))
      )
  }

  for ((data_array , i) <- data_arrays.zipWithIndex) {
    /**  bank match (vaddr[2]) */
    def wordMatch(addr: UInt) = addr.extract(log2Ceil(tl_out.d.bits.data.getWidth/8)-1, log2Ceil(wordBits/8)) === i.U
    def row(addr: UInt) = addr(untagBits-1, blockOffBits-log2Ceil(refillCycles))
    /** read_enable signal*/
    val s0_ren = (s0_valid && wordMatch(s0_vaddr)) || (s0_slaveValid && wordMatch(s0_slaveAddr))
    /** write_enable signal
     * refill from [[tl_out]] or ITIM write. */
    val wen = (refill_one_beat && !invalidated) || (s3_slaveValid && wordMatch(s1s3_slaveAddr))
    /** index to access [[data_array]]. */
    val mem_idx =
      // I$ refill. refill_idx[2:0] is the beats
      Mux(refill_one_beat, (refill_idx << log2Ceil(refillCycles)) | refill_cnt,
      // ITIM write.
                  Mux(s3_slaveValid, row(s1s3_slaveAddr),
      // ITIM read.
                  Mux(s0_slaveValid, row(s0_slaveAddr),
      // CPU read.
                  row(s0_vaddr))))
    when (wen) {
      //wr_data
      val data = Mux(s3_slaveValid, s1s3_slaveData, tl_out.d.bits.data(wordBits*(i+1)-1, wordBits*i))
      //the way to be replaced/written
      val way = Mux(s3_slaveValid, scratchpadWay(s1s3_slaveAddr), repl_way)
      data_array.write(mem_idx, VecInit(Seq.fill(nWays){dECC.encode(data)}), (0 until nWays).map(way === _.U))
    }
    // write access
    /** data read from [[data_array]]. */
    val dout = data_array.read(mem_idx, !wen && s0_ren)
    // Mux to select a way to [[s1_dout]]
    when (wordMatch(Mux(s1_slaveValid, s1s3_slaveAddr, io.s1_paddr))) {
      s1_dout := dout
    }
  }

  /** When writing full words to ITIM, ECC errors are correctable.
    * When writing a full scratchpad word, suppress the read so Xs don't leak out
    */
  val s1s2_full_word_write = WireDefault(false.B)
  val s1_dont_read = s1_slaveValid && s1s2_full_word_write

  /** clock gate signal for [[s2_tag_hit]], [[s2_dout]], [[s2_tag_disparity]], [[s2_tl_error]], [[s2_scratchpad_hit]]. */
  val s1_clk_en = s1_valid || s1_slaveValid
  val s2_tag_hit = RegEnable(Mux(s1_dont_read, 0.U.asTypeOf(s1_tag_hit), s1_tag_hit), s1_clk_en)
  /** way index to access [[data_arrays]]. */
  val s2_hit_way = OHToUInt(s2_tag_hit)
  /** ITIM index to access [[data_arrays]].
    * replace tag with way, word set to 0.
    */
  val s2_scratchpad_word_addr = Cat(s2_hit_way, Mux(s2_slaveValid, s1s3_slaveAddr, io.s2_vaddr)(untagBits-1, log2Ceil(wordBits/8)), 0.U(log2Ceil(wordBits/8).W))
  val s2_dout = RegEnable(s1_dout, s1_clk_en)
  val s2_way_mux = Mux1H(s2_tag_hit, s2_dout)
  val s2_tag_disparity = RegEnable(s1_tag_disparity, s1_clk_en).asUInt.orR
  val s2_tl_error = RegEnable(s1_tl_error.asUInt.orR, s1_clk_en)
  /** ECC decode result for [[data_arrays]]. */
  val s2_data_decoded = dECC.decode(s2_way_mux)
  /** ECC error happened, correctable or uncorrectable, ask CPU to replay. */
  val s2_disparity = s2_tag_disparity || s2_data_decoded.error
  /** access hit in ITIM, if [[s1_slaveValid]], this access is from [[tl_in]], else from CPU [[io]]. */
  val s1_scratchpad_hit = Mux(s1_slaveValid, lineInScratchpad(scratchpadLine(s1s3_slaveAddr)), addrInScratchpad(io.s1_paddr))
  /** stage 2 of [[s1_scratchpad_hit]]. */
  val s2_scratchpad_hit = RegEnable(s1_scratchpad_hit, s1_clk_en)
  /** ITIM uncorrectable read.
    * `s2_scratchpad_hit`: processing a scratchpad read(from [[tl_in]] or [[io]])
    * `s2_data_decoded.uncorrectable`: read a uncorrectable data.
    * `s2_valid`: [[io]] non-canceled read.
    * `(s2_slaveValid && !s2_full_word_write)`: [[tl_in]] read or write a word with wormhole.
    *                                           if write a full word, even stage 2 read uncorrectable.
    *                                           stage 3 full word write will recovery this.
    */
  val s2_report_uncorrectable_error = s2_scratchpad_hit && s2_data_decoded.uncorrectable && (s2_valid || (s2_slaveValid && !s1s2_full_word_write))
  /** ECC uncorrectable address, send to Bus Error Unit. */
  val s2_error_addr = scratchpadBase.map(base => Mux(s2_scratchpad_hit, base + s2_scratchpad_word_addr, 0.U)).getOrElse(0.U)

  // output signals
  outer.icacheParams.latency match {
    // if I$ latency is 1, no ITIM, no ECC.
    case 1 =>
      require(tECC.isInstanceOf[IdentityCode])
      require(dECC.isInstanceOf[IdentityCode])
      require(outer.icacheParams.itimAddr.isEmpty)
      // reply data to CPU at stage 2. no replay.
      io.resp.bits.data := Mux1H(s1_tag_hit, s1_dout)
      io.resp.bits.ae := s1_tl_error.asUInt.orR
      io.resp.valid := s1_valid && s1_hit
      io.resp.bits.replay := false.B

    // if I$ latency is 2, can have ITIM and ECC.
    case 2 =>
      // when some sort of memory bit error have occurred
      // @todo why so aggressive to invalidate all when ecc corrupted.
      when (s2_valid && s2_disparity) { invalidate := true.B }

      // reply data to CPU at stage 2.
      io.resp.bits.data := s2_data_decoded.uncorrected
      io.resp.bits.ae := s2_tl_error
      io.resp.bits.replay := s2_disparity
      io.resp.valid := s2_valid && s2_hit

      // report correctable error to BEU at stage 2.
      io.errors.correctable.foreach { c =>
        c.valid := (s2_valid || s2_slaveValid) && s2_disparity && !s2_report_uncorrectable_error
        c.bits := s2_error_addr
      }
      // report uncorrectable error to BEU at stage 2.
      io.errors.uncorrectable.foreach { u =>
        u.valid := s2_report_uncorrectable_error
        u.bits := s2_error_addr
      }

      // ITIM access
      tl_in.map { tl =>
        /** valid signal for D channel. */
        val respValid = RegInit(false.B)
        // ITIM access is unpipelined
        tl.a.ready := !(tl_out.d.valid || s1_slaveValid || s2_slaveValid || s3_slaveValid || respValid || !io.clock_enabled)
        /** register used to latch TileLink request for one cycle. */
        val s1_a = RegEnable(tl.a.bits, s0_slaveValid)
        // Write Data(Put / PutPartial all mask is 1)
        s1s2_full_word_write := edge_in.get.hasData(s1_a) && s1_a.mask.andR
        // (de)allocate ITIM
        when (s0_slaveValid) {
          val a = tl.a.bits
          // address
          s1s3_slaveAddr := tl.a.bits.address
          // store Put/PutP data
          s1s3_slaveData := tl.a.bits.data
          // S0
          when (edge_in.get.hasData(a)) {
            // access data in 0 -> way - 2 allocate and enable, access data in way - 1(last way), deallocate.
            val enable = scratchpadWayValid(scratchpadWay(a.address))
            //The address isn't in range,
            when (!lineInScratchpad(scratchpadLine(a.address))) {
              scratchpadMax.get := scratchpadLine(a.address)
              invalidate := true.B
            }
            scratchpadOn := enable

            val itim_allocated = !scratchpadOn && enable
            val itim_deallocated = scratchpadOn && !enable
            val itim_increase = scratchpadOn && enable && scratchpadLine(a.address) > scratchpadMax.get
            val refilling = refill_valid && refill_cnt > 0.U
            ccover(itim_allocated, "ITIM_ALLOCATE", "ITIM allocated")
            ccover(itim_allocated && refilling, "ITIM_ALLOCATE_WHILE_REFILL", "ITIM allocated while I$ refill")
            ccover(itim_deallocated, "ITIM_DEALLOCATE", "ITIM deallocated")
            ccover(itim_deallocated && refilling, "ITIM_DEALLOCATE_WHILE_REFILL", "ITIM deallocated while I$ refill")
            ccover(itim_increase, "ITIM_SIZE_INCREASE", "ITIM size increased")
            ccover(itim_increase && refilling, "ITIM_SIZE_INCREASE_WHILE_REFILL", "ITIM size increased while I$ refill")
          }
        }

        assert(!s2_valid || RegNext(RegNext(s0_vaddr)) === io.s2_vaddr)
        when (!(tl.a.valid || s1_slaveValid || s2_slaveValid || respValid)
              && s2_valid && s2_data_decoded.error && !s2_tag_disparity) {
          // handle correctable errors on CPU accesses to the scratchpad.
          // if there is an in-flight slave-port access to the scratchpad,
          // report the miss but don't correct the error (as there is
          // a structural hazard on s1s3_slaveData/s1s3_slaveAddress).
          s3_slaveValid := true.B
          s1s3_slaveData := s2_data_decoded.corrected
          s1s3_slaveAddr := s2_scratchpad_word_addr | s1s3_slaveAddr(log2Ceil(wordBits/8)-1, 0)
        }

        // back pressure is allowed on the [[tl]]
        // pull up [[respValid]] when [[s2_slaveValid]] until [[tl.d.fire]]
        respValid := s2_slaveValid || (respValid && !tl.d.ready)
        // if [[s2_full_word_write]] will overwrite data, and [[s2_data_decoded.uncorrectable]] can be ignored.
        val respError = RegEnable(s2_scratchpad_hit && s2_data_decoded.uncorrectable && !s1s2_full_word_write, s2_slaveValid)
        when (s2_slaveValid) {
          // need stage 3 if Put or correct decoding.
          // @todo if uncorrectable [[s2_data_decoded]]?
          when (edge_in.get.hasData(s1_a) || s2_data_decoded.error) { s3_slaveValid := true.B }
          /** data not masked by the TileLink PutData/PutPartialData.
            * means data is stored at [[s1s3_slaveData]] which was read at stage 1.
            */
          def byteEn(i: Int) = !(edge_in.get.hasData(s1_a) && s1_a.mask(i))
          // write [[s1s3_slaveData]] based on index of wordBits.
          // @todo seems a problem here?
          //       granularity of CPU fetch is `wordBits/8`,
          //       granularity of TileLink access is `TLBundleParameters.dataBits/8`
          //       these two granularity can be different.
          // store data read from RAM
          s1s3_slaveData := (0 until wordBits/8).map(i => Mux(byteEn(i), s2_data_decoded.corrected, s1s3_slaveData)(8*(i+1)-1, 8*i)).asUInt
        }

        tl.d.valid := respValid
        tl.d.bits := Mux(edge_in.get.hasData(s1_a),
          // PutData/PutPartialData -> AccessAck
          edge_in.get.AccessAck(s1_a),
          // Get -> AccessAckData
          edge_in.get.AccessAck(s1_a, 0.U, denied = false.B, corrupt = respError))
        tl.d.bits.data := s1s3_slaveData
        // Tie off unused channels
        tl.b.valid := false.B
        tl.c.ready := true.B
        tl.e.ready := true.B

        ccover(s0_valid && s1_slaveValid, "CONCURRENT_ITIM_ACCESS_1", "ITIM accessed, then I$ accessed next cycle")
        ccover(s0_valid && s2_slaveValid, "CONCURRENT_ITIM_ACCESS_2", "ITIM accessed, then I$ accessed two cycles later")
        ccover(tl.d.valid && !tl.d.ready, "ITIM_D_STALL", "ITIM response blocked by D-channel")
        ccover(tl_out.d.valid && !tl_out.d.ready, "ITIM_BLOCK_D", "D-channel blocked by ITIM access")
      }
  }

  tl_out.a.valid := s2_request_refill
  tl_out.a.bits := edge_out.Get(
                    fromSource = 0.U,
                    toAddress = (refill_paddr >> blockOffBits) << blockOffBits,
                    lgSize = lgCacheBlockBytes.U)._2

  // prefetch when next-line access does not cross a page
  if (cacheParams.prefetch) {
    /** [[crosses_page]]  indicate if there is a crosses page access
      * [[next_block]] : the address to be prefetched.
      */
    val (crosses_page, next_block) = Split(refill_paddr(pgIdxBits-1, blockOffBits) +& 1.U, pgIdxBits-blockOffBits)

    when (tl_out.a.fire) {
      send_hint := !hint_outstanding && io.s2_prefetch && !crosses_page
      when (send_hint) {
        send_hint := false.B
        hint_outstanding := true.B
      }
    }

    // @todo why refill_done will kill hint at this cycle?
    when (refill_done) {
      send_hint := false.B
    }

    // D channel reply with HintAck.
    when (tl_out.d.fire && !refill_one_beat) {
      hint_outstanding := false.B
    }

    when (send_hint) {
      tl_out.a.valid := true.B
      tl_out.a.bits := edge_out.Hint(
                        fromSource = 1.U,
                        toAddress = Cat(refill_paddr >> pgIdxBits, next_block) << blockOffBits,
                        lgSize = lgCacheBlockBytes.U,
                        param = TLHints.PREFETCH_READ)._2
    }

    ccover(send_hint && !tl_out.a.ready, "PREFETCH_A_STALL", "I$ prefetch blocked by A-channel")
    ccover(refill_valid && (tl_out.d.fire && !refill_one_beat), "PREFETCH_D_BEFORE_MISS_D", "I$ prefetch resolves before miss")
    ccover(!refill_valid && (tl_out.d.fire && !refill_one_beat), "PREFETCH_D_AFTER_MISS_D", "I$ prefetch resolves after miss")
    ccover(tl_out.a.fire && hint_outstanding, "PREFETCH_D_AFTER_MISS_A", "I$ prefetch resolves after second miss")
  }
  // Drive APROT information
  tl_out.a.bits.user.lift(AMBAProt).foreach { x =>
    // Rocket caches all fetch requests, and it's difficult to differentiate privileged/unprivileged on
    // cached data, so mark as privileged
    x.fetch       := true.B
    x.secure      := true.B
    x.privileged  := true.B
    x.bufferable  := true.B
    x.modifiable  := true.B
    x.readalloc   := io.s2_cacheable
    x.writealloc  := io.s2_cacheable
  }
  tl_out.b.ready := true.B
  tl_out.c.valid := false.B
  tl_out.e.valid := false.B
  assert(!(tl_out.a.valid && addrMaybeInScratchpad(tl_out.a.bits.address)))

  // if there is an outstanding refill, cannot flush I$.
  when (!refill_valid) { invalidated := false.B }
  when (refill_fire) { refill_valid := true.B }
  when (refill_done) { refill_valid := false.B}

  io.perf.acquire := refill_fire
  // don't gate I$ clock since there are outstanding transcations.
  io.keep_clock_enabled :=
    tl_in.map(tl => tl.a.valid || tl.d.valid || s1_slaveValid || s2_slaveValid || s3_slaveValid).getOrElse(false.B) || // ITIM
    s1_valid || s2_valid || refill_valid || send_hint || hint_outstanding // I$

  /** index to access [[data_arrays]] and [[tag_array]].
    * @note
    * if [[untagBits]] > [[pgIdxBits]] in
    * {{{
    *                        ┌──idxBits──┐
    *                        ↓           ↓
    * │          tag         │    set    │offset│
    * │              pageTag     │     pageIndex│
    *                        ↑   ↑       ↑      │
    *                   untagBits│  blockOffBits│
    *                       pgIdxBits    │
    *                        └msb┴──lsb──┘
    *                        vaddr paddr
    * }}}
    *
    * else use paddr directly.
    * Note: if [[untagBits]] > [[pgIdxBits]], there will be a alias issue which isn't addressend by the icache yet.
    */
  def index(vaddr: UInt, paddr: UInt) = {
    /** [[paddr]] as LSB to be used for VIPT. */
    val lsbs = paddr(pgUntagBits-1, blockOffBits)
    /** if [[untagBits]] > [[pgIdxBits]], append [[vaddr]] to higher bits of index as [[msbs]]. */
    val msbs = (idxBits+blockOffBits > pgUntagBits).option(vaddr(idxBits+blockOffBits-1, pgUntagBits))
    msbs ## lsbs
  }

  ccover(!send_hint && (tl_out.a.valid && !tl_out.a.ready), "MISS_A_STALL", "I$ miss blocked by A-channel")
  ccover(invalidate && refill_valid, "FLUSH_DURING_MISS", "I$ flushed during miss")

  def ccover(cond: Bool, label: String, desc: String)(implicit sourceInfo: SourceInfo) =
    property.cover(cond, s"ICACHE_$label", "MemorySystem;;" + desc)

  val mem_active_valid = Seq(property.CoverBoolean(s2_valid, Seq("mem_active")))
  val data_error = Seq(
    property.CoverBoolean(!s2_data_decoded.correctable && !s2_data_decoded.uncorrectable, Seq("no_data_error")),
    property.CoverBoolean(s2_data_decoded.correctable, Seq("data_correctable_error")),
    property.CoverBoolean(s2_data_decoded.uncorrectable, Seq("data_uncorrectable_error")))
  val request_source = Seq(
    property.CoverBoolean(!s2_slaveValid, Seq("from_CPU")),
    property.CoverBoolean(s2_slaveValid, Seq("from_TL"))
  )
  val tag_error = Seq(
    property.CoverBoolean(!s2_tag_disparity, Seq("no_tag_error")),
    property.CoverBoolean(s2_tag_disparity, Seq("tag_error"))
  )
  val mem_mode = Seq(
    property.CoverBoolean(s2_scratchpad_hit, Seq("ITIM_mode")),
    property.CoverBoolean(!s2_scratchpad_hit, Seq("cache_mode"))
  )

  val error_cross_covers = new property.CrossProperty(
    Seq(mem_active_valid, data_error, tag_error, request_source, mem_mode),
    Seq(
      // tag error cannot occur in ITIM mode
      Seq("tag_error", "ITIM_mode"),
      // Can only respond to TL in ITIM mode
      Seq("from_TL", "cache_mode")
    ),
    "MemorySystem;;Memory Bit Flip Cross Covers")

  property.cover(error_cross_covers)
}
