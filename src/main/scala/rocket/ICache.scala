// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._
import freechips.rocketchip.amba._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{DescribedSRAM, _}
import freechips.rocketchip.util.property._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.dontTouch
import chisel3.util.random.LFSR

/** Parameter of [[ICache]].
  *
  * @param nSets size of sets.
  * @param nWays size of ways in each sets.
  * @param tagECC tag ECC, will be parsed to [[freechips.rocketchip.util.Code]].
  * @param dataECC data ECC, will be parsed to [[freechips.rocketchip.util.Code]].
  * @param itimAddr optional base ITIM address,
  *                 if None, ITIM won't be generated,
  *                 if Some, ITIM will be generated, with number as ITIM base address.
  * @param prefetch if set, will send [[TLEdgeOut.Hint]] to manger.
  * @param blockBytes size of a cacheline, calculates in byte.
  * @param latency latency of a instruction fetch, 1 or 2 are available
  * @param fetchBytes byte size fetched by CPU for each cycle.
  */
case class ICacheParams(
    nSets: Int = 64,
    nWays: Int = 4,
    // @todo none-used parameter for ICache.
    rowBits: Int = 128,
    // @todo none-used parameter for ICache.
    nTLBSets: Int = 1,
    // @todo none-used parameter for ICache.
    nTLBWays: Int = 32,
    // @todo none-used parameter for ICache.
    nTLBBasePageSectors: Int = 4,
    // @todo none-used parameter for ICache.
    nTLBSuperpages: Int = 4,
    // @todo none-used parameter.
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
  val addr = UInt(width = vaddrBits)
}

class ICacheErrors(implicit p: Parameters) extends CoreBundle()(p)
    with HasL1ICacheParameters
    with CanHaveErrors {
  val correctable = (cacheParams.tagCode.canDetect || cacheParams.dataCode.canDetect).option(Valid(UInt(width = paddrBits)))
  val uncorrectable = (cacheParams.itimAddr.nonEmpty && cacheParams.dataCode.canDetect).option(Valid(UInt(width = paddrBits)))
  val bus = Valid(UInt(width = paddrBits))
}

/** [[ICache]] is a set associated cache I$(Instruction Cache) of Rocket.
  * It can have an optional dynamic configurable ITIM sharing SRAM with I$ by configuring [[icacheParams.itimAddr]].
  * if PutFullData/PutPartialData to the ITIM address, it will dynamically allocate base address to the address of this accessing from SRAM.
  * if access to last way of ITIM, it set will change back to I$.
  *
  * There will always be one way(the last way) used for I$, which cannot be allocated to ITIM.
  *
  * @param icacheParams parameter to this I$.
  * @param staticIdForMetadataUseOnly metadata used for hart id.
  */
class ICache(val icacheParams: ICacheParams, val staticIdForMetadataUseOnly: Int)(implicit p: Parameters) extends LazyModule {
  lazy val module = new ICacheModule(this)

  /** Diplomatic hardid bundle used for ITIM. */
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
    * 1: use [[TLEdgeOut.Hint]] to hint next level memory device fetching next page, if configured [[icacheParams.prefetch]].
    *
    * @todo why if no [[useVM]], will have AMBAProtField in requestFields?
    */
  val masterNode = TLClientNode(Seq(TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      sourceId = IdRange(0, 1 + icacheParams.prefetch.toInt), // 0=refill, 1=hint
      name = s"Core ${staticIdForMetadataUseOnly} ICache")),
    requestFields = useVM.option(Seq()).getOrElse(Seq(AMBAProtField())))))

  /** size of [[ICache]], count in byte. */
  val size: Int = icacheParams.nSets * icacheParams.nWays * icacheParams.blockBytes

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

  /** @todo Instruction Tightly Integrated Memory node. */
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
  val data = UInt(width = outer.icacheParams.fetchBytes*8)
  /** ask CPU to replay fetch when tag or data ECC error happened. */
  val replay = Bool()
  /** access exception:
    * indicate CPU an tag ECC error happened.
    * if [[outer.icacheParams.latency]] is 1, tie 0.
    */
  val ae = Bool()

  override def cloneType = new ICacheResp(outer).asInstanceOf[this.type]
}

class ICachePerfEvents extends Bundle {
  val acquire = Bool()
}

/** IO from CPU to ICache. */
class ICacheBundle(val outer: ICache) extends CoreBundle()(outer.p) {
  /** first cycle requested from CPU. */
  val req = Decoupled(new ICacheReq).flip

  /** delayed one cycle w.r.t. req */
  val s1_paddr = UInt(INPUT, paddrBits)

  /** delayed two cycles w.r.t. req. */
  val s2_vaddr = UInt(INPUT, vaddrBits)

  /** delayed one cycle w.r.t. req. */
  val s1_kill = Bool(INPUT)

  /** delayed two cycles; prevents I$ miss emission. */
  val s2_kill = Bool(INPUT)

  /** should I$ prefetch next line on a miss? */
  val s2_prefetch = Bool(INPUT)

  /** response to CPU. */
  val resp = Valid(new ICacheResp(outer))

  /** flush L1 cache from CPU. */
  val invalidate = Bool(INPUT)

  /** I$ has error, notify to bus. */
  val errors = new ICacheErrors

  /** for performance counting. */
  val perf = new ICachePerfEvents().asOutput

  /** enable clock. */
  val clock_enabled = Bool(INPUT)

  /** I$ miss or ITIM access will still enable clock even [[ICache]] is asked to be gated. */
  val keep_clock_enabled = Bool(OUTPUT)
}

/** Rocket virtually-indexed physically-tagged (VIPT) L1 Instruction Cache module,
  * which also can be used as an ITIM(Instruction Tightly Integrated Memory).
  * If ITIM is configured:
  *   set: if address to access is not to be configured to ITIM yet,
  *        a memory accessing to ITIM address range will modify `scratchpadMax`,
  *        from ITIM base to `scratchpadMax` will be used as ITIM.
  *   unset: @todo
  *
  *
  * Pipeline:
  * Stage 0: access `tag_array` and `data_array` with `vaddr`.
  * Stage 1: if hit in stage 0:
  *          tag comparison with paddr or variation of PT(mix of `vaddr` and `paddr`)
  *          if miss in stage 0:
  * Stage 2:
  * Stage 3:
  *
  * `tag_array` read with vaddr at Stage 0,
  *             write with `index(vaddr, paddr)`.
  * `data_array` read with vaddr truncating TileLink beat count at Stage 0.
  *              write with `index(vaddr, paddr)` truncating TileLink beat count.
  *
  *
  * Stage 1: Tag ECC
  * Stage 2: Data ECC
  */
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

  /** tag ecc. */
  val tECC = cacheParams.tagCode
  /** data ecc. */
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
  val scratchpadMax = tl_in.map(tl => Reg(UInt(width = log2Ceil(nSets * (nWays - 1)))))
  /** line is a minimal granularity accessing to SRAM, check if a line is in the scratchpad or not.
    * Accessing from [[tl_in]]: convert address to line with `untagBits+log2Ceil(nWays)-1, blockOffBits`(slices from address)
    * {{{
    * │          tag         │    set    │offset│
    *                    ├way┘                    → indicate way location
    *                    │    line       │
    * }}}
    * Accessing from [[io]]: normal cache access @todo
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
  def addrMaybeInScratchpad(addr: UInt) = scratchpadBase.map(base => addr >= base && addr < base + outer.size).getOrElse(false.B)

  /** check property this address exists in scratchpad.
    * @todo seems duplicated in `addrMaybeInScratchpad(addr)` between `lineInScratchpad(addr(untagBits+log2Ceil(nWays)-1, blockOffBits))`?
    */
  def addrInScratchpad(addr: UInt) = addrMaybeInScratchpad(addr) && lineInScratchpad(addr(untagBits+log2Ceil(nWays)-1, blockOffBits))

  /** return the way which will be used as scratchpad for accessing address?
    * {{{
    * │          tag         │    set    │offset│
    *                    └way┘
    * }}}
    * @param addr address to be found.
    */
  def scratchpadWay(addr: UInt) = addr.extract(untagBits+log2Ceil(nWays)-1, untagBits)

  /** can this way being used by scratchpad?
    * at least the last way should be reserved to cache.
    */
  def scratchpadWayValid(way: UInt) = way < nWays - 1

  /** return the cacheline which will be used as scratchpad for accessing address?
    *
    * @param addr address to be found.
    */
  def scratchpadLine(addr: UInt) = addr(untagBits+log2Ceil(nWays)-1, blockOffBits)

  /** scratchpad stage 0 to stage 1 valid. */
  val s0_slaveValid = tl_in.map(_.a.fire()).getOrElse(false.B)
  /** scratchpad stage 1 to stage 2 valid. */
  val s1_slaveValid = RegNext(s0_slaveValid, false.B)
  /** scratchpad stage 2 to stage 3 valid. */
  val s2_slaveValid = RegNext(s1_slaveValid, false.B)
  /** scratchpad stage 3 to stage 4 valid. */
  val s3_slaveValid = RegNext(false.B)

  /** valid signal for CPU accessing cache in stage 0. */
  val s0_valid = io.req.fire()
  /** virtual address from CPU in stage 0. */
  val s0_vaddr = io.req.bits.addr

  /** valid signal for CPU accessing cache in stage 1.*/
  val s1_valid = Reg(init=Bool(false))
  /** virtual address from CPU in stage 1. */
  val s1_vaddr = RegEnable(s0_vaddr, s0_valid)
  /** tag hit vector to indicate hit which way. */
  val s1_tag_hit = Wire(Vec(nWays, Bool()))
  /** CPU I$ Hit in stage 1.
    *
    * @note
    * for logic in `Mux(s1_slaveValid, true.B, addrMaybeInScratchpad(io.s1_paddr))`,
    * there are two different types based on latency:
    * if latency is 1: `s1_slaveValid === false.B` and `addrMaybeInScratchpad(io.s1_paddr) === false.B` ,
    *                   since in this case, ITIM must be empty.
    * if latency is 2: if `s1_slaveValid` is true, this SRAM accessing is coming from [[tl_in]], so it will hit.
    *                  if `s1_slaveValid` is false, but CPU is accessing memory range in scratchpad address, it will hit by default.
    *                  Hardware won't guarantee this access will access to a data which have been written in ITIM.
    *
    * @todo seem CPU access are both processed by `s1_tag_hit` and `Mux(s1_slaveValid, true.B, addrMaybeInScratchpad(io.s1_paddr))`?
    */
  val s1_hit = s1_tag_hit.reduce(_||_) || Mux(s1_slaveValid, true.B, addrMaybeInScratchpad(io.s1_paddr))
  dontTouch(s1_hit)
  /** stage 2 valid signal. */
  val s2_valid = RegNext(s1_valid && !io.s1_kill, Bool(false))
  /** CPU I$ Hit in stage 2. */
  val s2_hit = RegNext(s1_hit)

  /** status register to indicate flush is cache. */
  val invalidated = Reg(Bool())
  /** status register to indicate there is a outstanding refill. */
  val refill_valid = RegInit(false.B)
  /** register to indicate [[tl_out]] is performing a hint. */
  val send_hint = RegInit(false.B)
  /** indicate [[tl_out]] is performing a refill. */
  val refill_fire = tl_out.a.fire() && !send_hint
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
  /** physical address to refill. */
  val refill_paddr = RegEnable(io.s1_paddr, s1_valid && s1_can_request_refill)
  /** virtual address to refill. */
  val refill_vaddr = RegEnable(s1_vaddr, s1_valid && s1_can_request_refill)
  /** tag of address to refill. */
  val refill_tag = refill_paddr >> pgUntagBits
  /** index of address to refill. */
  val refill_idx = index(refill_vaddr, refill_paddr)
  /** AccessAckData, is refilling I$, it will block request form CPU. */
  val refill_one_beat = tl_out.d.fire() && edge_out.hasData(tl_out.d.bits)

  /** block request from CPU when refill or scratch pad access. */
  io.req.ready := !(refill_one_beat || s0_slaveValid || s3_slaveValid)
  // @todo refactor to RegNext.
  s1_valid := s0_valid

  val (_, _, d_done, refill_cnt) = edge_out.count(tl_out.d)
  /** at last beat of `tl_out.d.fire()`, finish refill. */
  val refill_done = refill_one_beat && d_done
  /** scratchpad is writing data. block refill. */
  tl_out.d.ready := !s3_slaveValid
  require (edge_out.manager.minLatency > 0)

  /** way to be replaced, implemented with a hardcoded random replacement algorithm */
  val repl_way = if (isDM) UInt(0) else {
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

  // init tag SRAM, indexed with virtual memory, content with `eccError ## tag` after ECC.
  val (tag_array, omSRAM) = DescribedSRAM(
    name = "tag_array",
    desc = "ICache Tag Array",
    size = nSets,
    data = Vec(nWays, UInt(width = tECC.width(1 + tagBits)))
  )

  /** read tag at stage 0 with virtual memory.
    * @todo why `!refill_done` ? `refill_one_beat` block `io.req.ready` block `s0_valid`
    */
  val tag_rdata = tag_array.read(s0_vaddr(untagBits-1,blockOffBits), !refill_done && s0_valid)

  /** register indicates the ongoing GetAckData transaction is corrupted. */
  val accruedRefillError = Reg(Bool())
  /** wire indicates the ongoing GetAckData transaction is corrupted. */
  val refillError = tl_out.d.bits.corrupt || (refill_cnt > 0 && accruedRefillError)
  when (refill_done) {
    // For AccessAckData, denied => corrupt
    /** data write to [[tag_array]]. */
    val enc_tag = tECC.encode(Cat(refillError, refill_tag))
    // write tag array with ECC encoded `refillError ## refill_tag`
    tag_array.write(refill_idx, Vec.fill(nWays)(enc_tag), Seq.tabulate(nWays)(repl_way === _))

    ccover(refillError, "D_CORRUPT", "I$ D-channel corrupt")
  }
  // notify CPU, I$ has corrupt.
  io.errors.bus.valid := tl_out.d.fire() && (tl_out.d.bits.denied || tl_out.d.bits.corrupt)
  io.errors.bus.bits  := (refill_paddr >> blockOffBits) << blockOffBits

  /** true indicate this cacheline is valid,
    * indexed by (wayIndex ## setIndex)
    */
  val vb_array = Reg(init=Bits(0, nSets*nWays))
  when (refill_one_beat) {
    accruedRefillError := refillError
    // @todo seems comments are not corresponding to codes?
    // clear bit when refill starts so hit-under-miss doesn't fetch bad data
    vb_array := vb_array.bitSet(Cat(repl_way, refill_idx), refill_done && !invalidated)
  }

  /** flush cache when invalidate is true. */
  val invalidate = Wire(init = io.invalidate)
  when (invalidate) {
    vb_array := Bits(0)
    invalidated := Bool(true)
  }

  /** wire indicate that tag is correctable or uncorrectable.
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
  val s1_dout = Wire(Vec(nWays, UInt(width = dECC.width(wordBits))))

  /** address accessed by [[tl_in]] for ITIM. */
  val s0_slaveAddr = tl_in.map(_.a.bits.address).getOrElse(0.U)
  /** address used at stage 1 and 3.
    * @todo In stage 1, it caches TileLink data, store in stage 2 if ECC passed.
    * @todo In stage 3, it caches corrected data from stage 2, and store in stage 4.
    */
  val s1s3_slaveAddr = Reg(UInt(width = log2Ceil(outer.size)))
  /** data used at stage 1 and 3.
    * In stage 1, it caches TileLink data, store in stage 2 if ECC passed.
    * In stage 3, it caches corrected data from stage 2, and store in stage 4.
    */
  val s1s3_slaveData = Reg(UInt(width = wordBits))

  for (i <- 0 until nWays) {
    /** set index from CPU request. */
    val s1_idx = index(s1_vaddr, io.s1_paddr)
    /** tag from CPU paddr request. */
    val s1_tag = io.s1_paddr >> pgUntagBits
    /** this way is used by scratchpad.
      * [[tag_array]] corrupted.
      */
    val scratchpadHit = scratchpadWayValid(i) &&
      Mux(s1_slaveValid,
        // scratchpad accessing form [[tl_in]].
        // @todo I think XBar will guarantee there won't be an illegal access on the bus?
        //       so why did have this check `lineInScratchpad(scratchpadLine(s1s3_slaveAddr))`?
        //       I think it will always be true.
        lineInScratchpad(scratchpadLine(s1s3_slaveAddr)) && scratchpadWay(s1s3_slaveAddr) === i,
        // scratchpad accessing from [[io]].
        // @todo Accessing ITIM correspond address will be able to read cacheline?
        //       is this desired behavior?
        addrInScratchpad(io.s1_paddr) && scratchpadWay(io.s1_paddr) === i)
    /** new valid block after scratchpad accessing. */
    val s1_vb = vb_array(Cat(UInt(i), s1_idx)) && !s1_slaveValid
    /** ECC decoded result form [[tag_rdata]]. */
    val enc_tag = tECC.decode(tag_rdata(i))
    /** [[tl_error]] ECC error bit.
      * [[tag]] of [[tag_array]] access.
      */
    val (tl_error, tag) = Split(enc_tag.uncorrected, tagBits)

    /** way [[i]] access is matched to CPU access from [[io]]. */
    val tagMatch = s1_vb && tag === s1_tag
    /** tag error happens. */
    s1_tag_disparity(i) := s1_vb && enc_tag.error
    // if tag matched but ecc checking failed, this access will trigger [[Causes.fetch_access]] exception.
    s1_tl_error(i) := tagMatch && tl_error.asBool
    // hit index [[i]] of [[tag_array]].
    s1_tag_hit(i) := tagMatch || scratchpadHit
  }
  assert(!(s1_valid || s1_slaveValid) || PopCount(s1_tag_hit zip s1_tag_disparity map { case (h, d) => h && !d }) <= 1)

  require(tl_out.d.bits.data.getWidth % wordBits == 0)

  /** init data SRAM banks,
    * banked with TileLink beat bytes / CPU fetch bytes,
    * indexed with [[index]] and multi-beats cycle,
    * content with `eccError ## wordBits` after ECC.
    * │                          │xx│xxxxxx│xxx│x│xx│
    *                                            ↑word
    *                                          ↑bank
    *                            ↑way
    *                               └─set──┴─offset─┘
    *                               └────row───┘
    */
  val data_arrays = Seq.tabulate(tl_out.d.bits.data.getWidth / wordBits) {
    i =>
      DescribedSRAM(
        name = s"data_arrays_${i}",
        desc = "ICache Data Array",
        size = nSets * refillCycles,
        data = Vec(nWays, UInt(width = dECC.width(wordBits)))
      )
  }

  for (((data_array, omSRAM), i) <- data_arrays zipWithIndex) {
    /** Is this address access in this bank? */
    def wordMatch(addr: UInt) = addr.extract(log2Ceil(tl_out.d.bits.data.getWidth/8)-1, log2Ceil(wordBits/8)) === i
    /** return row correspond to a address. */
    def row(addr: UInt) = addr(untagBits-1, blockOffBits-log2Ceil(refillCycles))
    /** CPU or ITIM read match this way. */
    val s0_ren = (s0_valid && wordMatch(s0_vaddr)) || (s0_slaveValid && wordMatch(s0_slaveAddr))
    /** refill from [[tl_out]] or ITIM write. */
    val wen = (refill_one_beat && !invalidated) || (s3_slaveValid && wordMatch(s1s3_slaveAddr))
    /** index to access [[data_array]]. */
    val mem_idx =
      // I$ refill.
      Mux(refill_one_beat, (refill_idx << log2Ceil(refillCycles)) | refill_cnt,
      // ITIM write.
                  Mux(s3_slaveValid, row(s1s3_slaveAddr),
      // ITIM read.
                  Mux(s0_slaveValid, row(s0_slaveAddr),
      // CPU read.
                  row(s0_vaddr))))
    when (wen) {
      val data = Mux(s3_slaveValid, s1s3_slaveData, tl_out.d.bits.data(wordBits*(i+1)-1, wordBits*i))
      val way = Mux(s3_slaveValid, scratchpadWay(s1s3_slaveAddr), repl_way)
      data_array.write(mem_idx, Vec.fill(nWays)(dECC.encode(data)), (0 until nWays).map(way === _))
    }
    // write access
    /** data read from [[data_array]]. */
    val dout = data_array.read(mem_idx, !wen && s0_ren)
    // Mux to select a way to [[s1_dout]]
    when (wordMatch(Mux(s1_slaveValid, s1s3_slaveAddr, io.s1_paddr))) {
      s1_dout := dout
    }
  }

  /** clock gate signal for [[s2_tag_hit]], [[s2_dout]], [[s2_tag_disparity]], [[s2_tl_error]], [[s2_scratchpad_hit]]. */
  val s1_clk_en = s1_valid || s1_slaveValid
  /** stage 2 of [[s1_tag_hit]]. */
  val s2_tag_hit = RegEnable(s1_tag_hit, s1_clk_en)
  /** way index to access [[data_arrays]]. */
  val s2_hit_way = OHToUInt(s2_tag_hit)
  /** ITIM index to access [[data_arrays]].
    * replace tag with way, word set to 0.
    */
  val s2_scratchpad_word_addr = Cat(s2_hit_way, Mux(s2_slaveValid, s1s3_slaveAddr, io.s2_vaddr)(untagBits-1, log2Ceil(wordBits/8)), UInt(0, log2Ceil(wordBits/8)))
  /** stage 2 of [[s1_dout]]. */
  val s2_dout = RegEnable(s1_dout, s1_clk_en)
  /** way selected from [[s2_dout]]. */
  val s2_way_mux = Mux1H(s2_tag_hit, s2_dout)

  /** stage 2 of [[s1_tag_disparity]]. */
  val s2_tag_disparity = RegEnable(s1_tag_disparity, s1_clk_en).asUInt.orR
  /** stage 2 of [[s1_tl_error]]. */
  val s2_tl_error = RegEnable(s1_tl_error.asUInt.orR, s1_clk_en)
  /** ECC decode result for [[data_arrays]]. */
  val s2_data_decoded = dECC.decode(s2_way_mux)
  /** ECC error happened, correctable or uncorrectable, ask CPU to replay. */
  val s2_disparity = s2_tag_disparity || s2_data_decoded.error
  /** When writing full words to ITIM, ECC errors are correctable. */
  val s2_full_word_write = Wire(init = false.B)

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
  val s2_report_uncorrectable_error = s2_scratchpad_hit && s2_data_decoded.uncorrectable && (s2_valid || (s2_slaveValid && !s2_full_word_write))
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

    // if I$ latency is 2, can have ITIM and ECC.
    case 2 =>
      // when some sort of memory bit error have occurred
      // @todo why so aggressive to invalidate all when ecc corrupted.
    when (s2_valid && s2_disparity) { invalidate := true }

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
        s2_full_word_write := edge_in.get.hasData(s1_a) && s1_a.mask.andR
        when (s0_slaveValid) {
          val a = tl.a.bits
          s1s3_slaveAddr := tl.a.bits.address
          s1s3_slaveData := tl.a.bits.data
          when (edge_in.get.hasData(a)) {
            // access data in 0 -> way - 2 allocate and enable, access data in way - 1(last way), deallocate.
            val enable = scratchpadWayValid(scratchpadWay(a.address))
            when (!lineInScratchpad(scratchpadLine(a.address))) {
              scratchpadMax.get := scratchpadLine(a.address)
              invalidate := true
            }
            scratchpadOn := enable

            val itim_allocated = !scratchpadOn && enable
            val itim_deallocated = scratchpadOn && !enable
            val itim_increase = scratchpadOn && enable && scratchpadLine(a.address) > scratchpadMax.get
            val refilling = refill_valid && refill_cnt > 0
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
          // report the a miss but don't correct the error (as there is
          // a structural hazard on s1s3_slaveData/s1s3_slaveAddress).
          s3_slaveValid := true
          s1s3_slaveData := s2_data_decoded.corrected
          s1s3_slaveAddr := s2_scratchpad_word_addr | s1s3_slaveAddr(log2Ceil(wordBits/8)-1, 0)
        }

        // back pressure is allowed on the [[tl]]
        // pull up [[respValid]] when [[s2_slaveValid]] until [[tl.d.fire()]]
        respValid := s2_slaveValid || (respValid && !tl.d.ready)
        // if [[s2_full_word_write]] will overwrite data, and [[s2_data_decoded.uncorrectable]] can be ignored.
        val respError = RegEnable(s2_scratchpad_hit && s2_data_decoded.uncorrectable && !s2_full_word_write, s2_slaveValid)
        when (s2_slaveValid) {
          // need stage 3 if Put or correct decoding.
          // @todo if uncorrectable [[s2_data_decoded]]?
          when (edge_in.get.hasData(s1_a) || s2_data_decoded.error) { s3_slaveValid := true }
          /** data not masked by the TileLink PutData/PutPartitalData.
            * means data is stored at [[s1s3_slaveData]] which was read at stage 1.
            */
          def byteEn(i: Int) = !(edge_in.get.hasData(s1_a) && s1_a.mask(i))
          // write [[s1s3_slaveData]] based on index of wordBits.
          // @todo seems a problem here?
          //       granularity of CPU fetch is `wordBits/8`,
          //       granularity of TileLink access is `TLBundleParameters.dataBits/8`
          //       these two granularity can be different.
          s1s3_slaveData := (0 until wordBits/8).map(i => Mux(byteEn(i), s2_data_decoded.corrected, s1s3_slaveData)(8*(i+1)-1, 8*i)).asUInt
        }

        tl.d.valid := respValid
        tl.d.bits := Mux(edge_in.get.hasData(s1_a),
          // PutData/PutPartialData -> AccessAck
          edge_in.get.AccessAck(s1_a),
          // Get -> AccessAckData
          edge_in.get.AccessAck(s1_a, UInt(0), denied = Bool(false), corrupt = respError))
        // @todo why directly `edge_in.get.AccessAck(s1_a, s1s3_slaveData, denied = Bool(false), corrupt = respError)`
        tl.d.bits.data := s1s3_slaveData

        // Tie off unused channels
        tl.b.valid := false
        tl.c.ready := true
        tl.e.ready := true

        ccover(s0_valid && s1_slaveValid, "CONCURRENT_ITIM_ACCESS_1", "ITIM accessed, then I$ accessed next cycle")
        ccover(s0_valid && s2_slaveValid, "CONCURRENT_ITIM_ACCESS_2", "ITIM accessed, then I$ accessed two cycles later")
        ccover(tl.d.valid && !tl.d.ready, "ITIM_D_STALL", "ITIM response blocked by D-channel")
        ccover(tl_out.d.valid && !tl_out.d.ready, "ITIM_BLOCK_D", "D-channel blocked by ITIM access")
      }
  }

  // refill
  tl_out.a.valid := s2_request_refill
  tl_out.a.bits := edge_out.Get(
                    fromSource = UInt(0),
                    toAddress = (refill_paddr >> blockOffBits) << blockOffBits,
                    lgSize = lgCacheBlockBytes)._2

  // prefetch when not access cross a page.
  if (cacheParams.prefetch) {
    /** [[crosses_page]]  the last address of this current page is requested to refill.
      * [[next_block]] next address to be accessed.
      */
    val (crosses_page, next_block) = Split(refill_paddr(pgIdxBits-1, blockOffBits) +& 1, pgIdxBits-blockOffBits)

    // assign to [[send_hint]] and [[hint_outstanding]]
    when (tl_out.a.fire()) {
      send_hint := !hint_outstanding && io.s2_prefetch && !crosses_page
      when (send_hint) {
        send_hint := false
        hint_outstanding := true
      }
    }

    // @todo why refill_done will kill hint at this cycle?
    when (refill_done) {
      send_hint := false
    }

    // D channel reply with HintAck.
    when (tl_out.d.fire() && !refill_one_beat) {
      hint_outstanding := false
    }

    when (send_hint) {
      tl_out.a.valid := true
      tl_out.a.bits := edge_out.Hint(
                        fromSource = UInt(1),
                        toAddress = Cat(refill_paddr >> pgIdxBits, next_block) << blockOffBits,
                        lgSize = lgCacheBlockBytes,
                        param = TLHints.PREFETCH_READ)._2
    }

    ccover(send_hint && !tl_out.a.ready, "PREFETCH_A_STALL", "I$ prefetch blocked by A-channel")
    ccover(refill_valid && (tl_out.d.fire() && !refill_one_beat), "PREFETCH_D_BEFORE_MISS_D", "I$ prefetch resolves before miss")
    ccover(!refill_valid && (tl_out.d.fire() && !refill_one_beat), "PREFETCH_D_AFTER_MISS_D", "I$ prefetch resolves after miss")
    ccover(tl_out.a.fire() && hint_outstanding, "PREFETCH_D_AFTER_MISS_A", "I$ prefetch resolves after second miss")
  }
  // Drive APROT information
  tl_out.a.bits.user.lift(AMBAProt).foreach { x =>
    // Rocket caches all fetch requests, and it's difficult to differentiate privileged/unprivileged on
    // cached data, so mark as privileged
    val user_bit_cacheable = true.B

    // enable outer caches for all fetches
    x.privileged  := user_bit_cacheable
    x.bufferable  := user_bit_cacheable
    x.modifiable  := user_bit_cacheable
    x.readalloc   := user_bit_cacheable
    x.writealloc  := user_bit_cacheable

    // Following are always tied off
    x.fetch       := true.B
    x.secure      := true.B
  }
  // Tie off unused channels
  tl_out.b.ready := Bool(true)
  tl_out.c.valid := Bool(false)
  tl_out.e.valid := Bool(false)
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
    * @todo why not VA directly?
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
    cover(cond, s"ICACHE_$label", "MemorySystem;;" + desc)

  val mem_active_valid = Seq(CoverBoolean(s2_valid, Seq("mem_active")))
  val data_error = Seq(
    CoverBoolean(!s2_data_decoded.correctable && !s2_data_decoded.uncorrectable, Seq("no_data_error")),
    CoverBoolean(s2_data_decoded.correctable, Seq("data_correctable_error")),
    CoverBoolean(s2_data_decoded.uncorrectable, Seq("data_uncorrectable_error")))
  val request_source = Seq(
    CoverBoolean(!s2_slaveValid, Seq("from_CPU")),
    CoverBoolean(s2_slaveValid, Seq("from_TL"))
  )
  val tag_error = Seq(
    CoverBoolean(!s2_tag_disparity, Seq("no_tag_error")),
    CoverBoolean(s2_tag_disparity, Seq("tag_error"))
  )
  val mem_mode = Seq(
    CoverBoolean(s2_scratchpad_hit, Seq("ITIM_mode")),
    CoverBoolean(!s2_scratchpad_hit, Seq("cache_mode"))
  )

  val error_cross_covers = new CrossProperty(
    Seq(mem_active_valid, data_error, tag_error, request_source, mem_mode),
    Seq(
      // tag error cannot occur in ITIM mode
      Seq("tag_error", "ITIM_mode"),
      // Can only respond to TL in ITIM mode
      Seq("from_TL", "cache_mode")
    ),
    "MemorySystem;;Memory Bit Flip Cross Covers")

  cover(error_cross_covers)
}
