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
  val addr = UInt(width = vaddrBits)
}

class ICacheErrors(implicit p: Parameters) extends CoreBundle()(p)
    with HasL1ICacheParameters
    with CanHaveErrors {
  val correctable = (cacheParams.tagCode.canDetect || cacheParams.dataCode.canDetect).option(Valid(UInt(width = paddrBits)))
  val uncorrectable = (cacheParams.itimAddr.nonEmpty && cacheParams.dataCode.canDetect).option(Valid(UInt(width = paddrBits)))
  val bus = Valid(UInt(width = paddrBits))
}

class ICache(val icacheParams: ICacheParams, val staticIdForMetadataUseOnly: Int)(implicit p: Parameters) extends LazyModule {
  lazy val module = new ICacheModule(this)
  val hartIdSinkNodeOpt = icacheParams.itimAddr.map(_ => BundleBridgeSink[UInt]())
  val mmioAddressPrefixSinkNodeOpt = icacheParams.itimAddr.map(_ => BundleBridgeSink[UInt]())
  val useVM = p(TileKey).core.useVM
  val masterNode = TLClientNode(Seq(TLMasterPortParameters.v2(
    masters = Seq(TLMasterParameters.v2(
      sourceId = IdRange(0, 1 + icacheParams.prefetch.toInt), // 0=refill, 1=hint
      name     = s"Core ${staticIdForMetadataUseOnly} ICache",
      emits    = TLMasterToSlaveTransferSizes(
        get = TransferSizes(icacheParams.blockBytes, icacheParams.blockBytes),
        hint = TransferSizes(icacheParams.blockBytes, icacheParams.blockBytes)
      ))),
    requestFields = useVM.option(Seq()).getOrElse(Seq(AMBAProtField())))))

  val size = icacheParams.nSets * icacheParams.nWays * icacheParams.blockBytes
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

  private val wordBytes = icacheParams.fetchBytes
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
  val data = UInt(width = outer.icacheParams.fetchBytes*8)
  val replay = Bool()
  val ae = Bool()

  override def cloneType = new ICacheResp(outer).asInstanceOf[this.type]
}

class ICachePerfEvents extends Bundle {
  val acquire = Bool()
}

class ICacheBundle(val outer: ICache) extends CoreBundle()(outer.p) {
  val req = Decoupled(new ICacheReq).flip
  val s1_paddr = UInt(INPUT, paddrBits) // delayed one cycle w.r.t. req
  val s2_vaddr = UInt(INPUT, vaddrBits) // delayed two cycles w.r.t. req
  val s1_kill = Bool(INPUT) // delayed one cycle w.r.t. req
  val s2_kill = Bool(INPUT) // delayed two cycles; prevents I$ miss emission
  val s2_prefetch = Bool(INPUT) // should I$ prefetch next line on a miss?

  val resp = Valid(new ICacheResp(outer))
  val invalidate = Bool(INPUT)

  val errors = new ICacheErrors
  val perf = new ICachePerfEvents().asOutput

  val clock_enabled = Bool(INPUT)
  val keep_clock_enabled = Bool(OUTPUT)
}

class ICacheModule(outer: ICache) extends LazyModuleImp(outer)
    with HasL1ICacheParameters {
  override val cacheParams = outer.icacheParams // Use the local parameters

  val io = IO(new ICacheBundle(outer))
  val (tl_out, edge_out) = outer.masterNode.out(0)
  // Option.unzip does not exist :-(
  val (tl_in, edge_in) = outer.slaveNode.in.headOption.unzip

  val tECC = cacheParams.tagCode
  val dECC = cacheParams.dataCode

  require(isPow2(nSets) && isPow2(nWays))
  require(!usingVM || outer.icacheParams.itimAddr.isEmpty || pgIdxBits >= untagBits,
    s"When VM and ITIM are enabled, I$$ set size must not exceed ${1<<(pgIdxBits-10)} KiB; got ${(outer.size/nWays)>>10} KiB")

  val io_hartid = outer.hartIdSinkNodeOpt.map(_.bundle)
  val io_mmio_address_prefix = outer.mmioAddressPrefixSinkNodeOpt.map(_.bundle)
  val scratchpadOn = RegInit(false.B)
  val scratchpadMax = tl_in.map(tl => Reg(UInt(width = log2Ceil(nSets * (nWays - 1)))))
  def lineInScratchpad(line: UInt) = scratchpadMax.map(scratchpadOn && line <= _).getOrElse(false.B)
  val scratchpadBase = outer.icacheParams.itimAddr.map { dummy =>
    p(LookupByHartId)(_.icache.flatMap(_.itimAddr.map(_.U)), io_hartid.get) | io_mmio_address_prefix.get
  }
  def addrMaybeInScratchpad(addr: UInt) = scratchpadBase.map(base => addr >= base && addr < base + outer.size).getOrElse(false.B)
  def addrInScratchpad(addr: UInt) = addrMaybeInScratchpad(addr) && lineInScratchpad(addr(untagBits+log2Ceil(nWays)-1, blockOffBits))
  def scratchpadWay(addr: UInt) = addr.extract(untagBits+log2Ceil(nWays)-1, untagBits)
  def scratchpadWayValid(way: UInt) = way < nWays - 1
  def scratchpadLine(addr: UInt) = addr(untagBits+log2Ceil(nWays)-1, blockOffBits)
  val s0_slaveValid = tl_in.map(_.a.fire()).getOrElse(false.B)
  val s1_slaveValid = RegNext(s0_slaveValid, false.B)
  val s2_slaveValid = RegNext(s1_slaveValid, false.B)
  val s3_slaveValid = RegNext(false.B)

  val s0_valid = io.req.fire()
  val s0_vaddr = io.req.bits.addr

  val s1_valid = Reg(init=Bool(false))
  val s1_vaddr = RegEnable(s0_vaddr, s0_valid)
  val s1_tag_hit = Wire(Vec(nWays, Bool()))
  val s1_hit = s1_tag_hit.reduce(_||_) || Mux(s1_slaveValid, true.B, addrMaybeInScratchpad(io.s1_paddr))
  dontTouch(s1_hit)
  val s2_valid = RegNext(s1_valid && !io.s1_kill, Bool(false))
  val s2_hit = RegNext(s1_hit)

  val invalidated = Reg(Bool())
  val refill_valid = RegInit(false.B)
  val send_hint = RegInit(false.B)
  val refill_fire = tl_out.a.fire() && !send_hint
  val hint_outstanding = RegInit(false.B)
  val s2_miss = s2_valid && !s2_hit && !io.s2_kill
  val s1_can_request_refill = !(s2_miss || refill_valid)
  val s2_request_refill = s2_miss && RegNext(s1_can_request_refill)
  val refill_paddr = RegEnable(io.s1_paddr, s1_valid && s1_can_request_refill)
  val refill_vaddr = RegEnable(s1_vaddr, s1_valid && s1_can_request_refill)
  val refill_tag = refill_paddr >> pgUntagBits
  val refill_idx = index(refill_vaddr, refill_paddr)
  val refill_one_beat = tl_out.d.fire() && edge_out.hasData(tl_out.d.bits)

  io.req.ready := !(refill_one_beat || s0_slaveValid || s3_slaveValid)
  s1_valid := s0_valid

  val (_, _, d_done, refill_cnt) = edge_out.count(tl_out.d)
  val refill_done = refill_one_beat && d_done
  tl_out.d.ready := !s3_slaveValid
  require (edge_out.manager.minLatency > 0)

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

  val (tag_array, omSRAM) = DescribedSRAM(
    name = "tag_array",
    desc = "ICache Tag Array",
    size = nSets,
    data = Vec(nWays, UInt(width = tECC.width(1 + tagBits)))
  )

  val tag_rdata = tag_array.read(s0_vaddr(untagBits-1,blockOffBits), !refill_done && s0_valid)
  val accruedRefillError = Reg(Bool())
  val refillError = tl_out.d.bits.corrupt || (refill_cnt > 0 && accruedRefillError)
  when (refill_done) {
    // For AccessAckData, denied => corrupt
    val enc_tag = tECC.encode(Cat(refillError, refill_tag))
    tag_array.write(refill_idx, Vec.fill(nWays)(enc_tag), Seq.tabulate(nWays)(repl_way === _))

    ccover(tl_out.d.bits.corrupt, "D_CORRUPT", "I$ D-channel corrupt")
  }
  io.errors.bus.valid := tl_out.d.fire() && (tl_out.d.bits.denied || tl_out.d.bits.corrupt)
  io.errors.bus.bits  := (refill_paddr >> blockOffBits) << blockOffBits

  val vb_array = Reg(init=Bits(0, nSets*nWays))
  when (refill_one_beat) {
    accruedRefillError := refillError
    // clear bit when refill starts so hit-under-miss doesn't fetch bad data
    vb_array := vb_array.bitSet(Cat(repl_way, refill_idx), refill_done && !invalidated)
  }
  val invalidate = Wire(init = io.invalidate)
  when (invalidate) {
    vb_array := Bits(0)
    invalidated := Bool(true)
  }

  val s1_tag_disparity = Wire(Vec(nWays, Bool()))
  val s1_tl_error = Wire(Vec(nWays, Bool()))
  val wordBits = outer.icacheParams.fetchBytes*8
  val s1_dout = Wire(Vec(nWays, UInt(width = dECC.width(wordBits))))

  val s0_slaveAddr = tl_in.map(_.a.bits.address).getOrElse(0.U)
  val s1s3_slaveAddr = Reg(UInt(width = log2Ceil(outer.size)))
  val s1s3_slaveData = Reg(UInt(width = wordBits))

  for (i <- 0 until nWays) {
    val s1_idx = index(s1_vaddr, io.s1_paddr)
    val s1_tag = io.s1_paddr >> pgUntagBits
    val scratchpadHit = scratchpadWayValid(i) &&
      Mux(s1_slaveValid,
        lineInScratchpad(scratchpadLine(s1s3_slaveAddr)) && scratchpadWay(s1s3_slaveAddr) === i,
        addrInScratchpad(io.s1_paddr) && scratchpadWay(io.s1_paddr) === i)
    val s1_vb = vb_array(Cat(UInt(i), s1_idx)) && !s1_slaveValid
    val enc_tag = tECC.decode(tag_rdata(i))
    val (tl_error, tag) = Split(enc_tag.uncorrected, tagBits)
    val tagMatch = s1_vb && tag === s1_tag
    s1_tag_disparity(i) := s1_vb && enc_tag.error
    s1_tl_error(i) := tagMatch && tl_error.asBool
    s1_tag_hit(i) := tagMatch || scratchpadHit
  }
  assert(!(s1_valid || s1_slaveValid) || PopCount(s1_tag_hit zip s1_tag_disparity map { case (h, d) => h && !d }) <= 1)

  require(tl_out.d.bits.data.getWidth % wordBits == 0)

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
    def wordMatch(addr: UInt) = addr.extract(log2Ceil(tl_out.d.bits.data.getWidth/8)-1, log2Ceil(wordBits/8)) === i
    def row(addr: UInt) = addr(untagBits-1, blockOffBits-log2Ceil(refillCycles))
    val s0_ren = (s0_valid && wordMatch(s0_vaddr)) || (s0_slaveValid && wordMatch(s0_slaveAddr))
    val wen = (refill_one_beat && !invalidated) || (s3_slaveValid && wordMatch(s1s3_slaveAddr))
    val mem_idx = Mux(refill_one_beat, (refill_idx << log2Ceil(refillCycles)) | refill_cnt,
                  Mux(s3_slaveValid, row(s1s3_slaveAddr),
                  Mux(s0_slaveValid, row(s0_slaveAddr),
                  row(s0_vaddr))))
    when (wen) {
      val data = Mux(s3_slaveValid, s1s3_slaveData, tl_out.d.bits.data(wordBits*(i+1)-1, wordBits*i))
      val way = Mux(s3_slaveValid, scratchpadWay(s1s3_slaveAddr), repl_way)
      data_array.write(mem_idx, Vec.fill(nWays)(dECC.encode(data)), (0 until nWays).map(way === _))
    }
    val dout = data_array.read(mem_idx, !wen && s0_ren)
    when (wordMatch(Mux(s1_slaveValid, s1s3_slaveAddr, io.s1_paddr))) {
      s1_dout := dout
    }
  }

  val s1_clk_en = s1_valid || s1_slaveValid
  val s2_tag_hit = RegEnable(s1_tag_hit, s1_clk_en)
  val s2_hit_way = OHToUInt(s2_tag_hit)
  val s2_scratchpad_word_addr = Cat(s2_hit_way, Mux(s2_slaveValid, s1s3_slaveAddr, io.s2_vaddr)(untagBits-1, log2Ceil(wordBits/8)), UInt(0, log2Ceil(wordBits/8)))
  val s2_dout = RegEnable(s1_dout, s1_clk_en)
  val s2_way_mux = Mux1H(s2_tag_hit, s2_dout)

  val s2_tag_disparity = RegEnable(s1_tag_disparity, s1_clk_en).asUInt.orR
  val s2_tl_error = RegEnable(s1_tl_error.asUInt.orR, s1_clk_en)
  val s2_data_decoded = dECC.decode(s2_way_mux)
  val s2_disparity = s2_tag_disparity || s2_data_decoded.error
  val s2_full_word_write = Wire(init = false.B)

  val s1_scratchpad_hit = Mux(s1_slaveValid, lineInScratchpad(scratchpadLine(s1s3_slaveAddr)), addrInScratchpad(io.s1_paddr))
  val s2_scratchpad_hit = RegEnable(s1_scratchpad_hit, s1_clk_en)
  val s2_report_uncorrectable_error = s2_scratchpad_hit && s2_data_decoded.uncorrectable && (s2_valid || (s2_slaveValid && !s2_full_word_write))
  val s2_error_addr = scratchpadBase.map(base => Mux(s2_scratchpad_hit, base + s2_scratchpad_word_addr, 0.U)).getOrElse(0.U)

  // output signals
  outer.icacheParams.latency match {
    case 1 =>
      require(tECC.isInstanceOf[IdentityCode])
      require(dECC.isInstanceOf[IdentityCode])
      require(outer.icacheParams.itimAddr.isEmpty)
      io.resp.bits.data := Mux1H(s1_tag_hit, s1_dout)
      io.resp.bits.ae := s1_tl_error.asUInt.orR
      io.resp.valid := s1_valid && s1_hit

    case 2 =>
      // when some sort of memory bit error have occurred
      when (s2_valid && s2_disparity) { invalidate := true }

      io.resp.bits.data := s2_data_decoded.uncorrected
      io.resp.bits.ae := s2_tl_error
      io.resp.bits.replay := s2_disparity
      io.resp.valid := s2_valid && s2_hit

      io.errors.correctable.foreach { c =>
        c.valid := (s2_valid || s2_slaveValid) && s2_disparity && !s2_report_uncorrectable_error
        c.bits := s2_error_addr
      }
      io.errors.uncorrectable.foreach { u =>
        u.valid := s2_report_uncorrectable_error
        u.bits := s2_error_addr
      }

      tl_in.map { tl =>
        val respValid = RegInit(false.B)
        tl.a.ready := !(tl_out.d.valid || s1_slaveValid || s2_slaveValid || s3_slaveValid || respValid || !io.clock_enabled)
        val s1_a = RegEnable(tl.a.bits, s0_slaveValid)
        s2_full_word_write := edge_in.get.hasData(s1_a) && s1_a.mask.andR
        when (s0_slaveValid) {
          val a = tl.a.bits
          s1s3_slaveAddr := tl.a.bits.address
          s1s3_slaveData := tl.a.bits.data
          when (edge_in.get.hasData(a)) {
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

        respValid := s2_slaveValid || (respValid && !tl.d.ready)
        val respError = RegEnable(s2_scratchpad_hit && s2_data_decoded.uncorrectable && !s2_full_word_write, s2_slaveValid)
        when (s2_slaveValid) {
          when (edge_in.get.hasData(s1_a) || s2_data_decoded.error) { s3_slaveValid := true }
          def byteEn(i: Int) = !(edge_in.get.hasData(s1_a) && s1_a.mask(i))
          s1s3_slaveData := (0 until wordBits/8).map(i => Mux(byteEn(i), s2_data_decoded.corrected, s1s3_slaveData)(8*(i+1)-1, 8*i)).asUInt
        }

        tl.d.valid := respValid
        tl.d.bits := Mux(edge_in.get.hasData(s1_a),
          edge_in.get.AccessAck(s1_a),
          edge_in.get.AccessAck(s1_a, UInt(0), denied = Bool(false), corrupt = respError))
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

  tl_out.a.valid := s2_request_refill
  tl_out.a.bits := edge_out.Get(
                    fromSource = UInt(0),
                    toAddress = (refill_paddr >> blockOffBits) << blockOffBits,
                    lgSize = lgCacheBlockBytes)._2

  if (cacheParams.prefetch) {
    val (crosses_page, next_block) = Split(refill_paddr(pgIdxBits-1, blockOffBits) +& 1, pgIdxBits-blockOffBits)
    when (tl_out.a.fire()) {
      send_hint := !hint_outstanding && io.s2_prefetch && !crosses_page
      when (send_hint) {
        send_hint := false
        hint_outstanding := true
      }
    }
    when (refill_done) {
      send_hint := false
    }
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
  tl_out.b.ready := Bool(true)
  tl_out.c.valid := Bool(false)
  tl_out.e.valid := Bool(false)
  assert(!(tl_out.a.valid && addrMaybeInScratchpad(tl_out.a.bits.address)))

  when (!refill_valid) { invalidated := false.B }
  when (refill_fire) { refill_valid := true.B }
  when (refill_done) { refill_valid := false.B}

  io.perf.acquire := refill_fire
  io.keep_clock_enabled :=
    tl_in.map(tl => tl.a.valid || tl.d.valid || s1_slaveValid || s2_slaveValid || s3_slaveValid).getOrElse(false.B) || // ITIM
    s1_valid || s2_valid || refill_valid || send_hint || hint_outstanding // I$

  def index(vaddr: UInt, paddr: UInt) = {
    val lsbs = paddr(pgUntagBits-1, blockOffBits)
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
