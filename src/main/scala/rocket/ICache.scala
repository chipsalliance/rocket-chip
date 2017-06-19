// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package rocket

import Chisel._
import config._
import diplomacy._
import tile._
import uncore.tilelink2._
import uncore.util.Code
import util._
import Chisel.ImplicitConversions._

case class ICacheParams(
    nSets: Int = 64,
    nWays: Int = 4,
    rowBits: Int = 128,
    nTLBEntries: Int = 32,
    cacheIdBits: Int = 0,
    ecc: Option[Code] = None,
    itimAddr: Option[BigInt] = None,
    blockBytes: Int = 64,
    latency: Int = 2,
    coreInstBytes: Int = 2,
    fetchWidth: Int = 2) extends L1CacheParams {
  def replacement = new RandomReplacement(nWays)
}

trait HasL1ICacheParameters extends HasL1CacheParameters with HasCoreParameters {
  val cacheParams = tileParams.icache.get
}

class ICacheReq(implicit p: Parameters) extends CoreBundle()(p) with HasL1ICacheParameters {
  val addr = UInt(width = vaddrBits)
}

class ICache(val icacheParams: ICacheParams, val hartid: Int)(implicit p: Parameters) extends LazyModule {
  lazy val module = new ICacheModule(this)
  val masterNode = TLClientNode(TLClientParameters(sourceId = IdRange(0,1)))

  val size = icacheParams.nSets * icacheParams.nWays * icacheParams.blockBytes
  val slaveNode = icacheParams.itimAddr.map { itimAddr =>
    val wordBytes = icacheParams.coreInstBytes * icacheParams.fetchWidth
    TLManagerNode(Seq(TLManagerPortParameters(
      Seq(TLManagerParameters(
        address         = Seq(AddressSet(itimAddr, size-1)),
        regionType      = RegionType.UNCACHED,
        executable      = true,
        supportsPutFull = TransferSizes(1, wordBytes),
        supportsPutPartial = TransferSizes(1, wordBytes),
        supportsGet     = TransferSizes(1, wordBytes),
        fifoId          = Some(0))), // requests handled in FIFO order
      beatBytes = wordBytes,
      minLatency = 1)))
  }
}

class ICacheBundle(outer: ICache) extends CoreBundle()(outer.p) {
  val hartid = UInt(INPUT, hartIdLen)
  val req = Decoupled(new ICacheReq).flip
  val s1_paddr = UInt(INPUT, paddrBits) // delayed one cycle w.r.t. req
  val s2_vaddr = UInt(INPUT, vaddrBits) // delayed two cycles w.r.t. req
  val s1_kill = Bool(INPUT) // delayed one cycle w.r.t. req
  val s2_kill = Bool(INPUT) // delayed two cycles; prevents I$ miss emission

  val resp = Valid(UInt(width = outer.icacheParams.coreInstBytes*8 * outer.icacheParams.fetchWidth))
  val invalidate = Bool(INPUT)
  val tl_out = outer.masterNode.bundleOut
  val tl_in = outer.slaveNode.map(_.bundleIn)
}

// get a tile-specific property without breaking deduplication
object GetPropertyByHartId {
  def apply[T <: Data](tiles: Seq[RocketTileParams], f: RocketTileParams => Option[T], hartId: UInt): T = {
    PriorityMux(tiles.zipWithIndex.collect { case (t, i) if f(t).nonEmpty => (hartId === i) -> f(t).get })
  }
}

class ICacheModule(outer: ICache) extends LazyModuleImp(outer)
    with HasL1ICacheParameters {
  override val cacheParams = outer.icacheParams // Use the local parameters

  val io = new ICacheBundle(outer)
  val edge_out = outer.masterNode.edgesOut.head
  val tl_out = io.tl_out.head
  val edge_in = outer.slaveNode.map(_.edgesIn.head)
  val tl_in = io.tl_in.map(_.head)

  require(isPow2(nSets) && isPow2(nWays))
  require(isPow2(outer.icacheParams.coreInstBytes))
  require(!usingVM || pgIdxBits >= untagBits)

  val scratchpadOn = RegInit(false.B)
  val scratchpadMax = tl_in.map(tl => Reg(UInt(width = log2Ceil(nSets * (nWays - 1)))))
  def lineInScratchpad(line: UInt) = scratchpadMax.map(scratchpadOn && line <= _).getOrElse(false.B)
  def addrMaybeInScratchpad(addr: UInt) = if (outer.icacheParams.itimAddr.isEmpty) false.B else {
    val base = GetPropertyByHartId(p(coreplex.RocketTilesKey), _.icache.flatMap(_.itimAddr.map(_.U)), io.hartid)
    addr >= base && addr < base + outer.size
  }
  def addrInScratchpad(addr: UInt) = addrMaybeInScratchpad(addr) && lineInScratchpad(addr(untagBits+log2Ceil(nWays)-1, blockOffBits))
  def scratchpadWay(addr: UInt) = addr.extract(untagBits+log2Ceil(nWays)-1, untagBits)
  def scratchpadWayValid(way: UInt) = way < nWays - 1
  def scratchpadLine(addr: UInt) = addr(untagBits+log2Ceil(nWays)-1, blockOffBits)
  val s0_slaveValid = tl_in.map(_.a.fire()).getOrElse(false.B)
  val s1_slaveValid = RegNext(s0_slaveValid, false.B)
  val s2_slaveValid = RegNext(s1_slaveValid, false.B)
  val s3_slaveValid = RegNext(false.B)

  val s_ready :: s_request :: s_refill :: Nil = Enum(UInt(), 3)
  val state = Reg(init=s_ready)
  val invalidated = Reg(Bool())

  val refill_addr = Reg(UInt(width = paddrBits))
  val refill_tag = refill_addr(tagBits+untagBits-1,untagBits)
  val refill_idx = refill_addr(untagBits-1,blockOffBits)
  val s1_tag_hit = Wire(Vec(nWays, Bool()))
  val s1_any_tag_hit = s1_tag_hit.reduce(_||_) || Mux(s1_slaveValid, true.B, addrMaybeInScratchpad(io.s1_paddr))

  val s1_valid = Reg(init=Bool(false))
  val s1_hit = s1_valid && s1_any_tag_hit
  val s1_miss = s1_valid && state === s_ready && !s1_any_tag_hit

  io.req.ready := !(tl_out.d.fire() || s0_slaveValid || s3_slaveValid)
  val s0_valid = io.req.fire()
  val s0_vaddr = io.req.bits.addr

  s1_valid := s0_valid

  when (s1_miss) { refill_addr := io.s1_paddr }
  val (_, _, refill_done, refill_cnt) = edge_out.count(tl_out.d)
  tl_out.d.ready := !s3_slaveValid
  require (edge_out.manager.minLatency > 0)

  val repl_way = if (isDM) UInt(0) else {
    // pick a way that is not used by the scratchpad
    val v0 = LFSR16(tl_out.a.fire())(log2Up(nWays)-1,0)
    var v = v0
    for (i <- log2Ceil(nWays) - 1 to 0 by -1) {
      val mask = nWays - (BigInt(1) << (i + 1))
      v = v | (lineInScratchpad(Cat(v0 | mask.U, refill_idx)) << i)
    }
    assert(!lineInScratchpad(Cat(v, refill_idx)))
    v
  }

  val entagbits = code.width(tagBits)
  val tag_array = SeqMem(nSets, Vec(nWays, Bits(width = entagbits)))
  val tag_rdata = tag_array.read(s0_vaddr(untagBits-1,blockOffBits), !refill_done && s0_valid)
  when (refill_done) {
    val tag = code.encode(refill_tag)
    tag_array.write(refill_idx, Vec.fill(nWays)(tag), Vec.tabulate(nWays)(repl_way === _))
  }

  val vb_array = Reg(init=Bits(0, nSets*nWays))
  when (tl_out.d.fire()) {
    // clear bit when refill starts so hit-under-miss doesn't fetch bad data
    vb_array := vb_array.bitSet(Cat(repl_way, refill_idx), refill_done && !invalidated)
  }
  val invalidate = Wire(init = io.invalidate)
  when (invalidate) {
    vb_array := Bits(0)
    invalidated := Bool(true)
  }

  val s1_tag_disparity = Wire(Vec(nWays, Bool()))
  val wordBits = outer.icacheParams.coreInstBytes*8 * outer.icacheParams.fetchWidth
  val s1_dout = Wire(Vec(nWays, UInt(width = code.width(wordBits))))

  val s0_slaveAddr = tl_in.map(_.a.bits.address).getOrElse(0.U)
  val s1s3_slaveAddr = Reg(UInt(width = log2Ceil(outer.size)))
  val s1s3_slaveData = Reg(UInt(width = wordBits))

  for (i <- 0 until nWays) {
    val s1_idx = io.s1_paddr(untagBits-1,blockOffBits)
    val s1_tag = io.s1_paddr(tagBits+untagBits-1,untagBits)
    val scratchpadHit = scratchpadWayValid(i) &&
      Mux(s1_slaveValid,
        lineInScratchpad(scratchpadLine(s1s3_slaveAddr)) && scratchpadWay(s1s3_slaveAddr) === i,
        addrInScratchpad(io.s1_paddr) && scratchpadWay(io.s1_paddr) === i)
    val s1_vb = vb_array(Cat(UInt(i), s1_idx)) && !s1_slaveValid
    s1_tag_disparity(i) := s1_vb && code.decode(tag_rdata(i)).error
    s1_tag_hit(i) := scratchpadHit || (s1_vb && code.decode(tag_rdata(i)).uncorrected === s1_tag)
  }
  assert(!(s1_valid || s1_slaveValid) || PopCount(s1_tag_hit zip s1_tag_disparity map { case (h, d) => h && !d }) <= 1)

  require(tl_out.d.bits.data.getWidth % wordBits == 0)
  val data_arrays = Seq.fill(tl_out.d.bits.data.getWidth / wordBits) { SeqMem(nSets * refillCycles, Vec(nWays, UInt(width = code.width(wordBits)))) }
  for ((data_array, i) <- data_arrays zipWithIndex) {
    def wordMatch(addr: UInt) = addr.extract(log2Ceil(tl_out.d.bits.data.getWidth/8)-1, log2Ceil(wordBits/8)) === i
    def row(addr: UInt) = addr(untagBits-1, blockOffBits-log2Ceil(refillCycles))
    val s0_ren = (s0_valid && wordMatch(s0_vaddr)) || (s0_slaveValid && wordMatch(s0_slaveAddr))
    val wen = (tl_out.d.fire() && !invalidated) || (s3_slaveValid && wordMatch(s1s3_slaveAddr))
    val mem_idx = Mux(tl_out.d.fire(), (refill_idx << log2Ceil(refillCycles)) | refill_cnt,
                  Mux(s3_slaveValid, row(s1s3_slaveAddr),
                  Mux(s0_slaveValid, row(s0_slaveAddr),
                  row(s0_vaddr))))
    when (wen) {
      val data = Mux(s3_slaveValid, s1s3_slaveData, tl_out.d.bits.data(wordBits*(i+1)-1, wordBits*i))
      val way = Mux(s3_slaveValid, scratchpadWay(s1s3_slaveAddr), repl_way)
      data_array.write(mem_idx, Vec.fill(nWays)(code.encode(data)), (0 until nWays).map(way === _))
    }
    val dout = data_array.read(mem_idx, !wen && s0_ren)
    when (wordMatch(Mux(s1_slaveValid, s1s3_slaveAddr, io.s1_paddr))) {
      s1_dout := dout
    }
  }

  // output signals
  outer.icacheParams.latency match {
    case 1 =>
      require(code.isInstanceOf[uncore.util.IdentityCode])
      require(outer.icacheParams.itimAddr.isEmpty)
      io.resp.bits := Mux1H(s1_tag_hit, s1_dout)
      io.resp.valid := s1_hit

    case 2 =>
      val s2_valid = RegNext(s1_valid && !io.s1_kill, Bool(false))
      val s2_hit = RegNext(s1_hit)
      val s2_tag_hit = RegEnable(s1_tag_hit, s1_valid || s1_slaveValid)
      val s2_dout = RegEnable(s1_dout, s1_valid || s1_slaveValid)
      val s2_way_mux = Mux1H(s2_tag_hit, s2_dout)

      val s2_tag_disparity = RegEnable(s1_tag_disparity, s1_valid || s1_slaveValid).asUInt.orR
      val s2_data_decoded = code.decode(s2_way_mux)
      val s2_disparity = s2_tag_disparity || s2_data_decoded.error
      when (s2_valid && s2_disparity) { invalidate := true }

      io.resp.bits := s2_data_decoded.uncorrected
      io.resp.valid := s2_valid && s2_hit && !s2_disparity

      tl_in.map { tl =>
        val respValid = RegInit(false.B)
        tl.a.ready := !(tl_out.d.valid || s1_slaveValid || s2_slaveValid || s3_slaveValid || respValid)
        val s1_a = RegEnable(tl.a.bits, s0_slaveValid)
        when (s0_slaveValid) {
          val a = tl.a.bits
          s1s3_slaveAddr := tl.a.bits.address
          s1s3_slaveData := tl.a.bits.data
          when (edge_in.get.hasData(a)) {
            val enable = scratchpadWayValid(scratchpadWay(a.address))
            when (!lineInScratchpad(scratchpadLine(a.address))) {
              scratchpadMax.get := scratchpadLine(a.address)
              when (enable) { invalidate := true }
            }
            scratchpadOn := enable
          }
        }

        assert(!s2_valid || RegNext(RegNext(s0_vaddr)) === io.s2_vaddr)
        when (!(tl.a.valid || s1_slaveValid || s2_slaveValid || respValid)
              && s2_valid && s2_data_decoded.correctable && !s2_tag_disparity) {
          // handle correctable errors on CPU accesses to the scratchpad.
          // if there is an in-flight slave-port access to the scratchpad,
          // report the a miss but don't correct the error (as there is
          // a structural hazard on s1s3_slaveData/s1s3_slaveAddress).
          s3_slaveValid := true
          s1s3_slaveData := s2_data_decoded.corrected
          s1s3_slaveAddr := Cat(OHToUInt(s2_tag_hit), io.s2_vaddr(untagBits-1, log2Ceil(wordBits/8)), s1s3_slaveAddr(log2Ceil(wordBits/8)-1, 0))
        }

        respValid := s2_slaveValid || (respValid && !tl.d.ready)
        when (s2_slaveValid) {
          when (edge_in.get.hasData(s1_a) || s2_data_decoded.correctable) { s3_slaveValid := true }
          def byteEn(i: Int) = !(edge_in.get.hasData(s1_a) && s1_a.mask(i))
          s1s3_slaveData := (0 until wordBits/8).map(i => Mux(byteEn(i), s2_data_decoded.corrected, s1s3_slaveData)(8*(i+1)-1, 8*i)).asUInt
        }

        tl.d.valid := respValid
        tl.d.bits := Mux(edge_in.get.hasData(s1_a),
          edge_in.get.AccessAck(s1_a, UInt(0)),
          edge_in.get.AccessAck(s1_a, UInt(0), UInt(0)))
        tl.d.bits.data := s1s3_slaveData

        // Tie off unused channels
        tl.b.valid := false
        tl.c.ready := true
        tl.e.ready := true
      }
  }
  tl_out.a.valid := state === s_request && !io.s2_kill
  tl_out.a.bits := edge_out.Get(
                    fromSource = UInt(0),
                    toAddress = (refill_addr >> blockOffBits) << blockOffBits,
                    lgSize = lgCacheBlockBytes)._2
  tl_out.b.ready := Bool(true)
  tl_out.c.valid := Bool(false)
  tl_out.e.valid := Bool(false)
  assert(!(tl_out.a.valid && addrMaybeInScratchpad(tl_out.a.bits.address)))

  // control state machine
  switch (state) {
    is (s_ready) {
      when (s1_miss && !io.s1_kill) { state := s_request }
      invalidated := Bool(false)
    }
    is (s_request) {
      when (tl_out.a.ready) { state := s_refill }
      when (io.s2_kill) { state := s_ready }
    }
  }
  when (refill_done) {
    assert(state === s_refill)
    state := s_ready
  }
}
