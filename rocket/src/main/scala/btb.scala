// See LICENSE for license details.

package rocket

import Chisel._
import junctions._
import cde.{Parameters, Field}
import Util._

case object BtbKey extends Field[BtbParameters]

case class BtbParameters(
  nEntries: Int = 62,
  nRAS: Int = 2,
  updatesOutOfOrder: Boolean = false)

abstract trait HasBtbParameters extends HasCoreParameters {
  val matchBits = pgIdxBits
  val entries = p(BtbKey).nEntries
  val nRAS = p(BtbKey).nRAS
  val updatesOutOfOrder = p(BtbKey).updatesOutOfOrder
  val nPages = ((1 max(log2Up(entries)))+1)/2*2 // control logic assumes 2 divides pages
  val opaqueBits = log2Up(entries)
  val nBHT = 1 << log2Up(entries*2)
}

abstract class BtbModule(implicit val p: Parameters) extends Module with HasBtbParameters
abstract class BtbBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasBtbParameters

class RAS(nras: Int) {
  def push(addr: UInt): Unit = {
    when (count < nras) { count := count + 1 }
    val nextPos = Mux(Bool(isPow2(nras)) || pos < nras-1, pos+1, UInt(0))
    stack(nextPos) := addr
    pos := nextPos
  }
  def peek: UInt = stack(pos)
  def pop(): Unit = when (!isEmpty) {
    count := count - 1
    pos := Mux(Bool(isPow2(nras)) || pos > 0, pos-1, UInt(nras-1))
  }
  def clear(): Unit = count := UInt(0)
  def isEmpty: Bool = count === UInt(0)

  private val count = Reg(UInt(width = log2Up(nras+1)))
  private val pos = Reg(UInt(width = log2Up(nras)))
  private val stack = Reg(Vec(nras, UInt()))
}

class BHTResp(implicit p: Parameters) extends BtbBundle()(p) {
  val history = UInt(width = log2Up(nBHT).max(1))
  val value = UInt(width = 2)
}

// BHT contains table of 2-bit counters and a global history register.
// The BHT only predicts and updates when there is a BTB hit.
// The global history:
//    - updated speculatively in fetch (if there's a BTB hit).
//    - on a mispredict, the history register is reset (again, only if BTB hit).
// The counter table:
//    - each counter corresponds with the address of the fetch packet ("fetch pc").
//    - updated when a branch resolves (and BTB was a hit for that branch).
//      The updating branch must provide its "fetch pc".
class BHT(nbht: Int)(implicit val p: Parameters) extends HasCoreParameters {
  val nbhtbits = log2Up(nbht)
  def get(addr: UInt, update: Bool): BHTResp = {
    val res = Wire(new BHTResp)
    val index = addr(nbhtbits+1, log2Up(coreInstBytes)) ^ history
    res.value := table(index)
    res.history := history
    val taken = res.value(0)
    when (update) { history := Cat(taken, history(nbhtbits-1,1)) }
    res
  }
  def update(addr: UInt, d: BHTResp, taken: Bool, mispredict: Bool): Unit = {
    val index = addr(nbhtbits+1, log2Up(coreInstBytes)) ^ d.history
    table(index) := Cat(taken, (d.value(1) & d.value(0)) | ((d.value(1) | d.value(0)) & taken))
    when (mispredict) { history := Cat(taken, d.history(nbhtbits-1,1)) }
  }

  private val table = Mem(nbht, UInt(width = 2))
  val history = Reg(UInt(width = nbhtbits))
}

// BTB update occurs during branch resolution (and only on a mispredict).
//  - "pc" is what future fetch PCs will tag match against.
//  - "br_pc" is the PC of the branch instruction.
class BTBUpdate(implicit p: Parameters) extends BtbBundle()(p) {
  val prediction = Valid(new BTBResp)
  val pc = UInt(width = vaddrBits)
  val target = UInt(width = vaddrBits)
  val taken = Bool()
  val isValid = Bool()
  val isJump = Bool()
  val isReturn = Bool()
  val br_pc = UInt(width = vaddrBits)
}

// BHT update occurs during branch resolution on all conditional branches.
//  - "pc" is what future fetch PCs will tag match against.
class BHTUpdate(implicit p: Parameters) extends BtbBundle()(p) {
  val prediction = Valid(new BTBResp)
  val pc = UInt(width = vaddrBits)
  val taken = Bool()
  val mispredict = Bool()
}

class RASUpdate(implicit p: Parameters) extends BtbBundle()(p) {
  val isCall = Bool()
  val isReturn = Bool()
  val returnAddr = UInt(width = vaddrBits)
  val prediction = Valid(new BTBResp)
}

//  - "bridx" is the low-order PC bits of the predicted branch (after
//     shifting off the lowest log(inst_bytes) bits off).
//  - "mask" provides a mask of valid instructions (instructions are
//     masked off by the predicted taken branch from the BTB).
class BTBResp(implicit p: Parameters) extends BtbBundle()(p) {
  val taken = Bool()
  val mask = Bits(width = fetchWidth)
  val bridx = Bits(width = log2Up(fetchWidth))
  val target = UInt(width = vaddrBits)
  val entry = UInt(width = opaqueBits)
  val bht = new BHTResp
}

class BTBReq(implicit p: Parameters) extends BtbBundle()(p) {
   val addr = UInt(width = vaddrBits)
}

// fully-associative branch target buffer
// Higher-performance processors may cause BTB updates to occur out-of-order,
// which requires an extra CAM port for updates (to ensure no duplicates get
// placed in BTB).
class BTB(implicit p: Parameters) extends BtbModule {
  val io = new Bundle {
    val req = Valid(new BTBReq).flip
    val resp = Valid(new BTBResp)
    val btb_update = Valid(new BTBUpdate).flip
    val bht_update = Valid(new BHTUpdate).flip
    val ras_update = Valid(new RASUpdate).flip
  }

  val idxs = Reg(Vec(entries, UInt(width=matchBits - log2Up(coreInstBytes))))
  val idxPages = Reg(Vec(entries, UInt(width=log2Up(nPages))))
  val tgts = Reg(Vec(entries, UInt(width=matchBits - log2Up(coreInstBytes))))
  val tgtPages = Reg(Vec(entries, UInt(width=log2Up(nPages))))
  val pages = Reg(Vec(nPages, UInt(width=vaddrBits - matchBits)))
  val pageValid = Reg(init = UInt(0, nPages))
  val idxPagesOH = idxPages.map(UIntToOH(_)(nPages-1,0))
  val tgtPagesOH = tgtPages.map(UIntToOH(_)(nPages-1,0))

  val isValid = Reg(init = UInt(0, entries))
  val isReturn = Reg(UInt(width = entries))
  val isJump = Reg(UInt(width = entries))
  val brIdx = Reg(Vec(entries, UInt(width=log2Up(fetchWidth))))

  private def page(addr: UInt) = addr >> matchBits
  private def pageMatch(addr: UInt) = {
    val p = page(addr)
    pageValid & pages.map(_ === p).toBits
  }
  private def tagMatch(addr: UInt, pgMatch: UInt) = {
    val idxMatch = idxs.map(_ === addr(matchBits-1, log2Up(coreInstBytes))).toBits
    val idxPageMatch = idxPagesOH.map(_ & pgMatch).map(_.orR).toBits
    idxMatch & idxPageMatch & isValid
  }

  val r_btb_update = Pipe(io.btb_update)
  val update_target = io.req.bits.addr

  val pageHit = pageMatch(io.req.bits.addr)
  val hitsVec = tagMatch(io.req.bits.addr, pageHit)
  val hits = hitsVec.toBits
  val updatePageHit = pageMatch(r_btb_update.bits.pc)

  val updateHits = tagMatch(r_btb_update.bits.pc, updatePageHit)
  val updateHit = if (updatesOutOfOrder) updateHits.orR else r_btb_update.bits.prediction.valid
  val updateHitAddr = if (updatesOutOfOrder) OHToUInt(updateHits) else r_btb_update.bits.prediction.bits.entry
  val nextRepl = Counter(r_btb_update.valid && !updateHit, entries)._1

  val useUpdatePageHit = updatePageHit.orR
  val usePageHit = pageHit.orR
  val doIdxPageRepl = !useUpdatePageHit
  val nextPageRepl = Reg(UInt(width = log2Ceil(nPages)))
  val idxPageRepl = Mux(usePageHit, Cat(pageHit(nPages-2,0), pageHit(nPages-1)), UIntToOH(nextPageRepl))
  val idxPageUpdateOH = Mux(useUpdatePageHit, updatePageHit, idxPageRepl)
  val idxPageUpdate = OHToUInt(idxPageUpdateOH)
  val idxPageReplEn = Mux(doIdxPageRepl, idxPageRepl, UInt(0))

  val samePage = page(r_btb_update.bits.pc) === page(update_target)
  val doTgtPageRepl = !samePage && !usePageHit
  val tgtPageRepl = Mux(samePage, idxPageUpdateOH, Cat(idxPageUpdateOH(nPages-2,0), idxPageUpdateOH(nPages-1)))
  val tgtPageUpdate = OHToUInt(Mux(usePageHit, pageHit, tgtPageRepl))
  val tgtPageReplEn = Mux(doTgtPageRepl, tgtPageRepl, UInt(0))

  when (r_btb_update.valid && (doIdxPageRepl || doTgtPageRepl)) {
    val both = doIdxPageRepl && doTgtPageRepl
    val next = nextPageRepl + Mux[UInt](both, 2, 1)
    nextPageRepl := Mux(next >= nPages, next(0), next)
  }

  when (r_btb_update.valid) {
    val waddr = Mux(updateHit, updateHitAddr, nextRepl)
    val mask = UIntToOH(waddr)
    idxs(waddr) := r_btb_update.bits.pc(matchBits-1, log2Up(coreInstBytes))
    tgts(waddr) := update_target(matchBits-1, log2Up(coreInstBytes))
    idxPages(waddr) := idxPageUpdate
    tgtPages(waddr) := tgtPageUpdate
    isValid := Mux(r_btb_update.bits.isValid, isValid | mask, isValid & ~mask)
    isReturn := Mux(r_btb_update.bits.isReturn, isReturn | mask, isReturn & ~mask)
    isJump := Mux(r_btb_update.bits.isJump, isJump | mask, isJump & ~mask)
    if (fetchWidth > 1)
      brIdx(waddr) := r_btb_update.bits.br_pc >> log2Up(coreInstBytes)

    require(nPages % 2 == 0)
    val idxWritesEven = !idxPageUpdate(0)

    def writeBank(i: Int, mod: Int, en: UInt, data: UInt) =
      for (i <- i until nPages by mod)
        when (en(i)) { pages(i) := data }

    writeBank(0, 2, Mux(idxWritesEven, idxPageReplEn, tgtPageReplEn),
      Mux(idxWritesEven, page(r_btb_update.bits.pc), page(update_target)))
    writeBank(1, 2, Mux(idxWritesEven, tgtPageReplEn, idxPageReplEn),
      Mux(idxWritesEven, page(update_target), page(r_btb_update.bits.pc)))
    pageValid := pageValid | tgtPageReplEn | idxPageReplEn
  }

  io.resp.valid := hits.orR
  io.resp.bits.taken := true
  io.resp.bits.target := Cat(Mux1H(Mux1H(hitsVec, tgtPagesOH), pages), Mux1H(hitsVec, tgts) << log2Up(coreInstBytes))
  io.resp.bits.entry := OHToUInt(hits)
  io.resp.bits.bridx := Mux1H(hitsVec, brIdx)
  io.resp.bits.mask := Cat((UInt(1) << ~Mux(io.resp.bits.taken, ~io.resp.bits.bridx, UInt(0)))-1, UInt(1))

  if (nBHT > 0) {
    val bht = new BHT(nBHT)
    val isBranch = !(hits & isJump).orR
    val res = bht.get(io.req.bits.addr, io.req.valid && io.resp.valid && isBranch)
    val update_btb_hit = io.bht_update.bits.prediction.valid
    when (io.bht_update.valid && update_btb_hit) {
      bht.update(io.bht_update.bits.pc, io.bht_update.bits.prediction.bits.bht, io.bht_update.bits.taken, io.bht_update.bits.mispredict)
    }
    when (!res.value(0) && isBranch) { io.resp.bits.taken := false }
    io.resp.bits.bht := res
  }

  if (nRAS > 0) {
    val ras = new RAS(nRAS)
    val doPeek = (hits & isReturn).orR
    when (!ras.isEmpty && doPeek) {
      io.resp.bits.target := ras.peek
    }
    when (io.ras_update.valid) {
      when (io.ras_update.bits.isCall) {
        ras.push(io.ras_update.bits.returnAddr)
        when (doPeek) {
          io.resp.bits.target := io.ras_update.bits.returnAddr
        }
      }.elsewhen (io.ras_update.bits.isReturn && io.ras_update.bits.prediction.valid) {
        ras.pop()
      }
    }
  }
}
