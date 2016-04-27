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
  def pop: Unit = when (!isEmpty) {
    count := count - 1
    pos := Mux(Bool(isPow2(nras)) || pos > 0, pos-1, UInt(nras-1))
  }
  def clear: Unit = count := UInt(0)
  def isEmpty: Bool = count === UInt(0)

  private val count = Reg(init=UInt(0,log2Up(nras+1)))
  private val pos = Reg(init=UInt(0,log2Up(nras)))
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
class BHT(nbht: Int)(implicit p: Parameters) {
  val nbhtbits = log2Up(nbht)
  def get(addr: UInt, update: Bool): BHTResp = {
    val res = Wire(new BHTResp)
    val index = addr(nbhtbits+1,2) ^ history
    res.value := table(index)
    res.history := history
    val taken = res.value(0)
    when (update) { history := Cat(taken, history(nbhtbits-1,1)) }
    res
  }
  def update(addr: UInt, d: BHTResp, taken: Bool, mispredict: Bool): Unit = {
    val index = addr(nbhtbits+1,2) ^ d.history
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
    val invalidate = Bool(INPUT)
  }

  val idxValid = Reg(init=UInt(0, entries))
  val idxs = Mem(entries, UInt(width=matchBits))
  val idxPages = Mem(entries, UInt(width=log2Up(nPages)))
  val tgts = Mem(entries, UInt(width=matchBits))
  val tgtPages = Mem(entries, UInt(width=log2Up(nPages)))
  val pages = Mem(nPages, UInt(width=vaddrBits-matchBits))
  val pageValid = Reg(init=UInt(0, nPages))
  val idxPagesOH = idxPages.map(UIntToOH(_)(nPages-1,0))
  val tgtPagesOH = tgtPages.map(UIntToOH(_)(nPages-1,0))

  val useRAS = Reg(Vec(entries, Bool()))
  val isJump = Reg(Vec(entries, Bool()))
  val brIdx  = Mem(entries, UInt(width=log2Up(fetchWidth)))

  private def page(addr: UInt) = addr >> matchBits
  private def pageMatch(addr: UInt) = {
    val p = page(addr)
    Vec(pages.map(_ === p)).toBits & pageValid
  }
  private def tagMatch(addr: UInt, pgMatch: UInt) = {
    val idx = addr(matchBits-1,0)
    val idxMatch = idxs.map(_ === idx).toBits
    val idxPageMatch = idxPagesOH.map(_ & pgMatch).map(_.orR).toBits
    idxValid & idxMatch & idxPageMatch
  }

  val r_btb_update = Pipe(io.btb_update)
  val update_target = io.req.bits.addr

  val pageHit = pageMatch(io.req.bits.addr)
  val hits = tagMatch(io.req.bits.addr, pageHit)
  val updatePageHit = pageMatch(r_btb_update.bits.pc)
  val updateHits = tagMatch(r_btb_update.bits.pc, updatePageHit)

  val updateHit = r_btb_update.bits.prediction.valid
  val nextRepl = Counter(r_btb_update.valid && !updateHit, entries)._1

  val useUpdatePageHit = updatePageHit.orR
  val doIdxPageRepl = !useUpdatePageHit
  val idxPageRepl = Wire(UInt(width = nPages))
  val idxPageUpdateOH = Mux(useUpdatePageHit, updatePageHit, idxPageRepl)
  val idxPageUpdate = OHToUInt(idxPageUpdateOH)
  val idxPageReplEn = Mux(doIdxPageRepl, idxPageRepl, UInt(0))

  val samePage = page(r_btb_update.bits.pc) === page(update_target)
  val usePageHit = (pageHit & ~idxPageReplEn).orR
  val doTgtPageRepl = !samePage && !usePageHit
  val tgtPageRepl = Mux(samePage, idxPageUpdateOH, idxPageUpdateOH(nPages-2,0) << 1 | idxPageUpdateOH(nPages-1))
  val tgtPageUpdate = OHToUInt(Mux(usePageHit, pageHit, tgtPageRepl))
  val tgtPageReplEn = Mux(doTgtPageRepl, tgtPageRepl, UInt(0))
  val doPageRepl = doIdxPageRepl || doTgtPageRepl

  val pageReplEn = idxPageReplEn | tgtPageReplEn
  idxPageRepl := UIntToOH(Counter(r_btb_update.valid && doPageRepl, nPages)._1)

  when (r_btb_update.valid) {
    assert(io.req.bits.addr === r_btb_update.bits.target, "BTB request != I$ target")

    val waddr =
      if (updatesOutOfOrder) Mux(updateHits.orR, OHToUInt(updateHits), nextRepl)
      else Mux(updateHit, r_btb_update.bits.prediction.bits.entry, nextRepl)

    // invalidate entries if we stomp on pages they depend upon
    val invalidateMask = Vec.tabulate(entries)(i => (pageReplEn & (idxPagesOH(i) | tgtPagesOH(i))).orR).toBits
    val validateMask = UIntToOH(waddr)
    idxValid := (idxValid & ~invalidateMask) | validateMask

    idxs(waddr) := r_btb_update.bits.pc
    tgts(waddr) := update_target
    idxPages(waddr) := idxPageUpdate
    tgtPages(waddr) := tgtPageUpdate
    useRAS(waddr) := r_btb_update.bits.isReturn
    isJump(waddr) := r_btb_update.bits.isJump
    if (fetchWidth == 1) {
      brIdx(waddr) := UInt(0)
    } else {
      brIdx(waddr) := r_btb_update.bits.br_pc >> log2Up(coreInstBytes)
    }

    require(nPages % 2 == 0)
    val idxWritesEven = (idxPageUpdateOH & Fill(nPages/2, UInt(1,2))).orR

    def writeBank(i: Int, mod: Int, en: Bool, data: UInt) =
      for (i <- i until nPages by mod)
        when (en && pageReplEn(i)) { pages(i) := data }

    writeBank(0, 2, Mux(idxWritesEven, doIdxPageRepl, doTgtPageRepl),
      Mux(idxWritesEven, page(r_btb_update.bits.pc), page(update_target)))
    writeBank(1, 2, Mux(idxWritesEven, doTgtPageRepl, doIdxPageRepl),
      Mux(idxWritesEven, page(update_target), page(r_btb_update.bits.pc)))

    when (doPageRepl) { pageValid := pageValid | pageReplEn }
  }

  when (io.invalidate) {
    idxValid := 0
    pageValid := 0
  }

  io.resp.valid := hits.orR
  io.resp.bits.taken := io.resp.valid
  io.resp.bits.target := Cat(Mux1H(Mux1H(hits, tgtPagesOH), pages), Mux1H(hits, tgts))
  io.resp.bits.entry := OHToUInt(hits)
  io.resp.bits.bridx := brIdx(io.resp.bits.entry)
  if (fetchWidth == 1) {
    io.resp.bits.mask := UInt(1)
  } else {
    // note: btb_resp is clock gated, so the mask is only relevant for the io.resp.valid case
    io.resp.bits.mask := Mux(io.resp.bits.taken, Cat((UInt(1) << brIdx(io.resp.bits.entry))-1, UInt(1)).toSInt,
                                                 SInt(-1)).toUInt
  }

  if (nBHT > 0) {
    val bht = new BHT(nBHT)
    val isBranch = !Mux1H(hits, isJump)
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
    val doPeek = Mux1H(hits, useRAS)
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
        ras.pop
      }
    }
    when (io.invalidate) { ras.clear }
  }
}
