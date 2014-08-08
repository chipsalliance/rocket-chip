package rocket

import Chisel._
import Util._
import Node._
import uncore._

case object Entries extends Field[Int]
case object NRAS extends Field[Int] 
case object MatchBits extends Field[Int]
case object Pages0 extends Field[Int]
case object Pages extends Field[Int]
case object OpaqueBits extends Field[Int]
case object NBHT extends Field[Int]

class RAS(nras: Int) {
  def push(addr: UInt): Unit = {
    when (count < nras) { count := count + 1 }
    val nextPos = Mux(Bool(isPow2(nras)) || pos > 0, pos+1, UInt(0))
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
  private val stack = Vec.fill(nras){Reg(UInt())}
}

class BHTResp extends Bundle {
  val index = UInt(width = log2Up(params(NBHT)).max(1))
  val value = UInt(width = 2)
}

class BHT(nbht: Int) {
  val nbhtbits = log2Up(nbht) 
  def get(addr: UInt): BHTResp = {
    val res = new BHTResp
    res.index := addr(nbhtbits+1,2) ^ history
    res.value := table(res.index)
    res
  }
  def update(d: BHTResp, taken: Bool): Unit = {
    table(d.index) := Cat(taken, (d.value(1) & d.value(0)) | ((d.value(1) | d.value(0)) & taken))
    history := Cat(taken, history(nbhtbits-1,1))
  }

  private val table = Mem(UInt(width = 2), nbht)
  val history = Reg(UInt(width = nbhtbits))
}

class BTBUpdate extends Bundle {
  val prediction = Valid(new BTBResp)
  val pc = UInt(width = params(VAddrBits))
  val target = UInt(width = params(VAddrBits))
  val returnAddr = UInt(width = params(VAddrBits))
  val taken = Bool()
  val isJump = Bool()
  val isCall = Bool()
  val isReturn = Bool()
  val incorrectTarget = Bool()
}

class BTBResp extends Bundle {
  val taken = Bool()
  val target = UInt(width = params(VAddrBits))
  val entry = UInt(width = params(OpaqueBits))
  val bht = new BHTResp
}

// fully-associative branch target buffer
class BTB extends Module {
  val io = new Bundle {
    val req = UInt(INPUT, params(VAddrBits))
    val resp = Valid(new BTBResp)
    val update = Valid(new BTBUpdate).flip
    val invalidate = Bool(INPUT)
  }

  val idxValid = Reg(init=UInt(0, params(Entries)))
  val idxs = Mem(UInt(width=params(MatchBits)), params(Entries))
  val idxPages = Mem(UInt(width=log2Up(params(Pages))), params(Entries))
  val tgts = Mem(UInt(width=params(MatchBits)), params(Entries))
  val tgtPages = Mem(UInt(width=log2Up(params(Pages))), params(Entries))
  val pages = Mem(UInt(width=params(VAddrBits)-params(MatchBits)), params(Pages))
  val pageValid = Reg(init=UInt(0, params(Pages)))
  val idxPagesOH = idxPages.map(UIntToOH(_)(params(Pages)-1,0))
  val tgtPagesOH = tgtPages.map(UIntToOH(_)(params(Pages)-1,0))

  val useRAS = Reg(UInt(width = params(Entries)))
  val isJump = Reg(UInt(width = params(Entries)))

  private def page(addr: UInt) = addr >> params(MatchBits)
  private def pageMatch(addr: UInt) = {
    val p = page(addr)
    Vec(pages.map(_ === p)).toBits & pageValid
  }
  private def tagMatch(addr: UInt, pgMatch: UInt): UInt = {
    val idx = addr(params(MatchBits)-1,0)
    val idxMatch = idxs.map(_ === idx).toBits
    val idxPageMatch = idxPagesOH.map(_ & pgMatch).map(_.orR).toBits
    idxValid & idxMatch & idxPageMatch
  }

  val update = Pipe(io.update)
  val update_target = io.req

  val pageHit = pageMatch(io.req)
  val hits = tagMatch(io.req, pageHit)
  val updatePageHit = pageMatch(update.bits.pc)
  val updateHits = tagMatch(update.bits.pc, updatePageHit)

  private var lfsr = LFSR16(update.valid)
  def rand(width: Int) = {
    lfsr = lfsr(lfsr.getWidth-1,1)
    Random.oneHot(width, lfsr)
  }

  val updateHit = update.bits.prediction.valid
  val updateValid = update.bits.incorrectTarget || updateHit && Bool(params(NBHT) > 0)
  val updateTarget = updateValid && update.bits.incorrectTarget

  val useUpdatePageHit = updatePageHit.orR
  val doIdxPageRepl = updateTarget && !useUpdatePageHit
  val idxPageRepl = UInt()
  val idxPageUpdateOH = Mux(useUpdatePageHit, updatePageHit, idxPageRepl)
  val idxPageUpdate = OHToUInt(idxPageUpdateOH)
  val idxPageReplEn = Mux(doIdxPageRepl, idxPageRepl, UInt(0))

  val samePage = page(update.bits.pc) === page(update_target)
  val usePageHit = (pageHit & ~idxPageReplEn).orR
  val doTgtPageRepl = updateTarget && !samePage && !usePageHit
  val tgtPageRepl = Mux(samePage, idxPageUpdateOH, idxPageUpdateOH(params(Pages)-2,0) << 1 | idxPageUpdateOH(params(Pages)-1))
  val tgtPageUpdate = OHToUInt(Mux(usePageHit, pageHit, tgtPageRepl))
  val tgtPageReplEn = Mux(doTgtPageRepl, tgtPageRepl, UInt(0))
  val doPageRepl = doIdxPageRepl || doTgtPageRepl

  val pageReplEn = idxPageReplEn | tgtPageReplEn
  idxPageRepl := UIntToOH(Counter(update.valid && doPageRepl, params(Pages))._1)

  when (update.valid && !(updateValid && !updateTarget)) {
    val nextRepl = Counter(!updateHit && updateValid, params(Entries))._1
    val waddr = Mux(updateHit, update.bits.prediction.bits.entry, nextRepl)

    // invalidate entries if we stomp on pages they depend upon
    idxValid := idxValid & ~Vec.tabulate(params(Entries))(i => (pageReplEn & (idxPagesOH(i) | tgtPagesOH(i))).orR).toBits

    idxValid(waddr) := updateValid
    when (updateTarget) {
      assert(io.req === update.bits.target, "BTB request != I$ target")
      idxs(waddr) := update.bits.pc
      tgts(waddr) := update_target
      idxPages(waddr) := idxPageUpdate
      tgtPages(waddr) := tgtPageUpdate
      useRAS(waddr) := update.bits.isReturn
      isJump(waddr) := update.bits.isJump
    }

    require(params(Pages) % 2 == 0)
    val idxWritesEven = (idxPageUpdateOH & Fill(params(Pages)/2, UInt(1,2))).orR

    def writeBank(i: Int, mod: Int, en: Bool, data: UInt) =
      for (i <- i until params(Pages) by mod)
        when (en && pageReplEn(i)) { pages(i) := data }

    writeBank(0, 2, Mux(idxWritesEven, doIdxPageRepl, doTgtPageRepl),
      Mux(idxWritesEven, page(update.bits.pc), page(update_target)))
    writeBank(1, 2, Mux(idxWritesEven, doTgtPageRepl, doIdxPageRepl),
      Mux(idxWritesEven, page(update_target), page(update.bits.pc)))

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

  if (params(NBHT) > 0) {
    val bht = new BHT(params(NBHT))
    val res = bht.get(io.req)
    when (update.valid && updateHit && !update.bits.isJump) { bht.update(update.bits.prediction.bits.bht, update.bits.taken) }
    when (!res.value(0) && !Mux1H(hits, isJump)) { io.resp.bits.taken := false }
    io.resp.bits.bht := res
  }

  if (params(NRAS) > 0) {
    val ras = new RAS(params(NRAS))
    val doPeek = Mux1H(hits, useRAS)
    when (!ras.isEmpty && doPeek) {
      io.resp.bits.target := ras.peek
    }
    when (io.update.valid) {
      when (io.update.bits.isCall) {
        ras.push(io.update.bits.returnAddr)
        when (doPeek) {
          io.resp.bits.target := io.update.bits.returnAddr
        }
      }.elsewhen (io.update.bits.isReturn && io.update.bits.prediction.valid) {
        ras.pop
      }
    }
    when (io.invalidate) { ras.clear }
  }
}
