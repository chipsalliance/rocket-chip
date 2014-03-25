package rocket

import Chisel._
import Util._
import Node._
import uncore.constants.AddressConstants._

case class BTBConfig(entries: Int) {
  val matchBits = PGIDX_BITS
  val pages0 = 1 + log2Up(entries) // is this sensible? what about matchBits?
  val pages = (pages0+1)/2*2 // control logic assumes 2 divides pages
}

// fully-associative branch target buffer
class BTB(conf: BTBConfig) extends Module {
  val io = new Bundle {
    val current_pc     = UInt(INPUT, VADDR_BITS)
    val hit            = Bool(OUTPUT)
    val target         = UInt(OUTPUT, VADDR_BITS)
    val wen            = Bool(INPUT)
    val taken          = Bool(INPUT)
    val invalidate     = Bool(INPUT)
    val correct_pc     = UInt(INPUT, VADDR_BITS)
    val correct_target = UInt(INPUT, VADDR_BITS)
  }

  val idxValid = Vec.fill(conf.entries){Reg(init=Bool(false))}
  val idxs = Vec.fill(conf.entries){Reg(UInt(width=conf.matchBits))}
  val idxPages = Vec.fill(conf.entries){Reg(UInt(width=log2Up(conf.pages)))}
  val idxPagesOH = idxPages.map(UIntToOH(_)(conf.pages-1,0))
  val tgts = Vec.fill(conf.entries){Reg(UInt(width=conf.matchBits))}
  val tgtPages = Vec.fill(conf.entries){Reg(UInt(width=log2Up(conf.pages)))}
  val tgtPagesOH = tgtPages.map(UIntToOH(_)(conf.pages-1,0))
  val pages = Vec.fill(conf.pages){Reg(UInt(width=VADDR_BITS-conf.matchBits))}
  val pageValid = Vec.fill(conf.pages){Reg(init=Bool(false))}

  private def page(addr: UInt) = addr >> conf.matchBits
  private def pageMatch(addr: UInt) = {
    val p = page(addr)
    Vec(pages.map(_ === p)).toBits & pageValid.toBits
  }
  private def tagMatch(addr: UInt): UInt = tagMatch(addr, pageMatch(addr))
  private def tagMatch(addr: UInt, pgMatch: UInt): UInt = {
    val idx = addr(conf.matchBits-1,0)
    val idxMatch = idxs.map(_ === idx).toBits
    val idxPageMatch = idxPagesOH.map(_ & pgMatch).map(_.orR).toBits
    idxValid.toBits & idxMatch & idxPageMatch
  }

  val hits = tagMatch(io.current_pc)
  val idxPageMatch = pageMatch(io.correct_pc)
  val tgtPageMatch = pageMatch(io.correct_target)
  val updates = tagMatch(io.correct_pc, idxPageMatch)
  val anyUpdates = updates.orR

  private var lfsr = LFSR16(io.wen)
  def rand(width: Int) = {
    lfsr = lfsr(lfsr.getWidth-1,1)
    Random.oneHot(width, lfsr)
  }
  def randOrInvalid(valid: UInt) =
    Mux(!valid.andR, PriorityEncoderOH(~valid), rand(valid.getWidth))

  val idxRepl = randOrInvalid(idxValid.toBits)
  val idxWen = updates.toBits | idxRepl & ~anyUpdates.toSInt

  val useIdxPageMatch = idxPageMatch.orR
  val doIdxPageRepl = !useIdxPageMatch && io.taken
  val idxPageRepl = rand(conf.pages)
  val idxPageUpdate = Mux(useIdxPageMatch, idxPageMatch, idxPageRepl)
  val idxPageReplEn = Mux(doIdxPageRepl, idxPageRepl, UInt(0))

  val samePage = page(io.correct_pc) === page(io.correct_target)
  val useTgtPageMatch = (tgtPageMatch & ~idxPageReplEn).orR
  val doTgtPageRepl = !useTgtPageMatch && io.taken && !samePage
  val tgtPageRepl = Mux(samePage, idxPageUpdate, idxPageUpdate(conf.pages-2,0) << 1 | idxPageUpdate(conf.pages-1))
  val tgtPageUpdate = Mux(useTgtPageMatch, tgtPageMatch, tgtPageRepl)
  val tgtPageReplEn = Mux(doTgtPageRepl, tgtPageRepl, UInt(0))

  val pageReplEn = idxPageReplEn | tgtPageReplEn

  when (io.wen) {
    for (i <- 0 until conf.entries) {
      when (idxWen(i)) {
        idxValid(i) := io.taken
        when (io.taken) {
          idxs(i) := io.correct_pc
          idxPages(i) := OHToUInt(idxPageUpdate)
          tgts(i) := io.correct_target
          tgtPages(i) := OHToUInt(tgtPageUpdate)
        }
      }.elsewhen ((pageReplEn & (idxPagesOH(i) | tgtPagesOH(i))).orR) {
        idxValid(i) := false
      }
    }

    require(conf.pages % 2 == 0)
    val idxWritesEven = (idxPageUpdate & Fill(conf.pages/2, UInt(1,2))).orR

    def writeBank(i: Int, mod: Int, en: Bool, data: UInt) = {
      for (i <- i until conf.pages by mod) {
        when (en && pageReplEn(i)) {
          pages(i) := data
          pageValid(i) := true
        }
      }
    }
    writeBank(0, 2, Mux(idxWritesEven, doIdxPageRepl, doTgtPageRepl),
      Mux(idxWritesEven, page(io.correct_pc), page(io.correct_target)))
    writeBank(1, 2, Mux(idxWritesEven, doTgtPageRepl, doIdxPageRepl),
      Mux(idxWritesEven, page(io.correct_target), page(io.correct_pc)))
  }

  when (io.invalidate) {
    idxValid.foreach(_ := false)
    pageValid.foreach(_ := false)
  }

  io.hit    := hits.toBits.orR
  io.target := Cat(Mux1H(Mux1H(hits, tgtPagesOH), pages), Mux1H(hits, tgts))
}
