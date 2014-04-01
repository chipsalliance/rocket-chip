package rocket

import Chisel._
import Util._
import Node._
import uncore.constants.AddressConstants._

case class BTBConfig(entries: Int, nras: Int = 0, inOrder: Boolean = true) {
  val matchBits = PGIDX_BITS
  val pages0 = 1 + log2Up(entries) // is this sensible? what about matchBits?
  val pages = (pages0+1)/2*2 // control logic assumes 2 divides pages
  val opaqueBits = log2Up(entries)
}

class BTBUpdate(implicit conf: BTBConfig) extends Bundle {
  val prediction = Valid(new BTBResp)
  val pc = UInt(width = VADDR_BITS)
  val target = UInt(width = VADDR_BITS)
  val returnAddr = UInt(width = VADDR_BITS)
  val taken = Bool()
  val isCall = Bool()
  val isReturn = Bool()
  val incorrectTarget = Bool()

  override def clone = new BTBUpdate().asInstanceOf[this.type]
}

class BTBResp(implicit conf: BTBConfig) extends Bundle {
  val taken = Bool()
  val target = UInt(width = VADDR_BITS)
  val opaque = UInt(width = conf.opaqueBits)

  override def clone = new BTBResp().asInstanceOf[this.type]
}

class RAS(implicit conf: BTBConfig) {
  def push(addr: UInt): Unit = {
    when (count < conf.nras-1) { count := count + 1 }
    stack(pos+1) := addr
    pos := pos+1
  }
  def pop: UInt = {
    count := count - 1
    pos := pos - 1
    stack(pos)
  }
  def clear: Unit = count := UInt(0)
  def isEmpty: Bool = count === UInt(0)

  require(isPow2(conf.nras))
  private val count = Reg(init=UInt(0,log2Up(conf.nras+1)))
  private val pos = Reg(init=UInt(0,log2Up(conf.nras)))
  private val stack = Vec.fill(conf.nras){Reg(UInt())}
}

// fully-associative branch target buffer
class BTB(implicit conf: BTBConfig) extends Module {
  val io = new Bundle {
    val req = UInt(INPUT, VADDR_BITS)
    val resp = Valid(new BTBResp)
    val update = Valid(new BTBUpdate).flip
    val invalidate = Bool(INPUT)
  }

  val idxValid = Vec.fill(conf.entries){Reg(init=Bool(false))}
  val idxs = Vec.fill(conf.entries){Reg(UInt(width=conf.matchBits))}
  val idxPages = Vec.fill(conf.entries){Reg(UInt(width=log2Up(conf.pages)))}
  val tgts = Vec.fill(conf.entries){Reg(UInt(width=conf.matchBits))}
  val tgtPages = Vec.fill(conf.entries){Reg(UInt(width=log2Up(conf.pages)))}
  val pages = Vec.fill(conf.pages){Reg(UInt(width=VADDR_BITS-conf.matchBits))}
  val pageValid = Vec.fill(conf.pages){Reg(init=Bool(false))}
  val idxPagesOH = idxPages.map(UIntToOH(_)(conf.pages-1,0))
  val tgtPagesOH = tgtPages.map(UIntToOH(_)(conf.pages-1,0))

  val useRAS = Vec.fill(conf.entries){Reg(Bool())}

  private def page(addr: UInt) = addr >> conf.matchBits
  private def pageMatch(addr: UInt) = {
    val p = page(addr)
    Vec(pages.map(_ === p)).toBits & pageValid.toBits
  }
  private def tagMatch(addr: UInt, pgMatch: UInt): UInt = {
    val idx = addr(conf.matchBits-1,0)
    val idxMatch = idxs.map(_ === idx).toBits
    val idxPageMatch = idxPagesOH.map(_ & pgMatch).map(_.orR).toBits
    idxValid.toBits & idxMatch & idxPageMatch
  }

  val update = Pipe(io.update)
  val update_target = io.req

  val pageHit = pageMatch(io.req)
  val hits = tagMatch(io.req, pageHit)
  val updatePageHit = pageMatch(update.bits.pc)
  val updateHits = tagMatch(update.bits.pc, updatePageHit)

  val taken = update.bits.incorrectTarget || update.bits.taken
  val predicted_taken = update.bits.prediction.valid && update.bits.prediction.bits.taken
  val correction = update.bits.incorrectTarget || update.bits.taken != predicted_taken

  private var lfsr = LFSR16(update.valid)
  def rand(width: Int) = {
    lfsr = lfsr(lfsr.getWidth-1,1)
    Random.oneHot(width, lfsr)
  }
  def randOrInvalid(valid: UInt) =
    Mux(!valid.andR, PriorityEncoderOH(~valid), rand(valid.getWidth))

  val idxRepl = randOrInvalid(idxValid.toBits)
  val idxWen =
    if (conf.inOrder) Mux(update.bits.prediction.valid, UIntToOH(update.bits.prediction.bits.opaque), idxRepl)
    else updateHits | Mux(updateHits.orR, UInt(0), idxRepl)

  val useUpdatePageHit = updatePageHit.orR
  val doIdxPageRepl = !useUpdatePageHit && update.valid
  val idxPageRepl = rand(conf.pages)
  val idxPageUpdate = Mux(useUpdatePageHit, updatePageHit, idxPageRepl)
  val idxPageReplEn = Mux(doIdxPageRepl, idxPageRepl, UInt(0))

  val samePage = page(update.bits.pc) === page(update_target)
  val usePageHit = (pageHit & ~idxPageReplEn).orR
  val doTgtPageRepl = !usePageHit && !samePage && update.valid
  val tgtPageRepl = Mux(samePage, idxPageUpdate, idxPageUpdate(conf.pages-2,0) << 1 | idxPageUpdate(conf.pages-1))
  val tgtPageUpdate = Mux(usePageHit, pageHit, tgtPageRepl)
  val tgtPageReplEn = Mux(doTgtPageRepl, tgtPageRepl, UInt(0))

  val pageReplEn = idxPageReplEn | tgtPageReplEn

  when (update.valid) {
    for (i <- 0 until conf.entries) {
      when (idxWen(i)) {
        idxValid(i) := taken
        when (correction) {
          idxs(i) := update.bits.pc
          idxPages(i) := OHToUInt(idxPageUpdate)
          tgts(i) := update_target
          tgtPages(i) := OHToUInt(tgtPageUpdate)
          useRAS(i) :=  update.bits.isReturn
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
      Mux(idxWritesEven, page(update.bits.pc), page(update_target)))
    writeBank(1, 2, Mux(idxWritesEven, doTgtPageRepl, doIdxPageRepl),
      Mux(idxWritesEven, page(update_target), page(update.bits.pc)))
  }

  when (io.invalidate) {
    idxValid.foreach(_ := false)
    pageValid.foreach(_ := false)
  }

  io.resp.valid := hits.toBits.orR
  io.resp.bits.taken := io.resp.valid
  io.resp.bits.target := Cat(Mux1H(Mux1H(hits, tgtPagesOH), pages), Mux1H(hits, tgts))
  io.resp.bits.opaque := OHToUInt(hits)

  if (conf.nras > 0) {
    val ras = new RAS
    when (!ras.isEmpty && Mux1H(hits, useRAS)) {
      io.resp.bits.target := ras.pop
    }
    when (io.update.valid && io.update.bits.isCall) {
      ras.push(io.update.bits.returnAddr)
    }
    when (io.invalidate) { ras.clear }
  }
}
