package rocket

import Chisel._
import Util._
import Node._

case class BTBConfig(as: uncore.AddressSpaceConfiguration, entries: Int, nras: Int = 0) {
  val matchBits = as.pgIdxBits
  val pages0 = 1 max log2Up(entries) // is this sensible?
  val pages = (pages0+1)/2*2 // control logic assumes 2 divides pages
  val opaqueBits = log2Up(entries)
  val nbht = 1 << log2Up(entries * 2)
}

class RAS(implicit conf: BTBConfig) {
  def push(addr: UInt): Unit = {
    when (count < conf.nras) { count := count + 1 }
    val nextPos = Mux(Bool(isPow2(conf.nras)) || pos > 0, pos+1, UInt(0))
    stack(nextPos) := addr
    pos := nextPos
  }
  def peek: UInt = stack(pos)
  def pop: Unit = when (!isEmpty) {
    count := count - 1
    pos := Mux(Bool(isPow2(conf.nras)) || pos > 0, pos-1, UInt(conf.nras-1))
  }
  def clear: Unit = count := UInt(0)
  def isEmpty: Bool = count === UInt(0)

  private val count = Reg(init=UInt(0,log2Up(conf.nras+1)))
  private val pos = Reg(init=UInt(0,log2Up(conf.nras)))
  private val stack = Vec.fill(conf.nras){Reg(UInt())}
}

class BHTResp(implicit conf: BTBConfig) extends Bundle {
  val index = UInt(width = log2Up(conf.nbht).max(1))
  val value = UInt(width = 2)
}

class BHT(implicit conf: BTBConfig) {
  def get(addr: UInt): BHTResp = {
    val res = new BHTResp
    res.index := addr(log2Up(conf.nbht)+1,2) ^ history
    res.value := table(res.index)
    res
  }
  def update(d: BHTResp, taken: Bool): Unit = {
    table(d.index) := Cat(taken, (d.value(1) & d.value(0)) | ((d.value(1) | d.value(0)) & taken))
    history := Cat(taken, history(log2Up(conf.nbht)-1,1))
  }

  private val table = Mem(UInt(width = 2), conf.nbht)
  val history = Reg(UInt(width = log2Up(conf.nbht)))
}

class BTBUpdate(implicit conf: BTBConfig) extends Bundle {
  val prediction = Valid(new BTBResp)
  val pc = UInt(width = conf.as.vaddrBits)
  val target = UInt(width = conf.as.vaddrBits)
  val returnAddr = UInt(width = conf.as.vaddrBits)
  val taken = Bool()
  val isJump = Bool()
  val isCall = Bool()
  val isReturn = Bool()
  val incorrectTarget = Bool()

  override def clone = new BTBUpdate().asInstanceOf[this.type]
}

class BTBResp(implicit conf: BTBConfig) extends Bundle {
  val taken = Bool()
  val target = UInt(width = conf.as.vaddrBits)
  val entry = UInt(width = conf.opaqueBits)
  val bht = new BHTResp

  override def clone = new BTBResp().asInstanceOf[this.type]
}

// fully-associative branch target buffer
class BTB(implicit conf: BTBConfig) extends Module {
  val io = new Bundle {
    val req = UInt(INPUT, conf.as.vaddrBits)
    val resp = Valid(new BTBResp)
    val update = Valid(new BTBUpdate).flip
    val invalidate = Bool(INPUT)
  }

  val idxValid = Vec.fill(conf.entries){Reg(init=Bool(false))}
  val idxs = Vec.fill(conf.entries){Reg(UInt(width=conf.matchBits))}
  val idxPages = Vec.fill(conf.entries){Reg(UInt(width=log2Up(conf.pages)))}
  val tgts = Vec.fill(conf.entries){Reg(UInt(width=conf.matchBits))}
  val tgtPages = Vec.fill(conf.entries){Reg(UInt(width=log2Up(conf.pages)))}
  val pages = Vec.fill(conf.pages){Reg(UInt(width=conf.as.vaddrBits-conf.matchBits))}
  val pageValid = Vec.fill(conf.pages){Reg(init=Bool(false))}
  val idxPagesOH = idxPages.map(UIntToOH(_)(conf.pages-1,0))
  val tgtPagesOH = tgtPages.map(UIntToOH(_)(conf.pages-1,0))

  val useRAS = Vec.fill(conf.entries){Reg(Bool())}
  val isJump = Vec.fill(conf.entries){Reg(Bool())}

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

  private var lfsr = LFSR16(update.valid)
  def rand(width: Int) = {
    lfsr = lfsr(lfsr.getWidth-1,1)
    Random.oneHot(width, lfsr)
  }

  val updateHit = update.bits.prediction.valid
  val updateValid = update.bits.incorrectTarget || updateHit && Bool(conf.nbht > 0)
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
  val tgtPageRepl = Mux(samePage, idxPageUpdateOH, idxPageUpdateOH(conf.pages-2,0) << 1 | idxPageUpdateOH(conf.pages-1))
  val tgtPageUpdate = OHToUInt(Mux(usePageHit, pageHit, tgtPageRepl))
  val tgtPageReplEn = Mux(doTgtPageRepl, tgtPageRepl, UInt(0))

  val pageReplEn = idxPageReplEn | tgtPageReplEn
  idxPageRepl := UIntToOH(Counter(update.valid && (doIdxPageRepl || doTgtPageRepl), conf.pages)._1)

  when (update.valid && !(updateValid && !updateTarget)) {
    val nextRepl = Counter(!updateHit && updateValid, conf.entries)._1
    val waddr = Mux(updateHit, update.bits.prediction.bits.entry, nextRepl)

    for (i <- 0 until conf.entries) {
      when (waddr === i) {
        idxValid(i) := updateValid
        when (updateTarget) {
          if (i == 0) assert(io.req === update.bits.target, "BTB request != I$ target")
          idxs(i) := update.bits.pc
          idxPages(i) := idxPageUpdate
          tgts(i) := update_target
          tgtPages(i) := tgtPageUpdate
          useRAS(i) := update.bits.isReturn
          isJump(i) := update.bits.isJump
        }
      }.elsewhen ((pageReplEn & (idxPagesOH(i) | tgtPagesOH(i))).orR) {
        idxValid(i) := false
      }
    }

    require(conf.pages % 2 == 0)
    val idxWritesEven = (idxPageUpdateOH & Fill(conf.pages/2, UInt(1,2))).orR

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
  io.resp.bits.entry := OHToUInt(hits)

  if (conf.nbht > 0) {
    val bht = new BHT
    val res = bht.get(io.req)
    when (update.valid && updateHit && !update.bits.isJump) { bht.update(update.bits.prediction.bits.bht, update.bits.taken) }
    when (!res.value(0) && !Mux1H(hits, isJump)) { io.resp.bits.taken := false }
    io.resp.bits.bht := res
  }

  if (conf.nras > 0) {
    val ras = new RAS
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
