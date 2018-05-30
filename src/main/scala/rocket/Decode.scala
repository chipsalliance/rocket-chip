// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._
import scala.collection.mutable.{ArrayBuffer, Map}

object DecodeLogic
{
  def term(lit: BitPat) =
    new Term(lit.value, BigInt(2).pow(lit.getWidth)-(lit.mask+1))
  def logic(addr: UInt, addrWidth: Int, cache: Map[Term,Bool], terms: Seq[Term]) = {
    terms.map { t =>
      cache.getOrElseUpdate(t, (if (t.mask == 0) addr else addr & Bits(BigInt(2).pow(addrWidth)-(t.mask+1), addrWidth)) === Bits(t.value, addrWidth))
    }.foldLeft(Bool(false))(_||_)
  }
	def apply(addr: UInt, default: BitPat, mapping: Iterable[(BitPat, BitPat)]): UInt = {
    val cache = caches.getOrElseUpdate(addr, Map[Term,Bool]())
    val dterm = term(default)
    val (keys, values) = mapping.unzip
    val addrWidth = keys.map(_.getWidth).max
    val terms = keys.toList.map(k => term(k))
    val termvalues = terms zip values.toList.map(term(_))

    for (t <- keys.zip(terms).tails; if !t.isEmpty)
      for (u <- t.tail)
        assert(!t.head._2.intersects(u._2), "DecodeLogic: keys " + t.head + " and " + u + " overlap")

    Cat((0 until default.getWidth.max(values.map(_.getWidth).max)).map({ case (i: Int) =>
      val mint = termvalues.filter { case (k,t) => ((t.mask >> i) & 1) == 0 && ((t.value >> i) & 1) == 1 }.map(_._1)
      val maxt = termvalues.filter { case (k,t) => ((t.mask >> i) & 1) == 0 && ((t.value >> i) & 1) == 0 }.map(_._1)
      val dc = termvalues.filter { case (k,t) => ((t.mask >> i) & 1) == 1 }.map(_._1)

      if (((dterm.mask >> i) & 1) != 0) {
        logic(addr, addrWidth, cache, SimplifyDC(mint, maxt, addrWidth))
      } else {
        val defbit = (dterm.value.toInt >> i) & 1
        val t = if (defbit == 0) mint else maxt
        val bit = logic(addr, addrWidth, cache, Simplify(t, dc, addrWidth))
        if (defbit == 0) bit else ~bit
      }
    }).reverse)
  }
  def apply(addr: UInt, default: Seq[BitPat], mappingIn: Iterable[(BitPat, Seq[BitPat])]): Seq[UInt] = {
    val mapping = ArrayBuffer.fill(default.size)(ArrayBuffer[(BitPat, BitPat)]())
    for ((key, values) <- mappingIn)
      for ((value, i) <- values zipWithIndex)
        mapping(i) += key -> value
    for ((thisDefault, thisMapping) <- default zip mapping)
      yield apply(addr, thisDefault, thisMapping)
  }
  def apply(addr: UInt, default: Seq[BitPat], mappingIn: List[(UInt, Seq[BitPat])]): Seq[UInt] =
    apply(addr, default, mappingIn.map(m => (BitPat(m._1), m._2)).asInstanceOf[Iterable[(BitPat, Seq[BitPat])]])
  def apply(addr: UInt, trues: Iterable[UInt], falses: Iterable[UInt]): Bool =
    apply(addr, BitPat.dontCare(1), trues.map(BitPat(_) -> BitPat("b1")) ++ falses.map(BitPat(_) -> BitPat("b0"))).toBool
  private val caches = Map[UInt,Map[Term,Bool]]()
}

class Term(val value: BigInt, val mask: BigInt = 0)
{
  var prime = true

  def covers(x: Term) = ((value ^ x.value) &~ mask | x.mask &~ mask) == 0
  def intersects(x: Term) = ((value ^ x.value) &~ mask &~ x.mask) == 0
  override def equals(that: Any) = that match {
    case x: Term => x.value == value && x.mask == mask
    case _ => false
  }
  override def hashCode = value.toInt
  def < (that: Term) = value < that.value || value == that.value && mask < that.mask
  def similar(x: Term) = {
    val diff = value - x.value
    mask == x.mask && value > x.value && (diff & diff-1) == 0
  }
  def merge(x: Term) = {
    prime = false
    x.prime = false
    val bit = value - x.value
    new Term(value &~ bit, mask | bit)
  }

  override def toString = value.toString(16) + "-" + mask.toString(16) + (if (prime) "p" else "")
}

object Simplify
{
  def getPrimeImplicants(implicants: Seq[Term], bits: Int) = {
    var prime = List[Term]()
    implicants.foreach(_.prime = true)
    val cols = (0 to bits).map(b => implicants.filter(b == _.mask.bitCount))
    val table = cols.map(c => (0 to bits).map(b => collection.mutable.Set(c.filter(b == _.value.bitCount):_*)))
    for (i <- 0 to bits) {
      for (j <- 0 until bits-i)
        table(i)(j).foreach(a => table(i+1)(j) ++= table(i)(j+1).filter(_.similar(a)).map(_.merge(a)))
      for (r <- table(i))
        for (p <- r; if p.prime)
          prime = p :: prime
    }
    prime.sortWith(_<_)
  }
  def getEssentialPrimeImplicants(prime: Seq[Term], minterms: Seq[Term]): (Seq[Term],Seq[Term],Seq[Term]) = {
    val primeCovers = prime.map(p => minterms.filter(p covers _))
    for (((icover, pi), i) <- (primeCovers zip prime).zipWithIndex) {
      for (((jcover, pj), j) <- (primeCovers zip prime).zipWithIndex.drop(i+1)) {
        if (icover.size > jcover.size && jcover.forall(pi covers _))
          return getEssentialPrimeImplicants(prime.filter(_ != pj), minterms)
      }
    }

    val essentiallyCovered = minterms.filter(t => prime.count(_ covers t) == 1)
    val essential = prime.filter(p => essentiallyCovered.exists(p covers _))
    val nonessential = prime.filterNot(essential contains _)
    val uncovered = minterms.filterNot(t => essential.exists(_ covers t))
    if (essential.isEmpty || uncovered.isEmpty)
      (essential, nonessential, uncovered)
    else {
      val (a, b, c) = getEssentialPrimeImplicants(nonessential, uncovered)
      (essential ++ a, b, c)
    }
  }
  def getCost(cover: Seq[Term], bits: Int) = cover.map(bits - _.mask.bitCount).sum
  def cheaper(a: List[Term], b: List[Term], bits: Int) = {
    val ca = getCost(a, bits)
    val cb = getCost(b, bits)
    def listLess(a: List[Term], b: List[Term]): Boolean = !b.isEmpty && (a.isEmpty || a.head < b.head || a.head == b.head && listLess(a.tail, b.tail))
    ca < cb || ca == cb && listLess(a.sortWith(_<_), b.sortWith(_<_))
  }
  def getCover(implicants: Seq[Term], minterms: Seq[Term], bits: Int) = {
    if (minterms.nonEmpty) {
      val cover = minterms.map(m => implicants.filter(_.covers(m)))
      val all = cover.tail.foldLeft(cover.head.map(Set(_)))((c0, c1) => c0.flatMap(a => c1.map(a + _)))
      all.map(_.toList).reduceLeft((a, b) => if (cheaper(a, b, bits)) a else b)
    } else
      Seq[Term]()
  }
  def stringify(s: Seq[Term], bits: Int) = s.map(t => (0 until bits).map(i => if ((t.mask & (1 << i)) != 0) "x" else ((t.value >> i) & 1).toString).reduceLeft(_+_).reverse).reduceLeft(_+" + "+_)

  def apply(minterms: Seq[Term], dontcares: Seq[Term], bits: Int) = {
    val prime = getPrimeImplicants(minterms ++ dontcares, bits)
    minterms.foreach(t => assert(prime.exists(_.covers(t))))
    val (eprime, prime2, uncovered) = getEssentialPrimeImplicants(prime, minterms)
    val cover = eprime ++ getCover(prime2, uncovered, bits)
    minterms.foreach(t => assert(cover.exists(_.covers(t)))) // sanity check
    cover
  }
}

object SimplifyDC
{
  def getImplicitDC(maxterms: Seq[Term], term: Term, bits: Int, above: Boolean): Term = {
    for (i <- 0 until bits) {
      var t: Term = null
      if (above && ((term.value | term.mask) & (BigInt(1) << i)) == 0)
        t = new Term(term.value | (BigInt(1) << i), term.mask)
      else if (!above && (term.value & (BigInt(1) << i)) != 0)
        t = new Term(term.value & ~(BigInt(1) << i), term.mask)
      if (t != null && !maxterms.exists(_.intersects(t)))
        return t
    }
    null
  }
  def getPrimeImplicants(minterms: Seq[Term], maxterms: Seq[Term], bits: Int) = {
    var prime = List[Term]()
    minterms.foreach(_.prime = true)
    var mint = minterms.map(t => new Term(t.value, t.mask))
    val cols = (0 to bits).map(b => mint.filter(b == _.mask.bitCount))
    val table = cols.map(c => (0 to bits).map(b => collection.mutable.Set(c.filter(b == _.value.bitCount):_*)))

    for (i <- 0 to bits) {
      for (j <- 0 until bits-i) {
        table(i)(j).foreach(a => table(i+1)(j) ++= table(i)(j+1).filter(_ similar a).map(_ merge a))
      }
      for (j <- 0 until bits-i) {
        for (a <- table(i)(j).filter(_.prime)) {
          val dc = getImplicitDC(maxterms, a, bits, true)
          if (dc != null)
            table(i+1)(j) += dc merge a
        }
        for (a <- table(i)(j+1).filter(_.prime)) {
          val dc = getImplicitDC(maxterms, a, bits, false)
          if (dc != null)
            table(i+1)(j) += a merge dc
        }
      }
      for (r <- table(i))
        for (p <- r; if p.prime)
          prime = p :: prime
    }
    prime.sortWith(_<_)
  }

  def verify(cover: Seq[Term], minterms: Seq[Term], maxterms: Seq[Term]) = {
    assert(minterms.forall(t => cover.exists(_ covers t)))
    assert(maxterms.forall(t => !cover.exists(_ intersects t)))
  }
  def apply(minterms: Seq[Term], maxterms: Seq[Term], bits: Int) = {
    val prime = getPrimeImplicants(minterms, maxterms, bits)
    val (eprime, prime2, uncovered) = Simplify.getEssentialPrimeImplicants(prime, minterms)
    val cover = eprime ++ Simplify.getCover(prime2, uncovered, bits)
    verify(cover, minterms, maxterms)
    cover
  }
}
