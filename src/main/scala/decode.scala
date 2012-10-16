package rocket

import Chisel._
import Node._

object DecodeLogic
{
  def term(b: Literal) = {
    if (b.isZ) {
      var (bits, mask, swidth) = Literal.parseLit(b.toString)
      new Term(BigInt(bits, 2), BigInt(2).pow(b.width)-(BigInt(mask, 2)+1))
    } else {
      new Term(b.value)
    }
  }
  def logic(addr: Bits, cache: scala.collection.mutable.Map[Term,Bits], terms: Seq[Term]) = {
    terms.map { t =>
      if (!cache.contains(t))
        cache += t -> ((if (t.mask == 0) addr else addr & Lit(BigInt(2).pow(addr.width)-(t.mask+1), addr.width){Bits()}) === Lit(t.value, addr.width){Bits()})
      cache(t)
    }.foldLeft(Bool(false))(_||_)
  }
  def apply(addr: Bits, default: Iterable[Bits], mapping: Iterable[(Bits, Iterable[Bits])]) = {
    var map = mapping
    var cache = scala.collection.mutable.Map[Term,Bits]()
    default map { d =>
      val dlit = d.litOf
      val dterm = term(dlit)
      val (keys, values) = map.unzip
      val keysterms = keys.toList.map(k => term(k.litOf)) zip values.toList.map(v => term(v.head.litOf))

      val result = (0 until math.max(dlit.width, values.map(_.head.litOf.width).max)).map({ case (i: Int) =>
        if (((dterm.mask >> i) & 1) != 0) {
          var mint = keysterms.filter { case (k,t) => ((t.mask >> i) & 1) == 0 && ((t.value >> i) & 1) == 1 }.map(_._1)
          var maxt = keysterms.filter { case (k,t) => ((t.mask >> i) & 1) == 0 && ((t.value >> i) & 1) == 0 }.map(_._1)
          logic(addr, cache, SimplifyDC(mint, maxt, addr.width)).toBits
        } else {
          val want = 1 - ((dterm.value.toInt >> i) & 1)
          val mint = keysterms.filter { case (k,t) => ((t.mask >> i) & 1) == 0 && ((t.value >> i) & 1) == want }.map(_._1)
          val dc = keysterms.filter { case (k,t) => ((t.mask >> i) & 1) == 1 }.map(_._1)
          val bit = logic(addr, cache, Simplify(mint, dc, addr.width)).toBits
          if (want == 1) bit else ~bit
        }
      }).reverse.reduceRight(Cat(_,_))
      map = map map { case (x,y) => (x, y.tail) }
      result
    }
  }
}

class Term(val value: BigInt, val mask: BigInt = 0)
{
  var prime = true

  def covers(x: Term) = ((value ^ x.value) &~ mask) == 0
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

  override def toString = value.toString + "-" + mask + (if (prime) "p" else "")
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
    prime.sort(_<_)
  }
  def getEssentialPrimeImplicants(prime: Seq[Term], minterms: Seq[Term]): (Seq[Term],Seq[Term],Seq[Term]) = {
    for (i <- 0 until prime.size) {
      val icover = minterms.filter(prime(i) covers _)
      for (j <- 0 until prime.size) {
        val jcover = minterms.filter(prime(j) covers _)
        if (icover.size > jcover.size && jcover.forall(prime(i) covers _))
          return getEssentialPrimeImplicants(prime.filter(_ != prime(j)), minterms)
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
    ca < cb || ca == cb && listLess(a.sort(_<_), b.sort(_<_))
  }
  def getCover(implicants: Seq[Term], minterms: Seq[Term], bits: Int) = {
    if (minterms.nonEmpty) {
      val cover = minterms.map(m => implicants.filter(_.covers(m)).map(i => collection.mutable.Set(i)))
      val all = cover.reduceLeft((c0, c1) => c0.map(a => c1.map(_ ++ a)).reduceLeft(_++_))
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
      if (above && ((term.value | term.mask) & (1L << i)) == 0)
        t = new Term(term.value | (1L << i), term.mask)
      else if (!above && (term.value & (1L << i)) != 0)
        t = new Term(term.value & ~(1L << i), term.mask)
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
    prime.sort(_<_)
  }

  def apply(minterms: Seq[Term], maxterms: Seq[Term], bits: Int) = {
    val prime = getPrimeImplicants(minterms, maxterms, bits)
    assert(minterms.forall(t => prime.exists(_ covers t)))
    val (eprime, prime2, uncovered) = Simplify.getEssentialPrimeImplicants(prime, minterms)
    assert(uncovered.forall(t => prime2.exists(_ covers t)))
    val cover = eprime ++ Simplify.getCover(prime2, uncovered, bits)
    minterms.foreach(t => assert(cover.exists(_.covers(t)))) // sanity check
    maxterms.foreach(t => assert(!cover.exists(_.intersects(t)))) // sanity check
    cover
  }
}
