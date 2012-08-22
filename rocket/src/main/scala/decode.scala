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
  def logic(addr: Bits, cache: scala.collection.mutable.Map[Term,Bits], terms: Set[Term]) = {
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
          var mint = keysterms.filter { case (k,t) => ((t.mask >> i) & 1) == 0 && ((t.value >> i) & 1) == 1 }.map(_._1).toSet
          var maxt = keysterms.filter { case (k,t) => ((t.mask >> i) & 1) == 0 && ((t.value >> i) & 1) == 0 }.map(_._1).toSet
          logic(addr, cache, SimplifyDC(mint, maxt, addr.width)).toBits
        } else {
          val want = 1 - ((dterm.value.toInt >> i) & 1)
          val mint = keysterms.filter { case (k,t) => ((t.mask >> i) & 1) == 0 && ((t.value >> i) & 1) == want }.map(_._1).toSet
          val dc = keysterms.filter { case (k,t) => ((t.mask >> i) & 1) == 1 }.map(_._1).toSet
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
  def getPrimeImplicants(implicants: Set[Term], bits: Int) = {
    var prime = Set[Term]()
    implicants.foreach(_.prime = true)
    val cols = (0 to bits).map(b => implicants.filter(b == _.mask.bitCount))
    val table = cols.map(c => (0 to bits).map(b => collection.mutable.Set() ++ c.filter(b == _.value.bitCount)))
    for (i <- 0 to bits) {
      for (j <- 0 until bits-i)
        table(i)(j).foreach(a => table(i+1)(j) ++= table(i)(j+1).filter(_.similar(a)).map(_.merge(a)))
      prime ++= table(i).map(_.filter(_.prime)).reduceLeft(_++_)
    }
    prime
  }
  def getEssentialPrimeImplicants(prime: Set[Term], minterms: Set[Term]): Tuple3[Set[Term],Set[Term],Set[Term]] = {
    val useful1 = prime.toSeq
    for (i <- 0 until useful1.size) {
      val icover = minterms.filter(useful1(i) covers _)
      for (j <- 0 until useful1.size) {
        val jcover = minterms.filter(useful1(j) covers _)
        if (icover.size > jcover.size && jcover.forall(useful1(i) covers _))
          return getEssentialPrimeImplicants(prime - useful1(j), minterms)
      }
    }

    val essentiallyCovered = minterms.filter(t => prime.count(_ covers t) == 1)
    val essential = prime.filter(p => essentiallyCovered.exists(p covers _))
    val nonessential = prime -- essential
    val uncovered = minterms.filterNot(t => essential.exists(_ covers t))
    if (essential.isEmpty || uncovered.isEmpty)
      (essential, nonessential, uncovered)
    else {
      val (a, b, c) = getEssentialPrimeImplicants(nonessential, uncovered)
      (essential ++ a, b, c)
    }
  }
  def getCost(cover: Set[Term], bits: Int) = cover.map(bits - _.mask.bitCount).sum
  def getCover(implicants: Set[Term], minterms: Set[Term], bits: Int) = {
    var cover = minterms.map(m => implicants.filter(_.covers(m)).map(i => Set(i))).toList
    while (cover.size > 1)
      cover = cover(0).map(a => cover(1).map(_ ++ a)).reduceLeft(_++_) :: cover.tail.tail
    if (cover.isEmpty)
      Set[Term]()
    else
      cover(0).reduceLeft((a, b) => if (getCost(a, bits) < getCost(b, bits)) a else b)
  }
  def stringify(s: Set[Term], bits: Int) = s.map(t => (0 until bits).map(i => if ((t.mask & (1 << i)) != 0) "x" else ((t.value >> i) & 1).toString).reduceLeft(_+_).reverse).reduceLeft(_+" + "+_)

  def apply(minterms: Set[Term], dontcares: Set[Term], bits: Int) = {
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
  def getImplicitDC(maxterms: Set[Term], term: Term, bits: Int, above: Boolean): Term = {
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
  def getPrimeImplicants(minterms: Set[Term], maxterms: Set[Term], bits: Int) = {
    var prime = Set[Term]()
    minterms.foreach(_.prime = true)
    var mint = minterms.map(t => new Term(t.value, t.mask))
    val cols = (0 to bits).map(b => mint.filter(b == _.mask.bitCount))
    val table = cols.map(c => (0 to bits).map(b => collection.mutable.Set() ++ c.filter(b == _.value.bitCount)))

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
      prime ++= table(i).map(_.filter(_.prime)).reduceLeft(_++_)
    }
    prime
  }

  def apply(minterms: Set[Term], maxterms: Set[Term], bits: Int) = {
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
