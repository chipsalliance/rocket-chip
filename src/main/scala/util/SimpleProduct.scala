// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

// To mix this trait, you must supply:
//   override def canEqual(that: Any): Boolean = that.isInstanceOf[YourClass]
//   override def productPrefix: String = "YourClass"
//   def productArity: Int = # of fields
//   def productElement(n: Int): Any = field accessors
// In exchange you get:
//   def equals(that: Any): Boolean = same type and same fields
//   def hashCode: Int = hash of all fields (and productPrefix)
//   def toString: String = formats as "productPrefix(field1, field2, ...)"
trait SimpleProduct extends Product with Equals {
  override def equals(other: Any): Boolean = other match {
    case that: SimpleProduct =>
      def canEq = that.canEqual(this) && this.canEqual(that)
      def iter = that.productIterator zip this.productIterator
      canEq && iter.forall { case (a, b) => a == b }
    case _ => false
  }

  override def hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

  override def toString: String = {
    val b = new StringBuilder(productPrefix)
    val iter = productIterator
    b += '('
    if (iter.hasNext) {
      b ++= iter.next().toString
      while (iter.hasNext) {
        b ++= ", "
        b ++= iter.next().toString
      }
    }
    b += ')'
    b.toString
  }
}
