package util

import Chisel._
import cde.Parameters

class ParameterizedBundle(implicit p: Parameters) extends Bundle {
  override def cloneType = {
    try {
      this.getClass.getConstructors.head.newInstance(p).asInstanceOf[this.type]
    } catch {
      case e: java.lang.IllegalArgumentException =>
        throwException("Unable to use ParamaterizedBundle.cloneType on " +
                       this.getClass + ", probably because " + this.getClass +
                       "() takes more than one argument.  Consider overriding " +
                       "cloneType() on " + this.getClass, e)
    }
  }
}

object DecoupledHelper {
  def apply(rvs: Bool*) = new DecoupledHelper(rvs)
}

class DecoupledHelper(val rvs: Seq[Bool]) {
  def fire(exclude: Bool, includes: Bool*) = {
    (rvs.filter(_ ne exclude) ++ includes).reduce(_ && _)
  }
}
