// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.
package freechips.rocketchip.prci

import chisel3._
import freechips.rocketchip.diplomacy._

object IOHelper {

  def forNonSynchronous[T <: Data](gen: => T, xs: Seq[ClockCrossingType], prefix: String): Seq[Option[ModuleValue[T]]] = {
   xs.zipWithIndex.map { case (x, i) => forNonSynchronous(gen, x, prefix + s"_$i") }
  }

  def forNonSynchronous[T <: Data](gen: => T, x: ClockCrossingType, name: String): Option[ModuleValue[T]] = {
    x match {
      case SynchronousCrossing(_) => None
      case _ => Some(InModuleBody(IO(Input(gen)).suggestName(name)))
    }
  }
}
