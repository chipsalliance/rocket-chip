// See LICENSE.Berkeley for license details.

package util
import Chisel._

object ReduceOthers {
  // Given a list of bools, create this output:
  //   out[i] = AND[j=0..out.size, j!=i] in[j]
  def apply(x: Seq[Bool]): Seq[Bool] = {
    if (x.size <= 1) {
      Seq.fill(x.size) { Bool(true) }
    } else if (x.size <= 3) {
      Seq.tabulate(x.size) { i =>
        (x.take(i) ++ x.drop(i+1)).reduce(_ && _)
      }
    } else {
      val half = ReduceOthers(x.grouped(2).map(_.reduce(_ && _)).toList)
      Seq.tabulate(x.size) { i =>
        if ((i ^ 1) >= x.size) half(i/2) else x(i ^ 1) && half(i / 2)
      }
    }
  }
  // Take pairs of (output_wire, input_bool)
  def apply(x: Seq[(Bool, Bool)]) {
    (x.map(_._1) zip apply(x.map(_._2))) foreach { case (w, x) => w := x }
  }
}
