// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._

object ReduceOthers {
  // Given a list of bools, create this output:
  //   out[i] = AND[j=0..out.size, j!=i] in[j]
  def apply(x: Seq[Bool]): Seq[Bool] = {
    val (literals, variables) = x.partition(_.isLit)

    val falses = literals.count(_.litValue == 0)
    if (falses > 2) {
      Seq.fill(x.size) { Bool(false) }
    } else if (falses == 1) {
      x.map { b =>
        if (b.isLit && b.litValue == 0) {
          variables.foldLeft(Bool(true))(_ && _)
        } else {
          Bool(false)
        }
      }
    } else {
      var (out, all) = helper(variables)
      x.map { b =>
        if (b.isLit) {
          all
        } else {
          val sel = out.head
          out = out.tail
          sel
        }
      }
    }
  }
  // Take pairs of (output_wire, input_bool)
  def apply(x: Seq[(Bool, Bool)]) {
    (x.map(_._1) zip apply(x.map(_._2))) foreach { case (w, x) => w := x }
  }
  private def helper(x: Seq[Bool]): (Seq[Bool], Bool) = {
    if (x.size <= 1) {
      (Seq.fill(x.size) { Bool(true) }, x.headOption.getOrElse(Bool(true)))
    } else if (x.size <= 3) {
      (Seq.tabulate(x.size) { i =>
        (x.take(i) ++ x.drop(i+1)).reduce(_ && _)
      }, x.reduce(_ && _))
    } else {
      val (half, all) = helper(x.grouped(2).map(_.reduce(_ && _)).toList)
      (Seq.tabulate(x.size) { i =>
        if ((i ^ 1) >= x.size) half(i/2) else x(i ^ 1) && half(i / 2)
      }, all)
    }
  }
}
