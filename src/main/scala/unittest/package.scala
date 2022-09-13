// See LICENSE.SiFive for license details.

package freechips.rocketchip

import chisel3._

package object unittest
{
  implicit class LazyUnitTestSeq(val seq: Seq[LazyUnitTest]) {
    def finished = seq.map(_.module.finished).foldLeft(true.B)(_ && _)
  }
}
