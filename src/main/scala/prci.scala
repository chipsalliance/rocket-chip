// See LICENSE for license details.

package uncore

import Chisel._
import Chisel.ImplicitConversions._
import junctions._
import junctions.NastiConstants._
import cde.{Parameters, Field}

/** Number of tiles */
case object NTiles extends Field[Int]

class PRCICoreIO(implicit p: Parameters) extends Bundle {
  val reset = Bool(OUTPUT)
  val id = UInt(OUTPUT, log2Up(p(NTiles)))
  val interrupts = new Bundle {
    val mtip = Bool()
    val msip = Bool()
    val meip = Bool()
    val seip = Bool()
  }.asOutput

  override def cloneType: this.type = new PRCICoreIO().asInstanceOf[this.type]
}
