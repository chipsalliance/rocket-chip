// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._
import scala.collection.mutable.{HashMap}

case class ROMConfig(name: String, depth: Int, width: Int)

class BlackBoxedROM(c: ROMConfig) extends BlackBox {
  val io = new Bundle {
    val clock = Clock(INPUT)
    val address = UInt(INPUT, log2Ceil(c.depth))
    val oe = Bool(INPUT)
    val me = Bool(INPUT)
    val q = UInt(OUTPUT, c.width)
  }

  override def desiredName: String = c.name
}

object ROMGenerator {
  private var finalized = false
  private val roms = HashMap[BlackBoxedROM, ROMConfig]()
  def apply(c: ROMConfig): BlackBoxedROM = {
    require(!finalized)
    val m = Module(new BlackBoxedROM(c))
    roms(m) = c
    m
  }
  def lookup(m: BlackBoxedROM): ROMConfig = {
    finalized = true
    roms(m)
  }
}
