// See LICENSE.SiFive for license details.

/*
package junctions
import Chisel._
import config._

class JTAGIO(drvTdo: Boolean = false) extends Bundle {
  val TCK = Clock(OUTPUT)
  val TMS = Bool(OUTPUT)
  val TDI = Bool(OUTPUT)
  val TDO = Bool(INPUT)
  val TRST = Bool(OUTPUT)

  val DRV_TDO = if (drvTdo) Some(Bool(INPUT)) else None
  override def cloneType = new JTAGIO(drvTdo).asInstanceOf[this.type]
}
 */
