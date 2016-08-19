package junctions
import Chisel._
import cde.{Parameters}

class JtagIO(drvTdo : Boolean = false) extends Bundle {

  val TCK = Clock(OUTPUT)
  val TMS = Bool(OUTPUT)
  val TDI = Bool(OUTPUT)
  val TDO = Bool(INPUT)
  val TRST = Bool(OUTPUT)

  val DRV_TDO = if (drvTdo) Some(Bool(OUTPUT)) else None
  override def cloneType = new JtagIO(drvTdo).asInstanceOf[this.type]

}
