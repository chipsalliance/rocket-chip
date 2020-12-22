// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import Chisel._
import freechips.rocketchip.util._

// Signal directions are from the master's point-of-view
class APBBundle(val params: APBBundleParameters) extends Bundle
{
  // Flow control signals from the master
  val psel      = Bool(OUTPUT)
  val penable   = Bool(OUTPUT)

  // Payload signals
  val pwrite    = Bool(OUTPUT)
  val paddr     = UInt(OUTPUT, width = params.addrBits)
  val pprot     = UInt(OUTPUT, width = params.protBits)
  val pwdata    = UInt(OUTPUT, width = params.dataBits)
  val pstrb     = UInt(OUTPUT, width = params.dataBits/8)
  val pauser    = BundleMap(params.requestFields)

  val pready    = Bool(INPUT)
  val pslverr   = Bool(INPUT)
  val prdata    = UInt(INPUT, width = params.dataBits)
  val pduser    = BundleMap(params.responseFields)

  def tieoff(): Unit = {
    pready.dir match {
      case INPUT =>
        pready  := Bool(false)
        pslverr := Bool(false)
        prdata  := UInt(0)
        pduser :<= BundleMap()
      case OUTPUT =>
        pwrite  := Bool(false)
        paddr   := UInt(0)
        pprot   := APBParameters.PROT_DEFAULT
        pwdata  := UInt(0)
        pstrb   := UInt(0)
        pauser :<= BundleMap()
      case _ =>
    }
  }
}

object APBBundle
{
  def apply(params: APBBundleParameters) = new APBBundle(params)
}
