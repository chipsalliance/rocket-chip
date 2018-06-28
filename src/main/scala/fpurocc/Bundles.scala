// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import Chisel._
import freechips.rocketchip.util.GenericParameterizedBundle

abstract class NAMESPACEBundleBase(params: NAMESPACEBundleParameters) extends GenericParameterizedBundle(params)

// Signal directions are from the master's point-of-view
class NAMESPACEBundle(params: NAMESPACEBundleParameters) extends NAMESPACEBundleBase(params)
{
  // Flow control signals from the master
  //val hmastlock = Bool(OUTPUT)
  //val htrans    = UInt(OUTPUT, width = params.transBits)
  //val hsel      = Bool(OUTPUT)
  //val hready    = Bool(OUTPUT) // on a master, drive this from readyout

  // Payload signals
  //val hwrite    = Bool(OUTPUT)
  //val haddr     = UInt(OUTPUT, width = params.addrBits)
  //val hsize     = UInt(OUTPUT, width = params.sizeBits)
  //val hburst    = UInt(OUTPUT, width = params.burstBits)
  //val hprot     = UInt(OUTPUT, width = params.protBits)
  //val hwdata    = UInt(OUTPUT, width = params.dataBits)

  //val hreadyout = Bool(INPUT)
  //val hresp     = Bool(INPUT)
  //val hrdata    = UInt(INPUT, width = params.dataBits)

  //def tieoff() {
  //  hreadyout.dir match {
  //    case INPUT =>
  //      hreadyout := Bool(false)
  //      hresp     := NAMESPACEParameters.RESP_OKAY
  //      hrdata    := UInt(0)
  //    case OUTPUT => 
  //      hmastlock := Bool(false)
  //      htrans    := NAMESPACEParameters.TRANS_IDLE
  //      hsel      := Bool(false)
  //      hready    := Bool(false)
  //      hwrite    := Bool(false)
  //      haddr     := UInt(0)
  //      hsize     := UInt(0)
  //      hburst    := NAMESPACEParameters.BURST_SINGLE
  //      hprot     := NAMESPACEParameters.PROT_DEFAULT
  //      hwdata    := UInt(0)
  //    case _ =>
  //  }
  //}
	//val rm = Bits(width = FPConstants.RM_SZ)
	//val fmaCmd = Bits(width = 2)
	//val typ = Bits(width = 2)
	//val in1 = Bits(width = fLen + 1)
	//val in2 = Bits(width = fLen + 1)
	//val in3 = Bits(width = fLen + 1)
	///////////////////////
	//val data = Bits(width = fLen + 1)
	//val exc = Bits(width = FPConstants.FLAGS_SZ)
	val fpu_req = Decoupled(new FPInput()).flip
	val fpu_resp = Decoupled(new FPResult())

}

object NAMESPACEBundle
{
	def apply(params: NAMESPACEBundleParameters) = new NAMESPACEBundle(params)
}
