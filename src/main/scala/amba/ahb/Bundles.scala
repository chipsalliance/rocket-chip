// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import Chisel._
import freechips.rocketchip.util.GenericParameterizedBundle

abstract class AHBBundleBase(params: AHBBundleParameters) extends GenericParameterizedBundle(params)

// Signal directions are from the master's point-of-view
class AHBBundle(params: AHBBundleParameters) extends AHBBundleBase(params)
{
  // Flow control signals from the master
  val hmastlock = Bool(OUTPUT)
  val htrans    = UInt(OUTPUT, width = params.transBits)
  val hsel      = Bool(OUTPUT)
  val hready    = Bool(OUTPUT) // on a master, drive this from readyout

  // Payload signals
  val hwrite    = Bool(OUTPUT)
  val haddr     = UInt(OUTPUT, width = params.addrBits)
  val hsize     = UInt(OUTPUT, width = params.sizeBits)
  val hburst    = UInt(OUTPUT, width = params.burstBits)
  val hprot     = UInt(OUTPUT, width = params.protBits)
  val hwdata    = UInt(OUTPUT, width = params.dataBits)

  val hreadyout = Bool(INPUT)
  val hresp     = Bool(INPUT)
  val hrdata    = UInt(INPUT, width = params.dataBits)
}

object AHBBundle
{
  def apply(params: AHBBundleParameters) = new AHBBundle(params)
}
