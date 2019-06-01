// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import Chisel._
import freechips.rocketchip.util.GenericParameterizedBundle

abstract class AHBBundleBase(params: AHBBundleParameters) extends GenericParameterizedBundle(params)

// Signal directions are from the master's point-of-view
class AHBSlaveBundle(params: AHBBundleParameters) extends AHBBundleBase(params)
{
  // Control signals from the arbiter to slave
  val hmastlock = Bool(OUTPUT)
  val hsel      = Bool(OUTPUT)

  // Flow control signals (hreadyout=FALSE => hready=FALSE)
  val hready    = Bool(OUTPUT) // from arbiter
  val hreadyout = Bool(INPUT)  // to arbiter

  // A-phase signals from arbiter to slave
  val htrans    = UInt(OUTPUT, width = params.transBits)
  val hsize     = UInt(OUTPUT, width = params.sizeBits)
  val hburst    = UInt(OUTPUT, width = params.burstBits)
  val hwrite    = Bool(OUTPUT)
  val hprot     = UInt(OUTPUT, width = params.protBits)
  val hauser    = if ( params.userBits > 0) Some(UInt(OUTPUT, width = params.userBits)) else None
  val haddr     = UInt(OUTPUT, width = params.addrBits)

  // D-phase signals from arbiter to slave
  val hwdata    = UInt(OUTPUT, width = params.dataBits)

  // D-phase signals from slave to arbiter
  val hresp     = Bool(INPUT)
  val hrdata    = UInt(INPUT, width = params.dataBits)

  // Split signals
/*
  val hmaster   = UInt(OUTPUT, width = 4)
  val hsplit    = UInt(INPUT, width = 16)
*/

  def tieoff() {
    hrdata.dir match {
      case INPUT =>
        hreadyout := Bool(false)
        hresp     := AHBParameters.RESP_OKAY
        hrdata    := UInt(0)
      case OUTPUT => 
        hmastlock := Bool(false)
        hsel      := Bool(false)
        hready    := Bool(false)
        htrans    := AHBParameters.TRANS_IDLE
        hsize     := UInt(0)
        hburst    := AHBParameters.BURST_SINGLE
        hwrite    := Bool(false)
        hprot     := AHBParameters.PROT_DEFAULT
        hauser.map {_:= UInt(0)}
        haddr     := UInt(0)
        hwdata    := UInt(0)
      case _ =>
    }
  }
}

class AHBMasterBundle(params: AHBBundleParameters) extends AHBBundleBase(params)
{
  // Control signals from master to arbiter
  val hlock   = Bool(OUTPUT) // AHBSlave: hmastlock
  val hbusreq = Bool(OUTPUT) // AHBSlave: hsel

  // Flow control from arbiter to master
  val hgrant  = Bool(INPUT) // AHBSlave: always true
  val hready  = Bool(INPUT) // AHBSlave: hreadyout

  // A-phase signals from master to arbiter
  val htrans  = UInt(OUTPUT, width = params.transBits)
  val hsize   = UInt(OUTPUT, width = params.sizeBits)
  val hburst  = UInt(OUTPUT, width = params.burstBits)
  val hwrite  = Bool(OUTPUT)
  val hprot   = UInt(OUTPUT, width = params.protBits)
  val hauser  = if ( params.userBits > 0) Some(UInt(OUTPUT, width = params.userBits)) else None
  val haddr   = UInt(OUTPUT, width = params.addrBits)

  // D-phase signals from master to arbiter
  val hwdata  = UInt(OUTPUT, width = params.dataBits)

  // D-phase response from arbiter to master
  val hresp   = UInt(INPUT, width = params.hrespBits)
  val hrdata  = UInt(INPUT, width = params.dataBits)

  def tieoff() {
    hrdata.dir match {
      case INPUT =>
        hgrant    := Bool(false)
        hready    := Bool(false)
        hresp     := AHBParameters.RESP_OKAY
        hrdata    := UInt(0)
      case OUTPUT =>
        hlock     := Bool(false)
        hbusreq   := Bool(false)
        htrans    := AHBParameters.TRANS_IDLE
        hsize     := UInt(0)
        hburst    := AHBParameters.BURST_SINGLE
        hwrite    := Bool(false)
        hprot     := AHBParameters.PROT_DEFAULT
        hauser.map {_:= UInt(0)}
        haddr     := UInt(0)
        hwdata    := UInt(0)
      case _ =>
    }
  }
}

object AHBSlaveBundle
{
  def apply(params: AHBBundleParameters) = new AHBSlaveBundle(params)
}

object AHBMasterBundle
{
  def apply(params: AHBBundleParameters) = new AHBMasterBundle(params)
}
