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
  val hauser    = if (params.userBits > 0) Some(UInt(OUTPUT, width = params.userBits)) else None
  val haddr     = UInt(OUTPUT, width = params.addrBits)

  // D-phase signals from arbiter to slave
  val hwdata    = UInt(OUTPUT, width = params.dataBits)

  // D-phase signals from slave to arbiter
  val hresp     = UInt(INPUT, width = params.hrespBits)
  val hrdata    = UInt(INPUT, width = params.dataBits)

  // Split signals
  val hmaster   = if (params.lite) None else Some(UInt(OUTPUT, width = 4))
  val hsplit    = if (params.lite) None else Some(UInt(INPUT, width = 16))

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
  val hmastlock = if (params.lite) Some(Bool(OUTPUT)) else None
  val hlock     = if (params.lite) None else Some(Bool(OUTPUT))
  val hbusreq   = if (params.lite) None else Some(Bool(OUTPUT))

  // Flow control from arbiter to master
  val hgrant  = if (params.lite) None else Some(Bool(INPUT))
  val hready  = Bool(INPUT)

  // Handy methods that don't care about lite
  def lock():   Bool = if (params.lite) hmastlock.get else hlock.get
  def busreq(): Bool = if (params.lite) Wire(init = Bool(true)) else hbusreq.get
  def grant():  Bool = if (params.lite) Wire(init = Bool(true)) else hgrant.get

  // A-phase signals from master to arbiter
  val htrans  = UInt(OUTPUT, width = params.transBits)
  val hsize   = UInt(OUTPUT, width = params.sizeBits)
  val hburst  = UInt(OUTPUT, width = params.burstBits)
  val hwrite  = Bool(OUTPUT)
  val hprot   = UInt(OUTPUT, width = params.protBits)
  val hauser  = if (params.userBits > 0) Some(UInt(OUTPUT, width = params.userBits)) else None
  val haddr   = UInt(OUTPUT, width = params.addrBits)

  // D-phase signals from master to arbiter
  val hwdata  = UInt(OUTPUT, width = params.dataBits)

  // D-phase response from arbiter to master
  val hresp   = UInt(INPUT, width = params.hrespBits)
  val hrdata  = UInt(INPUT, width = params.dataBits)

  def tieoff() {
    hrdata.dir match {
      case INPUT =>
        hgrant.foreach { _ := Bool(false) }
        hready    := Bool(false)
        hresp     := AHBParameters.RESP_OKAY
        hrdata    := UInt(0)
      case OUTPUT =>
        lock()     := Bool(false)
        busreq()   := Bool(false)
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
