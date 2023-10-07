// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import chisel3._
import freechips.rocketchip.util._

// Signal directions are from the master's point-of-view
class AHBSlaveBundle(val params: AHBBundleParameters) extends Bundle
{
  // Control signals from the arbiter to slave
  val hmastlock = Output(Bool())
  val hsel      = Output(Bool())

  // Flow control signals (hreadyout=FALSE => hready=FALSE)
  val hready    = Output(Bool()) // from arbiter
  val hreadyout = Input(Bool())  // to arbiter

  // A-phase signals from arbiter to slave
  val htrans    = Output(UInt(params.transBits.W))
  val hsize     = Output(UInt(params.sizeBits.W))
  val hburst    = Output(UInt(params.burstBits.W))
  val hwrite    = Output(Bool())
  val hprot     = Output(UInt(params.protBits.W))
  val haddr     = Output(UInt(params.addrBits.W))
  val hauser    = BundleMap(params.requestFields)

  // D-phase signals from arbiter to slave
  val hduser    = BundleMap(params.responseFields)
  val hwdata    = Output(UInt(params.dataBits.W))

  // D-phase signals from slave to arbiter
  val hresp     = Input(UInt(params.hrespBits.W))
  val hrdata    = Input(UInt(params.dataBits.W))

  // Split signals
  val hmaster   = if (params.lite) None else Some(Output(UInt(4.W)))
  val hsplit    = if (params.lite) None else Some(Input(UInt(16.W)))
}

class AHBMasterBundle(val params: AHBBundleParameters) extends Bundle
{
  // Control signals from master to arbiter
  val hmastlock = if (params.lite) Some(Output(Bool())) else None
  val hlock     = if (params.lite) None else Some(Output(Bool()))
  val hbusreq   = if (params.lite) None else Some(Output(Bool()))

  // Flow control from arbiter to master
  val hgrant  = if (params.lite) None else Some(Input(Bool()))
  val hready  = Input(Bool())

  // Handy methods that don't care about lite
  def lock():   Bool = if (params.lite) hmastlock.get    else hlock.get
  def busreq(): Bool = if (params.lite) WireInit(true.B) else hbusreq.get
  def grant():  Bool = if (params.lite) WireInit(true.B) else hgrant.get

  // A-phase signals from master to arbiter
  val htrans  = Output(UInt(params.transBits.W))
  val hsize   = Output(UInt(params.sizeBits.W))
  val hburst  = Output(UInt(params.burstBits.W))
  val hwrite  = Output(Bool())
  val hprot   = Output(UInt(params.protBits.W))
  val haddr   = Output(UInt(params.addrBits.W))
  val hauser  = BundleMap(params.requestFields)

  // D-phase signals from master to arbiter
  val hduser    = BundleMap(params.responseFields)
  val hwdata  = Output(UInt(params.dataBits.W))

  // D-phase response from arbiter to master
  val hresp   = Input(UInt(params.hrespBits.W))
  val hrdata  = Input(UInt(params.dataBits.W))
}

object AHBSlaveBundle
{
  def apply(params: AHBBundleParameters) = new AHBSlaveBundle(params)
}

object AHBMasterBundle
{
  def apply(params: AHBBundleParameters) = new AHBMasterBundle(params)
}
