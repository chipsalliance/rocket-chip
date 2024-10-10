// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import chisel3._
import freechips.rocketchip.util._

// Signal directions are from the manager's point-of-view
class AHBSubordinateBundle(val params: AHBBundleParameters) extends Bundle
{
  // Control signals from the arbiter to subordinate
  val hmastlock = Output(Bool())
  val hsel      = Output(Bool())

  // Flow control signals (hreadyout=FALSE => hready=FALSE)
  val hready    = Output(Bool()) // from arbiter
  val hreadyout = Input(Bool())  // to arbiter

  // A-phase signals from arbiter to subordinate
  val htrans    = Output(UInt(params.transBits.W))
  val hsize     = Output(UInt(params.sizeBits.W))
  val hburst    = Output(UInt(params.burstBits.W))
  val hwrite    = Output(Bool())
  val hprot     = Output(UInt(params.protBits.W))
  val haddr     = Output(UInt(params.addrBits.W))
  val hauser    = BundleMap(params.requestFields)

  // D-phase signals from arbiter to subordinate
  val hduser    = BundleMap(params.responseFields)
  val hwdata    = Output(UInt(params.dataBits.W))

  // D-phase signals from subordinate to arbiter
  val hresp     = Input(UInt(params.hrespBits.W))
  val hrdata    = Input(UInt(params.dataBits.W))

  // Split signals
  val hmanager   = if (params.lite) None else Some(Output(UInt(4.W)))
  val hsplit    = if (params.lite) None else Some(Input(UInt(16.W)))
}

class AHBManagerBundle(val params: AHBBundleParameters) extends Bundle
{
  // Control signals from manager to arbiter
  val hmastlock = if (params.lite) Some(Output(Bool())) else None
  val hlock     = if (params.lite) None else Some(Output(Bool()))
  val hbusreq   = if (params.lite) None else Some(Output(Bool()))

  // Flow control from arbiter to manager
  val hgrant  = if (params.lite) None else Some(Input(Bool()))
  val hready  = Input(Bool())

  // Handy methods that don't care about lite
  def lock():   Bool = if (params.lite) hmastlock.get    else hlock.get
  def busreq(): Bool = if (params.lite) WireInit(true.B) else hbusreq.get
  def grant():  Bool = if (params.lite) WireInit(true.B) else hgrant.get

  // A-phase signals from manager to arbiter
  val htrans  = Output(UInt(params.transBits.W))
  val hsize   = Output(UInt(params.sizeBits.W))
  val hburst  = Output(UInt(params.burstBits.W))
  val hwrite  = Output(Bool())
  val hprot   = Output(UInt(params.protBits.W))
  val haddr   = Output(UInt(params.addrBits.W))
  val hauser  = BundleMap(params.requestFields)

  // D-phase signals from manager to arbiter
  val hduser    = BundleMap(params.responseFields)
  val hwdata  = Output(UInt(params.dataBits.W))

  // D-phase response from arbiter to manager
  val hresp   = Input(UInt(params.hrespBits.W))
  val hrdata  = Input(UInt(params.dataBits.W))
}

object AHBSubordinateBundle
{
  def apply(params: AHBBundleParameters) = new AHBSubordinateBundle(params)
}

object AHBManagerBundle
{
  def apply(params: AHBBundleParameters) = new AHBManagerBundle(params)
}
