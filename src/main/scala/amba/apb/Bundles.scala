// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import chisel3._
import freechips.rocketchip.util._

// Signal directions are from the master's point-of-view
class APBBundle(val params: APBBundleParameters) extends Bundle
{
  // Flow control signals from the master
  val psel      = Output(Bool())
  val penable   = Output(Bool())

  // Payload signals
  val pwrite    = Output(Bool())
  val paddr     = Output(UInt(params.addrBits.W))
  val pprot     = Output(UInt(params.protBits.W))
  val pwdata    = Output(UInt(params.dataBits.W))
  val pstrb     = Output(UInt((params.dataBits/8).W))
  val pauser    = BundleMap(params.requestFields)

  val pready    = Input(Bool())
  val pslverr   = Input(Bool())
  val prdata    = Input(UInt(params.dataBits.W))
  val pduser    = BundleMap(params.responseFields)
}

object APBBundle
{
  def apply(params: APBBundleParameters) = new APBBundle(params)
}
