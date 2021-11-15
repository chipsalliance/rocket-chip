// See LICENSE.SiFive for license details.

package freechips.rocketchip.zk

import Chisel.RegInit
import chisel3._
import chisel3.util.HasBlackBoxInline
import freechips.rocketchip.tile.XLen

object ZKR {
  val xLen        = XLen
  val seedcsr     = 0x015
  val mseccfg     = 0x747
  val reg_mseccfg = RegInit(0x300.U)
}

class ZKRInterface(xLen: Int) extends Bundle {
  val clk      = Input(Clock())
  val rst      = Input(Bool())
  val req      = Input(Bool())
  val seedval  = Output(UInt(xLen.W))
}

class zkr(xLen: Int) extends BlackBox(Map("XLEN" -> xLen)) with HasBlackBoxInline {
  val io = IO(new ZKRInterface(xLen))
  setInline("zkr.v",
    s"""
       #module zkr #(parameter XLEN = 32) (
       #input  clk,
       #input  rst,
       #input  req,
       #output [XLEN-1:0] seedval);
       #
       #//if there is a TRNG implemented, it can be put here otherwise by default return no valid random bits.
       #assign seedval = {{(XLEN-32){1'b0}},32'hFFFFDEAD};
       #endmodule
     """.stripMargin('#'))
}


