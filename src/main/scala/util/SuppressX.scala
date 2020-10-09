// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util.{DecoupledIO, HasBlackBoxInline, IrrevocableIO, ReadyValidIO}
import chisel3.experimental.{IntParam, fromIntToIntParam}

object SuppressX {
  def apply[T <: Data](in: T, cond: Bool = true.B): T = {
    val xsuppressor = Module(new XSuppressor(in.getWidth))
    xsuppressor.io.in := in.asUInt
    xsuppressor.io.cond := cond
    xsuppressor.io.out.asTypeOf(chiselTypeOf(in))
  }

  def apply[T <: Data](in: ReadyValidIO[T]): ReadyValidIO[T] = {
    val res = Wire(chiselTypeOf(in))
    res.valid := in.valid
    res.bits := apply(in.bits, in.valid)
    in.ready := res.ready
    res
  }

  def apply[T <: Data](in: IrrevocableIO[T]): IrrevocableIO[T] =
    apply(in: ReadyValidIO[T]).asInstanceOf[IrrevocableIO[T]]

  def apply[T <: Data](in: DecoupledIO[T]): DecoupledIO[T] =
    apply(in: ReadyValidIO[T]).asInstanceOf[DecoupledIO[T]]
}

class XSuppressor(width: Int) extends BlackBox(Map("WIDTH" -> width)) with HasBlackBoxInline {
  val io = IO(new Bundle {
    val in = Input(UInt(width.W))
    val cond = Input(Bool())
    val out = Output(UInt(width.W))
  })

  setInline("XSuppressor.v",
    s"""
      |module XSuppressor #(parameter WIDTH = 1) (
      |    input  [WIDTH-1:0] in,
      |    input  cond,
      |    output [WIDTH-1:0] out);
      |
      |`ifdef SYNTHESIS
      |    assign out = in;
      |`else
      |`ifdef VERILATOR
      |    assign out = in;
      |`else
      |    reg [(WIDTH+31)/32*32-1:0] random_value;
      |    reg [WIDTH-1:0] output_value;
      |    integer i;
      |
      |    always @(in, cond) begin
      |      output_value = in;
      |
      |      if (cond && ^in === 1'bx) begin
      |        for (i = 0; i < WIDTH; i=i+32) begin
      |`ifdef RANDOM
      |          random_value[i +: 32] = `RANDOM;
      |`else
      |          random_value[i +: 32] = $$random;
      |`endif
      |        end
      |
      |        for (i = 0; i < WIDTH; i=i+1) begin
      |          if (in[i] === 1'bx) begin
      |            output_value[i] = random_value[i];
      |          end
      |        end
      |      end
      |    end
      |
      |    assign out = output_value;
      |`endif
      |`endif
      |
      |endmodule
     """.stripMargin)
}
