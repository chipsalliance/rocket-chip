// See LICENSE.SiFive for license details.

// No default parameter values are intended, nor does IEEE 1800-2012 require them (clause A.2.4 param_assignment),
// but Incisive demands them. These default values should never be used.
module plusarg_reader #(string FORMAT="borked", int DEFAULT=0) (
   output [31:0] out
);

reg [31:0] myplus;
assign out = myplus;

initial begin
   myplus = DEFAULT;
`ifndef SYNTHESIS
`ifndef verilator
   // Work-around for https://www.veripool.org/issues/1165
   if (!$value$plusargs(FORMAT, myplus)) myplus = DEFAULT;
`endif
`endif
end

endmodule
