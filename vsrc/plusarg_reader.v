// See LICENSE.SiFive for license details.

// No default parameter values are intended, nor does IEEE 1800-2012 require them (clause A.2.4 param_assignment),
// but Incisive demands them. These default values should never be used.
module plusarg_reader #(FORMAT="borked=%d", DEFAULT=0) (
   output [31:0] out
);

reg [31:0] myplus;
assign out = myplus;

initial begin
`ifdef SYNTHESIS
   myplus = DEFAULT;
`else
   if (!$value$plusargs(FORMAT, myplus)) myplus = DEFAULT;
`endif
end

endmodule
