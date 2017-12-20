// See LICENSE.SiFive for license details.

// No default parameter values are intended, nor does IEEE 1800-2012 require them (clause A.2.4 param_assignment),
// but Incisive demands them. These default values should never be used.
module test_plusarg_reader #(parameter FORMAT="borked") (
   output out
);

`ifdef SYNTHESIS
assign out = 1'b0;
`else
reg myplus;
assign out = myplus;

initial begin
   if ($test$plusargs(FORMAT)) myplus = 1'b1;
end
`endif

endmodule
