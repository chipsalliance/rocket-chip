// See LICENSE.SiFive for license details.

//VCS coverage exclude_file

// No default parameter values are intended, nor does IEEE 1800-2012 require them (clause A.2.4 param_assignment),
// but Incisive demands them. These default values should never be used.
module plusarg_test#(parameter PLUSARG="borked") (
   output out
);

reg t = 1'b0;
assign out = t;

`ifndef SYNTHESIS
initial begin
   if ($test$plusargs(PLUSARG)) begin
       t = 1'b1;
   end else begin
       t = 1'b0;
   end
end
`endif

endmodule
