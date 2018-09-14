/* verilator lint_off UNOPTFLAT */

module EICG_wrapper(
  output out,
  input en,
  input in
);

  reg en_latched /*verilator clock_enable*/;

  always @(en or in) begin
     if (!in) begin
        en_latched = en;
     end
  end

  assign out = en_latched && in;

endmodule
