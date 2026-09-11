// Minimal compatibility cells for standalone integration in Chipyard.
module edge_detect (
  input logic clk_i,
  input logic rst_ni,
  input logic d_i,
  output logic re_o,
  output logic fe_o
);
  logic q;
  always @(posedge clk_i or negedge rst_ni) begin
    if (!rst_ni) begin q <= 1'b0; re_o <= 1'b0; fe_o <= 1'b0; end
    else begin re_o <= d_i & ~q; fe_o <= ~d_i & q; q <= d_i; end
  end
endmodule

module pulp_clock_gating (
  input logic clk_i,
  input logic en_i,
  input logic test_en_i,
  output logic clk_o
);
  assign clk_o = clk_i;
endmodule
