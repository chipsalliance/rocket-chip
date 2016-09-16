// You can't divide clocks in Chisel
module ClockDivider(
  input  clock_in,
  input  reset_in,
  output clock_out,
  output reset_out
);

  reg [2:0] shift = 3'b001;

  always @(posedge clock_in)
  begin
    shift <= {shift[0], shift[2:1]};
  end
  
  assign reset_out = reset_in;
  assign clock_out = shift[0];

endmodule
