// See LICENSE.SiFive for license details.
//VCS coverage exclude_file

import "DPI-C" function int debug_tick
(
  output bit     debug_req_valid,
  input  bit     debug_req_ready,
  output int     debug_req_bits_addr,
  output int     debug_req_bits_op,
  output int     debug_req_bits_data,

  input  bit        debug_resp_valid,
  output bit        debug_resp_ready,
  input  int        debug_resp_bits_resp,
  input  int        debug_resp_bits_data
);

module SimDTM(
  input clk,
  input reset,

  output reg        debug_req_valid,
  input             debug_req_ready,
  output reg [ 6:0] debug_req_bits_addr,
  output reg [ 1:0] debug_req_bits_op,
  output reg [31:0] debug_req_bits_data,

  input             debug_resp_valid,
  output reg        debug_resp_ready,
  input      [ 1:0] debug_resp_bits_resp,
  input      [31:0] debug_resp_bits_data,

  output reg [31:0] exit
);

  bit r_reset;

  wire __debug_req_ready = debug_req_ready;
  wire __debug_resp_valid = debug_resp_valid;
  wire [31:0] __debug_resp_bits_resp = {30'b0, debug_resp_bits_resp};
  wire [31:0] __debug_resp_bits_data = debug_resp_bits_data;

  bit __debug_req_valid;
  int __debug_req_bits_addr;
  int __debug_req_bits_op;
  int __debug_req_bits_data;
  bit __debug_resp_ready;
  int __exit;

  always @(posedge clk) begin
    debug_req_valid <= __debug_req_valid;
    debug_req_bits_addr <= __debug_req_bits_addr[6:0];
    debug_req_bits_op <= __debug_req_bits_op[1:0];
    debug_req_bits_data <= __debug_req_bits_data[31:0];
    debug_resp_ready <= __debug_resp_ready;
    exit <= __exit;
  end

  always @(negedge clk)
  begin
    r_reset <= reset;
    if (reset || r_reset)
    begin
      __debug_req_valid = 0;
      __debug_resp_ready = 0;
      __exit = 0;
    end
    else
    begin
      __exit = debug_tick(
        __debug_req_valid,
        __debug_req_ready,
        __debug_req_bits_addr,
        __debug_req_bits_op,
        __debug_req_bits_data,
        __debug_resp_valid,
        __debug_resp_ready,
        __debug_resp_bits_resp,
        __debug_resp_bits_data
      );
    end
  end
endmodule
