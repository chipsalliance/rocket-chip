// See LICENSE for license details.

extern "A" void debug_tick
(
  output reg        debug_req_valid,
  input  reg        debug_req_ready,
  output reg [ 4:0] debug_req_bits_addr,
  output reg [ 1:0] debug_req_bits_op,
  output reg [33:0] debug_req_bits_data,

  input  reg        debug_resp_valid,
  output reg        debug_resp_ready,
  input  reg [ 1:0] debug_resp_bits_resp,
  input  reg [33:0] debug_resp_bits_data,

  output reg [31:0] exit
);

extern "A" void memory_tick
(
  input  reg [31:0]               channel,

  input  reg                      ar_valid,
  output reg                      ar_ready,
  input  reg [`MEM_ADDR_BITS-1:0] ar_addr,
  input  reg [`MEM_ID_BITS-1:0]   ar_id,
  input  reg [2:0]                ar_size,
  input  reg [7:0]                ar_len,

  input  reg                      aw_valid,
  output reg                      aw_ready,
  input  reg [`MEM_ADDR_BITS-1:0] aw_addr,
  input  reg [`MEM_ID_BITS-1:0]   aw_id,
  input  reg [2:0]                aw_size,
  input  reg [7:0]                aw_len,

  input  reg                      w_valid,
  output reg                      w_ready,
  input  reg [`MEM_STRB_BITS-1:0] w_strb,
  input  reg [`MEM_DATA_BITS-1:0] w_data,
  input  reg                      w_last,

  output reg                      r_valid,
  input  reg                      r_ready,
  output reg [1:0]                r_resp,
  output reg [`MEM_ID_BITS-1:0]   r_id,
  output reg [`MEM_DATA_BITS-1:0] r_data,
  output reg                      r_last,

  output reg                      b_valid,
  input  reg                      b_ready,
  output reg [1:0]                b_resp,
  output reg [`MEM_ID_BITS-1:0]   b_id
);

module rocketTestHarness;

  reg [31:0] seed;
  initial seed = $get_initial_random_seed();

  //-----------------------------------------------
  // Instantiate the processor

  reg clk   = 1'b0;
  reg reset = 1'b1;
  reg r_reset = 1'b1;
  reg start = 1'b0;

  always #`CLOCK_PERIOD clk = ~clk;

  reg [  31:0] n_mem_channel = `N_MEM_CHANNELS;
  reg [  31:0] mem_width = `MEM_DATA_BITS;
  reg [  63:0] max_cycles = 0;
  reg [  63:0] trace_count = 0;
  reg [1023:0] loadmem = 0;
  reg [1023:0] vcdplusfile = 0;
  reg [1023:0] vcdfile = 0;
  reg          verbose = 0;
  wire         printf_cond = verbose && !reset;
  wire         stop_cond = !reset;
  integer      stderr = 32'h80000002;

`include `TBVFRAG

  always @(posedge clk)
  begin
    r_reset <= reset;
  end

  reg [31:0] exit = 0;

  always @(posedge clk)
  begin
    if (reset || r_reset)
    begin
      debug_req_valid <= 0;
      debug_resp_ready <= 0;
    end
    else
    begin
      debug_tick
      (
        debug_req_valid,
        debug_req_ready,
        debug_req_bits_addr,
        debug_req_bits_op,
        debug_req_bits_data,
        debug_resp_valid,
        debug_resp_ready,
        debug_resp_bits_resp,
        debug_resp_bits_data,
        exit
      );
    end
  end

  //-----------------------------------------------
  // Start the simulation

  // Read input arguments and initialize
  initial
  begin
    $value$plusargs("max-cycles=%d", max_cycles);
    verbose = $test$plusargs("verbose");
`ifdef DEBUG
    if ($value$plusargs("vcdplusfile=%s", vcdplusfile))
    begin
      $vcdplusfile(vcdplusfile);
      $vcdpluson(0);
      $vcdplusmemon(0);
    end

    if ($value$plusargs("vcdfile=%s", vcdfile))
    begin
      $dumpfile(vcdfile);
      $dumpvars(0, dut);
      $dumpon;
    end
`define VCDPLUSCLOSE $vcdplusclose; $dumpoff;
`else
`define VCDPLUSCLOSE
`endif

    // Strobe reset
    #777.7 reset = 0;

  end

  reg [255:0] reason = 0;
  always @(posedge clk)
  begin
    if (max_cycles > 0 && trace_count > max_cycles)
      reason = "timeout";
    if (exit > 1)
      $sformat(reason, "tohost = %d", exit >> 1);

    if (reason)
    begin
      $fdisplay(stderr, "*** FAILED *** (%s) after %d simulation cycles", reason, trace_count);
      `VCDPLUSCLOSE
      $fatal;
    end

    if (exit == 1)
    begin
      if (verbose)
        $fdisplay(stderr, "Completed after %d simulation cycles", trace_count);
      `VCDPLUSCLOSE
      $finish;
    end
  end

  always @(posedge clk)
  begin
    trace_count = trace_count + 1;
`ifdef GATE_LEVEL
    if (verbose)
    begin
      $fdisplay(stderr, "C: %10d", trace_count-1);
    end
`endif
  end

endmodule
