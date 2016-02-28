// See LICENSE for license details.

extern "A" void htif_fini(input reg failure);

extern "A" void htif_tick
(
  output reg                    htif_in_valid,
  input  reg                    htif_in_ready,
  output reg  [`HTIF_WIDTH-1:0] htif_in_bits,

  input  reg                    htif_out_valid,
  output reg                    htif_out_ready,
  input  reg  [`HTIF_WIDTH-1:0] htif_out_bits,

  output reg  [31:0]            exit
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
  reg r_reset;
  reg start = 1'b0;

  always #`CLOCK_PERIOD clk = ~clk;

  reg [  31:0] n_mem_channel = `N_MEM_CHANNELS;
  reg [  31:0] htif_width = `HTIF_WIDTH;
  reg [  31:0] mem_width = `MEM_DATA_BITS;
  reg [  63:0] max_cycles = 0;
  reg [  63:0] trace_count = 0;
  reg [1023:0] loadmem = 0;
  reg [1023:0] vcdplusfile = 0;
  reg [1023:0] vcdfile = 0;
  reg          stats_active = 0;
  reg          stats_tracking = 0;
  reg          verbose = 0;
  integer      stderr = 32'h80000002;

`include `TBVFRAG

  always @(posedge clk)
  begin
    r_reset <= reset;
  end

  wire mem_bk_req_valid, mem_bk_req_rw, mem_bk_req_data_valid;
  wire [`MIF_TAG_BITS-1:0] mem_bk_req_tag;
  wire [`MIF_ADDR_BITS-1:0] mem_bk_req_addr;
  wire [`MIF_DATA_BITS-1:0] mem_bk_req_data_bits;
  wire mem_bk_req_ready, mem_bk_req_data_ready, mem_bk_resp_valid;
  wire [`MIF_TAG_BITS-1:0]  mem_bk_resp_tag;
  wire [`MIF_DATA_BITS-1:0] mem_bk_resp_data;

`ifdef MEM_BACKUP_EN
  memdessertMemDessert dessert
  (
    .clk(htif_clk),
    .reset(reset),

    .io_narrow_req_valid(mem_bk_out_valid),
    .io_narrow_req_ready(mem_bk_out_ready),
    .io_narrow_req_bits(htif_out_bits),

    .io_narrow_resp_valid(mem_bk_in_valid),
    .io_narrow_resp_bits(mem_in_bits),

    .io_wide_req_cmd_valid(mem_bk_req_valid),
    .io_wide_req_cmd_ready(mem_bk_req_ready),
    .io_wide_req_cmd_bits_rw(mem_bk_req_rw),
    .io_wide_req_cmd_bits_addr(mem_bk_req_addr),
    .io_wide_req_cmd_bits_tag(mem_bk_req_tag),

    .io_wide_req_data_valid(mem_bk_req_data_valid),
    .io_wide_req_data_ready(mem_bk_req_data_ready),
    .io_wide_req_data_bits_data(mem_bk_req_data_bits),

    .io_wide_resp_valid(mem_bk_resp_valid),
    .io_wide_resp_ready(),
    .io_wide_resp_bits_data(mem_bk_resp_data),
    .io_wide_resp_bits_tag(mem_bk_resp_tag)
  );

  BackupMemory mem
  (
    .clk(htif_clk),
    .reset(reset),

    .mem_req_valid(mem_bk_req_valid),
    .mem_req_ready(mem_bk_req_ready),
    .mem_req_rw(mem_bk_req_rw),
    .mem_req_addr(mem_bk_req_addr),
    .mem_req_tag(mem_bk_req_tag),

    .mem_req_data_valid(mem_bk_req_data_valid),
    .mem_req_data_ready(mem_bk_req_data_ready),
    .mem_req_data_bits(mem_bk_req_data_bits),

    .mem_resp_valid(mem_bk_resp_valid),
    .mem_resp_data(mem_bk_resp_data),
    .mem_resp_tag(mem_bk_resp_tag)
  );
`else
  // set dessert outputs to zero when !backupmem_en
  assign mem_bk_out_ready = 1'b0; 
  assign mem_bk_in_valid = 1'b0;
  assign mem_in_bits = {`HTIF_WIDTH {1'b0}};   
  assign mem_bk_req_valid = 1'b0;
  assign mem_bk_req_ready = 1'b0;
  assign mem_bk_req_addr = {`MIF_ADDR_BITS {1'b0}};
  assign mem_bk_req_rw = 1'b0;
  assign mem_bk_req_tag = {`MIF_TAG_BITS {1'b0}};
  assign mem_bk_req_data_valid = 1'b0;
  assign mem_bk_req_data_bits = 16'd0; 
`endif

  reg htif_in_valid_premux;
  reg [`HTIF_WIDTH-1:0] htif_in_bits_premux;
  assign htif_in_bits = mem_bk_in_valid ? mem_in_bits : htif_in_bits_premux;
  assign htif_in_valid = htif_in_valid_premux && !mem_bk_in_valid;
  wire htif_in_ready_premux = htif_in_ready && !mem_bk_in_valid;
  reg [31:0] exit = 0;

  always @(posedge htif_clk)
  begin
    if (reset || r_reset)
    begin
      htif_in_valid_premux <= 0;
      htif_out_ready <= 0;
      exit <= 0;
    end
    else
    begin
      htif_tick
      (
        htif_in_valid_premux,
        htif_in_ready_premux,
        htif_in_bits_premux,
        htif_out_valid,
        htif_out_ready,
        htif_out_bits,
        exit
      );
    end
  end

  //-----------------------------------------------
  // Start the simulation

  // Some helper functions for turning on, stopping, and finishing stat tracking
  task start_stats;
  begin
    if(!reset || !stats_active)
      begin
`ifdef DEBUG
      if(vcdplusfile)
      begin
        $vcdpluson(0);
        $vcdplusmemon(0);
      end
      if(vcdfile)
      begin
        $dumpon;
      end
`endif
      assign stats_tracking = 1;
    end
  end
  endtask
  task stop_stats;
  begin
`ifdef DEBUG
    $vcdplusoff; $dumpoff;
`endif
    assign stats_tracking = 0;
  end
  endtask
`ifdef DEBUG
`define VCDPLUSCLOSE $vcdplusclose; $dumpoff;
`else
`define VCDPLUSCLOSE
`endif

  // Read input arguments and initialize
  initial
  begin
    $value$plusargs("max-cycles=%d", max_cycles);
`ifdef MEM_BACKUP_EN
    $value$plusargs("loadmem=%s", loadmem);
    if (loadmem)
      $readmemh(loadmem, mem.ram);
`endif
    verbose = $test$plusargs("verbose");
`ifdef DEBUG
    stats_active = $test$plusargs("stats");
    if ($value$plusargs("vcdplusfile=%s", vcdplusfile))
    begin
      $vcdplusfile(vcdplusfile);
    end
    if ($value$plusargs("vcdfile=%s", vcdfile))
    begin
      $dumpfile(vcdfile);
      $dumpvars(0, dut);
    end
    if (!stats_active)
    begin
      start_stats;
    end
    else
    begin
      if(vcdfile)
      begin
        $dumpoff;
      end
    end
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
      htif_fini(1'b1);
    end

    if (exit == 1)
    begin
      `VCDPLUSCLOSE
      htif_fini(1'b0);
    end
  end

  //-----------------------------------------------
  // Tracing code

  always @(posedge clk)
  begin
    if(stats_active)
    begin
      if(!stats_tracking && htif_out_stats)
      begin
        start_stats;
      end
      if(stats_tracking && !htif_out_stats)
      begin
        stop_stats;
      end
    end
  end

  always @(posedge htif_clk)
  begin
    if (verbose && mem_bk_req_valid && mem_bk_req_ready)
    begin
      $fdisplay(stderr, "MB: rw=%d addr=%x", mem_bk_req_rw, {mem_bk_req_addr,6'd0});
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
