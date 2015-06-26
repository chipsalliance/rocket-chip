// See LICENSE for license details.

extern "A" void htif_init
(
  input reg [31:0] htif_width,
  input reg [31:0] mem_width
);

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
  input  reg                      mem_req_valid,
  output reg                      mem_req_ready,
  input  reg                      mem_req_store,
  input  reg [`MEM_ADDR_BITS-1:0] mem_req_bits_addr,
  input  reg [`MEM_TAG_BITS-1:0]  mem_req_bits_tag,

  input  reg                      mem_req_data_valid,
  output reg                      mem_req_data_ready,
  input  reg [`MEM_DATA_BITS-1:0] mem_req_data_bits,
  
  output reg                      mem_resp_valid,
  input reg                       mem_resp_ready,
  output reg [`MEM_TAG_BITS-1:0]  mem_resp_bits_tag,
  output reg [`MEM_DATA_BITS-1:0] mem_resp_bits_data
);
  
module rocketTestHarness;

  reg [31:0] seed;
  initial seed = $get_initial_random_seed();

  //-----------------------------------------------
  // Instantiate the processor

  reg clk   = 0;
  reg reset = 1;
  reg r_reset;
  reg start = 0;

  always #`CLOCK_PERIOD clk = ~clk;

  wire mem_req_valid;
  reg mem_req_ready;
  wire mem_req_bits_rw;
  wire [`MEM_ADDR_BITS-1:0] mem_req_bits_addr;
  wire [`MEM_TAG_BITS-1:0] mem_req_bits_tag;

  wire mem_req_data_valid;
  reg mem_req_data_ready;
  wire [`MEM_DATA_BITS-1:0] mem_req_data_bits;

  reg mem_resp_valid;
  wire mem_resp_ready;
  reg [`MEM_TAG_BITS-1:0] mem_resp_bits_tag;
  reg [`MEM_DATA_BITS-1:0] mem_resp_bits_data;

  reg htif_out_ready;
  wire htif_in_valid;
  wire [`HTIF_WIDTH-1:0] htif_in_bits;
  wire htif_in_ready, htif_out_valid;
  wire [`HTIF_WIDTH-1:0] htif_out_bits;

  wire mem_bk_in_valid;
  wire mem_bk_out_valid;
  wire mem_bk_out_ready;
  wire [`HTIF_WIDTH-1:0] mem_in_bits;

  wire htif_clk;
  wire #0.1 htif_in_valid_delay = htif_in_valid;
  wire htif_in_ready_delay; assign #0.1 htif_in_ready = htif_in_ready_delay;
  wire [`HTIF_WIDTH-1:0] #0.1 htif_in_bits_delay = htif_in_bits;

  wire htif_out_valid_delay; assign #0.1 htif_out_valid = htif_out_valid_delay;
  wire #0.1 htif_out_ready_delay = htif_out_ready;
  wire [`HTIF_WIDTH-1:0] htif_out_bits_delay; assign #0.1 htif_out_bits = htif_out_bits_delay;
  
  wire htif_out_stats_delay; assign #0.1 htif_out_stats = htif_out_stats_delay;

  wire mem_req_valid_delay; assign #0.1 mem_req_valid = mem_req_valid_delay;
  wire #0.1 mem_req_ready_delay = mem_req_ready;
  wire [`MEM_TAG_BITS-1:0] mem_req_bits_tag_delay; assign #0.1 mem_req_bits_tag = mem_req_bits_tag_delay;
  wire [`MEM_ADDR_BITS-1:0] mem_req_bits_addr_delay; assign #0.1 mem_req_bits_addr = mem_req_bits_addr_delay;
  wire mem_req_bits_rw_delay; assign #0.1 mem_req_bits_rw = mem_req_bits_rw_delay;

  wire mem_req_data_valid_delay; assign #0.1 mem_req_data_valid = mem_req_data_valid_delay;
  wire #0.1 mem_req_data_ready_delay = mem_req_data_ready;
  wire [`MEM_DATA_BITS-1:0] mem_req_data_bits_delay; assign #0.1 mem_req_data_bits = mem_req_data_bits_delay;

  wire #0.1 mem_resp_valid_delay = mem_resp_valid;
  wire mem_resp_ready_delay; assign #0.1 mem_resp_ready = mem_resp_ready_delay;
  wire [`MEM_TAG_BITS-1:0] #0.1 mem_resp_bits_tag_delay = mem_resp_bits_tag;
  wire [`MEM_DATA_BITS-1:0] #0.1 mem_resp_bits_data_delay = mem_resp_bits_data;

  wire #0.1 mem_bk_out_ready_delay = mem_bk_out_ready;
  wire #0.1 mem_bk_in_valid_delay = mem_bk_in_valid;
  wire mem_bk_out_valid_delay; assign #0.1 mem_bk_out_valid = mem_bk_out_valid_delay;

  Top dut
  (
    .clk(clk),
    .reset(reset),

    .io_host_in_valid(htif_in_valid_delay),
    .io_host_in_ready(htif_in_ready_delay),
    .io_host_in_bits(htif_in_bits_delay),
    .io_host_out_valid(htif_out_valid_delay),
    .io_host_out_ready(htif_out_ready_delay),
    .io_host_out_bits(htif_out_bits_delay),

`ifndef FPGA
    .io_host_clk(htif_clk),
    .io_host_clk_edge(),
    .io_host_debug_stats_pcr(htif_out_stats_delay),

`ifdef MEM_BACKUP_EN
    .io_mem_backup_ctrl_en(1'b1),
`else
    .io_mem_backup_ctrl_en(1'b0),
`endif
    .io_mem_backup_ctrl_in_valid(mem_bk_in_valid_delay),
    .io_mem_backup_ctrl_out_ready(mem_bk_out_ready_delay),
    .io_mem_backup_ctrl_out_valid(mem_bk_out_valid_delay),
`endif

    .io_mem_req_cmd_valid(mem_req_valid_delay),
    .io_mem_req_cmd_ready(mem_req_ready_delay),
    .io_mem_req_cmd_bits_rw(mem_req_bits_rw_delay),
    .io_mem_req_cmd_bits_addr(mem_req_bits_addr_delay),
    .io_mem_req_cmd_bits_tag(mem_req_bits_tag_delay),

    .io_mem_req_data_valid(mem_req_data_valid_delay),
    .io_mem_req_data_ready(mem_req_data_ready_delay),
    .io_mem_req_data_bits_data(mem_req_data_bits_delay),

    .io_mem_resp_valid(mem_resp_valid_delay),
    .io_mem_resp_ready(mem_resp_ready_delay),
    .io_mem_resp_bits_tag(mem_resp_bits_tag_delay),
    .io_mem_resp_bits_data(mem_resp_bits_data_delay)
  );
  
`ifdef FPGA
  assign htif_clk = clk;
`endif

  //-----------------------------------------------
  // Memory interface

  always @(negedge clk)
  begin
    r_reset <= reset;
    if (reset || r_reset)
    begin
      mem_req_ready <= 0;
      mem_req_data_ready <= 0;
      mem_resp_valid <= 0;
      mem_resp_bits_tag <= 0;
      mem_resp_bits_data <= 0;
    end
    else
    begin
      memory_tick
      (
        mem_req_valid,
        mem_req_ready,
        mem_req_bits_rw,
        mem_req_bits_addr,
        mem_req_bits_tag,

        mem_req_data_valid,
        mem_req_data_ready,
        mem_req_data_bits,
        
        mem_resp_valid,
        mem_resp_ready,
        mem_resp_bits_tag,
        mem_resp_bits_data
      );
    end
  end

  wire mem_bk_req_valid, mem_bk_req_rw, mem_bk_req_data_valid;
  wire [`MEM_TAG_BITS-1:0] mem_bk_req_tag;
  wire [`MEM_ADDR_BITS-1:0] mem_bk_req_addr;
  wire [`MEM_DATA_BITS-1:0] mem_bk_req_data_bits;
  wire mem_bk_req_ready, mem_bk_req_data_ready, mem_bk_resp_valid;
  wire [`MEM_TAG_BITS-1:0]  mem_bk_resp_tag;
  wire [`MEM_DATA_BITS-1:0] mem_bk_resp_data;

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
  assign mem_bk_out_ready = 0; 
  assign mem_bk_in_valid = 0;
  assign mem_in_bits = 0;   
  assign mem_bk_req_valid = 0;
  assign mem_bk_req_addr = 0; 
  assign mem_bk_req_rw = 0;
  assign mem_bk_req_tag = 0; 
  assign mem_bk_req_data_valid = 0;
  assign mem_bk_req_data_bits = 0; 
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
    htif_init(htif_width, mem_width);
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
      htif_fini(1);
    end

    if (exit == 1)
    begin
      `VCDPLUSCLOSE
      htif_fini(0);
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
    if (verbose && mem_req_valid && mem_req_ready)
    begin
      $fdisplay(stderr, "MC: rw=%d addr=%x", mem_req_bits_rw, {mem_req_bits_addr,6'd0});
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
