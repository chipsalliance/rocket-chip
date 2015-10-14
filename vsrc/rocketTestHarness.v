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

  wire ar_valid;
  reg ar_ready;
  wire [`MEM_ADDR_BITS-1:0] ar_addr;
  wire [`MEM_ID_BITS-1:0] ar_id;
  wire [2:0] ar_size;
  wire [7:0] ar_len;

  wire aw_valid;
  reg aw_ready;
  wire [`MEM_ADDR_BITS-1:0] aw_addr;
  wire [`MEM_ID_BITS-1:0] aw_id;
  wire [2:0] aw_size;
  wire [7:0] aw_len;

  wire w_valid;
  reg w_ready;
  wire [`MEM_STRB_BITS-1:0] w_strb;
  wire [`MEM_DATA_BITS-1:0] w_data;
  wire w_last;

  reg r_valid;
  wire r_ready;
  reg [1:0] r_resp;
  reg [`MEM_ID_BITS-1:0] r_id;
  reg [`MEM_DATA_BITS-1:0] r_data;
  reg r_last;

  reg b_valid;
  wire b_ready;
  reg [1:0] b_resp;
  reg [`MEM_ID_BITS-1:0] b_id;

  reg htif_out_ready;
  wire htif_in_valid;
  wire [`HTIF_WIDTH-1:0] htif_in_bits;
  wire htif_in_ready, htif_out_valid;
  wire [`HTIF_WIDTH-1:0] htif_out_bits;
  wire htif_out_stats;

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

  wire ar_valid_delay; assign #0.1 ar_valid = ar_valid_delay;
  wire #0.1 ar_ready_delay = ar_ready;
  wire [`MEM_ADDR_BITS-1:0] ar_addr_delay; assign #0.1 ar_addr = ar_addr_delay;
  wire [`MEM_ID_BITS-1:0] ar_id_delay; assign #0.1 ar_id = ar_id_delay;
  wire [2:0] ar_size_delay; assign #0.1 ar_size = ar_size_delay;
  wire [7:0] ar_len_delay; assign #0.1 ar_len = ar_len_delay;

  wire aw_valid_delay; assign #0.1 aw_valid = aw_valid_delay;
  wire #0.1 aw_ready_delay = aw_ready;
  wire [`MEM_ADDR_BITS-1:0] aw_addr_delay; assign #0.1 aw_addr = aw_addr_delay;
  wire [`MEM_ID_BITS-1:0] aw_id_delay; assign #0.1 aw_id = aw_id_delay;
  wire [2:0] aw_size_delay; assign #0.1 aw_size = aw_size_delay;
  wire [7:0] aw_len_delay; assign #0.1 aw_len = aw_len_delay;

  wire w_valid_delay; assign #0.1 w_valid = w_valid_delay;
  wire #0.1 w_ready_delay = w_ready;
  wire [`MEM_STRB_BITS-1:0] w_strb_delay; assign #0.1 w_strb = w_strb_delay;
  wire [`MEM_DATA_BITS-1:0] w_data_delay; assign #0.1 w_data = w_data_delay;
  wire w_last_delay; assign #0.1 w_last = w_last_delay;

  wire #0.1 r_valid_delay = r_valid;
  wire r_ready_delay; assign #0.1 r_ready = r_ready_delay;
  wire [1:0] #0.1 r_resp_delay = r_resp;
  wire [`MEM_ID_BITS-1:0] #0.1 r_id_delay = r_id;
  wire [`MEM_DATA_BITS-1:0] #0.1 r_data_delay = r_data;
  wire #0.1 r_last_delay = r_last;

  wire #0.1 b_valid_delay = b_valid;
  wire b_ready_delay; assign #0.1 b_ready = b_ready_delay;
  wire [1:0] #0.1 b_resp_delay = b_resp;
  wire [`MEM_ID_BITS-1:0] #0.1 b_id_delay = b_id;

  wire #0.1 mem_bk_out_ready_delay = mem_bk_out_ready;
  wire #0.1 mem_bk_in_valid_delay = mem_bk_in_valid;
  wire mem_bk_out_valid_delay; assign #0.1 mem_bk_out_valid = mem_bk_out_valid_delay;

`ifdef FPGA
  assign mem_bk_out_valid_delay = 1'b0;
  assign htif_out_stats_delay = 1'b0;
`endif

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
    .io_host_debug_stats_csr(htif_out_stats_delay),

`ifdef MEM_BACKUP_EN
    .io_mem_backup_ctrl_en(1'b1),
`else
    .io_mem_backup_ctrl_en(1'b0),
`endif // MEM_BACKUP_EN
    .io_mem_backup_ctrl_in_valid(mem_bk_in_valid_delay),
    .io_mem_backup_ctrl_out_ready(mem_bk_out_ready_delay),
    .io_mem_backup_ctrl_out_valid(mem_bk_out_valid_delay),
`else
    .io_host_clk (),
    .io_host_clk_edge (),
    .io_host_debug_stats_csr (),

    .io_mem_backup_ctrl_en (1'b0),
    .io_mem_backup_ctrl_in_valid (1'b0),
    .io_mem_backup_ctrl_out_ready (1'b0),
    .io_mem_backup_ctrl_out_valid (),
`endif // FPGA

    .io_mem_ar_valid (ar_valid_delay),
    .io_mem_ar_ready (ar_ready_delay),
    .io_mem_ar_bits_addr (ar_addr_delay),
    .io_mem_ar_bits_id (ar_id_delay),
    .io_mem_ar_bits_size (ar_size_delay),
    .io_mem_ar_bits_len (ar_len_delay),
    .io_mem_ar_bits_burst (),
    .io_mem_ar_bits_lock (),
    .io_mem_ar_bits_cache (),
    .io_mem_ar_bits_prot (),
    .io_mem_ar_bits_qos (),
    .io_mem_ar_bits_region (),
    .io_mem_ar_bits_user (),

    .io_mem_aw_valid (aw_valid_delay),
    .io_mem_aw_ready (aw_ready_delay),
    .io_mem_aw_bits_addr (aw_addr_delay),
    .io_mem_aw_bits_id (aw_id_delay),
    .io_mem_aw_bits_size (aw_size_delay),
    .io_mem_aw_bits_len (aw_len_delay),
    .io_mem_aw_bits_burst (),
    .io_mem_aw_bits_lock (),
    .io_mem_aw_bits_cache (),
    .io_mem_aw_bits_prot (),
    .io_mem_aw_bits_qos (),
    .io_mem_aw_bits_region (),
    .io_mem_aw_bits_user (),

    .io_mem_w_valid (w_valid_delay),
    .io_mem_w_ready (w_ready_delay),
    .io_mem_w_bits_strb (w_strb_delay),
    .io_mem_w_bits_data (w_data_delay),
    .io_mem_w_bits_last (w_last_delay),
    .io_mem_w_bits_user (),

    .io_mem_r_valid (r_valid_delay),
    .io_mem_r_ready (r_ready_delay),
    .io_mem_r_bits_resp (r_resp_delay),
    .io_mem_r_bits_id (r_id_delay),
    .io_mem_r_bits_data (r_data_delay),
    .io_mem_r_bits_last (r_last_delay),
    .io_mem_r_bits_user (1'b0),

    .io_mem_b_valid (b_valid_delay),
    .io_mem_b_ready (b_ready_delay),
    .io_mem_b_bits_resp (b_resp_delay),
    .io_mem_b_bits_id (b_id_delay),
    .io_mem_b_bits_user (1'b0)
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
      ar_ready <= 1'b0;
      aw_ready <= 1'b0;
      w_ready <= 1'b0;
      r_valid <= 1'b0;
      r_resp <= 2'b0;
      r_id <= {`MEM_ID_BITS {1'b0}};
      r_data <= {`MEM_DATA_BITS {1'b0}};
      r_last <= 1'b0;
      b_valid <= 1'b0;
      b_resp <= 2'b0;
      b_id <= {`MEM_ID_BITS {1'b0}};
    end
    else
    begin
      memory_tick
      (
        ar_valid,
        ar_ready,
        ar_addr,
        ar_id,
        ar_size,
        ar_len,

        aw_valid,
        aw_ready,
        aw_addr,
        aw_id,
        aw_size,
        aw_len,

        w_valid,
        w_ready,
        w_strb,
        w_data,
        w_last,

        r_valid,
        r_ready,
        r_resp,
        r_id,
        r_data,
        r_last,

        b_valid,
        b_ready,
        b_resp,
        b_id
      );
    end
  end

  wire mem_bk_req_valid, mem_bk_req_rw, mem_bk_req_data_valid;
  wire [`MEM_ID_BITS-1:0] mem_bk_req_tag;
  wire [`MEM_ADDR_BITS-1:0] mem_bk_req_addr;
  wire [`MEM_DATA_BITS-1:0] mem_bk_req_data_bits;
  wire mem_bk_req_ready, mem_bk_req_data_ready, mem_bk_resp_valid;
  wire [`MEM_ID_BITS-1:0]  mem_bk_resp_tag;
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
  assign mem_bk_out_ready = 1'b0; 
  assign mem_bk_in_valid = 1'b0;
  assign mem_in_bits = {`HTIF_WIDTH {1'b0}};   
  assign mem_bk_req_valid = 1'b0;
  assign mem_bk_req_ready = 1'b0;
  assign mem_bk_req_addr = {`MEM_ADDR_BITS {1'b0}}; 
  assign mem_bk_req_rw = 1'b0;
  assign mem_bk_req_tag = {`MEM_ID_BITS {1'b0}}; 
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
    if (verbose)
    begin
      if (ar_valid && ar_ready)
      begin
        $fdisplay(stderr, "MC: ar addr=%x", ar_addr);
      end
      if (aw_valid && aw_ready)
      begin
        $fdisplay(stderr, "MC: aw addr=%x", aw_addr);
      end
      if (w_valid && w_ready)
      begin
        $fdisplay(stderr, "MC: w data=%x", w_data);
      end
      if (r_valid && r_ready)
      begin
        $fdisplay(stderr, "MC: r data=%x", r_data);
      end
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
