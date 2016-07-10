// See LICENSE for license details.

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

module rocketDTMTestHarness;

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
  integer      stderr = 32'h80000002;

`include `TBVFRAG

   
   
  always @(posedge clk)
  begin
    r_reset <= reset;
  end

  reg [31:0] exit = 0;

   //-----------------------------------------------
  // Instantiate DTM and Synchronizers
                
  // JTAG Interface

   wire       debug_TDI;
   wire       debug_TDO;
   wire       debug_TCK;
   wire       debug_TMS;
   wire       debug_TRST;
   wire       debug_DRV_TDO;

   //=================================================
   // JTAG VPI Server

   wire      cheater_TCK;
   
   
   jtag_vpi #(	
        .DEBUG_INFO(0),
	//parameter TP = 1,
	.TCK_HALF_PERIOD(5),
        .CMD_DELAY(10)        
)   // Clock half period (Clock period = 10 ns => 100 MHz)
   jtag_vpi (
	.tms(debug_TMS),
	.tck(debug_TCK),
	.tdi(debug_TDI),
	.tdo(debug_TDO),
	.enable(~reset),
	.init_done(~reset)
 );

             
   //=================================================
   // DTM <-> Synchronizers Interface 
  
   localparam DEBUG_ADDR_BITS = 5;
   localparam DEBUG_DATA_BITS = 34;
   localparam DEBUG_OP_BITS = 2;

   
   wire      dtm_req_ready;
   wire      dtm_req_valid;
   wire [DEBUG_OP_BITS + DEBUG_ADDR_BITS + DEBUG_DATA_BITS - 1 : 0 ] dtm_req_data;
   
   wire                                                              dtm_resp_ready;
   wire                                                              dtm_resp_valid;
   wire [DEBUG_OP_BITS + DEBUG_DATA_BITS - 1 :0 ]                    dtm_resp_data;
   
   
   DebugTransportModuleJtag #(.DEBUG_OP_BITS(DEBUG_OP_BITS),
                                 .DEBUG_ADDR_BITS(DEBUG_ADDR_BITS),
                                 .DEBUG_DATA_BITS(DEBUG_DATA_BITS)
                                 ) debugTransportModuleJtag0 (
                                                              
                                                           //JTAG Interface
                                                           
                                                              .TDI(debug_TDI),
                                                              .TDO(debug_TDO),
                                                              .TCK(debug_TCK),
                                                              .TMS(debug_TMS),
                                                              .TRST(debug_TRST),

                                                              .DRV_TDO(debug_DRV_TDO),
                                                              
                                                              .dtm_req_ready(dtm_req_ready),
                                                              .dtm_req_valid(dtm_req_valid),
                                                              .dtm_req_data(dtm_req_data),
                                                              
                                                              .dtm_resp_ready(dtm_resp_ready),
                                                              .dtm_resp_valid(dtm_resp_valid),
                                                              .dtm_resp_data(dtm_resp_data)
                                                                                                        
                                                              );
  
`ifdef VERILOG_DEBUG_SYNC
   
  AsyncFifo #(.DEPTH_LG_2(0),
                  .WIDTH(DEBUG_OP_BITS + DEBUG_ADDR_BITS + DEBUG_DATA_BITS))
      debugBusReqFifo(
                      // Write Interface
                      
                      .clk_w(~debug_TCK),
                      .reset_w(debug_TRST),
                      .ready_w(dtm_req_ready), 
                      .valid_w(dtm_req_valid), 
                      .data_w(dtm_req_data),   

                      .clk_r(clk),
                      .reset_r(reset),
                      .ready_r(debug_req_ready), 
                      .valid_r(debug_req_valid),
                      .data_r({debug_req_bits_addr, debug_req_bits_data, debug_req_bits_op})
                      
                      );
      
      AsyncFifo #(.DEPTH_LG_2(0),
                  .WIDTH(DEBUG_OP_BITS + DEBUG_DATA_BITS))
                  debugBusRespFifo(
                                   .clk_w(clk),
                                   .reset_w(reset),
                                   .ready_w(debug_resp_ready),
                                   .valid_w(debug_resp_valid),
                                   .data_w({debug_resp_bits_data, debug_resp_bits_resp}),

                                   .clk_r(debug_TCK),
                                   .reset_r(debug_TRST),
                                   .ready_r(dtm_resp_ready),
                                   .valid_r(dtm_resp_valid),
                                   .data_r(dtm_resp_data) 
                                   );

   
   // This is cheating / potentially incorrect!!! It needs to be more
   // clearly specified as to what the behavior of TRST is.
   assign debug_TRST = reset;

   // The TCK cheat is not needed for this side of the ifdef
   //  because both DTM and synchronizer
   // logic is asynchronously reset as needed.
   
`else // !`ifdef VERILOG_DEBUG_SYNC
  
   // This is cheating / potentially incorrect!!! It needs to be more
   // clearly specified as to what the behavior of TRST is.
   assign debug_TRST = reset;

   // This is TOTAL cheating!!! The synchronizer
   // logic should be asynchronously reset, or we should
   // specify the TCK/TRST requirements for a synchronous reset.
   assign debug_clk = reset ? clk : debug_TCK;

   assign debug_reset = debug_TRST;
      
   assign debug_req_valid     = dtm_req_valid;
   assign {debug_req_bits_addr, debug_req_bits_data, debug_req_bits_op} = dtm_req_data;
   
   assign dtm_req_ready = debug_req_ready;

   assign dtm_resp_valid     = debug_resp_valid;
   assign dtm_resp_data = {debug_resp_bits_data, debug_resp_bits_resp};

   assign debug_resp_ready    = dtm_resp_ready;
      
`endif // !`ifdef VERILOG_DEBUG_SYNC
   
   

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
