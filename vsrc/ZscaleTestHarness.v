// See LICENSE for license details.
//
module ZscaleTestHarness;

  reg clk   = 0;
  reg reset = 1;

  always #`CLOCK_PERIOD clk = ~clk;

  wire        csr_resp_valid;
  wire [31:0] dummy;
  wire [31:0] csr_resp_bits;

  ZscaleTop dut
  (
    .clk(clk),
    .reset(reset),
    .io_host_reset(reset),

    .io_host_id(1'd0),
    .io_host_csr_req_ready(),
    .io_host_csr_req_valid(1'b1),
    .io_host_csr_req_bits_rw(1'b0),
    .io_host_csr_req_bits_addr(12'h780), // tohost register
    .io_host_csr_req_bits_data({dummy, 32'd0}),
    .io_host_csr_resp_ready(1'b1),
    .io_host_csr_resp_valid(csr_resp_valid),
    .io_host_csr_resp_bits({dummy, csr_resp_bits})
  );

  reg [1023:0] loadmem = 0;
  reg [1023:0] vcdplusfile = 0;
  reg [  63:0] max_cycles = 0;
  reg [  63:0] trace_count = 0;
  reg          verbose = 0;
  wire         printf_cond = verbose && !reset;
  integer      stderr = 32'h80000002;
  integer      i;
  reg [127:0]  image [8191:0];

  initial
  begin
    $value$plusargs("max-cycles=%d", max_cycles);
    verbose = $test$plusargs("verbose");
    if ($value$plusargs("loadmem=%s", loadmem))
    begin
      $readmemh(loadmem, image);
    end
    if ($value$plusargs("vcdplusfile=%s", vcdplusfile))
    begin
      $vcdplusfile(vcdplusfile);
      $vcdpluson(0);
      $vcdplusmemon(0);
    end

    #0.5;
    for (i=0; i<`BOOT_CAPACITY/16; i=i+1) begin
      dut.bootmem.ram.ram[4*i+0] = image[i][31:0];
      dut.bootmem.ram.ram[4*i+1] = image[i][63:32];
      dut.bootmem.ram.ram[4*i+2] = image[i][95:64];
      dut.bootmem.ram.ram[4*i+3] = image[i][127:96];
    end

    #777.7 reset = 0;
  end

  reg [255:0] reason = 0;
  always @(posedge clk)
  begin
    trace_count = trace_count + 1;

    if (max_cycles > 0 && trace_count > max_cycles)
      reason = "timeout";

    if (!reset)
    begin
      if (csr_resp_valid && csr_resp_bits > 1)
        $sformat(reason, "tohost = %d", csr_resp_bits >> 1);

      if (csr_resp_valid && csr_resp_bits == 1)
      begin
        $vcdplusclose;
        $finish;
      end
    end

    if (reason)
    begin
      $fdisplay(stderr, "*** FAILED *** (%s) after %d simulation cycles", reason, trace_count);
      $vcdplusclose;
      $finish;
    end
  end

endmodule
