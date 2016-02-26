// See LICENSE for license details.

`define ceilLog2(x) ( \
(x) > 2**30 ? 31 : \
(x) > 2**29 ? 30 : \
(x) > 2**28 ? 29 : \
(x) > 2**27 ? 28 : \
(x) > 2**26 ? 27 : \
(x) > 2**25 ? 26 : \
(x) > 2**24 ? 25 : \
(x) > 2**23 ? 24 : \
(x) > 2**22 ? 23 : \
(x) > 2**21 ? 22 : \
(x) > 2**20 ? 21 : \
(x) > 2**19 ? 20 : \
(x) > 2**18 ? 19 : \
(x) > 2**17 ? 18 : \
(x) > 2**16 ? 17 : \
(x) > 2**15 ? 16 : \
(x) > 2**14 ? 15 : \
(x) > 2**13 ? 14 : \
(x) > 2**12 ? 13 : \
(x) > 2**11 ? 12 : \
(x) > 2**10 ? 11 : \
(x) > 2**9 ? 10 : \
(x) > 2**8 ? 9 : \
(x) > 2**7 ? 8 : \
(x) > 2**6 ? 7 : \
(x) > 2**5 ? 6 : \
(x) > 2**4 ? 5 : \
(x) > 2**3 ? 4 : \
(x) > 2**2 ? 3 : \
(x) > 2**1 ? 2 : \
(x) > 2**0 ? 1 : 0)

`ifdef MEM_BACKUP_EN
module BackupMemory
(
  input         clk,
  input         reset,

  input                       mem_req_valid,
  output                      mem_req_ready,
  input                       mem_req_rw,
  input [`MIF_ADDR_BITS-1:0]  mem_req_addr,
  input [`MIF_TAG_BITS-1:0]   mem_req_tag,

  input                       mem_req_data_valid,
  output                      mem_req_data_ready,
  input [`MIF_DATA_BITS-1:0]  mem_req_data_bits,

  output reg                  mem_resp_valid,
  output reg [`MIF_DATA_BITS-1:0] mem_resp_data,
  output reg [`MIF_TAG_BITS-1:0] mem_resp_tag
);

  localparam DATA_CYCLES = 4;
  localparam DEPTH = 2*1024*1024;

  reg [`ceilLog2(DATA_CYCLES)-1:0] cnt;
  reg [`MIF_TAG_BITS-1:0] tag;
  reg state_busy, state_rw;
  reg [`MIF_ADDR_BITS-1:0] addr;

  reg [`MIF_DATA_BITS-1:0] ram [DEPTH-1:0];
  wire [`ceilLog2(DEPTH)-1:0] ram_addr = state_busy  ?         {addr[`ceilLog2(DEPTH/DATA_CYCLES)-1:0], cnt}
                                                     : {mem_req_addr[`ceilLog2(DEPTH/DATA_CYCLES)-1:0], cnt};
  wire do_read = mem_req_valid && mem_req_ready && !mem_req_rw || state_busy && !state_rw;
  wire do_write = mem_req_data_valid && mem_req_data_ready;

  initial
  begin : zero
    integer i;
    for (i = 0; i < DEPTH; i = i+1)
      ram[i] = 1'b0;
  end

  always @(posedge clk)
  begin
    if (reset)
      state_busy <= 1'b0;
    else if ((do_read || do_write) && cnt == DATA_CYCLES-1)
      state_busy <= 1'b0;
    else if (mem_req_valid && mem_req_ready)
      state_busy <= 1'b1;

    if (!state_busy && mem_req_valid)
    begin
      state_rw <= mem_req_rw;
      tag <= mem_req_tag;
      addr <= mem_req_addr;
    end

    if (reset)
      cnt <= 1'b0;
    else if(do_read || do_write)
      cnt <= cnt + 1'b1;

    if (do_write)
      ram[ram_addr] <= mem_req_data_bits;
    else
      mem_resp_data <= ram[ram_addr];

    if (reset)
      mem_resp_valid <= 1'b0;
    else
      mem_resp_valid <= do_read;

    mem_resp_tag <= state_busy ? tag : mem_req_tag;
  end

  assign mem_req_ready = !state_busy;
  assign mem_req_data_ready = state_busy && state_rw;

endmodule
`endif
