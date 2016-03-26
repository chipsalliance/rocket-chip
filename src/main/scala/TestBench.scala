// See LICENSE for license details.

package rocketchip

import Chisel._

object TestBenchGeneration extends FileSystemUtilities {
  def generateVerilogFragment(
    topModuleName: String, configClassName: String,
    nMemChannel: Int, chiselVersion: Int) = {

    // Chisel 3 uses $ during name expansion whereas Chisel 2 uses _
    require(chiselVersion == 2 || chiselVersion == 3)
    val delim = if (chiselVersion == 3) "$" else "_"

    // YUNSUP:
    // I originally wrote this using a 2d wire array, but of course Synopsys'
    // DirectC implementation totally chokes on it when the 2d array is
    // referenced by the first dimension: the wire shows up as a contiguous
    // bit collection on the DirectC side.  I had to individually define the
    // wires.

    val defs = s"""
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
"""
    val nasti_defs = (0 until nMemChannel) map { i => s"""
  wire ar_valid_$i;
  reg ar_ready_$i;
  wire [`MEM_ADDR_BITS-1:0] ar_addr_$i;
  wire [`MEM_ID_BITS-1:0] ar_id_$i;
  wire [2:0] ar_size_$i;
  wire [7:0] ar_len_$i;

  wire aw_valid_$i;
  reg aw_ready_$i;
  wire [`MEM_ADDR_BITS-1:0] aw_addr_$i;
  wire [`MEM_ID_BITS-1:0] aw_id_$i;
  wire [2:0] aw_size_$i;
  wire [7:0] aw_len_$i;

  wire w_valid_$i;
  reg w_ready_$i;
  wire [`MEM_STRB_BITS-1:0] w_strb_$i;
  wire [`MEM_DATA_BITS-1:0] w_data_$i;
  wire w_last_$i;

  reg r_valid_$i;
  wire r_ready_$i;
  reg [1:0] r_resp_$i;
  reg [`MEM_ID_BITS-1:0] r_id_$i;
  reg [`MEM_DATA_BITS-1:0] r_data_$i;
  reg r_last_$i;

  reg b_valid_$i;
  wire b_ready_$i;
  reg [1:0] b_resp_$i;
  reg [`MEM_ID_BITS-1:0] b_id_$i;

""" } mkString

    val delays = s"""
  wire htif_clk;
  wire htif_in_valid_delay;
  wire htif_in_ready_delay;
  wire [`HTIF_WIDTH-1:0] htif_in_bits_delay;

  wire htif_out_valid_delay;
  wire htif_out_ready_delay;
  wire [`HTIF_WIDTH-1:0] htif_out_bits_delay;
  
  wire htif_out_stats_delay;

  wire mem_bk_out_ready_delay;
  wire mem_bk_in_valid_delay;
  wire mem_bk_out_valid_delay;

  assign #0.1 htif_in_valid_delay = htif_in_valid;
  assign #0.1 htif_in_ready = htif_in_ready_delay;
  assign #0.1 htif_in_bits_delay = htif_in_bits;

  assign #0.1 htif_out_valid = htif_out_valid_delay;
  assign #0.1 htif_out_ready_delay = htif_out_ready;
  assign #0.1 htif_out_bits = htif_out_bits_delay;
  
  assign #0.1 htif_out_stats = htif_out_stats_delay;

  assign #0.1 mem_bk_out_ready_delay = mem_bk_out_ready;
  assign #0.1 mem_bk_in_valid_delay = mem_bk_in_valid;
  assign #0.1 mem_bk_out_valid = mem_bk_out_valid_delay;
"""

    val nasti_delays = (0 until nMemChannel) map { i => s"""
  wire ar_valid_delay_$i;
  wire ar_ready_delay_$i;
  wire [`MEM_ADDR_BITS-1:0] ar_addr_delay_$i;
  wire [`MEM_ID_BITS-1:0] ar_id_delay_$i;
  wire [2:0] ar_size_delay_$i;
  wire [7:0] ar_len_delay_$i;

  wire aw_valid_delay_$i;
  wire aw_ready_delay_$i;
  wire [`MEM_ADDR_BITS-1:0] aw_addr_delay_$i;
  wire [`MEM_ID_BITS-1:0] aw_id_delay_$i;
  wire [2:0] aw_size_delay_$i;
  wire [7:0] aw_len_delay_$i;

  wire w_valid_delay_$i;
  wire w_ready_delay_$i;
  wire [`MEM_STRB_BITS-1:0] w_strb_delay_$i;
  wire [`MEM_DATA_BITS-1:0] w_data_delay_$i;
  wire w_last_delay_$i;

  wire r_valid_delay_$i;
  wire r_ready_delay_$i;
  wire [1:0] r_resp_delay_$i;
  wire [`MEM_ID_BITS-1:0] r_id_delay_$i;
  wire [`MEM_DATA_BITS-1:0] r_data_delay_$i;
  wire r_last_delay_$i;

  wire b_valid_delay_$i;
  wire b_ready_delay_$i;
  wire [1:0] b_resp_delay_$i;
  wire [`MEM_ID_BITS-1:0] b_id_delay_$i;

  assign #0.1 ar_valid_$i = ar_valid_delay_$i;
  assign #0.1 ar_ready_delay_$i = ar_ready_$i;
  assign #0.1 ar_addr_$i = ar_addr_delay_$i;
  assign #0.1 ar_id_$i = ar_id_delay_$i;
  assign #0.1 ar_size_$i = ar_size_delay_$i;
  assign #0.1 ar_len_$i = ar_len_delay_$i;

  assign #0.1 aw_valid_$i = aw_valid_delay_$i;
  assign #0.1 aw_ready_delay_$i = aw_ready_$i;
  assign #0.1 aw_addr_$i = aw_addr_delay_$i;
  assign #0.1 aw_id_$i = aw_id_delay_$i;
  assign #0.1 aw_size_$i = aw_size_delay_$i;
  assign #0.1 aw_len_$i = aw_len_delay_$i;

  assign #0.1 w_valid_$i = w_valid_delay_$i;
  assign #0.1 w_ready_delay_$i = w_ready_$i;
  assign #0.1 w_strb_$i = w_strb_delay_$i;
  assign #0.1 w_data_$i = w_data_delay_$i;
  assign #0.1 w_last_$i = w_last_delay_$i;

  assign #0.1 r_valid_delay_$i = r_valid_$i;
  assign #0.1 r_ready_$i = r_ready_delay_$i;
  assign #0.1 r_resp_delay_$i = r_resp_$i;
  assign #0.1 r_id_delay_$i = r_id_$i;
  assign #0.1 r_data_delay_$i = r_data_$i;
  assign #0.1 r_last_delay_$i = r_last_$i;

  assign #0.1 b_valid_delay_$i = b_valid_$i;
  assign #0.1 b_ready_$i = b_ready_delay_$i;
  assign #0.1 b_resp_delay_$i = b_resp_$i;
  assign #0.1 b_id_delay_$i = b_id_$i;

""" } mkString

    val nasti_connections = (0 until nMemChannel) map { i => s"""
    .io${delim}mem${delim}${i}${delim}ar${delim}valid (ar_valid_delay_$i),
    .io${delim}mem${delim}${i}${delim}ar${delim}ready (ar_ready_delay_$i),
    .io${delim}mem${delim}${i}${delim}ar${delim}bits${delim}addr (ar_addr_delay_$i),
    .io${delim}mem${delim}${i}${delim}ar${delim}bits${delim}id (ar_id_delay_$i),
    .io${delim}mem${delim}${i}${delim}ar${delim}bits${delim}size (ar_size_delay_$i),
    .io${delim}mem${delim}${i}${delim}ar${delim}bits${delim}len (ar_len_delay_$i),
    .io${delim}mem${delim}${i}${delim}ar${delim}bits${delim}burst (),
    .io${delim}mem${delim}${i}${delim}ar${delim}bits${delim}lock (),
    .io${delim}mem${delim}${i}${delim}ar${delim}bits${delim}cache (),
    .io${delim}mem${delim}${i}${delim}ar${delim}bits${delim}prot (),
    .io${delim}mem${delim}${i}${delim}ar${delim}bits${delim}qos (),
    .io${delim}mem${delim}${i}${delim}ar${delim}bits${delim}region (),
    .io${delim}mem${delim}${i}${delim}ar${delim}bits${delim}user (),

    .io${delim}mem${delim}${i}${delim}aw${delim}valid (aw_valid_delay_$i),
    .io${delim}mem${delim}${i}${delim}aw${delim}ready (aw_ready_delay_$i),
    .io${delim}mem${delim}${i}${delim}aw${delim}bits${delim}addr (aw_addr_delay_$i),
    .io${delim}mem${delim}${i}${delim}aw${delim}bits${delim}id (aw_id_delay_$i),
    .io${delim}mem${delim}${i}${delim}aw${delim}bits${delim}size (aw_size_delay_$i),
    .io${delim}mem${delim}${i}${delim}aw${delim}bits${delim}len (aw_len_delay_$i),
    .io${delim}mem${delim}${i}${delim}aw${delim}bits${delim}burst (),
    .io${delim}mem${delim}${i}${delim}aw${delim}bits${delim}lock (),
    .io${delim}mem${delim}${i}${delim}aw${delim}bits${delim}cache (),
    .io${delim}mem${delim}${i}${delim}aw${delim}bits${delim}prot (),
    .io${delim}mem${delim}${i}${delim}aw${delim}bits${delim}qos (),
    .io${delim}mem${delim}${i}${delim}aw${delim}bits${delim}region (),
    .io${delim}mem${delim}${i}${delim}aw${delim}bits${delim}user (),

    .io${delim}mem${delim}${i}${delim}w${delim}valid (w_valid_delay_$i),
    .io${delim}mem${delim}${i}${delim}w${delim}ready (w_ready_delay_$i),
    .io${delim}mem${delim}${i}${delim}w${delim}bits${delim}strb (w_strb_delay_$i),
    .io${delim}mem${delim}${i}${delim}w${delim}bits${delim}data (w_data_delay_$i),
    .io${delim}mem${delim}${i}${delim}w${delim}bits${delim}last (w_last_delay_$i),
    .io${delim}mem${delim}${i}${delim}w${delim}bits${delim}user (),

    .io${delim}mem${delim}${i}${delim}r${delim}valid (r_valid_delay_$i),
    .io${delim}mem${delim}${i}${delim}r${delim}ready (r_ready_delay_$i),
    .io${delim}mem${delim}${i}${delim}r${delim}bits${delim}resp (r_resp_delay_$i),
    .io${delim}mem${delim}${i}${delim}r${delim}bits${delim}id (r_id_delay_$i),
    .io${delim}mem${delim}${i}${delim}r${delim}bits${delim}data (r_data_delay_$i),
    .io${delim}mem${delim}${i}${delim}r${delim}bits${delim}last (r_last_delay_$i),
    .io${delim}mem${delim}${i}${delim}r${delim}bits${delim}user (1'b0),

    .io${delim}mem${delim}${i}${delim}b${delim}valid (b_valid_delay_$i),
    .io${delim}mem${delim}${i}${delim}b${delim}ready (b_ready_delay_$i),
    .io${delim}mem${delim}${i}${delim}b${delim}bits${delim}resp (b_resp_delay_$i),
    .io${delim}mem${delim}${i}${delim}b${delim}bits${delim}id (b_id_delay_$i),
    .io${delim}mem${delim}${i}${delim}b${delim}bits${delim}user (1'b0),

""" } mkString

    val instantiation = s"""
`ifdef FPGA
  assign mem_bk_out_valid_delay = 1'b0;
  assign htif_out_stats_delay = 1'b0;
  assign htif_clk = clk;
`endif

  Top dut
  (
    .clk(clk),
    .reset(reset),

    $nasti_connections

`ifndef FPGA
    .io${delim}host${delim}clk(htif_clk),
    .io${delim}host${delim}clk_edge(),
    .io${delim}host${delim}debug_stats_csr(htif_out_stats_delay),

`ifdef MEM_BACKUP_EN
    .io${delim}mem_backup_ctrl${delim}en(1'b1),
`else
    .io${delim}mem_backup_ctrl${delim}en(1'b0),
`endif // MEM_BACKUP_EN
    .io${delim}mem_backup_ctrl${delim}in_valid(mem_bk_in_valid_delay),
    .io${delim}mem_backup_ctrl${delim}out_ready(mem_bk_out_ready_delay),
    .io${delim}mem_backup_ctrl${delim}out_valid(mem_bk_out_valid_delay),
`else
    .io${delim}host${delim}clk (),
    .io${delim}host${delim}clk_edge (),
    .io${delim}host${delim}debug_stats_csr (),

    .io${delim}mem_backup_ctrl${delim}en (1'b0),
    .io${delim}mem_backup_ctrl${delim}in_valid (1'b0),
    .io${delim}mem_backup_ctrl${delim}out_ready (1'b0),
    .io${delim}mem_backup_ctrl${delim}out_valid (),
`endif // FPGA

    .io${delim}host${delim}in${delim}valid(htif_in_valid_delay),
    .io${delim}host${delim}in${delim}ready(htif_in_ready_delay),
    .io${delim}host${delim}in${delim}bits(htif_in_bits_delay),
    .io${delim}host${delim}out${delim}valid(htif_out_valid_delay),
    .io${delim}host${delim}out${delim}ready(htif_out_ready_delay),
    .io${delim}host${delim}out${delim}bits(htif_out_bits_delay)
  );
"""

    val ticks = (0 until nMemChannel) map { i => s"""
  reg [31:0] channel_$i = $i;

  always @(posedge clk)
  begin
    if (reset || r_reset)
    begin
      ar_ready_$i <= 1'b0;
      aw_ready_$i <= 1'b0;
      w_ready_$i <= 1'b0;
      r_valid_$i <= 1'b0;
      r_resp_$i <= 2'b0;
      r_id_$i <= {`MEM_ID_BITS {1'b0}};
      r_data_$i <= {`MEM_DATA_BITS {1'b0}};
      r_last_$i <= 1'b0;
      b_valid_$i <= 1'b0;
      b_resp_$i <= 2'b0;
      b_id_$i <= {`MEM_ID_BITS {1'b0}};
    end
    else
    begin
      memory_tick
      (
        channel_$i,
        ar_valid_$i, ar_ready_$i, ar_addr_$i, ar_id_$i, ar_size_$i, ar_len_$i,
        aw_valid_$i, aw_ready_$i, aw_addr_$i, aw_id_$i, aw_size_$i, aw_len_$i,
        w_valid_$i, w_ready_$i, w_strb_$i, w_data_$i, w_last_$i,
        r_valid_$i, r_ready_$i, r_resp_$i, r_id_$i, r_data_$i, r_last_$i,
        b_valid_$i, b_ready_$i, b_resp_$i, b_id_$i
      );
    end
  end

  always @(posedge clk)
  begin
    if (verbose)
    begin
      if (ar_valid_$i && ar_ready_$i)
      begin
        $$fdisplay(stderr, "MC$i: ar addr=%x", ar_addr_$i);
      end
      if (aw_valid_$i && aw_ready_$i)
      begin
        $$fdisplay(stderr, "MC$i: aw addr=%x", aw_addr_$i);
      end
      if (w_valid_$i && w_ready_$i)
      begin
        $$fdisplay(stderr, "MC$i: w data=%x", w_data_$i);
      end
      if (r_valid_$i && r_ready_$i)
      begin
        $$fdisplay(stderr, "MC$i: r data=%x", r_data_$i);
      end
    end
  end
""" } mkString

    val f = createOutputFile(s"$topModuleName.$configClassName.tb.vfrag")
    f.write(defs + nasti_defs + delays + nasti_delays + instantiation + ticks)
    f.close
  }

  def generateCPPFragment(
      topModuleName: String, configClassName: String, nMemChannel: Int) {

    val assigns = (0 until nMemChannel).map { i => s"""
      mem_ar_valid[$i] = &tile.Top__io_mem_${i}_ar_valid;
      mem_ar_ready[$i] = &tile.Top__io_mem_${i}_ar_ready;
      mem_ar_bits_addr[$i] = &tile.Top__io_mem_${i}_ar_bits_addr;
      mem_ar_bits_id[$i] = &tile.Top__io_mem_${i}_ar_bits_id;
      mem_ar_bits_size[$i] = &tile.Top__io_mem_${i}_ar_bits_size;
      mem_ar_bits_len[$i] = &tile.Top__io_mem_${i}_ar_bits_len;

      mem_aw_valid[$i] = &tile.Top__io_mem_${i}_aw_valid;
      mem_aw_ready[$i] = &tile.Top__io_mem_${i}_aw_ready;
      mem_aw_bits_addr[$i] = &tile.Top__io_mem_${i}_aw_bits_addr;
      mem_aw_bits_id[$i] = &tile.Top__io_mem_${i}_aw_bits_id;
      mem_aw_bits_size[$i] = &tile.Top__io_mem_${i}_aw_bits_size;
      mem_aw_bits_len[$i] = &tile.Top__io_mem_${i}_aw_bits_len;

      mem_w_valid[$i] = &tile.Top__io_mem_${i}_w_valid;
      mem_w_ready[$i] = &tile.Top__io_mem_${i}_w_ready;
      mem_w_bits_data[$i] = &tile.Top__io_mem_${i}_w_bits_data;
      mem_w_bits_strb[$i] = &tile.Top__io_mem_${i}_w_bits_strb;
      mem_w_bits_last[$i] = &tile.Top__io_mem_${i}_w_bits_last;

      mem_b_valid[$i] = &tile.Top__io_mem_${i}_b_valid;
      mem_b_ready[$i] = &tile.Top__io_mem_${i}_b_ready;
      mem_b_bits_resp[$i] = &tile.Top__io_mem_${i}_b_bits_resp;
      mem_b_bits_id[$i] = &tile.Top__io_mem_${i}_b_bits_id;

      mem_r_valid[$i] = &tile.Top__io_mem_${i}_r_valid;
      mem_r_ready[$i] = &tile.Top__io_mem_${i}_r_ready;
      mem_r_bits_resp[$i] = &tile.Top__io_mem_${i}_r_bits_resp;
      mem_r_bits_id[$i] = &tile.Top__io_mem_${i}_r_bits_id;
      mem_r_bits_data[$i] = &tile.Top__io_mem_${i}_r_bits_data;
      mem_r_bits_last[$i] = &tile.Top__io_mem_${i}_r_bits_last;

    """ }.mkString

    val f = createOutputFile(s"$topModuleName.$configClassName.tb.cpp")
    f.write(assigns)
    f.close
  }
}
