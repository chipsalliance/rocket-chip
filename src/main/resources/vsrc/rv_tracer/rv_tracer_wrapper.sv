module rv_tracer_wrapper #(
  parameter N = 1
) (
  input logic clk_i,
  input logic rst_ni,
  input logic enable_i,
  input logic config_valid_i,
  input logic [7:0] config_addr_i,
  input logic [31:0] config_data_i,
  output logic config_ready_o,
  input logic [N-1:0] valid_i,
  input logic [N-1:0][3:0] itype_i,
  input logic [63:0] cause_i,
  input logic [63:0] tval_i,
  input logic [1:0] priv_i,
  input logic [N-1:0][63:0] iaddr_i,
  input logic [N-1:0][31:0] iretire_i,
  input logic [N-1:0] ilastsize_i,
  input logic [63:0] time_i,
  input logic encapsulator_ready_i,
  input logic external_enable_i,
  output logic [N-1:0] packet_valid_o,
  output te_pkg::it_packet_type_e [N-1:0] packet_type_o,
  output logic [N-1:0][te_pkg::P_LEN-1:0] packet_length_o,
  output logic [N-1:0][te_pkg::PAYLOAD_LEN-1:0] packet_payload_o,
  output logic stall_o
);
  logic [31:0] paddr;
  logic pwrite, psel, penable;
  logic [31:0] pwdata;
  logic pready;
  logic [31:0] prdata;
  logic programmed_enable;
  logic apb_access;
  logic config_pending;
  logic [7:0] config_addr_q;
  logic [31:0] config_data_q;
  assign paddr = config_pending ? config_addr_q : te_pkg::TRACE_STATE;
  assign pwrite = 1'b1;
  assign psel = apb_access || config_pending || (enable_i != programmed_enable);
  assign penable = apb_access;
  assign pwdata = config_pending ? config_data_q : {31'b0, enable_i};
  // Configuration writes are independent of the trace enable transition.
  // Requiring enable_i == programmed_enable here deadlocks a legal sequence
  // that enables tracing and then programs the M-mode filter: the controller
  // waits for config_ready while the wrapper waits to complete its implicit
  // TRACE_STATE APB transaction.  A pending APB transfer is the only state
  // that must back-pressure the decoupled request.
  assign config_ready_o = !config_pending && !apb_access;

  // te_reg requires APB setup followed by access.  Program trace activation
  // only when the Chipyard controller changes its enable bit.
  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (!rst_ni) begin
      programmed_enable <= 1'b0;
      apb_access <= 1'b0;
      config_pending <= 1'b0;
      config_addr_q <= '0;
      config_data_q <= '0;
    end else if (config_valid_i && config_ready_o) begin
      config_pending <= 1'b1;
      config_addr_q <= config_addr_i;
      config_data_q <= config_data_i;
    end else if (!apb_access && (config_pending || (enable_i != programmed_enable))) begin
      apb_access <= 1'b1;
    end else if (apb_access) begin
      apb_access <= 1'b0;
      if (config_pending) config_pending <= 1'b0;
      else programmed_enable <= enable_i;
    end
  end

  rv_tracer #(.N(N), .ONLY_BRANCHES(0)) i_rv_tracer (
    .clk_i(clk_i), .rst_ni(rst_ni), .valid_i(valid_i), .itype_i(itype_i),
    .cause_i(cause_i), .tval_i(tval_i), .priv_i(priv_i), .iaddr_i(iaddr_i),
    .iretire_i(iretire_i), .ilastsize_i(ilastsize_i), .time_i(time_i),
    .tvec_i('0), .epc_i('0), .encapsulator_ready_i(encapsulator_ready_i),
    .external_enable_i(external_enable_i),
    .paddr_i(paddr), .pwrite_i(pwrite), .psel_i(psel), .penable_i(penable),
    .pwdata_i(pwdata), .packet_valid_o(packet_valid_o),
    .packet_type_o(packet_type_o), .packet_length_o(packet_length_o),
    .packet_payload_o(packet_payload_o), .stall_o(stall_o),
    .pready_o(pready), .prdata_o(prdata)
  );

`ifndef SYNTHESIS
  integer dbg_valid_count;
  integer dbg_packet_count;
  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (!rst_ni) begin
      dbg_valid_count <= 0;
      dbg_packet_count <= 0;
    end else begin
      if (|valid_i && dbg_valid_count < 8) begin
        dbg_valid_count <= dbg_valid_count + 1;
        $display("PULP_RTL_VALID priv=%0d pc=%h retire=%h enable=%b ext=%b", priv_i,
                 iaddr_i[0], iretire_i[0], enable_i, external_enable_i);
      end
      if (|packet_valid_o && dbg_packet_count < 8) begin
        dbg_packet_count <= dbg_packet_count + 1;
        $display("PULP_RTL_PACKET len=%0d payload=%h", packet_length_o[0], packet_payload_o[0]);
      end
    end
  end
`endif
endmodule

// Chisel's BlackBox class uses this stable name in generated RocketTile RTL.
// Keep the implementation above as the native wrapper and provide a thin
// compatibility shell for simulators and downstream integrations.
module PulpRvTracerBlackBox #(
  parameter N = 1
) (
  input logic clk_i,
  input logic rst_ni,
  input logic enable_i,
  input logic config_valid_i,
  input logic [7:0] config_addr_i,
  input logic [31:0] config_data_i,
  output logic config_ready_o,
  input logic [N-1:0] valid_i,
  input logic [3:0] itype_i_0,
  input logic [63:0] cause_i,
  input logic [63:0] tval_i,
  input logic [1:0] priv_i,
  input logic [63:0] iaddr_i_0,
  input logic [31:0] iretire_i_0,
  input logic [N-1:0] ilastsize_i,
  input logic [63:0] time_i,
  input logic encapsulator_ready_i,
  input logic external_enable_i,
  output logic [N-1:0] packet_valid_o,
  output logic [3:0] packet_type_o_0,
  output logic [4:0] packet_length_o_0,
  output logic [247:0] packet_payload_o_0,
  output logic stall_o
);

  logic [N-1:0][3:0] itype_vec;
  logic [N-1:0][63:0] iaddr_vec;
  logic [N-1:0][31:0] iretire_vec;
  logic [N-1:0][3:0] packet_type_vec;
  logic [N-1:0][4:0] packet_length_vec;
  logic [N-1:0][247:0] packet_payload_vec;
  assign itype_vec[0] = itype_i_0;
  assign iaddr_vec[0] = iaddr_i_0;
  assign iretire_vec[0] = iretire_i_0;
  assign packet_type_o_0 = packet_type_vec[0];
  assign packet_length_o_0 = packet_length_vec[0];
  assign packet_payload_o_0 = packet_payload_vec[0];
  rv_tracer_wrapper #(.N(N)) impl (
    .clk_i, .rst_ni, .enable_i, .config_valid_i, .config_addr_i, .config_data_i,
    .config_ready_o, .valid_i, .itype_i(itype_vec), .cause_i, .tval_i,
    .priv_i, .iaddr_i(iaddr_vec), .iretire_i(iretire_vec), .ilastsize_i, .time_i,
    .encapsulator_ready_i, .external_enable_i, .packet_valid_o, .packet_type_o(packet_type_vec),
    .packet_length_o(packet_length_vec), .packet_payload_o(packet_payload_vec), .stall_o
  );
endmodule
