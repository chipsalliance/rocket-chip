// Copyright 2025 ETH Zurich and University of Bologna.
// Copyright and related rights are licensed under the Solderpad Hardware
// License, Version 0.51 (the "License"); you may not use this file except in
// compliance with the License.  You may obtain a copy of the License at
// http://solderpad.org/licenses/SHL-0.51. Unless required by applicable law
// or agreed to in writing, software, hardware and materials distributed under
// this License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
// CONDITIONS OF ANY KIND, either express or implied. See the License for the
// specific language governing permissions and limitations under the License.
// SPDX-License-Identifier: SHL-0.51

// Author:  Umberto Laghi
// Contact: umberto.laghi2@unibo.it
// Github:  @ubolakes

/*REG*/
/*
it stores values for the encoder in memory mapped registers
*/

// uncomment to enable 64bits arch support
`define TE_ARCH64

module te_reg #(
    parameter APB_ADDR_WIDTH = 32 // it is the max allowed, maybe change to a smaller value
)
(
    input logic                             clk_i,
    input logic                             rst_ni,

    // tracing management
    input logic                             trace_req_off_i, // from filter
    input logic                             trace_req_on_i, // directly from trigger unit
    input logic                             encapsulator_ready_i,

    // filter settings
    output logic                            cause_filter_o,
    output logic [te_pkg::XLEN-1:0]         upper_cause_o,
    output logic [te_pkg::XLEN-1:0]         lower_cause_o,
    output logic [te_pkg::XLEN-1:0]         match_cause_o,
    output logic                            cause_mode_o,
    output logic                            tvec_filter_o,
    output logic [te_pkg::XLEN-1:0]         upper_tvec_o,
    output logic [te_pkg::XLEN-1:0]         lower_tvec_o,
    output logic [te_pkg::XLEN-1:0]         match_tvec_o,
    output logic                            tvec_mode_o,
    output logic                            tval_filter_o,
    output logic [te_pkg::XLEN-1:0]         upper_tval_o,
    output logic [te_pkg::XLEN-1:0]         lower_tval_o,
    output logic [te_pkg::XLEN-1:0]         match_tval_o,
    output logic                            tval_mode_o,
    output logic                            priv_lvl_filter_o,
    output logic [te_pkg::PRIV_LEN-1:0]     upper_priv_o,
    output logic [te_pkg::PRIV_LEN-1:0]     lower_priv_o,
    output logic [te_pkg::PRIV_LEN-1:0]     match_priv_o,
    output logic                            priv_lvl_mode_o,
    output logic                            iaddr_filter_o,
    output logic [te_pkg::XLEN-1:0]         upper_iaddr_o,
    output logic [te_pkg::XLEN-1:0]         lower_iaddr_o,
    output logic [te_pkg::XLEN-1:0]         match_iaddr_o,
    output logic                            iaddr_mode_o,

    output logic                            trace_enable_o,    // turned off by filter
    output logic                            trace_activated_o, // managed by user
    // packet_emitter settings and control
    output logic                            nocontext_o,
    output logic                            notime_o,
    output logic                            encoder_mode_o, // hardwired to 0 - can only be 0 according to spec
    output te_pkg::ioptions_s               configuration_o,
    output logic                            lossless_trace_o,
    output logic                            shallow_trace_o,

    output logic                            clk_gated_o,

    // APB I/O interface
    input logic [APB_ADDR_WIDTH-1:0]    paddr_i,
    input logic                         pwrite_i,
    input logic                         psel_i,
    input logic                         penable_i,
    input logic [31:0]                  pwdata_i,
    output logic                        pready_o,
    output logic [31:0]                 prdata_o
);

    // FFs I/Os
    logic   trace_enable_d, trace_enable_q;
    // trace enabling
    logic   trace_req_off, trace_req_on;
    logic   turn_on, turn_off;
    logic   clk_gated;
    logic   test_enabled;

    // registers I/O
    logic                           cause_filter_d, cause_filter_q;
    logic [te_pkg::XLEN-1:0]        upper_cause_d, upper_cause_q;
    logic [te_pkg::XLEN-1:0]        lower_cause_d, lower_cause_q;
    logic [te_pkg::XLEN-1:0]        match_cause_d, match_cause_q;
    logic                           cause_mode_d, cause_mode_q;
    logic                           tvec_filter_d, tvec_filter_q;
    logic [te_pkg::XLEN-1:0]        upper_tvec_d, upper_tvec_q;
    logic [te_pkg::XLEN-1:0]        lower_tvec_d, lower_tvec_q;
    logic [te_pkg::XLEN-1:0]        match_tvec_d, match_tvec_q;
    logic                           tvec_mode_d, tvec_mode_q;
    logic                           tval_filter_d, tval_filter_q;
    logic [te_pkg::XLEN-1:0]        upper_tval_d, upper_tval_q;
    logic [te_pkg::XLEN-1:0]        lower_tval_d, lower_tval_q;
    logic [te_pkg::XLEN-1:0]        match_tval_d, match_tval_q;
    logic                           tval_mode_d, tval_mode_q;
    logic                           priv_lvl_filter_d, priv_lvl_filter_q;
    logic [te_pkg::PRIV_LEN-1:0]    upper_priv_d, upper_priv_q;
    logic [te_pkg::PRIV_LEN-1:0]    lower_priv_d, lower_priv_q;
    logic [te_pkg::PRIV_LEN-1:0]    match_priv_d, match_priv_q;
    logic                           priv_lvl_mode_d, priv_lvl_mode_q;
    logic                           iaddr_filter_d, iaddr_filter_q;
    logic [te_pkg::XLEN-1:0]        upper_iaddr_d, upper_iaddr_q;
    logic [te_pkg::XLEN-1:0]        lower_iaddr_d, lower_iaddr_q;
    logic [te_pkg::XLEN-1:0]        match_iaddr_d, match_iaddr_q;
    logic                           iaddr_mode_d, iaddr_mode_q;
    logic                           trace_activated_d, trace_activated_q;
    logic                           nocontext_d, nocontext_q;
    logic                           notime_d, notime_q;
    te_pkg::ioptions_s              configuration_d, configuration_q;
    logic                           lossless_trace_d, lossless_trace_q;
    logic                           shallow_trace_d, shallow_trace_q;

    // APB transaction
    logic read_access;
    logic write_access;
    logic setup_d, setup_q;

    // assignments
    // filter
    assign cause_filter_o = cause_filter_q;
    assign upper_cause_o = upper_cause_q;
    assign lower_cause_o = lower_cause_q;
    assign match_cause_o = match_cause_q;
    assign cause_mode_o = cause_mode_q;
    assign tvec_filter_o = tvec_filter_q;
    assign upper_tvec_o = upper_tvec_q;
    assign lower_tvec_o = lower_tvec_q;
    assign match_tvec_o = match_tvec_q;
    assign tvec_mode_o = tvec_mode_q;
    assign tval_filter_o = tval_filter_q;
    assign upper_tval_o = upper_tval_q;
    assign lower_tval_o = lower_tval_q;
    assign match_tval_o = match_tval_q;
    assign tval_mode_o = tval_mode_q;
    assign priv_lvl_filter_o = priv_lvl_filter_q;
    assign upper_priv_o = upper_priv_q;
    assign lower_priv_o = lower_priv_q;
    assign match_priv_o = match_priv_q;
    assign priv_lvl_mode_o = priv_lvl_mode_q;
    assign iaddr_filter_o = iaddr_filter_q;
    assign upper_iaddr_o = upper_iaddr_q;
    assign lower_iaddr_o = lower_iaddr_q;
    assign match_iaddr_o = match_iaddr_q;
    assign iaddr_mode_o = iaddr_mode_q;
    // packet emitter
    assign configuration_o = configuration_q;
    assign lossless_trace_o = lossless_trace_q; // if == 1 stalls the core when the encapsulator buffer is full
    assign shallow_trace_o = shallow_trace_q; // if == 1 flushes the branch map at each packet emitted
    assign nocontext_o = nocontext_q;
    assign notime_o = notime_q;
    assign encoder_mode_o = '0; // hardwired
    assign trace_activated_o = trace_activated_q;
    
    // tracing is switched on only when it's not enabled anc a request of turning on is received
    assign turn_on = (trace_enable_q == 0) && (trace_req_on /*|| encapsulator_ready_i*/); // encapsulator signal is temporarely disabled
    // tracing is switched off only when it's not enabled anc a request of turning off is received
    assign turn_off = (trace_enable_q == 1) && (trace_req_off /*|| ~encapsulator_ready_i*/);
    // the toggle of trace_enable value happens only when turn off or turn off is asserted
    assign trace_enable_d = (turn_off || turn_on) ? ~trace_enable_q : trace_enable_q;
    assign trace_enable_o = trace_enable_d;

    assign clk_gated_o = clk_gated;
    assign test_enabled = '0;

    assign setup_d = psel_i && ~penable_i;
    assign pready_o = setup_q && psel_i && penable_i;
    assign read_access = pready_o && !pwrite_i;
    assign write_access = pready_o && pwrite_i;

    /* APB read and write */
    always_comb begin
        // initialization
        // set _d as _q, because we don't to erase them all
        cause_filter_d = cause_filter_q;
        cause_mode_d = cause_mode_q;
        tvec_filter_d = tvec_filter_q;
        tvec_mode_d = tvec_mode_q;
        tval_filter_d = tval_filter_q;
        tval_mode_d = tval_mode_q;
        priv_lvl_filter_d = priv_lvl_filter_q;
        priv_lvl_mode_d = priv_lvl_mode_q;
        iaddr_filter_d = iaddr_filter_q;
        iaddr_mode_d = iaddr_mode_q;
        lower_priv_d = lower_priv_q;
        upper_priv_d = upper_priv_q;
        match_priv_d = match_priv_q;
        trace_activated_d = trace_activated_q;
        lossless_trace_d = lossless_trace_q;
        shallow_trace_d = shallow_trace_q;
        notime_d = notime_q;
        nocontext_d = nocontext_q;
        configuration_d = configuration_q;
        upper_cause_d = upper_cause_q;
        lower_cause_d = lower_cause_q;
        match_cause_d = match_cause_q;
        upper_tvec_d = upper_tvec_q;
        lower_tvec_d = lower_tvec_q;
        match_tvec_d = match_tvec_q;
        upper_tval_d = upper_tval_q;
        lower_tval_d = lower_tval_q;
        match_tval_d = match_tval_q;
        upper_iaddr_d = upper_iaddr_q;
        lower_iaddr_d = lower_iaddr_q;
        match_iaddr_d = match_iaddr_q;
        prdata_o = '0; // clean output

        if (read_access) begin
            case (paddr_i[7:0])
            // XLEN agnostic (or always <= than 32)
            // FILTER
            // enable and mode
            te_pkg::CAUSE_ENABLE_MODE: begin
                prdata_o[0] = cause_filter_q;
                prdata_o[1] = cause_mode_q;
            end

            te_pkg::TVEC_ENABLE_MODE: begin
                prdata_o[0] = tvec_filter_q;
                prdata_o[1] = tvec_mode_q;
            end

            te_pkg::TVAL_ENABLE_MODE: begin
                prdata_o[0] = tval_filter_q;
                prdata_o[1] = tval_mode_q;
            end

            te_pkg::PRIV_ENABLE_MODE: begin
                prdata_o[0] = priv_lvl_filter_q;
                prdata_o[1] = priv_lvl_mode_q;
            end

            te_pkg::IADDR_ENABLE_MODE: begin
                prdata_o[0] = iaddr_filter_q;
                prdata_o[1] = iaddr_mode_q;
            end

            // priv
            te_pkg::PRIV_RANGE: begin
                prdata_o[te_pkg::PRIV_LEN-1:0] = lower_priv_q;
                prdata_o[te_pkg::PRIV_LEN*2-1:te_pkg::PRIV_LEN] = upper_priv_q;
            end
        
            te_pkg::PRIV_MATCH: begin
                prdata_o[te_pkg::PRIV_LEN-1:0] = match_priv_q;
            end

            // TRACE MANAGEMENT
            te_pkg::TRACE_STATE: begin
                prdata_o[0] = trace_activated_q;
            end
        
            te_pkg::LOSSLESS_TRACE: begin
                prdata_o[0] = lossless_trace_q;
            end

            te_pkg::SHALLOW_TRACE: begin
                prdata_o[0] = shallow_trace_q;
            end

            // PACKET EMITTER
            te_pkg::NO_TIME: begin
                prdata_o[0] = notime_q;
            end

            te_pkg::NO_CONTEXT: begin
                prdata_o[0] = nocontext_q;
            end

            te_pkg::DELTA_ADDRESS: begin
                prdata_o[0] = configuration_q.delta_address_en;
            end
            te_pkg::FULL_ADDRESS: begin
                prdata_o[0] = configuration_q.full_address_en;
            end
            te_pkg::IMPLICIT_EXCEPTION: begin
                prdata_o[0] = configuration_q.implicit_exception_en;
            end
            te_pkg::SIJUMP: begin
                prdata_o[0] = configuration_q.sijump_en;
            end
            te_pkg::IMPLICIT_RETURN: begin
                prdata_o[0] = configuration_q.implicit_return_en;
            end
            te_pkg::BRANCH_PREDICTION: begin
                prdata_o[0] = configuration_q.branch_prediction_en;
            end
            te_pkg::JUMP_TARGET_CACHE: begin
                prdata_o[0] = configuration_q.jump_target_cache_en;
            end

            `ifdef TE_ARCH64 // 64bits case
            // FILTER    
            // cause
            te_pkg::CAUSE_UPPER: begin
                prdata_o[te_pkg::XLEN-1:0] = upper_cause_q;
            end

            te_pkg::CAUSE_LOWER: begin
                prdata_o[te_pkg::XLEN-1:0] = lower_cause_q;
            end

            te_pkg::CAUSE_MATCH: begin
                prdata_o[te_pkg::XLEN-1:0] = match_cause_q;
            end

            // tvec
            te_pkg::TVEC_UPPER_L: begin // least significant bits
                prdata_o = upper_tvec_q[31:0];
            end

            te_pkg::TVEC_UPPER_M: begin // most significant bits
                prdata_o = upper_tvec_q[63:32];
            end

            te_pkg::TVEC_LOWER_L: begin // least significant bits
                prdata_o = lower_tvec_q[31:0];
            end

            te_pkg::TVEC_LOWER_M: begin // most significant bits
                prdata_o = lower_tvec_q[63:32];
            end

            te_pkg::TVEC_MATCH_L: begin // least significant bits
                prdata_o = match_tvec_q[31:0];
            end

            te_pkg::TVEC_MATCH_M: begin // most significant bits
                prdata_o = match_tvec_q[63:32];
            end 

            // tval
            te_pkg::TVAL_UPPER_L: begin // least significant bits
                prdata_o = upper_tval_q[31:0];
            end

            te_pkg::TVAL_UPPER_M: begin // most significant bits
                prdata_o = upper_tval_q[63:32];
            end

            te_pkg::TVAL_LOWER_L: begin // least significant bits
                prdata_o = lower_tval_q[31:0];
            end

            te_pkg::TVAL_LOWER_M: begin // most significant bits
                prdata_o = lower_tval_q[63:32];
            end

            te_pkg::TVAL_MATCH_L: begin // least significant bits
                prdata_o = match_tval_q[31:0];
            end

            te_pkg::TVAL_MATCH_M: begin // most significant bits
                prdata_o = match_tval_q[63:32];
            end

            // iaddr
            te_pkg::IADDR_UPPER_L: begin // least significant bits
                prdata_o = upper_iaddr_q[31:0];
            end

            te_pkg::IADDR_UPPER_M: begin // most significant bits
                prdata_o = upper_iaddr_q[63:32];
            end 

            te_pkg::IADDR_LOWER_L: begin // least significant bits
                prdata_o = lower_iaddr_q[31:0];
            end

            te_pkg::IADDR_LOWER_M: begin // most significant bits
                prdata_o = lower_iaddr_q[63:32];
            end

            te_pkg::IADDR_MATCH_L: begin // least significant bits
                prdata_o = match_iaddr_q[31:0];
            end

            te_pkg::IADDR_MATCH_M: begin // most significant bits
                prdata_o = match_iaddr_q[63:32];
            end

            `else // 32bits case
            // FILTER
            // cause
            te_pkg::CAUSE_UPPER: begin
                prdata_o[te_pkg::XLEN-1:0] = upper_cause_q;
            end

            te_pkg::CAUSE_LOWER: begin
                prdata_o[te_pkg::XLEN-1:0] = lower_cause_q;
            end

            te_pkg::CAUSE_MATCH: begin
                prdata_o[te_pkg::XLEN-1:0] = match_cause_d;
            end

            // tvec
            te_pkg::TVEC_UPPER: begin
                prdata_o = upper_tvec_q;
            end
            
            te_pkg::TVEC_LOWER: begin
                prdata_o = lower_tvec_q;
            end

            te_pkg::TVEC_MATCH: begin
                prdata_o = match_tvec_q;
            end

            // tval
            te_pkg::TVAL_UPPER: begin
                prdata_o = upper_tval_q;
            end

            te_pkg::TVAL_LOWER: begin
                prdata_o = lower_tval_q;
            end

            te_pkg::TVAL_MATCH: begin
                prdata_o = match_tval_q;
            end

            // iaddr
            te_pkg::IADDR_UPPER: begin
                prdata_o = upper_iaddr_q;
            end

            te_pkg::IADDR_LOWER: begin
                prdata_o = lower_iaddr_q;
            end

            te_pkg::IADDR_MATCH: begin
                prdata_o = match_iaddr_q;
            end
            `endif
            endcase
        end else if (write_access) begin
            
            
            case (paddr_i[7:0])
            // common cases
            te_pkg::CAUSE_ENABLE_MODE: begin
                cause_filter_d = pwdata_i[0];
                cause_mode_d = pwdata_i[1];
            end

            te_pkg::TVEC_ENABLE_MODE: begin
                tvec_filter_d = pwdata_i[0];
                tvec_mode_d = pwdata_i[1];
            end

            te_pkg::TVAL_ENABLE_MODE: begin
                tval_filter_d = pwdata_i[0];
                tval_mode_d = pwdata_i[1];
            end

            te_pkg::PRIV_ENABLE_MODE: begin
                priv_lvl_filter_d = pwdata_i[0];
                priv_lvl_mode_d = pwdata_i[1];
            end

            te_pkg::IADDR_ENABLE_MODE: begin
                iaddr_filter_d = pwdata_i[0];
                iaddr_mode_d = pwdata_i[1];
            end

            // priv
            te_pkg::PRIV_RANGE: begin
                lower_priv_d = pwdata_i[te_pkg::PRIV_LEN-1:0];
                upper_priv_d = pwdata_i[te_pkg::PRIV_LEN*2-1:te_pkg::PRIV_LEN];
            end
        
            te_pkg::PRIV_MATCH: begin
                match_priv_d = pwdata_i[te_pkg::PRIV_LEN-1:0];
            end

            // TRACE MANAGEMENT
            te_pkg::TRACE_STATE: begin
                trace_activated_d = pwdata_i[0];
            end
        
            te_pkg::LOSSLESS_TRACE: begin
                lossless_trace_d = pwdata_i[0];
            end

            te_pkg::SHALLOW_TRACE: begin
                shallow_trace_d = pwdata_i[0];
            end

            // PACKET EMITTER
            te_pkg::NO_TIME: begin
                notime_d = pwdata_i[0];
            end

            te_pkg::NO_CONTEXT: begin
                nocontext_d = pwdata_i[0];
            end

            te_pkg::DELTA_ADDRESS: begin
                configuration_d.delta_address_en = pwdata_i[0];
            end

            te_pkg::FULL_ADDRESS: begin
                configuration_d.full_address_en = pwdata_i[0];
            end

            te_pkg::IMPLICIT_EXCEPTION: begin
                configuration_d.implicit_exception_en = pwdata_i[0];
            end

            te_pkg::SIJUMP: begin
                configuration_d.sijump_en = pwdata_i[0];
            end

            te_pkg::IMPLICIT_RETURN: begin
                configuration_d.implicit_return_en = pwdata_i[0];
            end

            te_pkg::BRANCH_PREDICTION: begin
                configuration_d.branch_prediction_en = pwdata_i[0];
            end

            te_pkg::JUMP_TARGET_CACHE: begin
                configuration_d.jump_target_cache_en = pwdata_i[0];
            end

            `ifdef TE_ARCH64
            // cause
            te_pkg::CAUSE_UPPER: begin
                upper_cause_d = pwdata_i[te_pkg::XLEN-1:0];
            end

            te_pkg::CAUSE_LOWER: begin
                lower_cause_d = pwdata_i[te_pkg::XLEN-1:0];
            end

            te_pkg::CAUSE_MATCH: begin
                match_cause_d = pwdata_i[te_pkg::XLEN-1:0];
            end

            // tvec
            te_pkg::TVEC_UPPER_L: begin // least significant bits
                upper_tvec_d[31:0] = pwdata_i;
            end

            te_pkg::TVEC_UPPER_M: begin // most significant bits
                upper_tvec_d[63:32] = pwdata_i;
            end

            te_pkg::TVEC_LOWER_L: begin // least significant bits
                lower_tvec_d[31:0] = pwdata_i;
            end

            te_pkg::TVEC_LOWER_M: begin // most significant bits
                lower_tvec_d[63:32] = pwdata_i;
            end

            te_pkg::TVEC_MATCH_L: begin // least significant bits
                match_tvec_d[31:0] = pwdata_i;
            end

            te_pkg::TVEC_MATCH_M: begin // most significant bits
                match_tvec_d[63:32] = pwdata_i;
            end 

            // tval
            te_pkg::TVAL_UPPER_L: begin // least significant bits
                upper_tval_d[31:0] = pwdata_i;
            end

            te_pkg::TVAL_UPPER_M: begin // most significant bits
                upper_tval_d[63:32] = pwdata_i;
            end

            te_pkg::TVAL_LOWER_L: begin // least significant bits
                lower_tval_d[31:0] = pwdata_i;
            end

            te_pkg::TVAL_LOWER_M: begin // most significant bits
                lower_tval_d[63:32] = pwdata_i;
            end

            te_pkg::TVAL_MATCH_L: begin // least significant bits
                match_tval_d[31:0] = pwdata_i;
            end

            te_pkg::TVAL_MATCH_M: begin // most significant bits
                match_tval_d[63:32] = pwdata_i;
            end

            // iaddr
            te_pkg::IADDR_UPPER_L: begin // least significant bits
                upper_iaddr_d[31:0] = pwdata_i;
            end

            te_pkg::IADDR_UPPER_M: begin // most significant bits
                upper_iaddr_d[63:32] = pwdata_i;
            end 

            te_pkg::IADDR_LOWER_L: begin // least significant bits
                lower_iaddr_d[31:0] = pwdata_i;
            end

            te_pkg::IADDR_LOWER_M: begin // most significant bits
                lower_iaddr_d[63:32] = pwdata_i;
            end

            te_pkg::IADDR_MATCH_L: begin // least significant bits
                match_iaddr_d[31:0] = pwdata_i;
            end

            te_pkg::IADDR_MATCH_M: begin // most significant bits
                match_iaddr_d[63:32] = pwdata_i;
            end
            `else // 32 bits case
            // cause
            te_pkg::CAUSE_UPPER: begin
                upper_cause_d = pwdata_i[te_pkg::XLEN-1:0];
            end

            te_pkg::CAUSE_LOWER: begin
                lower_cause_d = pwdata_i[te_pkg::XLEN-1:0];
            end

            te_pkg::CAUSE_MATCH: begin
                match_cause_d = pwdata_i[te_pkg::XLEN-1:0];
            end

            // tvec
            te_pkg::TVEC_UPPER: begin
                upper_tvec_d = pwdata_i;
            end

            te_pkg::TVEC_LOWER: begin
                lower_tvec_d = pwdata_i;
            end

            te_pkg::TVEC_MATCH: begin
                match_tvec_d = pwdata_i;
            end

            // tval
            te_pkg::TVAL_UPPER: begin
                upper_tval_d = pwdata_i;
            end

            te_pkg::TVAL_LOWER: begin
                lower_tval_d = pwdata_i;
            end

            te_pkg::TVAL_MATCH: begin
                match_tval_d = pwdata_i;
            end

            // iaddr
            te_pkg::IADDR_UPPER: begin
                upper_iaddr_d = pwdata_i;
            end

            te_pkg::IADDR_LOWER: begin
                lower_iaddr_d = pwdata_i;
            end

            te_pkg::IADDR_MATCH: begin
                match_iaddr_d = pwdata_i;
            end
            `endif
            endcase
        end
    end

    // clock gating module
    pulp_clock_gating i_clock_gating(
        .clk_i    (clk_i),
        .en_i     (trace_activated_o),
        .test_en_i(test_enabled),
        .clk_o    (clk_gated)
    );

    // edge detector for trace_req_on_i
    // turns on and off the tracing
    edge_detect i_edge_detect_on(
        .clk_i(clk_gated),
        .rst_ni(rst_ni),
        .d_i(trace_req_on_i),
        .re_o(trace_req_on),
        .fe_o()
    );

    // edge detector for trace_req_off_i
    // turns on and off the tracing
    edge_detect i_edge_detect_off(
        .clk_i(clk_gated),
        .rst_ni(rst_ni),
        .d_i(trace_req_off_i),
        .re_o(trace_req_off),
        .fe_o()
    );

    always_ff @(posedge clk_i, negedge rst_ni) begin
        if(~rst_ni) begin
            trace_enable_q <= '0;
            // storing config parameters
            cause_filter_q <= '0;
            upper_cause_q <= '0;
            lower_cause_q <= '0;
            match_cause_q <= '0;
            cause_mode_q <= '0;
            tvec_filter_q <= '0;
            upper_tvec_q <= '0;
            lower_tvec_q <= '0;
            match_tvec_q <= '0;
            tvec_mode_q <= '0;
            tval_filter_q <= '0;
            upper_tval_q <= '0;
            lower_tval_q <= '0;
            match_tval_q <= '0;
            tval_mode_q <= '0;
            priv_lvl_filter_q <= '0;
            upper_priv_q <= '0;
            lower_priv_q <= '0;
            match_priv_q <= '0;
            priv_lvl_mode_q <= '0;
            iaddr_filter_q <= '0;
            upper_iaddr_q <= '0;
            lower_iaddr_q <= '0;
            match_iaddr_q <= '0;
            iaddr_mode_q <= '0;
            configuration_q.delta_address_en <= '1; // delta address only one mandatory
            configuration_q.full_address_en <= '0;
            configuration_q.implicit_exception_en <= '0;
            configuration_q.sijump_en <= '0;
            configuration_q.implicit_return_en <= '0;
            configuration_q.branch_prediction_en <= '0;
            configuration_q.jump_target_cache_en <= '0;
            lossless_trace_q <= '0;
            shallow_trace_q <= '0;
            nocontext_q <= '1;
            notime_q <= '1;
            trace_activated_q <= '1;
            setup_q <= '0;
        end else begin
            trace_enable_q <= trace_enable_d;
            // storing config parameters
            cause_filter_q <= cause_filter_d;
            upper_cause_q <= upper_cause_d;
            lower_cause_q <= lower_cause_d;
            match_cause_q <= match_cause_d;
            cause_mode_q <= cause_mode_d;
            tvec_filter_q <= tvec_filter_d;
            upper_tvec_q <= upper_tvec_d;
            lower_tvec_q <= lower_tvec_d;
            match_tvec_q <= match_tvec_d;
            tvec_mode_q <= tvec_mode_d;
            tval_filter_q <= tval_filter_d;
            upper_tval_q <= upper_tval_d;
            lower_tval_q <= lower_tval_d;
            match_tval_q <= match_tval_d;
            tval_mode_q <= tval_mode_d;
            priv_lvl_filter_q <= priv_lvl_filter_d;
            upper_priv_q <= upper_priv_d;
            lower_priv_q <= lower_priv_d;
            match_priv_q <= match_priv_d;
            priv_lvl_mode_q <= priv_lvl_mode_d;
            iaddr_filter_q <= iaddr_filter_d;
            upper_iaddr_q <= upper_iaddr_d;
            lower_iaddr_q <= lower_iaddr_d;
            match_iaddr_q <= match_iaddr_d;
            iaddr_mode_q <= iaddr_mode_d;
            configuration_q <= configuration_d;
            lossless_trace_q <= lossless_trace_d;
            shallow_trace_q <= shallow_trace_d;
            nocontext_q <= nocontext_d;
            notime_q <= notime_d;
            trace_activated_q <= trace_activated_d;
            setup_q <= setup_d;
        end
    end
    
endmodule