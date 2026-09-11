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

/* TOP LEVEL MODULE */

module rv_tracer #(
    //parameter IRETIRE_LEN = 32, // set to 1 for single retirement
    parameter N = 1, // max number of special inst retired in a cycle, the input port number depends on this value
    parameter ONLY_BRANCHES = 0, // special inst are branches or not
    parameter APB_ADDR_WIDTH = 32 // address width for APB interface
)
(
    input logic                                     clk_i,
    input logic                                     rst_ni,

    input logic [N-1:0]                             valid_i, // used for multiple retirement

    /* data from the CPU */
    // mandatory inputs
    input logic [N-1:0][te_pkg::ITYPE_LEN-1:0]      itype_i, // termination type of block
    input logic [te_pkg::XLEN-1:0]                  cause_i,
    input logic [te_pkg::XLEN-1:0]                  tval_i,
    input logic [te_pkg::PRIV_LEN-1:0]              priv_i,
    input logic [N-1:0][te_pkg::XLEN-1:0]           iaddr_i, // pc of 1st inst in block
    input logic [N-1:0][te_pkg::IRETIRE_LEN-1:0]    iretire_i, // length of block in halfwords
    input logic [N-1:0]                             ilastsize_i, // length of last inst in 2^ilastsize halfword
    // non mandatory inputs
    input logic [te_pkg::TIME_LEN-1:0]              time_i,
    //input logic [:0]                                context_i,
    //input logic [te_pkg::CTYPE_LEN-1:0]             ctype_i, // spec says it's 1 or 2 bit wide
    //input logic [te_pkg::TRIGGER_LEN-1:0]           trigger_i, // must be supported CPU side

    // support inputs
    input logic [te_pkg::XLEN-1:0]                  tvec_i, // tvec_q, contains trap handler address
    input logic [te_pkg::XLEN-1:0]                  epc_i, // epc_q, required for format 3 subformat 1
    input logic                                     encapsulator_ready_i, // non mandatory
    // Optional external arm signal used by the Chipyard wrapper.  Kept
    // separate from the APB TRACE_STATE bit so the native standalone module
    // interface remains backward compatible through its default value.
    input logic                                     external_enable_i,
    
    // APB interface inputs
    input logic [APB_ADDR_WIDTH-1:0]                paddr_i,
    input logic                                     pwrite_i,
    input logic                                     psel_i,
    input logic                                     penable_i,
    input logic [31:0]                              pwdata_i,
    
    // outputs
    // info needed for the encapsulator
    output logic [N-1:0]                            packet_valid_o,
    output te_pkg::it_packet_type_e [N-1:0]         packet_type_o,
    output logic [N-1:0][te_pkg::P_LEN-1:0]         packet_length_o, // in bytes
    output logic [N-1:0][te_pkg::PAYLOAD_LEN-1:0]   packet_payload_o,
    // sideband signals
    output logic                                    stall_o,

    // APB interface outputs
    output logic                                    pready_o,
    output logic [31:0]                             prdata_o
);

    /* signals for management */
    // registers
    logic                                   trace_activated;
    logic                                   trace_enable;
    logic                                   nocontext;
    logic                                   notime;
    logic                                   encoder_mode;
    // filter
    logic                                   trigger_trace_on; // hardwired to 0?
    logic                                   trigger_trace_off; // hardwired to 0?
    //logic                                   qualified; // is it needed or I can use qualified_d?
    logic                                   trace_req_deactivate;
    // priority
    te_pkg::format_e                        packet_format[N-1:0];
    te_pkg::f_sync_subformat_e              packet_f_sync_subformat[N-1:0];
    te_pkg::f_opt_ext_subformat_e           packet_f_opt_ext_subformat[N-1:0];
    logic                                   thaddr[N-1:0];
    logic                                   lc_tc_mux[N-1:0];
    te_pkg::qual_status_e                   qual_status[N-1:0];
    logic                                   nc_branch_map_flush;
    logic [te_pkg::BRANCH_MAP_LEN-1:0]      branch_map;
    logic [te_pkg::BRANCH_COUNT_LEN-1:0]    branch_count;
    logic                                   resync_rst;
    // packet emitter
    logic                                   packet_valid[N-1:0];
    // resync counter
    logic [N-1:0]                           packet_emitted;
    // not classified
    logic                                   nc_branch_map_empty;
    logic                                   clk_gated;
    logic                                   turn_on_tracer_d, turn_on_tracer_q;
    logic                                   lossless_trace;
    logic                                   shallow_trace;

    // we have three phases, called last cycle (lc), this cycle (tc) and next
    // cycle (nc), based on which we make decision whether we need to emit a
    // packet or not.
    logic                                   tc_branch_map_empty;
    logic [N-1:0]                           tc_resync;
    logic [$clog2(te_pkg::XLEN):0]          keep_bits[N-1:0];
    logic [te_pkg::XLEN:0]                  addr_to_compress[N-1:0];
    
    /* MANAGING LC, TC, NC SIGNALS */
    /*
    To manage lc, tc, nc signals I decided to use two 
    serially connected FFs.
            ___________                    ___________              
    sig0_d--| D     Q |--sig0_q == sig1_d--| D     Q |--sig1_q
      nc    |         |    tc              |         |    lc
            |   FF0   |                    |   FF1   |
            |_________|                    |_________|
    
    For input signals I need another FF to sample them:
            ___________                    ___________                    ___________
    sig_i --| D     Q |--sig0_q == sig1_d--| D     Q |--sig1_q == sig2_d--| D     Q |--sig2_q
    input   |         |    nc              |         |    tc              |         |    lc
            |   FF0   |                    |   FF1   |                    |   FF2   |
            |_________|                    |_________|                    |_________|
    examples of this are: exception_i
    
    Nonetheless all inputs must be sampled and the output of the FF is considered nc.
    */

    // signals for FFs
    logic [N-1:0][te_pkg::IRETIRE_LEN-1:0]  iretired0_d, iretired0_q;
    logic [N-1:0][te_pkg::IRETIRE_LEN-1:0]  iretired1_d, iretired1_q;
    logic                                   exception0_d, exception0_q;
    logic                                   exception1_d, exception1_q;
    logic                                   exception2_d, exception2_q;
    logic                                   interrupt0_d, interrupt0_q;
    logic                                   interrupt1_d, interrupt1_q;
    logic                                   interrupt2_d, interrupt2_q;
    logic [te_pkg::XLEN-1:0]                cause0_d, cause0_q;
    logic [te_pkg::XLEN-1:0]                cause1_d, cause1_q;
    logic [te_pkg::XLEN-1:0]                cause2_d, cause2_q;
    logic [te_pkg::XLEN-1:0]                tvec0_d, tvec0_q;
    logic [te_pkg::XLEN-1:0]                tvec1_d, tvec1_q;
    logic [te_pkg::XLEN-1:0]                tval0_d, tval0_q;
    logic [te_pkg::XLEN-1:0]                tval1_d, tval1_q;
    logic [te_pkg::XLEN-1:0]                tval2_d, tval2_q;
    logic [N-1:0][te_pkg::ITYPE_LEN-1:0]    itype0_d, itype0_q;
    logic [N-1:0][te_pkg::ITYPE_LEN-1:0]    itype1_d, itype1_q;                        
    logic [te_pkg::PRIV_LEN-1:0]            priv_lvl0_d, priv_lvl0_q;
    logic [te_pkg::PRIV_LEN-1:0]            priv_lvl1_d, priv_lvl1_q;
    logic [N-1:0][te_pkg::XLEN-1:0]         address0_d, address0_q;
    logic [N-1:0][te_pkg::XLEN-1:0]         address1_d, address1_q;
    logic [te_pkg::XLEN-1:0]                epc0_d, epc0_q;
    logic [te_pkg::XLEN-1:0]                epc1_d, epc1_q;
    logic [te_pkg::XLEN-1:0]                epc2_d, epc2_q;
    logic [te_pkg::TIME_LEN-1:0]            time0_d, time0_q;
    logic [te_pkg::TIME_LEN-1:0]            time1_d, time1_q;
    logic                                   tc_first_qualified;
    logic                                   tc_final_qualified;
    logic [N-1:0]                           updiscon0_d, updiscon0_q;
    logic [N-1:0]                           updiscon1_d, updiscon1_q;
    logic [N-1:0]                           updiscon2_d, updiscon2_q;
    logic [N-1:0]                           qualified0_d, qualified0_q;
    logic [N-1:0]                           qualified1_d, qualified1_q;
    logic [N-1:0]                           qualified2_d, qualified2_q;
    logic [N-1:0]                           nc_qualified;
    logic [N-1:0]                           valid0_d, valid0_q;
    logic                                   privchange_d, privchange_q;
    // logic                                   context_change_d, context_change_q; // non mandatory
    // logic                                   precise_context_report_d, precise_context_report_q; // requires ctype signal CPU side
    // logic                                   context_report_as_disc_d, context_report_as_disc_q; // ibidem
    // logic                                   no_context_report_d, no_context_report_q; // ibidem
    // logic                                   imprecise_context_report_d, imprecise_context_report_q; // ibidem
    logic                                   gt_max_resync_d, gt_max_resync_q;
    logic                                   et_max_resync_d, et_max_resync_q;
    logic                                   branch_map_full_d, branch_map_full_q;
    //logic                                   branch_misprediction_d, branch_misprediction_q; // non mandatory
    logic                                   trace_activated_d, trace_activated_q;
    logic                                   enc_activated_d, enc_activated_q;
    logic                                   enc_deactivated_d, enc_deactivated_q;
    //logic                                   packets_lost_d, packets_lost_q; // non mandatory
    te_pkg::ioptions_s                      enc_config_d, enc_config_q;
    logic                                   enc_config_change_d, enc_config_change_q;
    logic                                   branch_d, branch_q;
    logic                                   branch_taken_d, branch_taken_q;
    
    // branch_map inputs
    logic [N-1:0]   branch_valid;
    logic [N-1:0]   branch_taken;
    
    // te_reg output to filter input
    logic                           cause_filter;
    logic [te_pkg::XLEN-1:0]        upper_cause;
    logic [te_pkg::XLEN-1:0]        lower_cause;
    logic [te_pkg::XLEN-1:0]        match_cause;
    logic                           cause_mode;
    logic                           tvec_filter;
    logic [te_pkg::XLEN-1:0]        upper_tvec;
    logic [te_pkg::XLEN-1:0]        lower_tvec;
    logic [te_pkg::XLEN-1:0]        match_tvec;
    logic                           tvec_mode;
    logic                           tval_filter;
    logic [te_pkg::XLEN-1:0]        upper_tval;
    logic [te_pkg::XLEN-1:0]        lower_tval;
    logic [te_pkg::XLEN-1:0]        match_tval;
    logic                           tval_mode;
    logic                           priv_lvl_filter;
    logic [te_pkg::PRIV_LEN-1:0]    upper_priv;
    logic [te_pkg::PRIV_LEN-1:0]    lower_priv;
    logic [te_pkg::PRIV_LEN-1:0]    match_priv;
    logic                           priv_lvl_mode;
    logic                           iaddr_filter;
    logic [te_pkg::XLEN-1:0]        upper_iaddr;
    logic [te_pkg::XLEN-1:0]        lower_iaddr;
    logic [te_pkg::XLEN-1:0]        match_iaddr;
    logic                           iaddr_mode;

    /*  the following commented section has non mandatory signals
        for now it's commented
    */
 /* combinatorial network to define the following 
    signals from ctype:
    - tc_no_context_report_i        -> ctype == 0
    - tc_precise_context_report_i   -> ctype == 2
    - tc_context_report_as_disc_i   -> ctype == 3
    - tc_imprecise_context_report_i -> ctype == 1
    - nc_precise_context_report_i   -> ctype == 2
    - nc_context_report_as_disc_i   -> ctype == 3*/
    /*
    always_comb begin : ctype_manager
        case(ctype_i)
        2'h0: // no report - add signal        
            tc_no_context_report = '1;
        2'h1:
            tc_imprecise_context_report = '1;
        2'h2:
            tc_precise_context_report = '1;
        2'h3:
            tc_context_report_as_disc = '1;
        endcase
    end
    */

    /*TODO: create a trigger decoder that produces:
                - trigger_trace_on  -> 2
                - trigger_trace_off -> 3
                - trigger_notify    -> 4
    */
    // maybe it's enough to define values and hardwire them to 0

    /* ASSIGNMENT */
    /* hardwired assignments */
    assign trigger_trace_on = '0;
    assign trigger_trace_off = '0;

    /* FFs inputs */
    assign valid0_d = valid_i;
    assign trace_activated_d = trace_activated;
    assign enc_activated_d = trace_activated_d && ~trace_activated_q;
    assign enc_deactivated_d = ~trace_activated_d && trace_activated_q;
    assign enc_config_change_d = enc_config_d != enc_config_q;

    /* next cycle */
    assign nc_branch_map_empty = nc_branch_map_flush || (tc_branch_map_empty /*&& ~branch_q*/);

    // output
    assign packet_valid_o = packet_emitted;
    // sideband
    assign stall_o = ~encapsulator_ready_i && lossless_trace;

    // other
    assign branch_taken_d = |branch_taken;

    // first and final qualified
    assign tc_first_qualified = trace_activated && |qualified1_q && !qualified2_q && trace_enable;
    assign tc_final_qualified = trace_activated && |qualified2_q && !qualified1_q && trace_enable;

    // not static assignments
    always_comb begin
        // init
        branch_valid = '0;
        branch_taken = '0;
        exception0_d = exception0_q;
        exception1_d = exception1_q;
        exception2_d = exception2_q;
        interrupt0_d = interrupt0_q;
        interrupt1_d = interrupt1_q;
        interrupt2_d = interrupt2_q;
        updiscon0_d = updiscon0_q;
        updiscon1_d = updiscon1_q;
        updiscon2_d = updiscon2_q;
        cause0_d = cause0_q;
        cause1_d = cause1_q;
        cause2_d = cause2_q;
        tval0_d = tval0_q;
        tval1_d = tval1_q;
        tval2_d = tval2_q;
        itype0_d = itype0_q;
        itype1_d = itype1_q;
        priv_lvl0_d = priv_lvl0_q;
        priv_lvl1_d = priv_lvl1_q;
        qualified0_d = qualified0_q;
        qualified1_d = qualified1_q;
        qualified2_d = qualified2_q;
        iretired0_d = iretired0_q;
        iretired1_d = iretired1_q;
        tvec0_d = tvec0_q;
        tvec1_d = tvec1_q;
        address0_d = address0_q;
        address1_d = address1_q;
        epc0_d = epc0_q;
        epc1_d = epc1_q;
        epc2_d = epc2_q;
        time0_d = time0_q;
        time1_d = time1_q;
        privchange_d = privchange_q;
        // context_change_d = context_change_q;
        // precise_context_report_d = precise_context_report_q; // requires ctype signal CPU side
        // context_report_as_disc_d = context_report_as_disc_q; //ibidem
        // no_context_report_d = no_context_report_q; // ibidem
        // imprecise_context_report_d = imprecise_context_report_q; // ibidem
        branch_d = branch_q;

        // itype, iretired and address
        // it works for all three cases
        for (int i = 0; i < N; i++) begin
            if (valid_i[i]) begin
                itype0_d[i] = itype_i[i];
                iretired0_d[i] = iretire_i[i];
                address0_d[i] = iaddr_i[i]+2*(iretire_i[i] - 2**ilastsize_i[i]);
            end
        end

        // assigning branch map inputs
        for (int i = 0; i < N; i++) begin
            if (itype0_q[i] == 4 || itype0_q[i] == 5) begin
                branch_valid[i] = '1;
            end
            if (itype0_q[i] == 5) begin
                branch_taken[i] = '1;
            end
        end

        // updiscon
        if (N == 1 || (N > 1 && ONLY_BRANCHES)) begin
            if (nc_qualified[0]) begin
                updiscon0_d[0] = itype_i[0] == 10;
            end
        end
        // wait for this case
        if (N > 1 && !ONLY_BRANCHES) begin
            for (int i = 0; i < N; i++) begin
                if (nc_qualified[i]) begin
                    updiscon0_d[i] = itype_i[i] == 10;
                end
            end
        end

        // updating registers values
        if (|valid_i) begin
            exception0_d = itype_i[0] == 1;
            exception1_d = exception0_q;
            exception2_d = exception1_q;
            interrupt0_d = itype_i[0] == 2;
            interrupt1_d = interrupt0_q;
            interrupt2_d = interrupt1_q;
            updiscon1_d = updiscon0_q;
            updiscon2_d = updiscon1_q;
            cause0_d = cause_i;
            cause1_d = cause0_q;
            cause2_d = cause1_q;
            tval0_d = tval_i;
            tval1_d = tval0_q;
            tval2_d = tval1_q;
            itype0_d = itype_i;
            itype1_d = itype0_q;
            priv_lvl0_d = priv_i;
            priv_lvl1_d = priv_lvl0_q;
            qualified0_d = nc_qualified;
            qualified1_d = qualified0_q;
            qualified2_d = qualified1_q;
            iretired0_d = iretire_i;
            iretired1_d = iretired0_q;
            tvec0_d = tvec_i;
            tvec1_d = tvec0_q;
            address1_d = address0_q;
            epc0_d = epc_i;
            epc1_d = epc0_q;
            epc2_d = epc1_q;
            time0_d = time_i;
            time1_d = time0_q;
            privchange_d = (priv_lvl0_q != priv_lvl1_q) && |valid_i;
            // context_change_d; // TODO
            //precise_context_report_d; // requires ctype signal CPU side
            //context_report_as_disc_d; //ibidem
            //no_context_report_d; // ibidem
            //imprecise_context_report_d; // ibidem
            branch_d = |branch_valid;
        end

        if (!turn_on_tracer_q) begin
            turn_on_tracer_d = |valid_i && qualified0_d;
        end
    end

    /* MODULES INSTANTIATION */
    // one instance for all 3 cases

    /* MAPPED REGISTERS */
    te_reg #(
        .APB_ADDR_WIDTH(APB_ADDR_WIDTH)
    ) i_te_reg(
        .clk_i               (clk_i),
        .rst_ni              (rst_ni),
        .trace_req_off_i     ('0), // from filter
        // In the integrated Chipyard path there is no separate trigger unit.
        // TRACE_STATE activation must therefore be sufficient to arm the
        // qualification logic; otherwise trace_enable starts at zero,
        // qualified stays zero, and turn_on_tracer can never assert.
        // Chipyard's controller enable is the authoritative arm signal.  The
        // wrapper also programs TRACE_STATE, but that APB transaction can
        // complete after the first retirement event; accepting the external
        // activation here prevents the qualification pipeline from remaining
        // permanently disabled in the integrated path.
        .trace_req_on_i      (turn_on_tracer_q || trace_activated || external_enable_i),
        .encapsulator_ready_i(encapsulator_ready_i),
        .cause_filter_o      (cause_filter),
        .upper_cause_o       (upper_cause),
        .lower_cause_o       (lower_cause),
        .match_cause_o       (match_cause),
        .cause_mode_o        (cause_mode),
        .tvec_filter_o       (tvec_filter),
        .upper_tvec_o        (upper_tvec),
        .lower_tvec_o        (lower_tvec),
        .match_tvec_o        (match_tvec),
        .tvec_mode_o         (tvec_mode),
        .tval_filter_o       (tval_filter),
        .upper_tval_o        (upper_tval),
        .lower_tval_o        (lower_tval),
        .match_tval_o        (match_tval),
        .tval_mode_o         (tval_mode),
        .priv_lvl_filter_o   (priv_lvl_filter),
        .upper_priv_o        (upper_priv),
        .lower_priv_o        (lower_priv),
        .match_priv_o        (match_priv),
        .priv_lvl_mode_o     (priv_lvl_mode),
        .iaddr_filter_o      (iaddr_filter),
        .upper_iaddr_o       (upper_iaddr),
        .lower_iaddr_o       (lower_iaddr),
        .match_iaddr_o       (match_iaddr),
        .iaddr_mode_o        (iaddr_mode),
        .trace_enable_o      (trace_enable),
        .trace_activated_o   (trace_activated),
        .nocontext_o         (nocontext),
        .notime_o            (notime),
        .encoder_mode_o      (encoder_mode),
        .configuration_o     (enc_config_d),
        .lossless_trace_o    (lossless_trace),
        .shallow_trace_o     (shallow_trace),
        .clk_gated_o         (clk_gated),
        .paddr_i             (paddr_i),
        .pwrite_i            (pwrite_i),
        .psel_i              (psel_i),
        .penable_i           (penable_i),
        .pwdata_i            (pwdata_i),
        .pready_o            (pready_o),
        .prdata_o            (prdata_o)
    );

    /* BRANCH MAP */
    te_branch_map #(
        .N(N)
    ) i_te_branch_map(
        .clk_i         (clk_gated),
        .rst_ni        (rst_ni),
        .valid_i       (branch_valid & qualified0_q & valid0_q),
        .branch_taken_i(branch_taken),
        .flush_i       (nc_branch_map_flush),
        //.branch_taken_prediction_i(), // non mandatory
        .map_o         (branch_map),
        .branches_o    (branch_count),
        //.pbc_o(), // non mandatory - branch prediction mode
        //.misprediction_o(), // non mandatory - ibidem
        .is_full_o     (branch_map_full_d),
        .is_empty_o    (tc_branch_map_empty)
    );

    /* RESYNC COUNTER */
    te_resync_counter #(
        .MODE(te_pkg::CYCLE_MODE), // count cycles
        .MAX_VALUE(13'h1FFF)         // 8192
    ) i_te_resync_counter( // for testing we keep the def settings
        .clk_i           (clk_gated),
        .rst_ni          (rst_ni),
        .trace_enabled_i (trace_enable),
        .packet_emitted_i(packet_emitted),
        .resync_rst_i    (resync_rst),
        .gt_resync_max_o (gt_max_resync_d),
        .et_resync_max_o (et_max_resync_d)
    );

    // N instances for all 3 cases

    /* FILTER */
    generate
        // The single-group Chipyard integration also needs a filter instance.
        // Without this branch nc_qualified remains undriven (X), preventing
        // the priority/emitter path from producing packets.
        if (N == 1 || (N > 1 && ONLY_BRANCHES)) begin
            for (genvar i = 0; i < N; i++) begin
                te_filter i_te_filter(
                    .trace_enable_i   (trace_enable || trace_activated), // tracks 1st branch if 1st inst
                    .cause_filter_i   (cause_filter),
                    .upper_cause_i    (upper_cause),
                    .lower_cause_i    (lower_cause),
                    .match_cause_i    (match_cause),
                    .cause_mode_i     (cause_mode),
                    .cause_i          (cause_i),
                    .tvec_filter_i    (tvec_filter),
                    .upper_tvec_i     (upper_tvec),
                    .lower_tvec_i     (lower_tvec),
                    .match_tvec_i     (match_tvec),
                    .tvec_mode_i      (tvec_mode),
                    .tvec_i           (tvec_i),
                    .tval_filter_i    (tval_filter),
                    .upper_tval_i     (upper_tval),
                    .lower_tval_i     (lower_tval),
                    .match_tval_i     (match_tval),
                    .tval_mode_i      (tval_mode),
                    .tval_i           (tval_i),
                    .priv_lvl_filter_i(priv_lvl_filter),
                    .upper_priv_i     (upper_priv),
                    .lower_priv_i     (lower_priv),
                    .match_priv_i     (match_priv),
                    .priv_lvl_mode_i  (priv_lvl_mode),
                    .priv_i           (priv_i),
                    .iaddr_filter_i   (iaddr_filter),
                    .upper_iaddr_i    (upper_iaddr),
                    .lower_iaddr_i    (lower_iaddr),
                    .match_iaddr_i    (match_iaddr),
                    .iaddr_mode_i     (iaddr_mode),
                    .iaddr_i          (iaddr_i[i]),
                    .nc_qualified_o   (nc_qualified[i])
                );
            end
        end
    endgenerate

    /* PRIORITY, PACKET EMITTER*/
    // case dependant
    generate
        // 1 instance for N == 1 || (N > 1 && ONLY_BRANCHES)
        if (N == 1 || (N > 1 && ONLY_BRANCHES)) begin
            /* PRIORITY */
            te_priority i_te_priority(
                .clk_i                    (clk_gated),
                .rst_ni                   (rst_ni),
                .valid_i                  (valid0_q[0] || (enc_activated_q || enc_deactivated_q)), // necessary to generate F3SF3 packet
                .lc_exception_i           (exception2_q || interrupt2_q),
                .lc_updiscon_i            (updiscon2_q[0]),
                .tc_qualified_i           (qualified1_q[0]),
                .tc_exception_i           (exception1_q || interrupt1_q),
                .tc_retired_i             (iretired1_q[0]),
                .tc_first_qualified_i     (tc_first_qualified),
                .tc_privchange_i          (privchange_q),
                //.tc_context_change_i(), // non mandatory
                //.tc_precise_context_report_i(), // requires ctype signal CPU side
                //.tc_context_report_as_disc_i(), // ibidem
                //.tc_imprecise_context_report_i(), // ibidem
                .tc_gt_max_resync_i       (gt_max_resync_q),
                .tc_et_max_resync_i       (et_max_resync_q),
                .tc_branch_map_empty_i    (tc_branch_map_empty),
                .tc_branch_map_full_i     (branch_map_full_q),
                //.tc_branch_misprediction_i(), // non mandatory
                //.tc_pbc_i(), // non mandatory
                .tc_enc_enabled_i         (enc_activated_q),
                .tc_enc_disabled_i        (enc_deactivated_q),
                .tc_opmode_change_i       (enc_config_change_q),
                .tc_final_qualified_i     (tc_final_qualified),
                .tc_packets_lost_i        (~encapsulator_ready_i), // non mandatory
                .nc_exception_i           (exception0_q || interrupt0_q),
                .nc_privchange_i          (privchange_d),
                //.nc_context_change_i(),
                //.nc_precise_context_report_i(), // requires ctype signal CPU side
                //.nc_context_report_as_disc_i(), // ibidem
                .nc_branch_map_empty_i    (nc_branch_map_empty),
                .nc_qualified_i           (qualified0_q[0]),
                .nc_retired_i             (iretired0_q[0]),
                //.halted_i(), // non mandatory side band signal
                //.reset_i(), // ibidem
                //.implicit_return_i(), // non mandatory
                //.tc_trigger_req_i(), // non mandatory
                //.notify_o(), // non mandatory, depends on trigger request
                .addr_to_compress_i       (addr_to_compress[0]),
                .valid_o                  (packet_valid[0]),
                .packet_format_o          (packet_format[0]),
                .packet_f_sync_subformat_o(packet_f_sync_subformat[0]),
                //.packet_f_opt_ext_subformat_o(packet_f_opt_ext_subformat), // non mandatory
                .thaddr_o                 (thaddr[0]),
                .lc_tc_mux_o              (lc_tc_mux[0]),
                .resync_timer_rst_o       (resync_rst),
                .qual_status_o            (qual_status[0]),
                .tc_resync_o              (tc_resync[0]),
                .keep_bits_o              (keep_bits[0])
            );

            /* PACKET EMITTER */
            te_packet_emitter i_te_packet_emitter(
                .clk_i                    (clk_gated),
                .rst_ni                   (rst_ni),
                .valid_i                  (packet_valid[0]),
                .packet_format_i          (packet_format[0]),
                .packet_f_sync_subformat_i(packet_f_sync_subformat[0]),
                //.packet_f_opt_ext_subformat_i(packet_f_opt_ext_subformat), // non mandatory
                .lc_cause_i               (cause2_q),
                .lc_tval_i                (tval2_q),
                .lc_interrupt_i           (interrupt2_q),
                .tc_cause_i               (cause1_q),
                .tc_tval_i                (tval1_q),
                .tc_interrupt_i           (interrupt1_q),
                .tc_resync_i              (tc_resync[0]),
                .nocontext_i              (nocontext),
                .notime_i                 (notime),
                .tc_branch_i              (branch_q),
                .tc_branch_taken_i        (branch_taken_q),
                .tc_priv_i                (priv_lvl1_q),
                .tc_time_i                (time1_q), // non mandatory
                //.context_i(), // non mandatory
                .tc_address_i             (address1_q[0]),
                .lc_tc_mux_i              (lc_tc_mux[0]),
                .thaddr_i                 (thaddr[0]),
                .tc_tvec_i                (tvec1_q),
                .lc_epc_i                 (epc2_q),
                .tc_ienable_i             (trace_enable),
                .encoder_mode_i           (encoder_mode),
                .qual_status_i            (qual_status[0]),
                .ioptions_i               (enc_config_q),
                //.denable_i(), // stand-by
                //.dloss_i(), //stand-by
                //.notify_i(), // non mandatory
                .lc_updiscon_i            (updiscon2_q[0]),
                //.irreport_i(), // non mandatory
                //.irdepth_i(), // non mandatory
                .branches_i               (branch_count),
                .branch_map_i             (branch_map),
                .keep_bits_i              (keep_bits[0]),
                .shallow_trace_i          (shallow_trace),
                .packet_valid_o           (packet_emitted[0]),
                .packet_type_o            (packet_type_o[0]),
                .packet_payload_o         (packet_payload_o[0]),
                .payload_length_o         (packet_length_o[0]),
                .branch_map_flush_o       (nc_branch_map_flush),
                .addr_to_compress_o       (addr_to_compress[0])
            );

        end else if (N > 1 && !ONLY_BRANCHES) begin
            // N instances for N > 1 && !ONLY_BRANCHES
            for (genvar i = 0; i < N; i++) begin
                if (i == 0) begin
                    /* PRIORITY */
                    te_priority i_te_priority(
                        .clk_i                    (clk_gated),
                        .rst_ni                   (rst_ni),
                        .valid_i                  (|valid0_q || (enc_activated_q || enc_deactivated_q)),
                        .lc_exception_i           (exception2_q || interrupt2_q),
                        .lc_updiscon_i            (updiscon2_q[i]),
                        .tc_qualified_i           (qualified1_q[i]),
                        .tc_exception_i           (exception1_q || interrupt1_q),
                        .tc_retired_i             (iretired1_q[i]),
                        .tc_first_qualified_i     (tc_first_qualified),
                        .tc_privchange_i          (privchange_q),
                        //.tc_context_change_i(), // non mandatory
                        //.tc_precise_context_report_i(), // requires ctype signal CPU side
                        //.tc_context_report_as_disc_i(), // ibidem
                        //.tc_imprecise_context_report_i(), // ibidem
                        .tc_gt_max_resync_i       (gt_max_resync_q), // connected only to the first port
                        .tc_et_max_resync_i       (et_max_resync_q), // connected only to the first port
                        .tc_branch_map_empty_i    (tc_branch_map_empty),
                        .tc_branch_map_full_i     (branch_map_full_q), // connected only to the first port
                        //.tc_branch_misprediction_i(), // non mandatory
                        //.tc_pbc_i(), // non mandatory
                        .tc_enc_enabled_i         (enc_activated_q),
                        .tc_enc_disabled_i        (enc_deactivated_q),
                        .tc_opmode_change_i       (enc_config_change_q),
                        .tc_final_qualified_i     (tc_final_qualified),
                        .tc_packets_lost_i        (~encapsulator_ready_i), // non mandatory
                        .nc_exception_i           (exception0_q || interrupt0_q),
                        .nc_privchange_i          (privchange_d),
                        //.nc_context_change_i(),
                        //.nc_precise_context_report_i(), // requires ctype signal CPU side
                        //.nc_context_report_as_disc_i(), // ibidem
                        .nc_branch_map_empty_i    (nc_branch_map_empty),
                        .nc_qualified_i           (qualified0_q[i]),
                        .nc_retired_i             (iretired0_q[i]),
                        //.halted_i(), // non mandatory side band signal
                        //.reset_i(), // ibidem
                        //.implicit_return_i(), // non mandatory
                        //.tc_trigger_req_i(), // non mandatory
                        //.notify_o(), // non mandatory, depends on trigger request
                        .addr_to_compress_i       (addr_to_compress[i]),
                        .valid_o                  (packet_valid[i]),
                        .packet_format_o          (packet_format[i]),
                        .packet_f_sync_subformat_o(packet_f_sync_subformat[i]),
                        //.packet_f_opt_ext_subformat_o(packet_f_opt_ext_subformat), // non mandatory
                        .thaddr_o                 (thaddr[i]),
                        .lc_tc_mux_o              (lc_tc_mux[i]),
                        .resync_timer_rst_o       (resync_rst),
                        .qual_status_o            (qual_status[i]),
                        .tc_resync_o              (tc_resync[i]),
                        .keep_bits_o              (keep_bits[i])
                    );

                    /* PACKET EMITTER */
                    te_packet_emitter i_te_packet_emitter(
                        .clk_i                    (clk_gated),
                        .rst_ni                   (rst_ni),
                        .valid_i                  (packet_valid[i]),
                        .packet_format_i          (packet_format[i]),
                        .packet_f_sync_subformat_i(packet_f_sync_subformat[i]),
                        //.packet_f_opt_ext_subformat_i(packet_f_opt_ext_subformat), // non mandatory
                        .lc_cause_i               (cause2_q),
                        .lc_tval_i                (tval2_q),
                        .lc_interrupt_i           (interrupt2_q),
                        .tc_cause_i               (cause1_q),
                        .tc_tval_i                (tval1_q),
                        .tc_interrupt_i           (interrupt1_q),
                        .tc_resync_i              (tc_resync[i]),
                        .nocontext_i              (nocontext),
                        .notime_i                 (notime),
                        .tc_branch_i              (branch_q),
                        .tc_branch_taken_i        (branch_taken_q),
                        .tc_priv_i                (priv_lvl1_q),
                        .tc_time_i                (time1_q), // non mandatory
                        //.context_i(), // non mandatory
                        .tc_address_i             (address1_q[i]),
                        .lc_tc_mux_i              (lc_tc_mux[i]),
                        .thaddr_i                 (thaddr[i]),
                        .tc_tvec_i                (tvec1_q),
                        .lc_epc_i                 (epc2_q),
                        .tc_ienable_i             (trace_enable),
                        .encoder_mode_i           (encoder_mode),
                        .qual_status_i            (qual_status[i]),
                        .ioptions_i               (enc_config_q),
                        //.denable_i(), // stand-by
                        //.dloss_i(), //stand-by
                        //.notify_i(), // non mandatory
                        .lc_updiscon_i            (updiscon2_q[i]),
                        //.irreport_i(), // non mandatory
                        //.irdepth_i(), // non mandatory
                        .branches_i               (branch_count),
                        .branch_map_i             (branch_map),
                        .keep_bits_i              (keep_bits[i]),
                        .shallow_trace_i          (shallow_trace),
                        .packet_valid_o           (packet_emitted[i]),
                        .packet_type_o            (packet_type_o[i]),
                        .packet_payload_o         (packet_payload_o[i]),
                        .payload_length_o         (packet_length_o[i]),
                        .branch_map_flush_o       (nc_branch_map_flush),
                        .addr_to_compress_o       (addr_to_compress[i])
                    );
                end else begin
                    /* PRIORITY */
                    te_priority i_te_priority(
                        .clk_i                    (clk_gated),
                        .rst_ni                   (rst_ni),
                        .valid_i                  (|valid0_q || (enc_activated_q || enc_deactivated_q)),
                        .lc_exception_i           (exception2_q || interrupt2_q),
                        .lc_updiscon_i            (updiscon2_q[i]),
                        .tc_qualified_i           (qualified1_q[i]),
                        .tc_exception_i           (exception1_q || interrupt1_q),
                        .tc_retired_i             (iretired1_q[i]),
                        .tc_first_qualified_i     (tc_first_qualified),
                        .tc_privchange_i          (privchange_q),
                        //.tc_context_change_i(), // non mandatory
                        //.tc_precise_context_report_i(), // requires ctype signal CPU side
                        //.tc_context_report_as_disc_i(), // ibidem
                        //.tc_imprecise_context_report_i(), // ibidem
                        .tc_gt_max_resync_i       (), // connected only to the first port
                        .tc_et_max_resync_i       (), // connected only to the first port
                        .tc_branch_map_empty_i    (tc_branch_map_empty),
                        .tc_branch_map_full_i     (), // connected only to the first port
                        //.tc_branch_misprediction_i(), // non mandatory
                        //.tc_pbc_i(), // non mandatory
                        .tc_enc_enabled_i         (enc_activated_q),
                        .tc_enc_disabled_i        (enc_deactivated_q),
                        .tc_opmode_change_i       (enc_config_change_q),
                        .tc_final_qualified_i     (tc_final_qualified),
                        .tc_packets_lost_i        (~encapsulator_ready_i), // non mandatory
                        .nc_exception_i           (exception0_q || interrupt0_q),
                        .nc_privchange_i          (privchange_d),
                        //.nc_context_change_i(),
                        //.nc_precise_context_report_i(), // requires ctype signal CPU side
                        //.nc_context_report_as_disc_i(), // ibidem
                        .nc_branch_map_empty_i    (nc_branch_map_empty),
                        .nc_qualified_i           (qualified0_q[i]),
                        .nc_retired_i             (iretired0_q[i]),
                        //.halted_i(), // non mandatory side band signal
                        //.reset_i(), // ibidem
                        //.implicit_return_i(), // non mandatory
                        //.tc_trigger_req_i(), // non mandatory
                        //.notify_o(), // non mandatory, depends on trigger request
                        .addr_to_compress_i       (addr_to_compress[i]),
                        .valid_o                  (packet_valid[i]),
                        .packet_format_o          (packet_format[i]),
                        .packet_f_sync_subformat_o(packet_f_sync_subformat[i]),
                        //.packet_f_opt_ext_subformat_o(packet_f_opt_ext_subformat), // non mandatory
                        .thaddr_o                 (thaddr[i]),
                        .lc_tc_mux_o              (lc_tc_mux[i]),
                        .resync_timer_rst_o       (), // connected only to the first port
                        .qual_status_o            (qual_status[i]),
                        .tc_resync_o              (tc_resync[i]),
                        .keep_bits_o              (keep_bits[i])
                    );

                    /* PACKET EMITTER */
                    te_packet_emitter i_te_packet_emitter(
                        .clk_i                    (clk_gated),
                        .rst_ni                   (rst_ni),
                        .valid_i                  (packet_valid[i]),
                        .packet_format_i          (packet_format[i]),
                        .packet_f_sync_subformat_i(packet_f_sync_subformat[i]),
                        //.packet_f_opt_ext_subformat_i(packet_f_opt_ext_subformat), // non mandatory
                        .lc_cause_i               (cause2_q),
                        .lc_tval_i                (tval2_q),
                        .lc_interrupt_i           (interrupt2_q),
                        .tc_cause_i               (cause1_q),
                        .tc_tval_i                (tval1_q),
                        .tc_interrupt_i           (interrupt1_q),
                        .tc_resync_i              (tc_resync[i]),
                        .nocontext_i              (nocontext),
                        .notime_i                 (notime),
                        .tc_branch_i              (branch_q),
                        .tc_branch_taken_i        (branch_taken_q),
                        .tc_priv_i                (priv_lvl1_q),
                        .tc_time_i                (time1_q), // non mandatory
                        //.context_i(), // non mandatory
                        .tc_address_i             (address1_q[i]),
                        .lc_tc_mux_i              (lc_tc_mux[i]),
                        .thaddr_i                 (thaddr[i]),
                        .tc_tvec_i                (tvec1_q),
                        .lc_epc_i                 (epc2_q),
                        .tc_ienable_i             (trace_enable),
                        .encoder_mode_i           (encoder_mode),
                        .qual_status_i            (qual_status[i]),
                        .ioptions_i               (enc_config_q),
                        //.denable_i(), // stand-by
                        //.dloss_i(), //stand-by
                        //.notify_i(), // non mandatory
                        .lc_updiscon_i            (updiscon2_q[i]),
                        //.irreport_i(), // non mandatory
                        //.irdepth_i(), // non mandatory
                        .branches_i               (branch_count),
                        .branch_map_i             (branch_map),
                        .keep_bits_i              (keep_bits[i]),
                        .shallow_trace_i          (shallow_trace),
                        .packet_valid_o           (packet_emitted[i]),
                        .packet_type_o            (packet_type_o[i]),
                        .packet_payload_o         (packet_payload_o[i]),
                        .payload_length_o         (packet_length_o[i]),
                        .branch_map_flush_o       (), // connected only to the first port
                        .addr_to_compress_o       (addr_to_compress[i])
                    );
                end
            end
        end
    endgenerate

    /* REGISTERS MANAGEMENT */
    always_ff @( posedge clk_i, negedge rst_ni ) begin : registers
        if(~rst_ni) begin
            valid0_q <= '0;
            exception0_q <= '0;
            exception1_q <= '0;
            exception2_q <= '0;
            interrupt0_q <= '0;
            interrupt1_q <= '0;
            interrupt2_q <= '0;
            updiscon0_q <= '0;
            updiscon1_q <= '0;
            updiscon2_q <= '0;
            cause0_q <= '0;
            cause1_q <= '0;
            cause2_q <= '0;
            tval0_q <= '0;
            tval1_q <= '0;
            tval2_q <= '0;
            itype0_q <= '0;
            itype1_q <= '0;
            priv_lvl0_q <= '0;
            priv_lvl1_q <= '0;
            qualified0_q <= '0;
            qualified1_q <= '0;
            qualified2_q <= '0;
            iretired0_q <= '0;
            iretired1_q <= '0;
            tvec0_q <= '0;
            tvec1_q <= '0;
            address0_q <= '0;
            address1_q <= '0;
            epc0_q <= '0;
            epc1_q <= '0;
            epc2_q <= '0;
            time0_q <= '0;
            time1_q <= '0;
            privchange_q <= '0;
            // context_change_q <= '0;
            //precise_context_report_q <= '0; // requires ctype signal CPU side
            //context_report_as_disc_q <= '0; //ibidem
            //no_context_report_q <= '0; // ibidem
            //imprecise_context_report_q <= '0; // ibidem
            gt_max_resync_q <= '0;
            et_max_resync_q <= '0;
            branch_map_full_q <= '0;
            //branch_misprediction_q <= '0; // non mandatory
            trace_activated_q <= '0;
            enc_activated_q <= '0;
            enc_deactivated_q <= '0;
            //packets_lost_q <= '0; // non mandatory
            enc_config_q <= te_pkg::DELTA_ADDRESS; // 3'b0
            enc_config_change_q <= '0;
            branch_taken_q <= '0;
            branch_q <= '0;
            turn_on_tracer_q <= '0;
        end else begin
            exception0_q <= exception0_d;
            exception1_q <= exception1_d;
            exception2_q <= exception2_d;
            interrupt0_q <= interrupt0_d;
            interrupt1_q <= interrupt1_d;
            interrupt2_q <= interrupt2_d;
            updiscon0_q <= updiscon0_d;
            updiscon1_q <= updiscon1_d;
            updiscon2_q <= updiscon2_d;
            cause0_q <= cause0_d;
            cause1_q <= cause1_d;
            cause2_q <= cause2_d;
            tval0_q <= tval0_d;
            tval1_q <= tval1_d;
            tval2_q <= tval2_d;
            itype0_q <= itype0_d;
            itype1_q <= itype1_d;
            priv_lvl0_q <= priv_lvl0_d;
            priv_lvl1_q <= priv_lvl1_d;
            qualified0_q <= qualified0_d;
            qualified1_q <= qualified1_d;
            qualified2_q <= qualified2_d;
            iretired0_q <= iretired0_d;
            iretired1_q <= iretired1_d;
            tvec0_q <= tvec0_d;
            tvec1_q <= tvec1_d;
            address0_q <= address0_d;
            address1_q <= address1_d;
            epc0_q <= epc0_d;
            epc1_q <= epc1_d;
            epc2_q <= epc2_d;
            time0_q <= time0_d;
            time1_q <= time1_d;
            privchange_q <= privchange_d;
            // context_change_q <= context_change_d;
            //precise_context_report_q <= precise_context_report_d; // requires ctype signal CPU side
            //context_report_as_disc_q <= context_report_as_disc_d; //ibidem
            //no_context_report_q <= no_context_report_d; // ibidem
            //imprecise_context_report_q <= imprecise_context_report_d; // ibidem
            branch_q <= branch_d;
            valid0_q <= valid0_d;
            gt_max_resync_q <= gt_max_resync_d;
            et_max_resync_q <= et_max_resync_d;
            branch_map_full_q <= branch_map_full_d;
            //branch_misprediction_q <= branch_misprediction_d; // non mandatory
            trace_activated_q <= trace_activated_d;
            enc_activated_q <= enc_activated_d;
            enc_deactivated_q <= enc_deactivated_d;
            //packets_lost_q <= packets_lost_d; // non mandatory
            enc_config_q <= enc_config_d;
            enc_config_change_q <= enc_config_change_d;
            branch_taken_q <= branch_taken_d;
            turn_on_tracer_q <= turn_on_tracer_d;
        end
    end
    
endmodule
