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

/* PACKET EMITTER */
/*
it produces the packets for the output interface
*/

module te_packet_emitter
(
    input logic                                 clk_i,
    input logic                                 rst_ni,
    input logic                                 valid_i,

    // necessary info to assemble packet
    input te_pkg::format_e                      packet_format_i,
    input te_pkg::f_sync_subformat_e            packet_f_sync_subformat_i, // SF for F3
    //input f_opt_ext_subformat_e                 packet_f_opt_ext_subformat_i, // non mandatory, SF for F0

    // lc (last cycle) signals
    input logic [te_pkg::XLEN-1:0]              lc_cause_i,
    input logic [te_pkg::XLEN-1:0]              lc_tval_i,
    input logic                                 lc_interrupt_i,

    // tc (this cycle) signals
    input logic [te_pkg::XLEN-1:0]              tc_cause_i,
    input logic [te_pkg::XLEN-1:0]              tc_tval_i,
    input logic                                 tc_interrupt_i,
    input logic                                 tc_resync_i,

    // nc (next cycle) signals

    /*  the following signals used to determine 
        if the packet emitter has to put context 
        and/or time in the payload*/
    input logic                                 nocontext_i,  // both read from registers
    input logic                                 notime_i,
    // in this implementation both hardwired to 0

    // format 3 subformat 0 specific signals
    input logic                                 tc_branch_i,
    input logic                                 tc_branch_taken_i,
    input logic [te_pkg::PRIV_LEN-1:0]          tc_priv_i,
    input logic [te_pkg::TIME_LEN-1:0]          tc_time_i,    // optional
    //input logic [:0]                            context_i, // optional
    input logic [te_pkg::XLEN-1:0]              tc_address_i,

    // format 3 subformat 1 specific signals
    input logic                                 lc_tc_mux_i,
    /*  format 3 subformat 1 packets require sometimes lc_cause o tc_cause
        To discriminate I use a mux to choose between lc or tc */

    input logic                                 thaddr_i,
    input logic [te_pkg::XLEN-1:0]              tc_tvec_i, // trap handler address
    input logic [te_pkg::XLEN-1:0]              lc_epc_i,
    
    // format 3 subformat 3 specific signals
    input logic                                 tc_ienable_i, // trace encoder enabled
    input logic                                 encoder_mode_i, // only branch trace supported (value==0)
    input te_pkg::qual_status_e                 qual_status_i,
    input te_pkg::ioptions_s                    ioptions_i,
    // about DATA trace, in stand-by at the moment
    //input logic                                 denable_i, // DATA trace enabled, if supported
    //input logic                                 dloss_i, // one or more DATA trace packets lost, if supported
    //input logic [:0]                            doptions_i, // it's like ioptions, but for DATA trace


    // format 2 specific signals
    /*  notify -> means the packet was requested by the cpu trigger unit*/ 
    //input logic                                 notify_i, // non mandatory
    
    // most of the time these 2 values can be compressed
    input logic                                 lc_updiscon_i,

    // necessary if implicit_return mode is enabled
    //input logic irreport_i,

    //input logic [2**te_pkg::CALL_COUNTER_SIZE-1:0] irdepth_i, // non mandatory, traces nested calls

    // format 1 specific signals
    /*  this format exists in two modes:
            - address, branch map
            - NO address, branch maps
        
        Their generation depends on the value of branches:
            - 0: no need for address
            - >0: address required
    */
    input logic [te_pkg::BRANCH_COUNT_LEN-1:0]  branches_i,
    input logic [te_pkg::BRANCH_MAP_LEN-1:0]    branch_map_i, // can change size to improve efficiency
    
    // format 0 specific signals
    /*  This format can have two possible subformats:
            - subformat 0: number of correctly predicted branches
            - subformat 1: jump target cache index

        Non mandatory, required support by the encoder.
    */
    input logic [$clog2(te_pkg::XLEN):0]        keep_bits_i, // required for address compression
    input logic                                 shallow_trace_i, // used to flush branch map at each packet

    // outputs
    output logic                                packet_valid_o, // asserted when a packet is generated
    output te_pkg::it_packet_type_e             packet_type_o,
    output logic [te_pkg::PAYLOAD_LEN-1:0]      packet_payload_o,
    output logic [te_pkg::P_LEN-1:0]            payload_length_o, // in bytes
    output logic                                branch_map_flush_o, // flushing done after each request
    // to send back to priority module in order to compress them
    output logic [te_pkg::XLEN:0]               addr_to_compress_o
);
    
    // internal signals
    logic                                       branch;
    logic [te_pkg::XLEN-1:0]                    address;
    logic [te_pkg::XLEN-1:0]                    ecause;
    logic signed [te_pkg::XLEN:0]               diff_addr;
    logic [te_pkg::XLEN-1:0]                    latest_addr_d, latest_addr_q; // address of the latest packet emitted
    logic [te_pkg::XLEN-1:0]                    tval;
    logic [1:0]                                 time_and_context; // if payload requires time/context
    logic                                       notify;
    logic                                       updiscon;
    logic                                       irreport;
    logic [2**te_pkg::CALL_COUNTER_SIZE-1:0]    irdepth;
    logic [4:0]                                 branch_map_off;
    logic [3:0]                                 address_off;
    logic [8:0]                                 used_bits; // counts the bits used inside each payload
    logic                                       flush_d, flush_q;
    logic                                       update_latest_addr;
    logic                                       resync_d, resync_q;
    logic [te_pkg::XLEN-1:0]                    resync_addr_d, resync_addr_q;

    // assigning values
    assign branch = ~(tc_branch_i && tc_branch_taken_i);
    assign address = thaddr_i ? tc_tvec_i : lc_epc_i;
    assign ecause = lc_tc_mux_i ? tc_cause_i : lc_cause_i;
    assign tval = lc_tc_mux_i ? tc_tval_i : lc_tval_i;
    assign interrupt = lc_tc_mux_i ? tc_interrupt_i : lc_interrupt_i;
    assign time_and_context = {~notime_i, ~nocontext_i};
    assign branch_map_flush_o = flush_q;
    assign payload_length_o = (used_bits + 7) >> 3;
    assign latest_branch_addr_d = tc_address_i;

    always_comb begin
        // init
        resync_d = resync_q;
        resync_addr_d = resync_addr_q;
        
        // updates resync signal
        if (tc_resync_i) begin
            resync_d = '1;
            resync_addr_d = tc_address_i;
        end

        // synchronous reset of resync signal
        if (packet_type_o == te_pkg::F3SF0 && resync_q) begin
            resync_d = '0;
        end
    end


    // register to store the last address emitted in a packet
    always_ff @(posedge clk_i, negedge rst_ni) begin
        if(~rst_ni) begin
            latest_addr_q <= '0;
            flush_q <= '0;
            resync_q <= '0;
            resync_addr_q <= '0;
        end else begin
            latest_addr_q <= latest_addr_d;
            resync_q <= resync_d;
            resync_addr_q <= resync_addr_d;
            flush_q <= flush_d;
        end
    end

    // combinatorial network to compute the offset to compress the branch_map
    always_comb begin : branch_map_offset
        branch_map_off = '0;
        if(branches_i == 1) begin
            branch_map_off = 1;
        end else if(branches_i <= 3) begin
            branch_map_off = 3;
        end else if(branches_i <= 7) begin
            branch_map_off = 7;
        end else if(branches_i <= 15) begin
            branch_map_off = 15;
        end else begin
            branch_map_off = 31;
        end
    end

    /*  
    the address compression works in byte chunks: based on the value of
    keep_bits_i, the number of least significant bytes to keep is determined.
    
    example:
        keep_bits_i == 7  -> 1 lsB kept
        keep_bits_i == 10 -> 2 lsB kept
        keep_bits_i == 25 -> 4 lsB kept
    */

    // find the number of least significant bytes to keep in the compressed address
    assign address_off = (keep_bits_i + 7)>>3;
    
    // combinatorial network to output packets
    always_comb begin
        // init values
        packet_payload_o = '0;
        packet_valid_o = '0;
        flush_d = '0;
        used_bits = '0;
        packet_type_o = te_pkg::F0SF0;
        update_latest_addr = '0;
        
        if(valid_i) begin
        /*  branch_map_flush_o
            the signal is output in this cycle, but the branch map does
            the flush in the next cycle to leave time to the packet
            emitter to read values and put them in the payload 
        */
            // flushes at each packet emitted to get a less precise tracing
            flush_d = shallow_trace_i;

            // setting the packet to emit as valid
            packet_valid_o = '1;

        /*  packet payload creation: 
            at the beginning it's put in the payload the common part (i.e. the packet format)
            then, for each format and subformat it's put the rest of the payload
        */
            
            // setting the packet format - common for all payloads
            packet_payload_o[1:0] = packet_format_i;
            
            // format bits
            used_bits += 2;

            case(packet_format_i)
            te_pkg::F_SYNC: begin // format 3
                // setting packet subformat - common for all type 3 payloads
                packet_payload_o[3:2] = packet_f_sync_subformat_i;
                
                used_bits += 2; // subformat bits

                // setting the rest of payload for each type
                case(packet_f_sync_subformat_i)
                te_pkg::SF_START: begin // subformat 0
                    // updating packet type
                    packet_type_o = te_pkg::F3SF0;
                    update_latest_addr = '1;

                    case(time_and_context)
                    2'b00: begin
                        used_bits += 1 + (address_off * 8)+1 + te_pkg::PRIV_LEN;

                        packet_payload_o[4+:1+te_pkg::PRIV_LEN] = {
                            tc_priv_i,
                            branch
                        };
                        // address compression
                        case (address_off)
                        1: begin
                            packet_payload_o[5+te_pkg::PRIV_LEN+:9] = {
                                addr_to_compress_o[8:0]
                            };
                        end 
                        2: begin
                            packet_payload_o[5+te_pkg::PRIV_LEN+:17] = {
                                addr_to_compress_o[16:0]
                            };
                        end
                        3: begin
                            packet_payload_o[5+te_pkg::PRIV_LEN+:25] = {
                                addr_to_compress_o[24:0]
                            };
                        end
                        4: begin
                            packet_payload_o[5+te_pkg::PRIV_LEN+:33] = {
                                addr_to_compress_o[32:0]
                            };
                        end
                        5: begin
                            packet_payload_o[5+te_pkg::PRIV_LEN+:41] = {
                                addr_to_compress_o[40:0]
                            };
                        end 
                        6: begin
                            packet_payload_o[5+te_pkg::PRIV_LEN+:49] = {
                                addr_to_compress_o[48:0]
                            };
                        end
                        7: begin
                            packet_payload_o[5+te_pkg::PRIV_LEN+:57] = {
                                addr_to_compress_o[56:0]
                            };
                        end
                        8: begin
                            packet_payload_o[5+te_pkg::PRIV_LEN+:65] = {
                                addr_to_compress_o
                            };
                        end
                        endcase
                    end

                    2'b10: begin
                        used_bits += 1 + te_pkg::PRIV_LEN + te_pkg::TIME_LEN + (address_off * 8)+1;

                        packet_payload_o[4+:1+te_pkg::PRIV_LEN+te_pkg::TIME_LEN] = {
                            tc_time_i,
                            tc_priv_i,
                            branch
                        };
                        // address compression
                        case (address_off)
                        1: begin
                            packet_payload_o[5+te_pkg::PRIV_LEN+te_pkg::XLEN+:9] = {
                                addr_to_compress_o[8:0]
                            };
                        end 
                        2: begin
                            packet_payload_o[5+te_pkg::PRIV_LEN+te_pkg::XLEN+:17] = {
                                addr_to_compress_o[16:0]
                            };
                        end
                        3: begin
                            packet_payload_o[5+te_pkg::PRIV_LEN+te_pkg::XLEN+:25] = {
                                addr_to_compress_o[24:0]
                            };
                        end
                        4: begin
                            packet_payload_o[5+te_pkg::PRIV_LEN+te_pkg::XLEN+:33] = {
                                addr_to_compress_o[32:0]
                            };
                        end
                        5: begin
                            packet_payload_o[5+te_pkg::PRIV_LEN+te_pkg::XLEN+:41] = {
                                addr_to_compress_o[40:0]
                            };
                        end 
                        6: begin
                            packet_payload_o[5+te_pkg::PRIV_LEN+te_pkg::XLEN+:49] = {
                                addr_to_compress_o[48:0]
                            };
                        end
                        7: begin
                            packet_payload_o[5+te_pkg::PRIV_LEN+te_pkg::XLEN+:57] = {
                                addr_to_compress_o[56:0]
                            };
                        end
                        8: begin
                            packet_payload_o[5+te_pkg::PRIV_LEN+te_pkg::XLEN+:65] = {
                                addr_to_compress_o
                            };
                        end
                        endcase
                    end
                    /*TODO: other cases*/
                    endcase
                end
                te_pkg::SF_TRAP: begin // subformat 1
                    // updating packet type
                    packet_type_o = te_pkg::F3SF1;
                    update_latest_addr = '1;
                    
                    case(time_and_context)
                    2'b00: begin
                        used_bits += 1 + te_pkg::PRIV_LEN + te_pkg::XLEN + 2 + address_off * 8 + 1 + te_pkg::XLEN;

                        packet_payload_o[4+:1+te_pkg::PRIV_LEN+te_pkg::XLEN+2] = {
                            thaddr_i,
                            interrupt,
                            ecause,
                            tc_priv_i,
                            branch
                        };
                        // address compression
                        case (address_off)
                        1: begin
                            packet_payload_o[7+te_pkg::PRIV_LEN+te_pkg::XLEN+:9+te_pkg::XLEN] = {
                                tval,
                                addr_to_compress_o[8:0]
                            };
                        end
                        2: begin
                            packet_payload_o[7+te_pkg::PRIV_LEN+te_pkg::XLEN+:17+te_pkg::XLEN] = {
                                tval,
                                addr_to_compress_o[16:0]
                            };
                        end
                        3: begin
                            packet_payload_o[7+te_pkg::PRIV_LEN+te_pkg::XLEN+:25+te_pkg::XLEN] = {
                                tval,
                                addr_to_compress_o[24:0]
                            };
                        end
                        4: begin
                            packet_payload_o[7+te_pkg::PRIV_LEN+te_pkg::XLEN+:33+te_pkg::XLEN] = {
                                tval,
                                addr_to_compress_o[32:0]
                            };
                        end
                        5: begin
                            packet_payload_o[7+te_pkg::PRIV_LEN+te_pkg::XLEN+:41+te_pkg::XLEN] = {
                                tval,
                                addr_to_compress_o[40:0]
                            };
                        end
                        6: begin
                            packet_payload_o[7+te_pkg::PRIV_LEN+te_pkg::XLEN+:49+te_pkg::XLEN] = {
                                tval,
                                addr_to_compress_o[48:0]
                            };
                        end
                        7: begin
                            packet_payload_o[7+te_pkg::PRIV_LEN+te_pkg::XLEN+:57+te_pkg::XLEN] = {
                                tval,
                                addr_to_compress_o[56:0]
                            };
                        end
                        8: begin
                            packet_payload_o[7+te_pkg::PRIV_LEN+te_pkg::XLEN+:65+te_pkg::XLEN] = {
                                tval,
                                addr_to_compress_o
                            };
                        end
                        endcase
                    end

                    2'b10: begin
                        used_bits += 1 + te_pkg::PRIV_LEN + te_pkg::XLEN + 2 + address_off * 8 + 1 + te_pkg::TIME_LEN * 2;

                        packet_payload_o[4+:1+te_pkg::PRIV_LEN+te_pkg::XLEN+2+te_pkg::TIME_LEN] = {
                            thaddr_i,
                            interrupt,
                            ecause,
                            tc_time_i,
                            tc_priv_i,
                            branch
                        };
                        // address compression
                        case (address_off)
                        1: begin
                            packet_payload_o[7+te_pkg::PRIV_LEN+te_pkg::XLEN+te_pkg::XLEN+:9+te_pkg::XLEN] = {
                                tval,
                                addr_to_compress_o[8:0]
                            };
                        end
                        2: begin
                            packet_payload_o[7+te_pkg::PRIV_LEN+te_pkg::XLEN+te_pkg::XLEN+:17+te_pkg::XLEN] = {
                                tval,
                                addr_to_compress_o[16:0]
                            };
                        end
                        3: begin
                            packet_payload_o[7+te_pkg::PRIV_LEN+te_pkg::XLEN+te_pkg::XLEN+:25+te_pkg::XLEN] = {
                                tval,
                                addr_to_compress_o[24:0]
                            };
                        end
                        4: begin
                            packet_payload_o[7+te_pkg::PRIV_LEN+te_pkg::XLEN+te_pkg::XLEN+:33+te_pkg::XLEN] = {
                                tval,
                                addr_to_compress_o[32:0]
                            };
                        end
                        5: begin
                            packet_payload_o[7+te_pkg::PRIV_LEN+te_pkg::XLEN+te_pkg::XLEN+:41+te_pkg::XLEN] = {
                                tval,
                                addr_to_compress_o[40:0]
                            };
                        end
                        6: begin
                            packet_payload_o[7+te_pkg::PRIV_LEN+te_pkg::XLEN+te_pkg::XLEN+:49+te_pkg::XLEN] = {
                                tval,
                                addr_to_compress_o[48:0]
                            };
                        end
                        7: begin
                            packet_payload_o[7+te_pkg::PRIV_LEN+te_pkg::XLEN+te_pkg::XLEN+:57+te_pkg::XLEN] = {
                                tval,
                                addr_to_compress_o[56:0]
                            };
                        end
                        8: begin
                            packet_payload_o[7+te_pkg::PRIV_LEN+te_pkg::XLEN+te_pkg::XLEN+:65+te_pkg::XLEN] = {
                                tval,
                                addr_to_compress_o
                            };
                        end
                        endcase
                    end
                    /*TODO: other cases*/
                    endcase
                end
                te_pkg::SF_CONTEXT: begin // subformat 2
                    // updating packet type
                    packet_type_o = te_pkg::F3SF2;

                    case(time_and_context)
                    2'b00: begin
                        used_bits += te_pkg::PRIV_LEN;

                        packet_payload_o[4+:te_pkg::PRIV_LEN] = {
                            tc_priv_i
                        };
                    end

                    2'b10: begin
                        used_bits += te_pkg::PRIV_LEN + te_pkg::TIME_LEN;

                        packet_payload_o[4+:te_pkg::PRIV_LEN+te_pkg::TIME_LEN] = {
                            tc_time_i,
                            tc_priv_i
                        };
                    end
                    /*TODO: other cases*/
                    endcase
                end
                te_pkg::SF_SUPPORT: begin // subformat 3
                    // updating packet type
                    packet_type_o = te_pkg::F3SF3;

                    used_bits += 7;

                    packet_payload_o[4+:1+1+2+7] = {
                        /* info required for data tracing - in the future
                        doptions_i,
                        dloss_i,
                        denable_i, */
                        ioptions_i,
                        qual_status_i,
                        encoder_mode_i,
                        tc_ienable_i
                    };
                end
                endcase
            end

            te_pkg::F_ADDR_ONLY: begin // format 2
                // updating packet type
                packet_type_o = te_pkg::F2;
                update_latest_addr = '1;

                // requires trigger unit in CPU
                /*
                if(notify_i) begin // request from trigger unit
                    notify = !tc_address_i[XLEN-1];
                    updiscon = notify;
                    irreport = updsicon;
                    irdepth = irdepth_i;
                end else begin*/

                // case of an updiscon
                if(lc_updiscon_i) begin
                    notify = tc_address_i[te_pkg::XLEN-1];
                    updiscon = !notify;
                    irreport = updiscon;
                    irdepth = {2**te_pkg::CALL_COUNTER_SIZE{updiscon}};
                /* non mandatory
                end else if(implicit_mode_i && irreport_i) begin // request for implicit return mode
                    notify = tc_address_i[XLEN-1];
                    updiscon = notify;
                    irreport = !updiscon;
                    irdepth = irdepth_i;
                */
                end else begin //other cases
                    notify = tc_address_i[te_pkg::XLEN-1];
                    updiscon = notify;
                    irreport = updiscon;
                    irdepth = {2**te_pkg::CALL_COUNTER_SIZE{updiscon}};
                end

                // checking if implicit return is supported
                if (te_pkg::CALL_COUNTER_SIZE == 0 &&
                    te_pkg::RETURN_STACK_SIZE == 0) begin // not supported
                    
                    // payload bits
                    used_bits += 3 + address_off*8 + 1;

                    // address compression
                    case (address_off)
                    1: begin
                        packet_payload_o[2+:9+3] = {
                            irreport,
                            updiscon,
                            notify,
                            addr_to_compress_o[8:0]
                        };
                    end
                    2: begin
                        packet_payload_o[2+:17+3] = {
                            irreport,
                            updiscon,
                            notify,
                            addr_to_compress_o[16:0]
                        };
                    end
                    3: begin
                        packet_payload_o[2+:25+3] = {
                            irreport,
                            updiscon,
                            notify,
                            addr_to_compress_o[24:0]
                        };
                    end
                    4: begin
                        packet_payload_o[2+:33+3] = {
                            irreport,
                            updiscon,
                            notify,
                            addr_to_compress_o[32:0]
                        };
                    end
                    5: begin
                        packet_payload_o[2+:41+3] = {
                            irreport,
                            updiscon,
                            notify,
                            addr_to_compress_o[40:0]
                        };
                    end
                    6: begin
                        packet_payload_o[2+:49+3] = {
                            irreport,
                            updiscon,
                            notify,
                            addr_to_compress_o[48:0]
                        };
                    end
                    7: begin
                        packet_payload_o[2+:57+3] = {
                            irreport,
                            updiscon,
                            notify,
                            addr_to_compress_o[56:0]
                        };
                    end
                    8: begin
                        packet_payload_o[2+:65+3] = {
                            irreport,
                            updiscon,
                            notify,
                            addr_to_compress_o
                        };
                    end
                    endcase
                    
                end else begin // supported
                    // payload bits
                    used_bits += 3 + 2**te_pkg::CALL_COUNTER_SIZE + address_off*8 + 1;

                    // address compression
                    case (address_off)
                    1: begin
                        packet_payload_o[2+:9+3+2**te_pkg::CALL_COUNTER_SIZE] = {
                            irdepth,
                            irreport,
                            updiscon,
                            notify,
                            addr_to_compress_o[8:0]
                        };
                    end
                    2: begin
                        packet_payload_o[2+:17+3+2**te_pkg::CALL_COUNTER_SIZE] = {
                            irdepth,
                            irreport,
                            updiscon,
                            notify,
                            addr_to_compress_o[16:0]
                        };
                    end
                    3: begin
                        packet_payload_o[2+:25+3+2**te_pkg::CALL_COUNTER_SIZE] = {
                            irdepth,
                            irreport,
                            updiscon,
                            notify,
                            addr_to_compress_o[24:0]
                        };
                    end
                    4: begin
                        packet_payload_o[2+:33+3+2**te_pkg::CALL_COUNTER_SIZE] = {
                            irdepth,
                            irreport,
                            updiscon,
                            notify,
                            addr_to_compress_o[32:0]
                        };
                    end
                    5: begin
                        packet_payload_o[2+:41+3+2**te_pkg::CALL_COUNTER_SIZE] = {
                            irdepth,
                            irreport,
                            updiscon,
                            notify,
                            addr_to_compress_o[40:0]
                        };
                    end
                    6: begin
                        packet_payload_o[2+:49+3+2**te_pkg::CALL_COUNTER_SIZE] = {
                            irdepth,
                            irreport,
                            updiscon,
                            notify,
                            addr_to_compress_o[48:0]
                        };
                    end
                    7: begin
                        packet_payload_o[2+:57+3+2**te_pkg::CALL_COUNTER_SIZE] = {
                            irdepth,
                            irreport,
                            updiscon,
                            notify,
                            addr_to_compress_o[56:0]
                        };
                    end
                    8: begin
                        packet_payload_o[2+:65+3+2**te_pkg::CALL_COUNTER_SIZE] = {
                            irdepth,
                            irreport,
                            updiscon,
                            notify,
                            addr_to_compress_o
                        };
                    end
                    endcase
                end
            end

            te_pkg::F_DIFF_DELTA: begin // format 1
                /*  There can be two type of payloads for this format:
                    1. address, branch map
                    2. no address, branch map
                    
                    Type 1 payload is used when there has been at least
                    one branch from last packet. This can be determined
                    by the number of branches in the branch map.

                    Type 2 payload is used when the address is not needed,
                    for examples if the branch map is full.
                */
                // flushing the branch_map
                flush_d = '1;
                // updating packet type
                packet_type_o = te_pkg::F1;

                // requires trigger unit in CPU
                /*
                if(notify_i) begin // request from trigger unit
                    notify = !tc_address_i[XLEN-1];
                    updiscon = notify;
                    irreport = updsicon;
                    irdepth = irdepth_i;
                end else begin*/

                // case of an updiscon
                if(lc_updiscon_i) begin
                    notify = tc_address_i[te_pkg::XLEN-1];
                    updiscon = !notify;
                    irreport = updiscon;
                    irdepth = {2**te_pkg::CALL_COUNTER_SIZE{updiscon}};
                /* non mandatory
                end else if(implicit_return_i && irreport_i) begin // request for implicit return mode
                    notify = tc_address_i[XLEN-1];
                    updiscon = notify;
                    irreport = !updiscon;
                    irdepth = irdepth_i;
                */
                end else begin // other cases
                    notify = tc_address_i[te_pkg::XLEN-1];
                    updiscon = notify;
                    irreport = updiscon;
                    irdepth = {2**te_pkg::CALL_COUNTER_SIZE{updiscon}};
                end

                // branches and branch_map bits
                used_bits += te_pkg::BRANCH_COUNT_LEN + branch_map_off;

                // adding branch count and branch map
                if (branch_map_off == 1) begin
                    packet_payload_o[2+:te_pkg::BRANCH_COUNT_LEN+1] = {
                        branch_map_i[0],
                        branches_i
                    };
                end else if (branch_map_off == 3) begin
                    packet_payload_o[2+:te_pkg::BRANCH_COUNT_LEN+3] = {
                        branch_map_i[2:0],
                        branches_i
                    };
                end else if (branch_map_off == 7) begin
                    packet_payload_o[2+:te_pkg::BRANCH_COUNT_LEN+7] = {
                        branch_map_i[6:0],
                        branches_i
                    };
                end else if (branch_map_off == 15) begin
                    packet_payload_o[2+:te_pkg::BRANCH_COUNT_LEN+15] = {
                        branch_map_i[14:0],
                        branches_i
                    };
                end else if (branch_map_off == 31) begin
                    if (lc_updiscon_i) begin
                        packet_payload_o[2+:te_pkg::BRANCH_COUNT_LEN+31] = {
                            branch_map_i[30:0],
                            branches_i
                        };
                    end else begin
                        packet_payload_o[2+:te_pkg::BRANCH_COUNT_LEN+31] = {
                            branch_map_i[30:0],
                            5'b00000
                        };
                    end
                end

                // attaching the rest of the payload
                if(branches_i < 31) begin // branch map not full - address
                    update_latest_addr = '1;
                    // checking if implicit return is supported
                    if (te_pkg::CALL_COUNTER_SIZE == 0 &&
                        te_pkg::RETURN_STACK_SIZE == 0) begin // not supported
                        // rest of the payload bits
                        used_bits += 3 + address_off*8 + 1;

                        // address compression
                        case (address_off)
                        1: begin
                            packet_payload_o[2+te_pkg::BRANCH_COUNT_LEN+branch_map_off+:9+3] = {
                                irreport,
                                updiscon,
                                notify,
                                addr_to_compress_o[8:0]
                            };
                        end
                        2: begin
                            packet_payload_o[2+te_pkg::BRANCH_COUNT_LEN+branch_map_off+:17+3] = {
                                irreport,
                                updiscon,
                                notify,
                                addr_to_compress_o[16:0]
                            };
                        end
                        3: begin
                            packet_payload_o[2+te_pkg::BRANCH_COUNT_LEN+branch_map_off+:25+3] = {
                                irreport,
                                updiscon,
                                notify,
                                addr_to_compress_o[24:0]
                            };
                        end
                        4: begin
                            packet_payload_o[2+te_pkg::BRANCH_COUNT_LEN+branch_map_off+:33+3] = {
                                irreport,
                                updiscon,
                                notify,
                                addr_to_compress_o[32:0]
                            };
                        end
                        5: begin
                            packet_payload_o[2+te_pkg::BRANCH_COUNT_LEN+branch_map_off+:41+3] = {
                                irreport,
                                updiscon,
                                notify,
                                addr_to_compress_o[40:0]
                            };
                        end
                        6: begin
                            packet_payload_o[2+te_pkg::BRANCH_COUNT_LEN+branch_map_off+:49+3] = {
                                irreport,
                                updiscon,
                                notify,
                                addr_to_compress_o[48:0]
                            };
                        end
                        7: begin
                            packet_payload_o[2+te_pkg::BRANCH_COUNT_LEN+branch_map_off+:57+3] = {
                                irreport,
                                updiscon,
                                notify,
                                addr_to_compress_o[56:0]
                            };
                        end
                        8: begin
                            packet_payload_o[2+te_pkg::BRANCH_COUNT_LEN+branch_map_off+:65+3] = {
                                irreport,
                                updiscon,
                                notify,
                                addr_to_compress_o
                            };
                        end
                        endcase

                    end else begin // supported -> uses irdepth and irreport
                        // rest of the payload bits
                        used_bits += 3 + 2**te_pkg::CALL_COUNTER_SIZE + address_off*8 + 1;

                        // address compression
                        case (address_off)
                        1: begin
                            packet_payload_o[2+te_pkg::BRANCH_COUNT_LEN+branch_map_off+:9+3+2**te_pkg::CALL_COUNTER_SIZE] = {
                                irdepth,
                                irreport,
                                updiscon,
                                notify,
                                addr_to_compress_o[8:0]
                            };
                        end
                        2: begin
                            packet_payload_o[2+te_pkg::BRANCH_COUNT_LEN+branch_map_off+:17+3+2**te_pkg::CALL_COUNTER_SIZE] = {
                                irdepth,
                                irreport,
                                updiscon,
                                notify,
                                addr_to_compress_o[16:0]
                            };
                        end
                        3: begin
                            packet_payload_o[2+te_pkg::BRANCH_COUNT_LEN+branch_map_off+:25+3+2**te_pkg::CALL_COUNTER_SIZE] = {
                                irdepth,
                                irreport,
                                updiscon,
                                notify,
                                addr_to_compress_o[24:0]
                            };
                        end
                        4: begin
                            packet_payload_o[2+te_pkg::BRANCH_COUNT_LEN+branch_map_off+:33+3+2**te_pkg::CALL_COUNTER_SIZE] = {
                                irdepth,
                                irreport,
                                updiscon,
                                notify,
                                addr_to_compress_o[31:0]
                            };
                        end
                        5: begin
                            packet_payload_o[2+te_pkg::BRANCH_COUNT_LEN+branch_map_off+:41+3+2**te_pkg::CALL_COUNTER_SIZE] = {
                                irdepth,
                                irreport,
                                updiscon,
                                notify,
                                addr_to_compress_o[40:0]
                            };
                        end
                        6: begin
                            packet_payload_o[2+te_pkg::BRANCH_COUNT_LEN+branch_map_off+:49+3+2**te_pkg::CALL_COUNTER_SIZE] = {
                                irdepth,
                                irreport,
                                updiscon,
                                notify,
                                addr_to_compress_o[48:0]
                            };
                        end
                        7: begin
                            packet_payload_o[2+te_pkg::BRANCH_COUNT_LEN+branch_map_off+:57+3+2**te_pkg::CALL_COUNTER_SIZE] = {
                                irdepth,
                                irreport,
                                updiscon,
                                notify,
                                addr_to_compress_o[56:0]
                            };
                        end
                        8: begin
                            packet_payload_o[2+te_pkg::BRANCH_COUNT_LEN+branch_map_off+:65+3+2**te_pkg::CALL_COUNTER_SIZE] = {
                                irdepth,
                                irreport,
                                updiscon,
                                notify,
                                addr_to_compress_o
                            };
                        end
                        endcase
                    end
                end
            end

            //F_OPT_EXT: begin // format 0 // TODO
                // requires trigger unit in CPU
                /*
                if(notify_i) begin // request from trigger unit
                    notify = !tc_address_i[XLEN-1];
                    updiscon = notify;
                    irreport = updsicon;
                    irdepth = irdepth_i;
                end else begin
                notify = tc_address_i[XLEN-1];
                updiscon = notify;
                irreport = updiscon;
                irdepth = {2**CALL_COUNTER_SIZE{updiscon}};
                end */
                
                /* requires non mandatory support for jtc and branch prediction
                case(packet_f_opt_ext_subformat_i)
                SF_PBC: begin // subformat 0
                /*  There can be two type of payloads for this subformat:
                    1. no address, branch count
                    2. address, branch count
                * /    
                
                    // only for F0SF0 payload w/address
                    // updating latest address sent in a packet

                    packet_payload_o = {F_OPT_EXT, SF_PBC, etc..};
                    packet_valid_o = '1;
                end

                SF_JTC: begin // subformat 1
                    packet_payload_o = {F_OPT_EXT, SF_JTC, etc..};
                    packet_valid_o = '1;
                end
                endcase
                */
            //end
            endcase
        end
    end

    always_comb begin : address_to_compress
        diff_addr = '0;
        addr_to_compress_o = '0;
        latest_addr_d = latest_addr_q;

        // determines differential address for F1, F2
        if (packet_valid_o && (packet_format_i == te_pkg::F_DIFF_DELTA || 
            packet_format_i == te_pkg::F_ADDR_ONLY)) begin
            if (ioptions_i.delta_address_en) begin
                diff_addr = signed'(tc_address_i) - latest_addr_q;
                addr_to_compress_o = diff_addr;
            end else if (ioptions_i.full_address_en) begin
                addr_to_compress_o = tc_address_i;
            end
        end

        // determines latest address
        if (packet_valid_o && update_latest_addr) begin
            if (packet_type_o == te_pkg::F1 || 
                packet_type_o == te_pkg::F2) begin // F1, F2
                latest_addr_d = tc_address_i;
                addr_to_compress_o = diff_addr;
            end
            if (packet_type_o == te_pkg::F3SF0) begin // F3SF0
                if (resync_q) begin
                    latest_addr_d = tc_address_i;
                    addr_to_compress_o = tc_address_i;
                end else begin
                    latest_addr_d = tc_address_i;
                    addr_to_compress_o = tc_address_i;
                end
            end
            if (packet_type_o == te_pkg::F3SF1) begin // F3SF1
                latest_addr_d = address;
                addr_to_compress_o = address;
            end
        end
    end

endmodule