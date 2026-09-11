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

// uncomment to enable 64bits arch support
`define TE_ARCH64

package te_pkg;
    localparam PRIV_LEN = 2; // depends on CPU implementation
    localparam INST_LEN = 32;
    localparam PTYPE_LEN = 4; // is it F + SF? spec not clear
    localparam P_LEN = 5;
    localparam PAYLOAD_LEN = 248;
    localparam TRIGGER_LEN = 4;
    localparam CTYPE_LEN = 2;
`ifdef TE_ARCH64 // 64bit arch specific parameters
    localparam XLEN = 64;
    // APB addresses to access registers
    // the value is added to the peripheral address
    /* FILTER */
    // cause
    localparam CAUSE_UPPER = 8'h5;
    localparam CAUSE_LOWER = 8'h6;
    localparam CAUSE_MATCH = 8'h7;
    // tvec
    localparam TVEC_UPPER_L = 8'h8; // least significant bits
    localparam TVEC_UPPER_M = 8'h9; // most significant bits
    localparam TVEC_LOWER_L = 8'ha; // least significant bits
    localparam TVEC_LOWER_M = 8'hb; // most significant bits
    localparam TVEC_MATCH_L = 8'hc; // least significant bits
    localparam TVEC_MATCH_M = 8'hd; // most significant bits
    // tval
    localparam TVAL_UPPER_L = 8'he; // least significant bits
    localparam TVAL_UPPER_M = 8'hf; // most significant bits
    localparam TVAL_LOWER_L = 8'h10; // least significant bits
    localparam TVAL_LOWER_M = 8'h11; // most significant bits
    localparam TVAL_MATCH_L = 8'h12; // least significant bits
    localparam TVAL_MATCH_M = 8'h13; // most significant bits
    // iaddr
    localparam IADDR_UPPER_L = 8'h16; // least significant bits
    localparam IADDR_UPPER_M = 8'h17; // most significant bits
    localparam IADDR_LOWER_L = 8'h18; // least significant bits
    localparam IADDR_LOWER_M = 8'h19; // most significant bits
    localparam IADDR_MATCH_L = 8'h1a; // least significant bits
    localparam IADDR_MATCH_M = 8'h1b; // most significant bits
`else // 32bit arch
    localparam XLEN = 32;
    // APB addresses to access registers
    // APB addresses to access registers
    // the value is added to the peripheral address
    /* FILTER */
    // cause
    localparam CAUSE_UPPER = 8'h5;
    localparam CAUSE_LOWER = 8'h6;
    localparam CAUSE_MATCH = 8'h7;
    // tvec
    localparam TVEC_UPPER = 8'h8;
    localparam TVEC_LOWER = 8'ha;
    localparam TVEC_MATCH = 8'hc;
    // tval
    localparam TVAL_UPPER = 8'he;
    localparam TVAL_LOWER = 8'h10;
    localparam TVAL_MATCH = 8'h12;
    // iaddr
    localparam IADDR_UPPER = 8'h16;
    localparam IADDR_LOWER = 8'h18;
    localparam IADDR_MATCH = 8'h1a;
`endif
    /* both archs parameters */
    // localparams for resync counter
    localparam CYCLE_MODE = 0;
    localparam PACKET_MODE = 1;
    // localparams for irreport and irdepth
    localparam CALL_COUNTER_SIZE = '0;
    localparam RETURN_STACK_SIZE = '0;
    // localparams for branch map - defined by spec
    localparam BRANCH_MAP_LEN = 31;
    localparam BRANCH_COUNT_LEN = 5;
    // localparams for filter mode
    localparam RANGE_MODE = 1'b0;
    localparam EQUAL_MODE = 1'b1;
    // localparam for time signal
    localparam TIME_LEN = 64;
    // localparam for itype
    localparam ITYPE_LEN = 4;
    // localparam for iretire
    localparam IRETIRE_LEN = 32;

    // common APB addresses to access registers
    /* FILTER */
    // input and mode
    localparam CAUSE_ENABLE_MODE = 8'h0;
    localparam TVEC_ENABLE_MODE = 8'h1;
    localparam TVAL_ENABLE_MODE = 8'h2;
    localparam PRIV_ENABLE_MODE = 8'h3;
    localparam IADDR_ENABLE_MODE = 8'h4;
    // priv
    localparam PRIV_RANGE = 8'h14;
    localparam PRIV_MATCH = 8'h15;
    /* TRACE MANAGEMENT */
    localparam TRACE_STATE = 8'h1c;
    localparam LOSSLESS_TRACE = 8'h1d;
    localparam SHALLOW_TRACE = 8'h1e;
    /* PACKET EMITTER */
    localparam NO_TIME = 8'h1f;
    localparam NO_CONTEXT = 8'h20;
    localparam DELTA_ADDRESS = 8'h21;
    localparam FULL_ADDRESS = 8'h22;
    localparam IMPLICIT_EXCEPTION = 8'h23;
    localparam SIJUMP = 8'h24;
    localparam IMPLICIT_RETURN = 8'h25;
    localparam BRANCH_PREDICTION = 8'h26;
    localparam JUMP_TARGET_CACHE = 8'h27;

// packet types
typedef enum logic[1:0] { 
    F_OPT_EXT       = 2'h0,
    F_DIFF_DELTA    = 2'h1,
    F_ADDR_ONLY     = 2'h2,
    F_SYNC          = 2'h3
} format_e;

// subformats available for type 3 packets (F_SYNC)
typedef enum logic[1:0] { 
    SF_START    = 2'h0,
    SF_TRAP     = 2'h1,
    SF_CONTEXT  = 2'h2,
    SF_SUPPORT  = 2'h3
} f_sync_subformat_e;

// subformats available for type 0 packets (F_OPT_EXT)
// used a struct for future extensions
typedef enum logic[0:0] {
    SF_PBC = 1'h0, // correctly predicted branches
    SF_JTC = 1'h1 // jump target cache in spec
} f_opt_ext_subformat_e;

// qual_status values necessary for format 3 subformat 3
// packet payload
typedef enum logic[1:0] {
    NO_CHANGE   = 2'h0,
    ENDED_REP   = 2'h1,
    TRACE_LOST  = 2'h2,
    ENDED_NTR   = 2'h3
} qual_status_e;

// struct to determine the ioptions enabled/disabled
// for format 3 subformat 3 packets
typedef struct packed {
    logic delta_address_en;
    logic full_address_en;
    logic implicit_exception_en;
    logic sijump_en;
    logic implicit_return_en;
    logic branch_prediction_en;
    logic jump_target_cache_en;
} ioptions_s;

// enum that expresses the packet format and 
// subformat to better readability
typedef enum logic[3:0] {
    F0SF0   = 4'h0,
    F0SF1   = 4'h1,
    F1      = 4'h4,
    F2      = 4'h8,
    F3SF0   = 4'hC,
    F3SF1   = 4'hD,
    F3SF2   = 4'hE,
    F3SF3   = 4'hF
} it_packet_type_e; // "it" stands for "instruction trace"

/*TODO:
    doptions struct for data tracing
    refer to page 36 of the spec */

endpackage