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

/* BRANCH MAP */
/*
It keeps track of taken and non taken branches.

Whenever a branch happens it updates the branch map
and the number of branches stored.
    
When flush_i signal is asserted, the branch map is
cleaned.
*/

module te_branch_map #(
    parameter N = 1 // max number of committed branches in one cycle
)
(
    input logic                                 clk_i,
    input logic                                 rst_ni,

    input logic [N-1:0]                         valid_i,
    input logic [N-1:0]                         branch_taken_i,
    input logic                                 flush_i,
    //input logic branch_taken_prediction_i, // non mandatory
    
    output logic [te_pkg::BRANCH_MAP_LEN-1:0]   map_o, // array of branch taken and not 
    output logic [te_pkg::BRANCH_COUNT_LEN-1:0] branches_o, // number of branches stored, up to 31
    //output logic [:0] pbc_o, // non mandatory - branch prediction mode
    //output logic misprediction_o, // non mandatory - ibidem
    output logic                                is_full_o,
    output logic                                is_empty_o
);
    /* from the spec:
        A vector where each bit represents the outcome of a branch.
        A 0 indicates the branch was taken, a 1 indicates that it was not.

        This means at the beginning I set all the branch map values as 1s.
    */

    logic [te_pkg::BRANCH_MAP_LEN-1:0]      map_d, map_q;
    logic [te_pkg::BRANCH_COUNT_LEN-1:0]    branch_cnt_d, branch_cnt_q;
    logic [$clog2(N):0]                     valid_left_d, valid_left_q;
    logic [N**3-1:0]                        status_left_d, status_left_q;
    logic [$clog2(N**3)-1:0]                served;
    logic [$clog2(N**3)-1:0]                to_serve;
    logic [N-1:0]                           valid_d, valid_q;
    logic [N-1:0]                           branch_taken_d, branch_taken_q;
 
    assign map_o = map_q;
    assign branches_o = branch_cnt_q;
    assign is_full_o = (branch_cnt_q == 31) && ~flush_i;
    assign is_empty_o = (branch_cnt_q == 0);
    assign branch_taken_d = branch_taken_i;
    assign valid_d = valid_i;

    always_comb begin
        // init
        map_d = map_q;
        branch_cnt_d = branch_cnt_q;
        served = '0;
        to_serve = '0;

        // loading stored info
        to_serve = valid_left_q;
        status_left_d = status_left_q;

        if (flush_i) begin
            branch_cnt_d = '0;
            map_d = '0;
        end

        // summing the inputs
        for (int i = 0; i < N; i++) begin
            if (valid_q[i]) begin
                status_left_d[to_serve] = branch_taken_q[i];
                to_serve += 1;
            end
        end

        // serving what's possible
        for (int i = 0; i < to_serve; i++) begin
            // normal increment and branch_map assignment
            if (branch_cnt_d < 31 && to_serve > 0) begin
                map_d[branch_cnt_d] = ~status_left_d[i];
                branch_cnt_d += 1;
                served += 1;
            end
        end

        // storing what couldn't be served
        status_left_d = status_left_d >> served; // shift right
        valid_left_d = to_serve - served;

        // flush and no branch received
        if (flush_i && |valid_q == '0) begin
            map_d = '0;
            branch_cnt_d = '0;
        end

        // synchronous reset
        if (served == to_serve) begin
            valid_left_d = '0;
        end
    end


    always_ff @(posedge clk_i, negedge rst_ni) begin
        if(~rst_ni) begin
            map_q <= '0;
            branch_cnt_q <= '0;
            valid_left_q <= 0;
            status_left_q <= '0;
            valid_q <= '0;
            branch_taken_q <= '0;
        end else begin
            valid_q <= valid_d;
            branch_taken_q <= branch_taken_d;
            map_q <= map_d;
            branch_cnt_q <= branch_cnt_d;            
            valid_left_q <= valid_left_d;
            status_left_q <= status_left_d;
        end
    end     
    
endmodule