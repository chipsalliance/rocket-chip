
interface tilelink_if(input clk, input reset); //{
    logic        acquire_ready;
    logic        acquire_valid;
    logic [25:0] acquire_bits_addr_block;
    logic [ 1:0] acquire_bits_client_xact_id;
    logic [ 2:0] acquire_bits_addr_beat;
    logic        acquire_bits_is_builtin_type;
    logic [ 2:0] acquire_bits_a_type;
    logic [11:0] acquire_bits_union;
    logic [63:0] acquire_bits_data;
    logic        acquire_bits_client_id;
    logic        grant_ready;
    logic        grant_valid;
    logic [ 2:0] grant_bits_addr_beat;
    logic [ 1:0] grant_bits_client_xact_id;
    logic [ 3:0] grant_bits_manager_xact_id;
    logic        grant_bits_is_builtin_type;
    logic [ 3:0] grant_bits_g_type;
    logic [63:0] grant_bits_data;
    logic        grant_bits_client_id;
    logic        finish_ready;
    logic        finish_valid;
    logic [ 3:0] finish_bits_manager_xact_id;
    logic        probe_ready;
    logic        probe_valid;
    logic [25:0] probe_bits_addr_block;
    logic [ 1:0] probe_bits_p_type;
    logic        probe_bits_client_id;
    logic        release_ready;
    logic        release_valid;
    logic [ 2:0] release_bits_addr_beat;
    logic [25:0] release_bits_addr_block;
    logic [ 1:0] release_bits_client_xact_id;
    logic        release_bits_voluntary;
    logic [ 2:0] release_bits_r_type;
    logic [63:0] release_bits_data;
    logic        release_bits_client_id;

cover_acquire: cover property ( @(posedge clk) acquire_ready && acquire_valid );
cover_grant:   cover property ( @(posedge clk) grant_ready && grant_valid );
cover_probe:   cover property ( @(posedge clk) probe_ready && probe_valid );
cover_release: cover property ( @(posedge clk) release_ready && release_valid );
cover_finish:  cover property ( @(posedge clk) finish_ready && finish_valid );

covergroup acquire_type_cg
    @(posedge clk iff (acquire_ready && acquire_valid));
    coverpoint acquire_bits_a_type;
endgroup

covergroup grant_type_cg
    @(posedge clk iff (grant_ready && grant_valid));
    coverpoint grant_bits_g_type;
endgroup

covergroup probe_type_cg
    @(posedge clk iff (probe_ready && probe_valid));
    coverpoint probe_bits_p_type;
endgroup

covergroup release_type_cg
    @(posedge clk iff (release_ready && release_valid));
    coverpoint release_bits_r_type;
endgroup


acquire_type_cg acquire_type = new;
grant_type_cg grant_type = new;
probe_type_cg probe_type = new;
release_type_cg release_type = new;

endinterface: tilelink_if //}

