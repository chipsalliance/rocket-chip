
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

endinterface: tilelink_if //}

