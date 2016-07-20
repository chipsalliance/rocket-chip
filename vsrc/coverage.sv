
module coverage(); //{

`define HUB Top.uncore.outmemsys.L2BroadcastHub_1

    tilelink_if bh_inner(.clk(`HUB.clk), .reset(`HUB.reset));

    assign bh_inner.acquire_ready                = `HUB.io_inner_acquire_ready;
    assign bh_inner.acquire_valid                = `HUB.io_inner_acquire_valid;
    assign bh_inner.acquire_bits_addr_block      = `HUB.io_inner_acquire_bits_addr_block;
    assign bh_inner.acquire_bits_client_xact_id  = `HUB.io_inner_acquire_bits_client_xact_id;
    assign bh_inner.acquire_bits_addr_beat       = `HUB.io_inner_acquire_bits_addr_beat;
    assign bh_inner.acquire_bits_is_builtin_type = `HUB.io_inner_acquire_bits_is_builtin_type;
    assign bh_inner.acquire_bits_a_type          = `HUB.io_inner_acquire_bits_a_type;
    assign bh_inner.acquire_bits_union           = `HUB.io_inner_acquire_bits_union;
    assign bh_inner.acquire_bits_data            = `HUB.io_inner_acquire_bits_data;
    assign bh_inner.acquire_bits_client_id       = `HUB.io_inner_acquire_bits_client_id;
    assign bh_inner.grant_ready                  = `HUB.io_inner_grant_ready;
    assign bh_inner.grant_valid                  = `HUB.io_inner_grant_valid;
    assign bh_inner.grant_bits_addr_beat         = `HUB.io_inner_grant_bits_addr_beat;
    assign bh_inner.grant_bits_client_xact_id    = `HUB.io_inner_grant_bits_client_xact_id;
    assign bh_inner.grant_bits_manager_xact_id   = `HUB.io_inner_grant_bits_manager_xact_id;
    assign bh_inner.grant_bits_is_builtin_type   = `HUB.io_inner_grant_bits_is_builtin_type;
    assign bh_inner.grant_bits_g_type            = `HUB.io_inner_grant_bits_g_type;
    assign bh_inner.grant_bits_data              = `HUB.io_inner_grant_bits_data;
    assign bh_inner.grant_bits_client_id         = `HUB.io_inner_grant_bits_client_id;
    assign bh_inner.finish_ready                 = `HUB.io_inner_finish_ready;
    assign bh_inner.finish_valid                 = `HUB.io_inner_finish_valid;
    assign bh_inner.finish_bits_manager_xact_id  = `HUB.io_inner_finish_bits_manager_xact_id;
    assign bh_inner.probe_ready                  = `HUB.io_inner_probe_ready;
    assign bh_inner.probe_valid                  = `HUB.io_inner_probe_valid;
    assign bh_inner.probe_bits_addr_block        = `HUB.io_inner_probe_bits_addr_block;
    assign bh_inner.probe_bits_p_type            = `HUB.io_inner_probe_bits_p_type;
    assign bh_inner.probe_bits_client_id         = `HUB.io_inner_probe_bits_client_id;
    assign bh_inner.release_ready                = `HUB.io_inner_release_ready;
    assign bh_inner.release_valid                = `HUB.io_inner_release_valid;
    assign bh_inner.release_bits_addr_beat       = `HUB.io_inner_release_bits_addr_beat;
    assign bh_inner.release_bits_addr_block      = `HUB.io_inner_release_bits_addr_block;
    assign bh_inner.release_bits_client_xact_id  = `HUB.io_inner_release_bits_client_xact_id;
    assign bh_inner.release_bits_voluntary       = `HUB.io_inner_release_bits_voluntary;
    assign bh_inner.release_bits_r_type          = `HUB.io_inner_release_bits_r_type;
    assign bh_inner.release_bits_data            = `HUB.io_inner_release_bits_data;
    assign bh_inner.release_bits_client_id       = `HUB.io_inner_release_bits_client_id;

endmodule: coverage //}

