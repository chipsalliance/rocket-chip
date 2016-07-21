
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


    // From uncore/src/main/scala/coherence/Policies.scala class MESICoherence:
    //// val acquireShared :: acquireExclusive :: Nil = Enum(UInt(), nAcquireTypes)
    //// val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(UInt(), nProbeTypes)
    //// val releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(UInt(), nReleaseTypes)
    //// val grantShared :: grantExclusive :: grantExclusiveAck :: Nil = Enum(UInt(), nGrantTypes)

    // Builtin types come from uncore/src/main/scala/tilelink/Definitions.scala object Acquire:
    typedef enum logic [3:0] {
        acquireShared   = 4'b0000,
        acquireExclusive= 4'b0001,
        getType         = 4'b1000,
        getBlockType    = 4'b1001,
        putType         = 4'b1010,
        putBlockType    = 4'b1011,
        putAtomicType   = 4'b1100,
        getPrefetchType = 4'b1101,
        putPrefetchType = 4'b1110
    } acquire_type_e;

    typedef enum logic [3:0] {
        grantShared      = 4'b0000,
        grantExclusive   = 4'b0001,
        grantExclusiveAck= 4'b0010,
        voluntaryAckType = 4'b1000,
        prefetchAckType  = 4'b1001,
        putAckType       = 4'b1011,
        getDataBeatType  = 4'b1100,
        getDataBlockType = 4'b1101
    } grant_type_e;

    typedef enum logic [1:0] {
        probeInvalidate = 2'd0,
        probeDowngrade  = 2'd1,
        probeCopy       = 2'd2
    } probe_type_e;

    typedef enum logic [2:0] {
        releaseInvalidateData = 3'd0,
        releaseDowngradeData  = 3'd1,
        releaseCopyData       = 3'd2,
        releaseInvalidateAck  = 3'd3,
        releaseDowngradeAck   = 3'd4,
        releaseCopyAck        = 3'd5
    } release_type_e;

// Trivial coverage: at least one transaction in each channel
cover_acquire: cover property ( @(posedge clk) acquire_ready && acquire_valid );
cover_grant:   cover property ( @(posedge clk) grant_ready && grant_valid );
cover_probe:   cover property ( @(posedge clk) probe_ready && probe_valid );
cover_release: cover property ( @(posedge clk) release_ready && release_valid );
cover_finish:  cover property ( @(posedge clk) finish_ready && finish_valid );

// Cover all different values for *type fields:
acquire_type_e acquire_type;
assign acquire_type = acquire_type_e'({acquire_bits_is_builtin_type, acquire_bits_a_type});
covergroup acquire_type_cg
    @(posedge clk iff (acquire_ready && acquire_valid));
    coverpoint acquire_type;
endgroup
acquire_type_cg acquire_type_i = new;


grant_type_e grant_type;
assign grant_type = grant_type_e'({grant_bits_is_builtin_type, grant_bits_g_type});
covergroup grant_type_cg
    @(posedge clk iff (grant_ready && grant_valid));
    coverpoint grant_type;
endgroup
grant_type_cg grant_type_i = new;



probe_type_e probe_type;
assign probe_type = probe_type_e'(probe_bits_p_type);
covergroup probe_type_cg
    @(posedge clk iff (probe_ready && probe_valid));
    coverpoint probe_type;
endgroup
probe_type_cg probe_type_i = new;


release_type_e release_type;
assign release_type = release_type_e'(release_bits_r_type);
covergroup release_type_cg
    @(posedge clk iff (release_ready && release_valid));
    coverpoint release_type;
endgroup
release_type_cg release_type_i = new;

// HACK consider reset in all these
logic acquire_has_wmask;
assign acquire_has_wmask = (acquire_type == putType) || (acquire_type == putBlockType); // HACK use inside
logic [7:0] acquire_wmask;
assign acquire_wmask = acquire_bits_union[8:1];
covergroup acquire_wmask_cg
    @(posedge clk iff (acquire_ready && acquire_valid && acquire_has_wmask));
    coverpoint acquire_wmask {
        bins all[] = { [0:$] };
    }
endgroup
acquire_wmask_cg acquire_wmask_i = new;


endinterface: tilelink_if //}

