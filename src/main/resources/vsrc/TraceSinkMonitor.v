module TraceSinkMonitor
#(
    parameter FILE_NAME = "trace_sink_monitor.txt"
)
(
    input clk,
    input reset,
    input in_fire,
    input[7:0] in_byte
);

`ifdef VCS

integer file;

initial begin
    file = $fopen(FILE_NAME, "w");
    if (file == 0) begin
        $display("Failed to open %s", FILE_NAME);
        $finish;
    end
end

always @(posedge clk) begin
    if (in_fire & ~reset) begin
        $fwrite(file, "%c", in_byte);
    end
end 

final begin
    $fclose(file);
end

`endif

endmodule