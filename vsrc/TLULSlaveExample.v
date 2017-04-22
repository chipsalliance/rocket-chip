// See LICENSE.SiFive for license details.

/** This is a parameterized template for
  * a TL-UL compliant slave device
  *  
  *  @param io_slave_a_* Decoupled request interface
  *  @param io_slave_d_* Decoupled response interface
  *  
  */

module TLULSlaveExample #(
  parameter int NREGS  2,
  parameter int SIZEBITS = 4,
  parameter int SOURCEBITS = 3, 
  parameter int ADDRESSBITS = 12,
  parameter int MASKBITS = 4,
  parameter int DATABITS = 32,
  parameter int SINKBITS = 1,
  parameter int ADDRLOBITS = 4
) (
    input   clock,
    input   reset,
    input   io_slave_a_ready,
    input   io_slave_a_valid,
    input   [2:0] io_slave_a_bits_opcode,
    input   [2:0] io_slave_a_bits_param,
    input   [SIZEBITS-1:0] io_slave_a_bits_size,
    input   [SOURCEBITS-1:0] io_slave_a_bits_source,
    input   [ADDRESSBITS-1:0] io_slave_a_bits_address,
    input   [MASKBITS-1:0] io_slave_a_bits_mask,
    input   [DATABITS-1:0] io_slave_a_bits_data,
    input   io_slave_d_ready,
    output  io_slave_d_valid,
    output  [2:0] io_slave_d_bits_opcode,
    output  [1:0] io_slave_d_bits_param,
    output  [SIZEBITS-1:0] io_slave_d_bits_size,
    output  [SOURCEBITS-1:0] io_slave_d_bits_source,
    output  [SINKBITS-1:0] io_slave_d_bits_sink,
    output  [ADDRLOBITS-1:0] io_slave_d_bits_addr_lo,
    output  [DATABITS-1:0] io_slave_d_bits_data,
    output   io_slave_d_bits_error
  );

    reg [DATABITS-1:0] regs [0:NREGS-1];
  
    always @(posedge clk) begin
        if (io_slave_a_valid && io_slave_a_opcode == 3'd4) begin
            regs[io_slave_a_bits_address[0]] <= d;
        end
    end


endmodule // TLULSlaveTemplate
