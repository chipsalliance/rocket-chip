

module AsyncMailbox (
                  // Write Interface

                    enq_clock
                  , enq_reset
                  , enq_ready
                  , enq_valid
                  , enq_bits

                  // Read Interface
                  , deq_clock
                  , deq_reset
                  , deq_ready
                  , deq_valid
                  , deq_bits
                  
                  );

   
   //--------------------------------------------------------
   // Parameter Declarations
   
   parameter WIDTH  = 64;

   //--------------------------------------------------------
   // I/O Declarations

   // Write Interface
   input enq_clock;
   wire  w_clock = enq_clock;
   
   input enq_reset;
   wire  w_reset = enq_reset;
 
   output enq_ready;
   wire w_ready;
   assign enq_ready = w_ready;
   
   input enq_valid;
   wire  w_valid = enq_valid;
   
   input [WIDTH - 1 : 0 ] enq_bits;
   wire  [WIDTH - 1 : 0 ] w_bits = enq_bits;
    
   // Read Interface
   input               deq_clock;
   wire                r_clock = deq_clock;
   
   input               deq_reset;
   wire                r_reset = deq_reset;
   
   output              deq_valid;
   wire                r_valid;
   assign deq_valid = r_valid;
          
   input               deq_ready;
   wire                r_ready = deq_ready;
   
   output [WIDTH - 1 : 0] deq_bits;
   wire [WIDTH - 1 : 0]   r_bits;
   assign deq_bits = r_bits;
      
   //--------------------------------------------------------
   // FIFO Memory Declaration

   reg [WIDTH - 1 :0]     mailboxReg;
         
   //--------------------------------------------------------
   // Reg and Wire Declarations 

   wire                     w_full;
   wire                     w_fire;
   
   wire                     r_empty;
   wire                     r_fire;
   
   
   // Read & Write Address Pointers
   reg                      w_wrAddrReg;
   wire                     w_wrAddrNxt;
   reg                      r_rdAddrReg;
   wire                     r_rdAddrNxt;
   
   
   reg                      wrAddrReg_sync;
   reg                      rdAddrReg_sync;
   
   reg                      r_wrAddrReg;
   reg                      w_rdAddrReg;
   
   //--------------------------------------------------------
   // Reg and Wire Declarations 

   assign w_full = ~(w_wrAddrReg == r_rdAddrReg);
   
   assign w_wrAddrNxt     = ~w_wrAddrReg ;
   
   assign r_rdAddrNxt     = ~r_rdAddrReg;
   
   assign r_empty = (r_wrAddrReg == r_rdAddrReg);
   
   assign w_ready = ~w_full;
   assign w_fire  = w_ready & w_valid;
   
   // Read Logic
   assign r_valid = ~r_empty;
   assign r_fire = r_ready & r_valid;
   
   
   assign r_bits = mailboxReg;
   
   always @(posedge w_clock) begin
      if (w_fire) begin
         mailboxReg <= w_bits;
      end
   end
   
   //--------------------------------------------------------
   // Sequential logic
   //  
 
   always @(posedge w_clock or posedge w_reset) begin
      if (w_reset ) begin
         w_wrAddrReg     <= 1'b0;
         
         rdAddrReg_sync <= 1'b0;
         w_rdAddrReg    <= 1'b0;
      end else begin
         if (w_fire) begin
            w_wrAddrReg     <= w_wrAddrNxt;
         end
         rdAddrReg_sync <= r_rdAddrReg;
         w_rdAddrReg    <= rdAddrReg_sync;
      end
   end
   
   always @(posedge r_clock or posedge r_reset) begin
      if (r_reset) begin
         r_rdAddrReg     <= 1'b0;

         wrAddrReg_sync  <= 1'b0;
         r_wrAddrReg     <= 1'b0;
      end else begin
         if (r_fire) begin
            r_rdAddrReg <= r_rdAddrNxt;
         end
         wrAddrReg_sync <= w_wrAddrReg;
         r_wrAddrReg    <= wrAddrReg_sync;
      end
   end // always @ (posedge r_clock)
         
endmodule // AsyncMailbox
