

module AsyncFifo (
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
   
   parameter DEPTH_LG_2 = 0; // default is 'mailbox'.
   parameter WIDTH  = 64;

   localparam DEPTH = 2**DEPTH_LG_2;
                             
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

   generate 
      if  (DEPTH > 1) begin : mem1_scope
         reg [WIDTH - 1 : 0] fifoMem [ 0 : DEPTH - 1];
      end else begin : mem2_scope
         reg [WIDTH - 1 :0] fifoMem;
      end
   endgenerate
      
   //--------------------------------------------------------
   // Reg and Wire Declarations 

   wire                     w_full;
   wire                     w_fire;
   
   wire                     r_empty;
   wire                     r_fire;
   
   
   // Read & Write Address Pointers
   generate 
      if  (DEPTH_LG_2 > 0) begin : reg1_scope
         reg [DEPTH_LG_2 : 0 ] w_wrAddrReg;
         wire [DEPTH_LG_2 : 0] w_wrAddrNxt;
         reg [DEPTH_LG_2 : 0 ] w_wrAddrGrayReg;
         wire [DEPTH_LG_2 : 0] w_wrAddrGrayNxt;
         
         reg [DEPTH_LG_2 : 0 ] r_rdAddrReg;
         wire [DEPTH_LG_2 : 0] r_rdAddrNxt;
         reg [DEPTH_LG_2 : 0 ] r_rdAddrGrayReg;
         wire [DEPTH_LG_2 : 0] r_rdAddrGrayNxt;

         reg [DEPTH_LG_2 : 0 ] wrAddrGrayReg_sync;
         reg [DEPTH_LG_2 : 0 ] rdAddrGrayReg_sync;
         
         reg [DEPTH_LG_2 : 0 ] r_wrAddrGrayReg;
         reg [DEPTH_LG_2 : 0 ] w_rdAddrGrayReg;         
      end else begin : reg2_scope
         
         reg w_wrAddrReg;
         wire w_wrAddrNxt;
         reg  r_rdAddrReg;
         wire r_rdAddrNxt;
         
         
         reg wrAddrReg_sync;
         reg rdAddrReg_sync;
             
         reg r_wrAddrReg;
         reg w_rdAddrReg;
      end
   endgenerate

  //--------------------------------------------------------
   // Reg and Wire Declarations 

   
   // Pointer Logic
   generate 
      if  (DEPTH_LG_2 > 0) begin : ptr_logic_scope
         assign w_full  = ~(reg1_scope.w_wrAddrGrayReg[DEPTH_LG_2]         == reg1_scope.w_rdAddrGrayReg[DEPTH_LG_2]) &
                           (reg1_scope.w_wrAddrGrayReg[DEPTH_LG_2 - 1 : 0] == reg1_scope.w_rdAddrGrayReg[DEPTH_LG_2 - 1 : 0 ]);
         
         assign reg1_scope.w_wrAddrNxt      = reg1_scope.w_wrAddrReg + 1'b1; // OK / expected to overflow
         assign reg1_scope.w_wrAddrGrayNext = reg1_scope.w_wrAddrNxt ^ (reg1_scope.w_wrAddrNxt >> 1);

         assign reg1_scope.r_rdAddrNxt      = reg1_scope.r_rdAddrReg + 1'b1; // OK / expected to overflow
         assign reg1_scope.r_rdAddrGrayNext = reg1_scope.r_rdAddrNxt ^ (reg1_scope.r_rdAddrNxt >> 1);
         
         assign r_empty = (reg1_scope.r_wrAddrGrayReg == reg1_scope.r_rdAddrGrayReg);
         
      end else begin : ptr_logic_scope
         assign w_full = ~(reg2_scope.w_wrAddrReg == reg2_scope.r_rdAddrReg);
         
         assign reg2_scope.w_wrAddrNxt     = ~reg2_scope.w_wrAddrReg ;
         
         assign reg2_scope.r_rdAddrNxt     = ~reg2_scope.r_rdAddrReg;
         
         assign r_empty = (reg2_scope.r_wrAddrReg == reg2_scope.r_rdAddrReg);

      end
   endgenerate  

   assign w_ready = ~w_full;
   assign w_fire  = w_ready & w_valid;
   
   // Read Logic
   assign r_valid = ~r_empty;
   assign r_fire = r_ready & r_valid;
 
   generate 
      if  (DEPTH > 1) begin : rw_scope1
         if (DEPTH > 2) begin : rw_scope2
  
            assign r_bits = mem1_scope.fifoMem[reg1_scope.r_rdAddrReg[DEPTH_LG_2-1:0]];

            always @(posedge w_clock) begin
               if (w_fire) begin
                  mem1_scope.fifoMem[reg1_scope.w_wrAddrReg[DEPTH_LG_2-1:0]] <= w_bits;
               end
            end
           
         end else begin

            assign r_bits = mem1_scope.fifoMem[reg1_scope.r_rdAddrReg[0]];

            always @(posedge w_clock) begin
               if (w_fire) begin
                  mem1_scope.fifoMem[reg1_scope.w_wrAddrReg[0]] <= w_bits;
               end
            end
            
         end
      end else begin

         assign r_bits = mem2_scope.fifoMem;

         always @(posedge w_clock) begin
            if (w_fire) begin
               mem2_scope.fifoMem <= w_bits;
            end
         end
         
      end
   endgenerate
   
   //--------------------------------------------------------
   // Sequential logic
   //  
   generate 
   if  (DEPTH_LG_2 > 0) begin : seq1_scope
   always @(posedge w_clock or posedge w_reset) begin
      if (w_reset) begin
         reg1_scope.w_wrAddrReg     <= 'b0;
         reg1_scope.w_wrAddrGrayReg <= 'b0;
         
         reg1_scope.rdAddrGrayReg_sync <= 'b0;
         reg1_scope.w_rdAddrGrayReg    <= 'b0;
      end else begin
         if (w_fire) begin
           reg1_scope.w_wrAddrReg     <= reg1_scope.w_wrAddrNxt;
           reg1_scope.w_wrAddrGrayReg <= reg1_scope.w_wrAddrGrayNxt;  
         end
         reg1_scope.rdAddrGrayReg_sync <= reg1_scope.r_rdAddrGrayReg;
         reg1_scope.w_rdAddrGrayReg    <= reg1_scope.rdAddrGrayReg_sync;
      end
   end
         
   always @(posedge r_clock or posedge r_reset) begin
      if (r_reset) begin
         reg1_scope.r_rdAddrReg     <= 'b0;
         reg1_scope.r_rdAddrGrayReg <= 'b0;

         reg1_scope.reg1_scope.wrAddrGrayReg_sync  <= 'b0;
         reg1_scope.reg1_scope.r_wrAddrGrayReg     <= 'b0;
      end else begin
         if (r_fire) begin
            reg1_scope.r_rdAddrReg     <= reg1_scope.r_rdAddrNxt;
            reg1_scope.r_rdAddrGrayReg <= reg1_scope.r_rdAddrGrayNxt;
         end
         reg1_scope.wrAddrGrayReg_sync <= reg1_scope.w_wrAddrGrayReg;
         reg1_scope.r_wrAddrGrayReg    <= reg1_scope.wrAddrGrayReg_sync;
      end
   end // always @ (posedge r_clock)
      
   end else begin : seq2_scope // block: seq1_scope
        always @(posedge w_clock or posedge w_reset) begin
      if (w_reset ) begin
         reg2_scope.w_wrAddrReg     <= 1'b0;
          
         reg2_scope.rdAddrReg_sync <= 1'b0;
         reg2_scope.w_rdAddrReg    <= 1'b0;
      end else begin
         if (w_fire) begin
           reg2_scope.w_wrAddrReg     <= reg2_scope.w_wrAddrNxt;
         end
         reg2_scope.rdAddrReg_sync <= reg2_scope.r_rdAddrReg;
         reg2_scope.w_rdAddrReg    <= reg2_scope.rdAddrReg_sync;
      end
   end
         
   always @(posedge r_clock or posedge r_reset) begin
      if (r_reset) begin
         reg2_scope.r_rdAddrReg     <= 1'b0;

         reg2_scope.wrAddrReg_sync  <= 1'b0;
         reg2_scope.r_wrAddrReg     <= 1'b0;
      end else begin
         if (r_fire) begin
            reg2_scope.r_rdAddrReg <= reg2_scope.r_rdAddrNxt;
         end
         reg2_scope.wrAddrReg_sync <= reg2_scope.w_wrAddrReg;
         reg2_scope.r_wrAddrReg    <= reg2_scope.wrAddrReg_sync;
      end
   end // always @ (posedge r_clock)
   end // block: seq2_scope
      
endgenerate
   

      
endmodule // AsyncFifo
