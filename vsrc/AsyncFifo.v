

module AsyncFifo (
                  // Write Interface

                   clk_w
                  , reset_w
                  , ready_w
                  , valid_w
                  , data_w

                  // Read Interface
                  , clk_r
                  , reset_r
                  , ready_r
                  , valid_r
                  , data_r
                  
                  );

   
   //--------------------------------------------------------
   // Parameter Declarations
   
   parameter DEPTH_LG_2 = 0; // default is 'mailbox'.
   parameter WIDTH  = 32;

   localparam DEPTH = 2**DEPTH_LG_2;
                             
   //--------------------------------------------------------
   // I/O Declarations

   // Write Interface
   input clk_w;
   input reset_w;

   output ready_w;
   input  valid_w;

   input [WIDTH - 1 : 0 ] data_w;

   
   
   input               clk_r;
   input               reset_r;
   output              valid_r;
   input               ready_r;

   output [WIDTH - 1 : 0] data_r;
   
   
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

   wire                     full_w;
   wire                     fire_w;
   
   wire                     empty_r;
   wire                     fire_r;
   
   
   // Read & Write Address Pointers
   generate 
      if  (DEPTH_LG_2 > 0) begin : reg1_scope
         reg [DEPTH_LG_2 : 0 ] wrAddrReg_w;
         wire [DEPTH_LG_2 : 0] wrAddrNxt_w;
         reg [DEPTH_LG_2 : 0 ] wrAddrGrayReg_w;
         wire [DEPTH_LG_2 : 0] wrAddrGrayNxt_w;
         
         reg [DEPTH_LG_2 : 0 ] rdAddrReg_r;
         wire [DEPTH_LG_2 : 0] rdAddrNxt_r;
         reg [DEPTH_LG_2 : 0 ] rdAddrGrayReg_r;
         wire [DEPTH_LG_2 : 0] rdAddrGrayNxt_r;

         reg [DEPTH_LG_2 : 0 ] wrAddrGrayReg_sync;
         reg [DEPTH_LG_2 : 0 ] rdAddrGrayReg_sync;
         
         reg [DEPTH_LG_2 : 0 ] wrAddrGrayReg_r;
         reg [DEPTH_LG_2 : 0 ] rdAddrGrayReg_w;         
      end else begin : reg2_scope
         
         reg wrAddrReg_w;
         wire wrAddrNxt_w;
         reg  rdAddrReg_r;
         wire rdAddrNxt_r;
         
         
         reg wrAddrReg_sync;
         reg rdAddrReg_sync;
             
         reg wrAddrReg_r;
         reg rdAddrReg_w;
      end
   endgenerate

  //--------------------------------------------------------
   // Reg and Wire Declarations 

   
   // Pointer Logic
   generate 
      if  (DEPTH_LG_2 > 0) begin : ptr_logic_scope
         assign full_w  = ~(reg1_scope.wrAddrGrayReg_w[DEPTH_LG_2]         == reg1_scope.rdAddrGrayReg_w[DEPTH_LG_2]) &
                           (reg1_scope.wrAddrGrayReg_w[DEPTH_LG_2 - 1 : 0] == reg1_scope.rdAddrGrayReg_w[DEPTH_LG_2 - 1 : 0 ]);
         
         assign reg1_scope.wrAddrNxt_w      = reg1_scope.wrAddrReg_w + 1'b1; // OK / expected to overflow
         assign reg1_scope.wrAddrGrayNext_w = reg1_scope.wrAddrNxt_w ^ (reg1_scope.wrAddrNxt_w >> 1);

         assign reg1_scope.rdAddrNxt_r      = reg1_scope.rdAddrReg_r + 1'b1; // OK / expected to overflow
         assign reg1_scope.rdAddrGrayNext_r = reg1_scope.rdAddrNxt_r ^ (reg1_scope.rdAddrNxt_r >> 1);
         
         assign empty_r = (reg1_scope.wrAddrGrayReg_r == reg1_scope.rdAddrGrayReg_r);
         
      end else begin : ptr_logic_scope
         assign full_w = ~(reg2_scope.wrAddrReg_w == reg2_scope.rdAddrReg_r);
         
         assign reg2_scope.wrAddrNxt_w     = ~reg2_scope.wrAddrReg_w ;
         
         assign reg2_scope.rdAddrNxt_r     = ~reg2_scope.rdAddrReg_r;
         
         assign empty_r = (reg2_scope.wrAddrReg_r == reg2_scope.rdAddrReg_r);

      end
   endgenerate  

   assign ready_w = ~full_w;
   assign fire_w  = ready_w & valid_w;
   
   // Read Logic
   assign valid_r = ~empty_r;
   assign fire_r = ready_r & valid_r;
 
   generate 
      if  (DEPTH > 1) begin : rw_scope1
         if (DEPTH > 2) begin : rw_scope2
  
            assign data_r = mem1_scope.fifoMem[reg1_scope.rdAddrReg_r[DEPTH_LG_2-1:0]];

            always @(posedge clk_w) begin
               if (fire_w) begin
                  mem1_scope.fifoMem[reg1_scope.wrAddrReg_w[DEPTH_LG_2-1:0]] <= data_w;
               end
            end
           
         end else begin

            assign data_r = mem1_scope.fifoMem[reg1_scope.rdAddrReg_r[0]];

            always @(posedge clk_w) begin
               if (fire_w) begin
                  mem1_scope.fifoMem[reg1_scope.wrAddrReg_w[0]] <= data_w;
               end
            end
            
         end
      end else begin

         assign data_r = mem2_scope.fifoMem;

         always @(posedge clk_w) begin
            if (fire_w) begin
               mem2_scope.fifoMem <= data_w;
            end
         end
         
      end
   endgenerate
   
   //--------------------------------------------------------
   // Sequential logic
   //  
   generate 
   if  (DEPTH_LG_2 > 0) begin : seq1_scope
   always @(posedge clk_w or posedge reset_w) begin
      if (reset_w) begin
         reg1_scope.wrAddrReg_w     <= 'b0;
         reg1_scope.wrAddrGrayReg_w <= 'b0;
         
         reg1_scope.rdAddrGrayReg_sync <= 'b0;
         reg1_scope.rdAddrGrayReg_w    <= 'b0;
      end else begin
         if (fire_w) begin
           reg1_scope.wrAddrReg_w     <= reg1_scope.wrAddrNxt_w;
           reg1_scope.wrAddrGrayReg_w <= reg1_scope.wrAddrGrayNxt_w;  
         end
         reg1_scope.rdAddrGrayReg_sync <= reg1_scope.rdAddrGrayReg_r;
         reg1_scope.rdAddrGrayReg_w    <= reg1_scope.rdAddrGrayReg_sync;
      end
   end
         
   always @(posedge clk_r or posedge reset_r) begin
      if (reset_r) begin
         reg1_scope.rdAddrReg_r     <= 'b0;
         reg1_scope.rdAddrGrayReg_r <= 'b0;

         reg1_scope.reg1_scope.wrAddrGrayReg_sync  <= 'b0;
         reg1_scope.reg1_scope.wrAddrGrayReg_r     <= 'b0;
      end else begin
         if (fire_r) begin
            reg1_scope.rdAddrReg_r     <= reg1_scope.rdAddrNxt_r;
            reg1_scope.rdAddrGrayReg_r <= reg1_scope.rdAddrGrayNxt_r;
         end
         reg1_scope.wrAddrGrayReg_sync <= reg1_scope.wrAddrGrayReg_w;
         reg1_scope.wrAddrGrayReg_r    <= reg1_scope.wrAddrGrayReg_sync;
      end
   end // always @ (posedge clk_r)
      
   end else begin : seq2_scope // block: seq1_scope
        always @(posedge clk_w or posedge reset_w) begin
      if (reset_w ) begin
         reg2_scope.wrAddrReg_w     <= 1'b0;
          
         reg2_scope.rdAddrReg_sync <= 1'b0;
         reg2_scope.rdAddrReg_w    <= 1'b0;
      end else begin
         if (fire_w) begin
           reg2_scope.wrAddrReg_w     <= reg2_scope.wrAddrNxt_w;
         end
         reg2_scope.rdAddrReg_sync <= reg2_scope.rdAddrReg_r;
         reg2_scope.rdAddrReg_w    <= reg2_scope.rdAddrReg_sync;
      end
   end
         
   always @(posedge clk_r or posedge reset_r) begin
      if (reset_r) begin
         reg2_scope.rdAddrReg_r     <= 1'b0;

         reg2_scope.wrAddrReg_sync  <= 1'b0;
         reg2_scope.wrAddrReg_r     <= 1'b0;
      end else begin
         if (fire_r) begin
            reg2_scope.rdAddrReg_r <= reg2_scope.rdAddrNxt_r;
         end
         reg2_scope.wrAddrReg_sync <= reg2_scope.wrAddrReg_w;
         reg2_scope.wrAddrReg_r    <= reg2_scope.wrAddrReg_sync;
      end
   end // always @ (posedge clk_r)
   end // block: seq2_scope
      
endgenerate
   

      
endmodule // AsyncFifo
