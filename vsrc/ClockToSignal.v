  
/* This blackbox is needed by 
 * Chisel in order to do type conversion.
 * It may be useful for some synthesis flows
 * as well which require special 
 * flagging on conversion from data to clock.
 */


module ClockToSignal(
             output signal_out,
             input  clock_in
                      );
   

  assign signal_out = clock_in;
                       
endmodule // ClockToSignal

