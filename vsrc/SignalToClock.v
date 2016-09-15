  
/* This blackbox is needed by 
 * Chisel in order to do type conversion.
 * It may be useful for some synthesis flows
 * as well which require special 
 * flagging on conversion from data to clock.
 */


module SignalToClock (
             output clock_out,
             input  signal_in
                      );
   

  assign clock_out = signal_in;
                       
endmodule // SignalToClock
