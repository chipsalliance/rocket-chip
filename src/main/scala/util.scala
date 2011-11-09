package Top
{

import Chisel._
import Node._;
import scala.math._;

class ioPriorityDecoder(in_width: Int, out_width: Int) extends Bundle
{
  val in  = UFix(in_width, 'input);
  val out = Bits(out_width, 'output);
}

class priorityDecoder(width: Int) extends Component
{
  val in_width = ceil(log10(width)/log10(2)).toInt;  
  val io = new ioPriorityEncoder(in_width, width);
  val l_out = Wire() { Bits() };
  
  for (i <- 0 to width-1) {
    when (io.in === UFix(i, in_width)) {
      l_out <== Bits(1,1) << UFix(i);
    }
  }
  
  l_out <== Bits(0, width);
  io.out := l_out;
}

class ioPriorityEncoder(in_width: Int, out_width: Int) extends Bundle
{
  val in = Bits(in_width, 'input);
  val out = UFix(out_width, 'output);
}

class priorityEncoder(width: Int) extends Component
{
  val out_width = ceil(log10(width)/log10(2)).toInt;  
  val io = new ioPriorityDecoder(width, out_width);
  val l_out = Wire() { UFix() };
  
  for (i <- 0 to width-1) {
    when (io.in(i).toBool) {
      l_out <== UFix(i, out_width);
    }
  }
  
  l_out <== UFix(0, out_width);
  io.out := l_out;
}

}