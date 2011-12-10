package Top
{

import Chisel._
import Node._;
import scala.math._;

class MuxN[T <: Data](n: Int)(data: => T) extends Component {
  val io = new Bundle {
    val sel = Bits(width = ceil(log(n)/log(2)).toInt)
    val in  = Vec(n) { data }.asInput()
    val out = data.asOutput()
  }

  val out = Vec(n) { Wire() { data } }
  out(0) <== io.in(0)
  for (i <- 1 to n-1) {
    out(i) <== Mux(io.sel === UFix(i), io.in(i), out(i-1))
  }

  out(n-1) ^^ io.out
}

class Mux1H(n: Int, w: Int) extends Component
{
  val io = new Bundle {
    val sel = Vec(n) { Bool(dir = 'input) }
    val in  = Vec(n) { Bits(width = w, dir = 'input) }
    val out = Bits(width = w, dir = 'output)
  }

  if (n > 1) {
    val out = Vec(n) { Wire() { Bits(width = w) } }
    out(0) <== io.in(0) & Fill(w, io.sel(0))
    for (i <- 1 to n-1) {
      out(i) <== out(i-1) | (io.in(i) & Fill(w, io.sel(i)))
    }

    io.out := out(n-1)
  } else {
    io.out := io.in(0)
  }
}

class ioDecoupled[T <: Data]()(data: => T) extends Bundle
{
  val valid = Bool('input)
  val ready = Bool('output)
  val bits  = data.asInput
}

class ioArbiter[T <: Data](n: Int)(data: => T) extends Bundle {
  val in  = Vec(n) { (new ioDecoupled()) { data } }
  val out = (new ioDecoupled()) { data }.flip()
}

class Arbiter[T <: Data](n: Int)(data: => T) extends Component {
  val io = new ioArbiter(n)(data)
  val dout = Vec(n) { Wire() { data } }
  val vout = Wire { Bool() }

  io.in(0).ready := io.out.ready
  for (i <- 1 to n-1) {
    io.in(i).ready := !io.in(i-1).valid && io.in(i-1).ready
  }

  dout(0) <== io.in(n-1).bits
  for (i <- 1 to n-1) {
    dout(i) <== Mux(io.in(n-1-i).valid, io.in(n-1-i).bits, dout(i-1))
  }

  for (i <- 0 to n-2) {
    when (io.in(i).valid) { vout <== Bool(true) }
  }
  vout <== io.in(n-1).valid

  vout      ^^ io.out.valid
  dout(n-1) ^^ io.out.bits
}

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
  val in  = Bits(in_width, 'input);
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
