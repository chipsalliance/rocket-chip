package rocket

import Chisel._
import Node._
import scala.math._

class Mux1H [T <: Data](n: Int)(gen: => T) extends Component
{
  val io = new Bundle {
    val sel = Vec(n) { Bool(dir = INPUT) }
    val in  = Vec(n) { gen }.asInput
    val out = gen.asOutput
  }

  io.out := Mux1H(io.sel, io.in)
}
