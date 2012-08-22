package rocket

import Chisel._
import Node._
import scala.math._

object FillInterleaved
{
  def apply(n: Int, in: Bits) =
  {
    var out = Fill(n, in(0))
    for (i <- 1 until in.getWidth)
      out = Cat(Fill(n, in(i)), out)
    out
  }
}

class Mux1H [T <: Data](n: Int)(gen: => T) extends Component
{
  val io = new Bundle {
    val sel = Vec(n) { Bool(dir = INPUT) }
    val in  = Vec(n) { gen }.asInput
    val out = gen.asOutput
  }

  io.out := Mux1H(io.sel, io.in)
}

class ioLockingArbiter[T <: Data](n: Int)(data: => T) extends Bundle {
  val in   = Vec(n) { (new FIFOIO()) { data } }.flip
  val lock = Vec(n) { Bool() }.asInput
  val out  = (new FIFOIO()) { data }
}

class LockingArbiter[T <: Data](n: Int)(data: => T) extends Component {
  val io = new ioLockingArbiter(n)(data)
  val locked = Vec(n) { Reg(resetVal = Bool(false)) }
  val any_lock_held = (locked.toBits & io.lock.toBits).orR
  val valid_arr = Vec(n) { Bool() } 
  val bits_arr = Vec(n) { data } 
  for(i <- 0 until n) {
    valid_arr(i) := io.in(i).valid
    bits_arr(i) := io.in(i).bits
  }

  io.in(0).ready := Mux(any_lock_held, io.out.ready && locked(0), io.out.ready)
  locked(0) := Mux(any_lock_held, locked(0), io.in(0).ready && io.lock(0))
  for (i <- 1 until n) {
    io.in(i).ready := Mux(any_lock_held, io.out.ready && locked(i), 
                          !io.in(i-1).valid && io.in(i-1).ready)
    locked(i) := Mux(any_lock_held, locked(i), io.in(i).ready && io.lock(i))
  }

  var dout = io.in(n-1).bits
  for (i <- 1 until n)
    dout = Mux(io.in(n-1-i).valid, io.in(n-1-i).bits, dout)

  var vout = io.in(0).valid
  for (i <- 1 until n)
    vout = vout || io.in(i).valid

  val lock_idx = PriorityEncoder(locked.toBits)
  io.out.valid := Mux(any_lock_held, valid_arr(lock_idx), vout)
  io.out.bits  := Mux(any_lock_held, bits_arr(lock_idx), dout)
}

object PriorityMux
{
  def apply[T <: Data](sel: Seq[Bits], in: Seq[T]): T = {
    if (in.size == 1)
      in.head
    else
      Mux(sel.head, in.head, apply(sel.tail, in.tail))
  }
  def apply[T <: Data](sel: Bits, in: Seq[T]): T = apply((0 until in.size).map(sel(_)), in)
}

object PriorityEncoder
{
  def apply(in: Seq[Bits]): UFix = PriorityMux(in, (0 until in.size).map(UFix(_)))
  def apply(in: Bits): UFix = apply((0 until in.getWidth).map(in(_)))
}

object PriorityEncoderOH
{
  def apply(in: Bits): Bits = Vec(apply((0 until in.getWidth).map(in(_)))){Bool()}.toBits
  def apply(in: Seq[Bits]): Seq[Bool] = {
    var none_hot = Bool(true)
    val out = collection.mutable.ArrayBuffer[Bool]()
    for (i <- 0 until in.size) {
      out += none_hot && in(i)
      none_hot = none_hot && !in(i)
    }
    out
  }
}

object Counter
{
  def apply(cond: Bool, n: Int) = {
    val c = Reg(resetVal = UFix(0, log2Up(n)))
    val wrap = c === UFix(n-1)
    when (cond) {
      c := Mux(Bool(!isPow2(n)) && wrap, UFix(0), c + UFix(1))
    }
    (c, wrap && cond)
  }
}
