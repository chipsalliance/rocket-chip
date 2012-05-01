package rocket

import Chisel._
import Node._
import scala.math._


object foldR
{
  def apply[T <: Bits](x: Seq[T])(f: (T, T) => T): T =
    if (x.length == 1) x(0) else f(x(0), foldR(x.slice(1, x.length))(f))
}

object log2up
{
  def apply(in: Int) = ceil(log(in)/log(2)).toInt
}

object ispow2
{
  def apply(in: Int) = in > 0 && ((in & (in-1)) == 0)
}

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

// http://aggregate.ee.engr.uky.edu/MAGIC/#Population%20Count%20%28Ones%20Count%29
// http://bits.stephan-brumme.com/countBits.html
object PopCount
{
  def apply(in: Bits) =
  {
    require(in.width <= 32)
    val w = log2up(in.width+1)
    var x = in
    if(in.width == 2) { 
      x = x - ((x >> UFix(1)) & Bits("h_5555_5555"))
    } else if(in.width <= 4) {
      x = x - ((x >> UFix(1)) & Bits("h_5555_5555"))
      x = (((x >> UFix(2)) & Bits("h_3333_3333")) + (x & Bits("h_3333_3333")))
    } else if(in.width <= 8) {
      x = x - ((x >> UFix(1)) & Bits("h_5555_5555"))
      x = (((x >> UFix(2)) & Bits("h_3333_3333")) + (x & Bits("h_3333_3333")))
      x = ((x >> UFix(4)) + x) 
    } else {
      // count bits of each 2-bit chunk
      x = x - ((x >> UFix(1)) & Bits("h_5555_5555"))
      // count bits of each 4-bit chunk
      x = (((x >> UFix(2)) & Bits("h_3333_3333")) + (x & Bits("h_3333_3333")))
      // count bits of each 8-bit chunk
      x = ((x >> UFix(4)) + x) 
      // mask junk in upper bits
      x = x & Bits("h_0f0f_0f0f")
      // add all four 8-bit chunks
      x = x + (x >> UFix(8))
      x = x + (x >> UFix(16))
    }
    x(w-1,0)
  }
}

object Reverse
{
  def doit(in: Bits, base: Int, length: Int): Bits =
  {
    val half = (1 << log2up(length))/2
    if (length == 1)
      in(base)
    else
      Cat(doit(in, base, half), doit(in, base+half, length-half))
  }
  def apply(in: Bits) = doit(in, 0, in.getWidth)
}

object OHToUFix
{
  def apply(in: Seq[Bits]): UFix = {
    if (in.size <= 1) return UFix(0)
    if (in.size == 2) return in(1)
    val hi = in.slice(in.size/2, in.size)
    val lo = in.slice(0, in.size/2)
    Cat(hi.reduceLeft(_||_), apply(hi zip lo map { case (x, y) => x || y }))
  }
  def apply(in: Bits): UFix = apply((0 until in.getWidth).map(in(_)))
}

object UFixToOH
{
  def apply(in: UFix, width: Int): Bits =
  {
    (UFix(1) << in(log2up(width)-1,0))
  }
}

object LFSR16
{
  def apply(increment: Bool = Bool(true)) =
  {
    val width = 16
    val lfsr = Reg(resetVal = UFix(1, width))
    when (increment) { lfsr := Cat(lfsr(0)^lfsr(2)^lfsr(3)^lfsr(5), lfsr(width-1,1)).toUFix }
    lfsr
  }
}

object ShiftRegister
{
  def apply [T <: Data](n: Int, in: T): T =
    if (n > 0) Reg(apply(n-1, in)) else in
}

object Mux1H 
{
  def buildMux[T <: Data](sel: Bits, in: Seq[T], i: Int, n: Int): T = {
    if (n == 1)
      in(i)
    else
    {
      val half_n = (1 << log2up(n))/2
      val left = buildMux(sel, in, i, half_n)
      val right = buildMux(sel, in, i + half_n, n - half_n)
      Mux(sel(i+n-1,i+half_n).orR, right, left)
    }
  }

  def apply [T <: Data](sel: Bits, in: Seq[T]): T = buildMux(sel, in, 0, sel.getWidth)
  def apply [T <: Data](sel: Seq[Bool], in: Seq[T]): T = buildMux(Cat(Bits(0),sel.reverse:_*), in, 0, sel.size)
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


class ioDecoupled[+T <: Data]()(data: => T) extends Bundle
{
  val ready = Bool(INPUT)
  val valid = Bool(OUTPUT)
  val bits  = data.asOutput
}

class ioPipe[+T <: Data]()(data: => T) extends Bundle
{
  val valid = Bool(OUTPUT)
  val bits = data.asOutput
}

class ioArbiter[T <: Data](n: Int)(data: => T) extends Bundle {
  val in  = Vec(n) { (new ioDecoupled()) { data } }.flip
  val out = (new ioDecoupled()) { data }
  val chosen = Bits(log2up(n), OUTPUT)
}

object ArbiterCtrl
{
  def apply(request: Seq[Bool]) = {
    Bool(true) +: (1 until request.length).map(i => !foldR(request.slice(0, i))(_||_))
  }
}

class Arbiter[T <: Data](n: Int)(data: => T) extends Component {
  val io = new ioArbiter(n)(data)

  val grant = ArbiterCtrl(io.in.map(_.valid))
  (0 until n).map(i => io.in(i).ready := grant(i) && io.out.ready)

  var dout = io.in(n-1).bits
  var choose = Bits(n-1)
  for (i <- n-2 to 0 by -1) {
    dout = Mux(io.in(i).valid, io.in(i).bits, dout)
    choose = Mux(io.in(i).valid, Bits(i), choose)
  }

  io.out.valid := foldR(io.in.map(_.valid))(_||_)
  io.out.bits <> dout
  io.chosen := choose
}

class RRArbiter[T <: Data](n: Int)(data: => T) extends Component {
  val io = new ioArbiter(n)(data)

  val last_grant = Reg(resetVal = Bits(0, log2up(n)))
  val g = ArbiterCtrl((0 until n).map(i => io.in(i).valid && UFix(i) > last_grant) ++ io.in.map(_.valid))
  val grant = (0 until n).map(i => g(i) && UFix(i) > last_grant || g(i+n))
  (0 until n).map(i => io.in(i).ready := grant(i) && io.out.ready)

  var choose = Bits(n-1)
  for (i <- n-2 to 0 by -1)
    choose = Mux(io.in(i).valid, Bits(i), choose)
  for (i <- n-1 to 1 by -1)
    choose = Mux(io.in(i).valid && UFix(i) > last_grant, Bits(i), choose)
  when (io.out.valid && io.out.ready) {
    last_grant := choose
  }

  val dvec = Vec(n) { Wire() { data } }
  (0 until n).map(i => dvec(i) := io.in(i).bits )

  io.out.valid := foldR(io.in.map(_.valid))(_||_)
  io.out.bits := dvec(choose)
  io.chosen := choose
}

class ioLockingArbiter[T <: Data](n: Int)(data: => T) extends Bundle {
  val in   = Vec(n) { (new ioDecoupled()) { data } }.flip
  val lock = Vec(n) { Bool() }.asInput
  val out  = (new ioDecoupled()) { data }
}

class LockingArbiter[T <: Data](n: Int)(data: => T) extends Component {
  val io = new ioLockingArbiter(n)(data)
  val locked = Vec(n) { Reg(resetVal = Bool(false)) }
  val any_lock_held = (locked.toBits & io.lock.toBits).orR
  val valid_arr = Vec(n) { Wire() { Bool() } }
  val bits_arr = Vec(n) { Wire() { data } }
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

object PriorityEncoder
{
  def apply(in: Bits): UFix = doApply(in, 0)
  def doApply(in: Bits, n: Int = 0): UFix = {
    if (n >= in.getWidth-1)
      UFix(n)
    else
      Mux(in(n), UFix(n), doApply(in, n+1))
  }
}

object PriorityEncoderOH
{
  def apply(in: Bits): UFix = doApply(in, 0)
  def doApply(in: Bits, n: Int = 0): UFix = {
    val out = Vec(in.getWidth) { Wire() { Bool() } }
    var none_hot = Bool(true)
    for (i <- 0 until in.getWidth) {
      out(i) := none_hot && in(i)
      none_hot = none_hot && !in(i)
    }
    out.toBits
  }
}
