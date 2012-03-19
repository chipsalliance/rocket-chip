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
  def apply(in: Bits) =
  {
    var out = in(in.getWidth-1)
    for (i <- 1 until in.getWidth)
      out = Cat(in(in.getWidth-i-1), out)
    out
  }
}

object OHToUFix
{
  def apply(in: Bits): UFix = 
  {
    val out = MuxCase( UFix(0), (0 until in.getWidth).map( i => (in(i).toBool, UFix(i))))
    out.toUFix
  }
  def apply(in: Seq[Bool]): UFix = 
  {
    val out = MuxCase( UFix(0), in.zipWithIndex map {case (b,i) => (b, UFix(i))})
    out.toUFix
  }
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
  def buildMux[T <: Data](sel: Bits, in: Vec[T], i: Int, n: Int): T = {
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

  def apply [T <: Data](sel: Bits, in: Vec[T]): T = buildMux(sel, in, 0, sel.getWidth)
  def apply [T <: Data](sel: Vec[Bool], in: Vec[T]): T = apply(sel.toBits, in)
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

class Arbiter[T <: Data](n: Int)(data: => T) extends Component {
  val io = new ioArbiter(n)(data)

  io.in(0).ready := io.out.ready
  for (i <- 1 to n-1) {
    io.in(i).ready := !io.in(i-1).valid && io.in(i-1).ready
  }

  var dout = io.in(n-1).bits
  var choose = Bits(n-1)
  for (i <- 1 to n-1) {
    dout = Mux(io.in(n-1-i).valid, io.in(n-1-i).bits, dout)
    choose = Mux(io.in(n-1-i).valid, Bits(n-1-i), choose)
  }

  var vout = io.in(0).valid
  for (i <- 1 to n-1)
    vout = vout || io.in(i).valid

  vout <> io.out.valid
  dout <> io.out.bits
  choose <> io.chosen
}

class RRArbiter[T <: Data](n: Int)(data: => T) extends Component {
  val io = new ioArbiter(n)(data)

  val last_grant = Reg(resetVal = UFix(0, log2up(n)))
  var valid = io.in(n-1).valid
  var next_grant = UFix(n-1)
  var mux = (new Mux1H(n)) { data }

  for (i <- n-2 to 0 by -1) {
    valid = valid || io.in(i).valid
    next_grant = Mux(io.in(i).valid, UFix(i), next_grant)
  }
  for (i <- n-1 to 1 by -1)
    next_grant = Mux(last_grant < UFix(i) && io.in(i).valid, UFix(i), next_grant)
  for (i <- 0 until n) {
    mux.io.sel(i) := next_grant === UFix(i)
    mux.io.in(i) := io.in(i).bits
    io.in(i).ready := io.out.ready && next_grant === UFix(i)
  }
  when (valid && io.out.ready) {
    last_grant := next_grant
  }

  io.out.valid := valid
  io.out.bits := mux.io.out
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
