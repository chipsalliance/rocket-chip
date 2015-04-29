// See LICENSE for license details.

package uncore
import Chisel._

class BigMem[T <: Data](n: Int, preLatency: Int, postLatency: Int, leaf: Mem[UInt], noMask: Boolean = false)(gen: => T) extends Module
{
  class Inputs extends Bundle {
    val addr = UInt(INPUT, log2Up(n))
    val rw = Bool(INPUT)
    val wdata = gen.asInput
    val wmask = gen.asInput
    override def clone = new Inputs().asInstanceOf[this.type]
  }
  val io = new Bundle {
    val in = Valid(new Inputs).flip
    val rdata = gen.asOutput
  }
  val data = gen
  val colMux = if (2*data.getWidth <= leaf.data.getWidth && n > leaf.n) 1 << math.floor(math.log(leaf.data.getWidth/data.getWidth)/math.log(2)).toInt else 1
  val nWide = if (data.getWidth > leaf.data.getWidth) 1+(data.getWidth-1)/leaf.data.getWidth else 1
  val nDeep = if (n > colMux*leaf.n) 1+(n-1)/(colMux*leaf.n) else 1
  if (nDeep > 1 || colMux > 1)
    require(isPow2(n) && isPow2(leaf.n))

  val rdataDeep = Vec.fill(nDeep){Bits()}
  val rdataSel = Vec.fill(nDeep){Bool()}
  for (i <- 0 until nDeep) {
    val in = Pipe(io.in.valid && (if (nDeep == 1) Bool(true) else UInt(i) === io.in.bits.addr(log2Up(n)-1, log2Up(n/nDeep))), io.in.bits, preLatency)
    val idx = in.bits.addr(log2Up(n/nDeep/colMux)-1, 0)
    val wdata = in.bits.wdata.toBits
    val wmask = in.bits.wmask.toBits
    val ren = in.valid && !in.bits.rw
    val reg_ren = Reg(next=ren)
    val rdata = Vec.fill(nWide){Bits()}

    val r = Pipe(ren, in.bits.addr, postLatency)

    for (j <- 0 until nWide) {
      val mem = leaf.clone
      var dout: Bits = null
      val ridx = if (postLatency > 0) Reg(Bits()) else null

      var wmask0 = Fill(colMux, wmask(math.min(wmask.getWidth, leaf.data.getWidth*(j+1))-1, leaf.data.getWidth*j))
      if (colMux > 1)
        wmask0 = wmask0 & FillInterleaved(gen.getWidth, UIntToOH(in.bits.addr(log2Up(n/nDeep)-1, log2Up(n/nDeep/colMux)), log2Up(colMux)))
      val wdata0 = Fill(colMux, wdata(math.min(wdata.getWidth, leaf.data.getWidth*(j+1))-1, leaf.data.getWidth*j))
      when (in.valid) {
        when (in.bits.rw) {
          if (noMask)
            mem.write(idx, wdata0)
          else
            mem.write(idx, wdata0, wmask0)
        }
        .otherwise { if (postLatency > 0) ridx := idx }
      }

      if (postLatency == 0) {
        dout = mem(idx)
      } else if (postLatency == 1) {
        dout = mem(ridx)
      } else
        dout = Pipe(reg_ren, mem(ridx), postLatency-1).bits

      rdata(j) := dout
    }
    val rdataWide = rdata.reduceLeft((x, y) => Cat(y, x))

    var colMuxOut = rdataWide
    if (colMux > 1) {
      val colMuxIn = Vec((0 until colMux).map(k => rdataWide(gen.getWidth*(k+1)-1, gen.getWidth*k)))
      colMuxOut = colMuxIn(r.bits(log2Up(n/nDeep)-1, log2Up(n/nDeep/colMux)))
    }

    rdataDeep(i) := colMuxOut
    rdataSel(i) := r.valid
  }

  io.rdata := Mux1H(rdataSel, rdataDeep)
}
