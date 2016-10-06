package uncore

import Chisel._
import diplomacy._

package object tilelink2
{
  type TLOutwardNode = OutwardNode[TLClientPortParameters, TLManagerPortParameters, TLBundle]
  type TLAsyncOutwardNode = OutwardNode[TLAsyncClientPortParameters, TLAsyncManagerPortParameters, TLAsyncBundle]
  type IntOutwardNode = OutwardNode[IntSourcePortParameters, IntSinkPortParameters, Vec[Bool]]
  def OH1ToUInt(x: UInt) = OHToUInt((x << 1 | UInt(1)) ^ x)
  def UIntToOH1(x: UInt, width: Int) = ~(SInt(-1, width=width).asUInt << x)(width-1, 0)
  def trailingZeros(x: Int) = if (x > 0) Some(log2Ceil(x & -x)) else None
  def highOR(x: UInt) = {
    val w = x.getWidth
    def helper(s: Int, x: UInt): UInt =
      if (s >= w) x else helper(s+s, x | (x << s)(w-1,0))
    helper(1, x)
  }
  // This gets used everywhere, so make the smallest circuit possible ...
  def maskGen(addr_lo: UInt, lgSize: UInt, beatBytes: Int): UInt = {
    val lgBytes = log2Ceil(beatBytes)
    val sizeOH = UIntToOH(lgSize, log2Up(beatBytes))
    def helper(i: Int): Seq[(Bool, Bool)] = {
      if (i == 0) {
        Seq((lgSize >= UInt(lgBytes), Bool(true)))
      } else {
        val sub = helper(i-1)
        val size = sizeOH(lgBytes - i)
        val bit = addr_lo(lgBytes - i)
        val nbit = !bit
        Seq.tabulate (1 << i) { j =>
          val (sub_acc, sub_eq) = sub(j/2)
          val eq = sub_eq && (if (j % 2 == 1) bit else nbit)
          val acc = sub_acc || (size && eq)
          (acc, eq)
        }
      }
    }
    Cat(helper(lgBytes).map(_._1).reverse)
  }
}
