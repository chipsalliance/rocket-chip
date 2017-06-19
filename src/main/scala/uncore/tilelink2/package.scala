// See LICENSE.SiFive for license details.

package uncore

import Chisel._
import diplomacy._
import util._

package object tilelink2
{
  type TLInwardNode = InwardNodeHandle[TLClientPortParameters, TLManagerPortParameters, TLBundle]
  type TLOutwardNode = OutwardNodeHandle[TLClientPortParameters, TLManagerPortParameters, TLBundle]
  type TLAsyncOutwardNode = OutwardNodeHandle[TLAsyncClientPortParameters, TLAsyncManagerPortParameters, TLAsyncBundle]
  type TLRationalOutwardNode = OutwardNodeHandle[TLRationalClientPortParameters, TLRationalManagerPortParameters, TLRationalBundle]
  type IntOutwardNode = OutwardNodeHandle[IntSourcePortParameters, IntSinkPortParameters, Vec[Bool]]
  type TLMixedNode = MixedNode[TLClientPortParameters, TLManagerPortParameters, TLEdgeIn, TLBundle,
                               TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLBundle]

  def OH1ToOH(x: UInt) = (x << 1 | UInt(1)) & ~Cat(UInt(0, width=1), x)
  def OH1ToUInt(x: UInt) = OHToUInt(OH1ToOH(x))
  def UIntToOH1(x: UInt, width: Int) = ~(SInt(-1, width=width).asUInt << x)(width-1, 0)
  def trailingZeros(x: Int) = if (x > 0) Some(log2Ceil(x & -x)) else None
  // Fill 1s from low bits to high bits
  def leftOR(x: UInt): UInt = leftOR(x, x.getWidth)
  def leftOR(x: UInt, w: Integer): UInt = {
    def helper(s: Int, x: UInt): UInt =
      if (s >= w) x else helper(s+s, x | (x << s)(w-1,0))
    helper(1, x)(w-1, 0)
  }
  // Fill 1s form high bits to low bits
  def rightOR(x: UInt): UInt = rightOR(x, x.getWidth)
  def rightOR(x: UInt, w: Integer): UInt = {
    def helper(s: Int, x: UInt): UInt =
      if (s >= w) x else helper(s+s, x | (x >> s))
    helper(1, x)(w-1, 0)
  }
  // This gets used everywhere, so make the smallest circuit possible ...
  // Given an address and size, create a mask of beatBytes size
  // eg: (0x3, 0, 4) => 0001, (0x3, 1, 4) => 0011, (0x3, 2, 4) => 1111
  // groupBy applies an interleaved OR reduction; groupBy=2 take 0010 => 01
  def maskGen(addr_lo: UInt, lgSize: UInt, beatBytes: Int, groupBy: Int = 1): UInt = {
    require (groupBy >= 1 && beatBytes >= groupBy)
    require (isPow2(beatBytes) && isPow2(groupBy))
    val lgBytes = log2Ceil(beatBytes)
    val sizeOH = UIntToOH(lgSize, log2Up(beatBytes)) | UInt(groupBy*2 - 1)
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
    if (groupBy == beatBytes) UInt(1) else
    Cat(helper(lgBytes-log2Ceil(groupBy)).map(_._1).reverse)
  }
}
