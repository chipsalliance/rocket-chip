// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3.{Data, SyncReadMem, Vec}
import chisel3.util.log2Ceil

object DescribedSRAM {
  def apply[T <: Data](
    name: String,
    desc: String,
    size: BigInt, // depth
    data: T
  ): SyncReadMem[T] = {

    val mem = SyncReadMem(size, data)

    mem.suggestName(name)

    val granWidth = data match {
      case v: Vec[_] => v.head.getWidth
      case d => d.getWidth
    }

    val uid = 0

    mem
  }
}
