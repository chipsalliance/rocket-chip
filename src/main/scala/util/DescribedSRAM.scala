// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3.internal.InstanceId
import freechips.rocketchip.util.Annotated
import freechips.rocketchip.diplomacy.DiplomaticSRAM
import Chisel._
import chisel3.SyncReadMem
import freechips.rocketchip.amba.axi4.AXI4RAM

import scala.math.log10

object DescribedSRAM {
  def apply[T <: Data](
    name: String,
    desc: String,
    size: Int, // depth
    data: T
  ): SyncReadMem[T] = {

    val mem = SeqMem(size, data)

    mem.suggestName(name)

    val granWidth = data match {
      case v: Vec[_] => v.head.getWidth
      case d => d.getWidth
    }

    Annotated.srams(
      component = mem,
      name = name,
      address_width = log2Ceil(size),
      data_width = data.getWidth,
      depth = size,
      description = desc,
      write_mask_granularity = granWidth)

    mem
  }
}
