// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.util


import Chisel._
import chisel3.SyncReadMem

case class DescribedSRAM[T <: Data](
  name: String,
  desc: String,
  depth: Int,
  dataWidth: Int,
  mem: SyncReadMem[T],
  granWidth: Int,
) {
  def hash: () => Int = () => mem.toNamed.toString.hashCode
}

object DescribedSRAM {
  private def granWidth[T <: Data](data: T): Int = {
    data match {
      case v: Vec[_] => v.head.getWidth
      case d => d.getWidth
    }
  }

  private def mem[T <: Data](
    name: String,
    desc: String,
    size: Int, // depth
    data: T
  ): SyncReadMem[T] = {
    val mem = SeqMem(size, data)

    mem.suggestName(name)

    Annotated.srams(
      component = mem,
      name = name,
      address_width = log2Ceil(size),
      data_width = data.getWidth,
      depth = size,
      description = desc,
      write_mask_granularity = granWidth(data))

    mem
  }

  def apply[T <: Data](
    name: String,
    desc: String,
    size: Int, // depth
    data: T
  ): SyncReadMem[T] = {
    mem(name, desc, size, data)
  }

  def build[T <: Data](
    name: String,
    desc: String,
    size: Int, // depth
    data: T
  ): DescribedSRAM[T] = {
    new DescribedSRAM[T](
      name = name,
      desc = desc,
      depth = size,
      dataWidth = data.getWidth,
      mem = mem(name, desc, size, data),
      granWidth = granWidth(data)
    )
  }
}
