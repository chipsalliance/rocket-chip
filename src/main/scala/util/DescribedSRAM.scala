// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._
import chisel3.SyncReadMem
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelAddressing
import freechips.rocketchip.diplomaticobjectmodel.model.OMSRAM

object DescribedSRAMIdAssigner {
  private var nextId: Int = 0
  def genId(): Int = this.synchronized {
    val id = nextId
    nextId += 1
    id
  }
}

object DescribedSRAM {
  def apply[T <: Data](
    name: String,
    desc: String,
    size: BigInt, // depth
    data: T
  ): (SyncReadMem[T], OMSRAM) = {

    val mem = SyncReadMem(size, data)

    mem.suggestName(name)

    val granWidth = data match {
      case v: Vec[_] => v.head.getWidth
      case d => d.getWidth
    }

    val uid = DescribedSRAMIdAssigner.genId()

    val omSRAM = DiplomaticObjectModelAddressing.makeOMSRAM(
      desc = "mem-" + uid,
      width = data.getWidth,
      depth = size,
      granWidth = granWidth,
      uid = uid
    )

    Annotated.srams(
      component = mem,
      name = name,
      address_width = log2Ceil(size),
      data_width = data.getWidth,
      depth = size,
      description = desc,
      write_mask_granularity = granWidth,
      uid = uid
    )

    (mem, omSRAM)
  }
}
