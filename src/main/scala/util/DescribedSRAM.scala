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
  def sramMaker(
    name: String,
    desc: String,
    size: Int, // depth
    addressWidth: Int,
    channels: Int, // lanes
    channelDataWidth: Int
  ): SyncReadMem[Vec[Chisel.UInt]] = {

    val data = Vec(channels, Bits(width = channelDataWidth))
    val dataWidth = data.size
    val mem = SeqMem(size, data)

    Annotated.srams(
      component = mem,
      name = name,
      address_width = addressWidth,
      data_width = dataWidth,
      depth = size,
      description = desc,
      write_mask_granularity = channelDataWidth)

    mem
  }

}
