// See LICENSE.SiFive for license details.

package freechips.rocketchip.system // TODO this should really be in a testharness package

import chisel3._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.amba.AMBACorrupt
import freechips.rocketchip.amba.axi4.{AXI4RAM, AXI4ManagerNode, AXI4EdgeParameters, AXI4Xbar, AXI4Buffer, AXI4Fragmenter}
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.subsystem.{CanHaveManagerAXI4MMIOPort, CanHaveManagerAXI4MemPort, ExtBus, ExtMem}

/** Memory with AXI port for use in elaboratable test harnesses.
 * 
 * Topology: AXIRAM <-< AXI4Buffer <-< AXI4Fragmenter <-< AXI4Xbar <-< AXI4ManagerNode
*/
class SimAXIMem(edge: AXI4EdgeParameters, size: BigInt, base: BigInt = 0)(implicit p: Parameters) extends SimpleLazyModule {
  val node = AXI4ManagerNode(List(edge.manager))
  val srams = AddressSet.misaligned(base, size).map { aSet =>
    LazyModule(new AXI4RAM(
      address = aSet,
      beatBytes = edge.bundle.dataBits/8,
      wcorrupt=edge.subordinate.requestKeys.contains(AMBACorrupt)))
  }
  val xbar = AXI4Xbar()
  srams.foreach{ s => s.node := AXI4Buffer() := AXI4Fragmenter() := xbar }
  xbar := node
  val io_axi4 = InModuleBody { node.makeIOs() }
}

/**
  * Connect Manager AXI4 Mem/MMIO Port to SimAXIMem.
  */
object SimAXIMem {
  def connectMMIO(dut: CanHaveManagerAXI4MMIOPort)(implicit p: Parameters): Seq[SimAXIMem] = {
    dut.mmio_axi4.zip(dut.mmioAXI4Node.in).map { case (io, (_, edge)) =>
      // test harness size capped to 4KB (ignoring p(ExtMem).get.manager.size)
      val mmio_mem = LazyModule(new SimAXIMem(edge, base = p(ExtBus).get.base, size = 4096))
      Module(mmio_mem.module).suggestName("mmio_mem")
      mmio_mem.io_axi4.head <> io
      mmio_mem
    }.toSeq
  }

  def connectMem(dut: CanHaveManagerAXI4MemPort)(implicit p: Parameters): Seq[SimAXIMem] = {
    dut.mem_axi4.zip(dut.memAXI4Node.in).map { case (io, (_, edge)) =>
      val mem = LazyModule(new SimAXIMem(edge, base = p(ExtMem).get.client.base, size = p(ExtMem).get.client.size))
      Module(mem.module).suggestName("mem")
      mem.io_axi4.head <> io
      mem
    }.toSeq
  }
}
