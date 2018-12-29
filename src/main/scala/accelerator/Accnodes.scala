// See LICENSE.SiFive for license details.

//package tpu_pkg
package freechips.rocketchip.subsystem

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.util._

/** Adds a port to the system intended to master an AXI4 DRAM controller. */
trait CanHaveAccAXI4MemPort { this: BaseSubsystem =>
  val module: CanHaveAccAXI4MemPortModuleImp

  val accAXI4Node = p(AccMem).map { case MemoryPortParams(memPortParams, nMemoryChannels) =>
    val portName = "axi4"
    val device = new MemoryDevice

    val accAXI4Node = AXI4SlaveNode(Seq.tabulate(nMemoryChannels) { channel =>
      val base = AddressSet(memPortParams.base, memPortParams.size-1)
      val filter = AddressSet(channel * abus.blockBytes, ~((nMemoryChannels-1) * abus.blockBytes))

      AXI4SlavePortParameters(
        slaves = Seq(AXI4SlaveParameters(
          address       = base.intersect(filter).toList,
          resources     = device.reg,
          regionType    = RegionType.UNCACHED, // cacheable
          executable    = true,
          supportsWrite = TransferSizes(1, abus.blockBytes),
          supportsRead  = TransferSizes(1, abus.blockBytes),
          interleavedId = Some(0))), // slave does not interleave read responses
        beatBytes = memPortParams.beatBytes)
    })

    accAXI4Node := abus.toDRAMController(Some(portName)) {
      AXI4UserYanker() := AXI4IdIndexer(memPortParams.idBits) := TLToAXI4()
    }

    accAXI4Node
  }
}

/** Actually generates the corresponding IO in the concrete Module */
trait CanHaveAccAXI4MemPortModuleImp extends LazyModuleImp {
  val outer: CanHaveAccAXI4MemPort

  val acc_axi4 = outer.accAXI4Node.map(x => IO(HeterogeneousBag.fromNode(x.in)))
  (acc_axi4 zip outer.accAXI4Node) foreach { case (io, node) =>
    (io zip node.in).foreach { case (io, (bundle, _)) => io <> bundle }
  }

  //def connectSimAXIMem() {
  //  (acc_axi4 zip outer.accAXI4Node).foreach { case (io, node) =>
  //    (io zip node.in).foreach { case (io, (_, edge)) =>
  //      val mem = LazyModule(new SimAXIMem(edge, size = p(ExtMem).get.master.size))
  //      Module(mem.module).io.axi4.head <> io
  //    }
  //  }
  //}
}

