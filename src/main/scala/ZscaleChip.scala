// See LICENSE for license details.

package rocketchip

import Chisel._
import junctions._
import uncore._
import rocket._
import zscale._

case object UseZscale extends Field[Boolean]
case object BuildZscale extends Field[(Bool) => Zscale]
case object BootROMCapacity extends Field[Int]
case object DRAMCapacity extends Field[Int]

class ZscaleSystem extends Module {
  val io = new Bundle {
    val host = new HTIFIO
    val jtag = new HASTIMasterIO().flip
    val bootmem = new HASTISlaveIO().flip
    val dram = new HASTISlaveIO().flip
    val spi = new HASTISlaveIO().flip
    val led = new POCIIO
    val corereset = new POCIIO
  }

  val core = params(BuildZscale)(io.host.reset)

  val bootmem_afn = (addr: UInt) => addr(31, 14) === UInt(0)

  val sbus_afn = (addr: UInt) => addr(31, 29).orR
  val dram_afn = (addr: UInt) => addr(31, 26) === UInt(8)
  val spi_afn = (addr: UInt) => addr(31, 26) === UInt(9) && addr(25, 14) === UInt(0)

  val pbus_afn = (addr: UInt) => addr(31) === UInt(1)
  val led_afn = (addr: UInt) => addr(31) === UInt(1) && addr(30, 10) === UInt(0)
  val corereset_afn = (addr: UInt) => addr(31) === UInt(1) && addr(30, 10) === UInt(1)

  val xbar = Module(new HASTIXbar(3, Seq(bootmem_afn, sbus_afn)))
  val sadapter = Module(new HASTISlaveToMaster)
  val sbus = Module(new HASTIBus(Seq(dram_afn, spi_afn, pbus_afn)))
  val padapter = Module(new HASTItoPOCIBridge)
  val pbus = Module(new POCIBus(Seq(led_afn, corereset_afn)))

  core.io.host <> io.host
  xbar.io.masters(0) <> io.jtag
  xbar.io.masters(1) <> core.io.dmem
  xbar.io.masters(2) <> core.io.imem

  io.bootmem <> xbar.io.slaves(0)
  sadapter.io.in <> xbar.io.slaves(1)

  sbus.io.master <> sadapter.io.out
  io.dram <> sbus.io.slaves(0)
  io.spi <> sbus.io.slaves(1)
  padapter.io.in <> sbus.io.slaves(2)

  pbus.io.master <> padapter.io.out
  io.led <> pbus.io.slaves(0)
  io.corereset <> pbus.io.slaves(1)
}

class ZscaleTop extends Module {
  val io = new Bundle {
    val host = new HTIFIO
  }

  val sys = Module(new ZscaleSystem)
  val bootmem = Module(new HASTISRAM(params(BootROMCapacity)/4))
  val dram = Module(new HASTISRAM(params(DRAMCapacity)/4))

  sys.io.host <> io.host
  bootmem.io <> sys.io.bootmem
  dram.io <> sys.io.dram
}
