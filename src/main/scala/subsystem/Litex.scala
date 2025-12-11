// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.subsystem

import chisel3.util._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.devices.debug.{DebugModuleKey, DefaultDebugModuleParams, ExportDebug, JTAG, APB}
import freechips.rocketchip.devices.tilelink.{
  BuiltInErrorDeviceParams, BootROMLocated, BootROMParams, CLINTKey, DevNullDevice, CLINTParams, PLICKey, PLICParams, DevNullParams
}
import freechips.rocketchip.prci.{SynchronousCrossing, AsynchronousCrossing, RationalCrossing, ClockCrossingType}
import freechips.rocketchip.diplomacy.{
  AddressSet, MonitorsEnabled,
}
import freechips.rocketchip.resources.{
  DTSModel, DTSCompat, DTSTimebase, BigIntHexContext
}
import freechips.rocketchip.tile.{
  MaxHartIdBits, RocketTileParams, BuildRoCC, AccumulatorExample, OpcodeSet, TranslatorExample, CharacterCountExample, BlackBoxExample
}
import freechips.rocketchip.util.ClockGateModelFile
import scala.reflect.ClassTag

class WithLitexMemPort extends Config((site, here, up) => {
  case ExtMem => Some(MemoryPortParams(MasterPortParams(
                      base = x"8000_0000",
                      size = x"8000_0000",
                      beatBytes = site(MemoryBusKey).beatBytes,
                      idBits = 4), 1))
})

class WithLitexMMIOPort extends Config((site, here, up) => {
  case ExtBus => Some(MasterPortParams(
                      base = x"1000_0000",
                      size = x"7000_0000",
                      beatBytes = site(SystemBusKey).beatBytes,
                      idBits = 4))
})

class WithLitexSlavePort extends Config((site, here, up) => {
  case ExtIn  => Some(SlavePortParams(
                      beatBytes = site(SystemBusKey).beatBytes,
                      idBits = 8,
                      sourceBits = 4))
})

class WithNBitMemoryBus(dataBits: Int) extends Config((site, here, up) => {
  case MemoryBusKey => up(MemoryBusKey).copy(beatBytes = dataBits/8)
})
