// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.coreplex._
import freechips.rocketchip.diplomacy._
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Paths}

/** Size, location and contents of the boot rom. */
case class BootROMParams(
  address: BigInt = 0x10000,
  size: Int = 0x10000,
  hang: BigInt = 0x10040,
  contentFileName: String,
  patchFunction: ByteBuffer => Unit = (buffer: ByteBuffer) => ())

case object BootROMParams extends Field[BootROMParams]

/** Adds a boot ROM that contains the DTB describing the system's coreplex. */
trait HasPeripheryBootROM extends HasPeripheryBus {
  val dtb: DTB
  private val params = p(BootROMParams)
  private lazy val contents = {
    val romdata = Files.readAllBytes(Paths.get(params.contentFileName))
    val rom = ByteBuffer.wrap(romdata)
    params.patchFunction(rom)
    rom.array() ++ dtb.contents
  }
  def resetVector: BigInt = params.hang

  val bootrom = LazyModule(new TLROM(params.address, params.size, contents, true, pbus.beatBytes))

  bootrom.node := pbus.toVariableWidthSlaves
}

/** Coreplex will power-on running at 0x10040 (BootROM) */
trait HasPeripheryBootROMModuleImp extends LazyMultiIOModuleImp
    with HasResetVectorWire {
  val outer: HasPeripheryBootROM
  global_reset_vector := UInt(outer.resetVector, width = resetVectorBits)
}
