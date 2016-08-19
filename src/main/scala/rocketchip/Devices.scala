package rocketchip

import Chisel._
import junctions._
import uncore.tilelink._
import cde.{Parameters, Field}

case object ExtraTopPorts extends Field[Parameters => Bundle]
case object ExtraDevices extends Field[Seq[Device]]

abstract class Device {
  /** Does this device have an MMIO port? */
  def hasMMIOPort: Boolean
  /** Does this device have a client port? */
  def hasClientPort: Boolean
  /**
   * This function elaborates the hardware for the device and connects
   * it to the mmio port, client port, and extra top-level ports.
   *
   * @param mmioPort The port from the MMIO network that goes to this device.
   *  If hasMMIOPort is false, this will be None
   *
   * @param clientPort The client port provided for this device to make
   *  requests to the memory system. If hasClientPort is false, this will be None
   */
  def builder(
    mmioPort: Option[ClientUncachedTileLinkIO],
    clientPort: Option[ClientUncachedTileLinkIO],
    extra: Bundle, p: Parameters): Unit
  /**
   * The entry that will be placed into the address map for this device.
   * If hasMMIOPort is false, you do not need to override this
   */
  def addrMapEntry: AddrMapEntry =
    throw new UnsupportedOperationException("no addrMapEntry defined")

  /**
   * Create the config string entry for this device that goes into the
   * Boot ROM. You generally won't need to override this
   */
  def makeConfigString(region: MemRegion): String = {
    s"${addrMapEntry.name} {\n" +
    s"  addr 0x${region.start.toString(16)};\n" +
    s"  size 0x${region.size.toString(16)}; \n" +
     "}\n"
  }
}
