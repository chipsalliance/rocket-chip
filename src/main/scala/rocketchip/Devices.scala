package rocketchip

import Chisel._
import junctions._
import uncore.tilelink._
import scala.collection.immutable.HashMap
import cde.{Parameters, Field}

case object ExtraTopPorts extends Field[Parameters => Bundle]
case object ExtraDevices extends Field[DeviceBlock]

abstract class DeviceBlock {
  /** How many client ports will the devices use */
  def nClientPorts: Int
  /** Address map entries for all of the devices */
  def addrMapEntries: Seq[AddrMapEntry]

  /**
   * The function that elaborates all the extra devices and connects them
   * to the TileLink ports and extra top-level ports.
   *
   * @param mmioPorts A hashmap for the mmio ports.
   *    Use the names specified in addrMapEntries to get
   *    the mmio port for each device.
   * @param clientPorts All the client ports available for the devices
   * @param extra The extra top-level IO bundle
   * @param p The CDE parameters for the devices
   */
  def builder(
    mmioPorts: HashMap[String, ClientUncachedTileLinkIO],
    clientPorts: Seq[ClientUncachedTileLinkIO],
    extra: Bundle, p: Parameters): Unit

  /**
   * Create the config string entry for this device that goes into the
   * Boot ROM. You generally won't need to override this
   *
   * @param fullAddrMap The full global address map
   */
  def makeConfigString(fullAddrMap: AddrMap): String = {
    addrMapEntries.map { entry =>
      val region = fullAddrMap("io:ext:" + entry.name)
      s"${entry.name} {\n" +
      s"  addr 0x${region.start.toString(16)};\n" +
      s"  size 0x${region.size.toString(16)}; \n" +
       "}\n"
    }.mkString
  }
}

class EmptyDeviceBlock extends DeviceBlock {
  def nClientPorts = 0
  def addrMapEntries = Seq.empty

  def builder(
      mmioPorts: HashMap[String, ClientUncachedTileLinkIO],
      clientPorts: Seq[ClientUncachedTileLinkIO],
      extra: Bundle, p: Parameters) {}
}
