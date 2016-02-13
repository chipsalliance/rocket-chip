package rocketchip

import Chisel.log2Ceil
import cde.{Parameters, Field}
import scala.collection.mutable.HashMap
import junctions._
import junctions.AddrMapConsts._

case object GlobalDeviceSet extends Field[DeviceSet]

case class Device(name: String, size: Int, dtype: String,
  readable: Boolean = true, writeable: Boolean = true)

class DeviceSet {
  val deviceMap = new HashMap[String, Device]()

  def addDevice(name: String, size: Int, dtype: String, readable: Boolean = true, writeable: Boolean = true): Unit =
      addDevice(Device(name, size, dtype, readable, writeable))

  def addDevice(dev: Device): Unit =
    deviceMap(dev.name) = dev

  private def roundup(size: Int): Int = (1 << log2Ceil(size))

  def toSeq: Seq[Device] = deviceMap.values.toSeq

  def getAddrMap: AddrMap = {
    val devices = this.toSeq.sortWith((a, b) => a.size > b.size)
    val entries = devices.map { case Device(name, size, _, readable, writeable) =>
      val prot = (if (readable) R else 0) | (if (writeable) W else 0)
      val realsize = roundup(size)
      new AddrMapEntry(name, None, new MemSize(realsize, prot))
    }
    new AddrMap(entries)
  }
}
