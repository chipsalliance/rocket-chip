package rocketchip

import Chisel._
import junctions._
import uncore.tilelink._
import cde.{Parameters, Field}

case object ExtraTopPorts extends Field[Parameters => Bundle]
case object ExtraDevices extends Field[Seq[Device]]

abstract class Device {
  def builder(port: ClientUncachedTileLinkIO, extra: Bundle, p: Parameters): Unit
  def addrMapEntry: AddrMapEntry
  def makeConfigString(region: MemRegion): String = {
    s"  ${addrMapEntry.name} {\n" +
    s"    addr 0x${region.start.toString(16)};\n" +
    s"    size 0x${region.size.toString(16)}; \n" +
     "  }\n"
  }
}
