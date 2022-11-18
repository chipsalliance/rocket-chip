// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import chisel3._
import chisel3.util.BitPat
import chisel3.util.log2Ceil
import chisel3.util.experimental.decode._

object EspressoAddressDecoder
{
  type Port = Seq[AddressSet]
  type Ports = Seq[Port]

  def apply(addr: UInt, ports: Ports, mask: Seq[Boolean]): Seq[Bool] = {
    if (ports.flatten.isEmpty) {
      return ports.map(_ => true.B)
    }
    // Verify the user did not give us an impossible problem
    ports.combinations(2).foreach { case Seq(x, y) =>
      x.foreach { a =>
        y.foreach { b =>
          require(!a.overlaps(b), s"Ports cannot overlap: $a $b")
        }
      }
    }
    val maxBits = log2Ceil(1 + ports.flatMap(_.map(_.base)).max)
    val truthMap = {
      ports.zipWithIndex.zip(mask).flatMap {
        case ((port, i), true) =>
          port.map { range =>
            (AddressSetToBitPat(range, maxBits), new BitPat(value = 1 << i, mask = -1, maxBits))
          }
        case _ => Seq()
      }
    }
    if (truthMap.isEmpty) {
      return ports.map(_ => true.B)
    }
    val dontCarePat: BitPat = new BitPat(value = 0x0, mask = 0x0, maxBits)
    decoder(addr, TruthTable(truthMap, dontCarePat)).asBools.take(ports.length)
  }

  def AddressSetToBitPat(addrs: AddressSet, XLen: Int): BitPat = {
    new BitPat(addrs.base, ~addrs.mask, XLen)
  }
}
