// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import Chisel._
import chisel3.util.Irrevocable
import freechips.rocketchip.util._

abstract class AXI4BundleBase(params: AXI4BundleParameters) extends GenericParameterizedBundle(params)

abstract class AXI4BundleA(params: AXI4BundleParameters) extends AXI4BundleBase(params)
{
  val id     = UInt(width = params.idBits)
  val addr   = UInt(width = params.addrBits)
  val len    = UInt(width = params.lenBits)  // number of beats - 1
  val size   = UInt(width = params.sizeBits) // bytes in beat = 2^size
  val burst  = UInt(width = params.burstBits)
  val lock   = UInt(width = params.lockBits)
  val cache  = UInt(width = params.cacheBits)
  val prot   = UInt(width = params.protBits)
  val qos    = UInt(width = params.qosBits)  // 0=no QoS, bigger = higher priority
  val user = if (params.userBits > 0) Some(UInt(width = params.userBits)) else None
  // val region = UInt(width = 4) // optional

  // Number of bytes-1 in this operation
  def bytes1(x:Int=0) = {
    val maxShift = 1 << params.sizeBits
    val tail = UInt((BigInt(1) << maxShift) - 1)
    (Cat(len, tail) << size) >> maxShift
  }
}

// A non-standard bundle that can be both AR and AW
class AXI4BundleARW(params: AXI4BundleParameters) extends AXI4BundleA(params)
{
  val wen = Bool()
}

class AXI4BundleAW(params: AXI4BundleParameters) extends AXI4BundleA(params)
class AXI4BundleAR(params: AXI4BundleParameters) extends AXI4BundleA(params)

class AXI4BundleW(params: AXI4BundleParameters) extends AXI4BundleBase(params)
{
  // id ... removed in AXI4
  val data = UInt(width = params.dataBits)
  val strb = UInt(width = params.dataBits/8)
  val last = Bool()
  val corrupt = if (params.wcorrupt) Some(Bool()) else None
}

class AXI4BundleR(params: AXI4BundleParameters) extends AXI4BundleBase(params)
{
  val id   = UInt(width = params.idBits)
  val data = UInt(width = params.dataBits)
  val resp = UInt(width = params.respBits)
  val user = if (params.userBits > 0) Some(UInt(width = params.userBits)) else None
  val last = Bool()
}

class AXI4BundleB(params: AXI4BundleParameters) extends AXI4BundleBase(params)
{
  val id   = UInt(width = params.idBits)
  val resp = UInt(width = params.respBits)
  val user = if (params.userBits > 0) Some(UInt(width = params.userBits)) else None
}

class AXI4Bundle(params: AXI4BundleParameters) extends AXI4BundleBase(params)
{
  val aw = Irrevocable(new AXI4BundleAW(params))
  val w  = Irrevocable(new AXI4BundleW (params))
  val b  = Irrevocable(new AXI4BundleB (params)).flip
  val ar = Irrevocable(new AXI4BundleAR(params))
  val r  = Irrevocable(new AXI4BundleR (params)).flip

  def tieoff() {
    ar.ready.dir match {
      case INPUT =>
        ar.ready := Bool(false)
        aw.ready := Bool(false)
        w.ready  := Bool(false)
        r.valid  := Bool(false)
        b.valid  := Bool(false)
      case OUTPUT =>
        ar.valid := Bool(false)
        aw.valid := Bool(false)
        w.valid  := Bool(false)
        r.ready  := Bool(false)
        b.ready  := Bool(false)
      case _ =>
    }
  }
}

object AXI4Bundle
{
  def apply(params: AXI4BundleParameters) = new AXI4Bundle(params)
}

class AXI4AsyncBundleBase(params: AXI4AsyncBundleParameters) extends GenericParameterizedBundle(params)

class AXI4AsyncBundle(params: AXI4AsyncBundleParameters) extends AXI4AsyncBundleBase(params)
{
  val aw = new AsyncBundle(new AXI4BundleAW(params.base), params.async)
  val w  = new AsyncBundle(new AXI4BundleW (params.base), params.async)
  val b  = new AsyncBundle(new AXI4BundleB (params.base), params.async).flip
  val ar = new AsyncBundle(new AXI4BundleAR(params.base), params.async)
  val r  = new AsyncBundle(new AXI4BundleR (params.base), params.async).flip
}
