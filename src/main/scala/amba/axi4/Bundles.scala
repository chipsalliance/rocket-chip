// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import chisel3._
import chisel3.util._
import freechips.rocketchip.util._

abstract class AXI4BundleBase(val params: AXI4BundleParameters) extends Bundle

/**
  * Common signals of AW and AR channels of AXI4 protocol
  */
abstract class AXI4BundleA(params: AXI4BundleParameters) extends AXI4BundleBase(params)
{
  val id     = UInt(params.idBits.W)
  val addr   = UInt(params.addrBits.W)
  val len    = UInt(params.lenBits.W)  // number of beats - 1
  val size   = UInt(params.sizeBits.W) // bytes in beat = 2^size
  val burst  = UInt(params.burstBits.W)
  val lock   = UInt(params.lockBits.W)
  val cache  = UInt(params.cacheBits.W)
  val prot   = UInt(params.protBits.W)
  val qos    = UInt(params.qosBits.W)  // 0=no QoS, bigger = higher priority
  val user   = BundleMap(params.requestFields.filter(_.key.isControl))
  val echo   = BundleMap(params.echoFields)
  // val region = UInt(4.W) // optional

  // Number of bytes-1 in this operation
  def bytes1(x:Int=0) = {
    val maxShift = 1 << params.sizeBits
    val tail = ((BigInt(1) << maxShift) - 1).U
    (Cat(len, tail) << size) >> maxShift
  }
}

/**
  * A non-standard bundle that can be both AR and AW
  */
class AXI4BundleARW(params: AXI4BundleParameters) extends AXI4BundleA(params)
{
  val wen = Bool()
}

/**
  * AW channel of AXI4 protocol
  */
class AXI4BundleAW(params: AXI4BundleParameters) extends AXI4BundleA(params)

/**
  * AR channel of AXI4 protocol
  */
class AXI4BundleAR(params: AXI4BundleParameters) extends AXI4BundleA(params)

/**
  * W channel of AXI4 protocol
  */
class AXI4BundleW(params: AXI4BundleParameters) extends AXI4BundleBase(params)
{
  // id ... removed in AXI4
  val data = UInt(params.dataBits.W)
  val strb = UInt((params.dataBits/8).W)
  val last = Bool()
  val user = BundleMap(params.requestFields.filter(_.key.isData))
}

/**
  * R channel of AXI4 protocol
  */
class AXI4BundleR(params: AXI4BundleParameters) extends AXI4BundleBase(params)
{
  val id   = UInt(params.idBits.W)
  val data = UInt(params.dataBits.W)
  val resp = UInt(params.respBits.W)
  val user = BundleMap(params.responseFields) // control and data
  val echo = BundleMap(params.echoFields)
  val last = Bool()
}

/**
  * B channel of AXI4 protocol
  */
class AXI4BundleB(params: AXI4BundleParameters) extends AXI4BundleBase(params)
{
  val id   = UInt(params.idBits.W)
  val resp = UInt(params.respBits.W)
  val user = BundleMap(params.responseFields.filter(_.key.isControl))
  val echo = BundleMap(params.echoFields)
}

/**
  * AXI4 protocol bundle
  */
class AXI4Bundle(params: AXI4BundleParameters) extends AXI4BundleBase(params)
{
  val aw = Irrevocable(new AXI4BundleAW(params))
  val w  = Irrevocable(new AXI4BundleW (params))
  val b  = Flipped(Irrevocable(new AXI4BundleB (params)))
  val ar = Irrevocable(new AXI4BundleAR(params))
  val r  = Flipped(Irrevocable(new AXI4BundleR (params)))
}

object AXI4Bundle
{
  def apply(params: AXI4BundleParameters) = new AXI4Bundle(params)
}

class AXI4AsyncBundleBase(params: AXI4AsyncBundleParameters) extends Bundle

class AXI4AsyncBundle(params: AXI4AsyncBundleParameters) extends AXI4AsyncBundleBase(params)
{
  val aw = new AsyncBundle(new AXI4BundleAW(params.base), params.async)
  val w  = new AsyncBundle(new AXI4BundleW (params.base), params.async)
  val b  = Flipped(new AsyncBundle(new AXI4BundleB (params.base), params.async))
  val ar = new AsyncBundle(new AXI4BundleAR(params.base), params.async)
  val r  = Flipped(new AsyncBundle(new AXI4BundleR (params.base), params.async))
}

class AXI4CreditedBundle(params: AXI4BundleParameters) extends AXI4BundleBase(params)
{
  val aw = CreditedIO(new AXI4BundleAW(params))
  val w  = CreditedIO(new AXI4BundleW (params))
  val b  = Flipped(CreditedIO(new AXI4BundleB (params)))
  val ar = CreditedIO(new AXI4BundleAR(params))
  val r  = Flipped(CreditedIO(new AXI4BundleR (params)))
}
