// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import chisel3.util.{ReadyValidIO}
import diplomacy._
import util._

abstract class TLBundleBase(params: TLBundleParameters) extends GenericParameterizedBundle(params)

// common combos in lazy policy:
//   Put + Acquire
//   Release + AccessAck

object TLMessages 
{
  //                                  A    B    C    D    E
  val PutFullData    = UInt(0) //     .    .                   => AccessAck
  val PutPartialData = UInt(1) //     .    .                   => AccessAck
  val ArithmeticData = UInt(2) //     .    .                   => AccessAckData
  val LogicalData    = UInt(3) //     .    .                   => AccessAckData
  val Get            = UInt(4) //     .    .                   => AccessAckData
  val Hint           = UInt(5) //     .    .                   => HintAck
  val Acquire        = UInt(6) //     .                        => Grant[Data]
  val Probe          = UInt(6) //          .                   => ProbeAck[Data]
  val AccessAck      = UInt(0) //               .    .
  val AccessAckData  = UInt(1) //               .    .
  val HintAck        = UInt(2) //               .    .
  val ProbeAck       = UInt(4) //               .
  val ProbeAckData   = UInt(5) //               .
  val Release        = UInt(6) //               .              => ReleaseAck
  val ReleaseData    = UInt(7) //               .              => ReleaseAck
  val Grant          = UInt(4) //                    .         => GrantAck
  val GrantData      = UInt(5) //                    .         => GrantAck
  val ReleaseAck     = UInt(6) //                    .
  val GrantAck       = UInt(0) //                         .
 
  def isA(x: UInt) = x <= Acquire
  def isB(x: UInt) = x <= Probe
  def isC(x: UInt) = x <= ReleaseData
  def isD(x: UInt) = x <= ReleaseAck
}

/**
  * The three primary TileLink permissions are:
  *   (T)runk: the agent is (or is on the path to) the global point of serialization.
  *   (B)ranch: the agent 
  *   (N)one: 
  * These permissions are permuted by transfer operations in various ways.
  * Messages for 
  */
object TLPermissions
{
  val aWidth = 2
  val bdWidth = 2
  val cWidth = 3

  // Cap types (Grant = new permissions, Probe = permisions <= target)
  val toT = UInt(0, bdWidth)
  val toB = UInt(1, bdWidth)
  val toN = UInt(2, bdWidth)
  def isCap(x: UInt) = x <= toN

  // Grow types (Acquire = permissions >= target)
  val NtoB = UInt(0, aWidth)
  val NtoT = UInt(1, aWidth)
  val BtoT = UInt(2, aWidth)
  def isGrow(x: UInt) = x <= BtoT

  // Shrink types (ProbeAck, Release)
  val TtoB = UInt(0, cWidth)
  val TtoN = UInt(1, cWidth)
  val BtoN = UInt(2, cWidth)
  def isShrink(x: UInt) = x <= BtoN

  // Report types (ProbeAck)
  val TtoT = UInt(3, cWidth)
  val BtoB = UInt(4, cWidth)
  val NtoN = UInt(5, cWidth)
  def isReport(x: UInt) = x <= NtoN
}

object TLAtomics
{
  val width = 3 

  // Arithmetic types
  val MIN  = UInt(0, width)
  val MAX  = UInt(1, width)
  val MINU = UInt(2, width)
  val MAXU = UInt(3, width)
  val ADD  = UInt(4, width)
  def isArithmetic(x: UInt) = x <= ADD

  // Logical types
  val XOR  = UInt(0, width)
  val OR   = UInt(1, width)
  val AND  = UInt(2, width)
  val SWAP = UInt(3, width)
  def isLogical(x: UInt) = x <= SWAP
}

object TLHints
{
  val width = 1

  val PREFETCH_READ  = UInt(0, width)
  val PREFETCH_WRITE = UInt(1, width)
}

sealed trait TLChannel extends TLBundleBase {
  val channelName: String
}
sealed trait TLDataChannel extends TLChannel
sealed trait TLAddrChannel extends TLDataChannel

final class TLBundleA(params: TLBundleParameters)
  extends TLBundleBase(params) with TLAddrChannel
{
  val channelName = "'A' channel"
  // fixed fields during multibeat:
  val opcode  = UInt(width = 3)
  val param   = UInt(width = List(TLAtomics.width, TLPermissions.aWidth, TLHints.width).max) // amo_opcode || grow perms || hint
  val size    = UInt(width = params.sizeBits)
  val source  = UInt(width = params.sourceBits) // from
  val address = UInt(width = params.addressBits) // to
  // variable fields during multibeat:
  val mask    = UInt(width = params.dataBits/8)
  val data    = UInt(width = params.dataBits)
}

final class TLBundleB(params: TLBundleParameters)
  extends TLBundleBase(params) with TLAddrChannel
{
  val channelName = "'B' channel"
  // fixed fields during multibeat:
  val opcode  = UInt(width = 3)
  val param   = UInt(width = TLPermissions.bdWidth) // cap perms
  val size    = UInt(width = params.sizeBits)
  val source  = UInt(width = params.sourceBits) // to
  val address = UInt(width = params.addressBits) // from
  // variable fields during multibeat:
  val mask    = UInt(width = params.dataBits/8)
  val data    = UInt(width = params.dataBits)
}

final class TLBundleC(params: TLBundleParameters)
  extends TLBundleBase(params) with TLAddrChannel
{
  val channelName = "'C' channel"
  // fixed fields during multibeat:
  val opcode  = UInt(width = 3)
  val param   = UInt(width = TLPermissions.cWidth) // shrink or report perms
  val size    = UInt(width = params.sizeBits)
  val source  = UInt(width = params.sourceBits) // from
  val address = UInt(width = params.addressBits) // to
  // variable fields during multibeat:
  val data    = UInt(width = params.dataBits)
  val error   = Bool() // AccessAck[Data]
}

final class TLBundleD(params: TLBundleParameters)
  extends TLBundleBase(params) with TLDataChannel
{
  val channelName = "'D' channel"
  // fixed fields during multibeat:
  val opcode  = UInt(width = 3)
  val param   = UInt(width = TLPermissions.bdWidth) // cap perms
  val size    = UInt(width = params.sizeBits)
  val source  = UInt(width = params.sourceBits) // to
  val sink    = UInt(width = params.sinkBits)   // from
  val addr_lo = UInt(width = params.addrLoBits) // instead of mask
  // variable fields during multibeat:
  val data    = UInt(width = params.dataBits)
  val error   = Bool() // AccessAck[Data], Grant[Data]
}

final class TLBundleE(params: TLBundleParameters)
  extends TLBundleBase(params) with TLChannel
{
  val channelName = "'E' channel"
  val sink = UInt(width = params.sinkBits) // to
}

class TLBundle(params: TLBundleParameters) extends TLBundleBase(params)
{
  val a = Decoupled(new TLBundleA(params))
  val b = Decoupled(new TLBundleB(params)).flip
  val c = Decoupled(new TLBundleC(params))
  val d = Decoupled(new TLBundleD(params)).flip
  val e = Decoupled(new TLBundleE(params))
}

object TLBundle
{
  def apply(params: TLBundleParameters) = new TLBundle(params)
}

final class DecoupledSnoop[+T <: Data](gen: T) extends Bundle
{
  val ready = Bool()
  val valid = Bool()
  val bits = gen.asOutput

  def fire(dummy: Int = 0) = ready && valid
  override def cloneType: this.type = new DecoupledSnoop(gen).asInstanceOf[this.type]
}

object DecoupledSnoop
{
  def apply[T <: Data](i: DecoupledIO[T]) = {
    val out = Wire(new DecoupledSnoop(i.bits))
    out.ready := i.ready
    out.valid := i.valid
    out.bits  := i.bits
    out
  }
}

class TLBundleSnoop(params: TLBundleParameters) extends TLBundleBase(params)
{
  val a = new DecoupledSnoop(new TLBundleA(params))
  val b = new DecoupledSnoop(new TLBundleB(params))
  val c = new DecoupledSnoop(new TLBundleC(params))
  val d = new DecoupledSnoop(new TLBundleD(params))
  val e = new DecoupledSnoop(new TLBundleE(params))
}

object TLBundleSnoop
{
  def apply(x: TLBundle) = {
    val out = Wire(new TLBundleSnoop(x.params))
    out.a <> DecoupledSnoop(x.a)
    out.b <> DecoupledSnoop(x.b)
    out.c <> DecoupledSnoop(x.c)
    out.d <> DecoupledSnoop(x.d)
    out.e <> DecoupledSnoop(x.e)
    out
  }
}

class TLAsyncBundleBase(params: TLAsyncBundleParameters) extends GenericParameterizedBundle(params)

class TLAsyncBundle(params: TLAsyncBundleParameters) extends TLAsyncBundleBase(params)
{
  val a = new AsyncBundle(params.depth, new TLBundleA(params.base))
  val b = new AsyncBundle(params.depth, new TLBundleB(params.base)).flip
  val c = new AsyncBundle(params.depth, new TLBundleC(params.base))
  val d = new AsyncBundle(params.depth, new TLBundleD(params.base)).flip
  val e = new AsyncBundle(params.depth, new TLBundleE(params.base))
}
