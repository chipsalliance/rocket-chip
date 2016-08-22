// See LICENSE for license details.

package uncore.tilelink2

import Chisel._

abstract class TLBundleBase(val params: TLBundleParameters) extends Bundle
{
  override def cloneType = {
    try {
      this.getClass.getConstructors.head.newInstance(params).asInstanceOf[this.type]
    } catch {
      case e: java.lang.IllegalArgumentException =>
        throwException("Unable to use TLBundleBase.cloneType on " +
                       this.getClass + ", probably because " + this.getClass +
                       "() takes more than one argument.  Consider overriding " +
                       "cloneType() on " + this.getClass, e)
    }
  }
}

object TLMessages 
{
  //                                  A    B    C    D    E
  val Get            = UInt(0) //     .    .
  val PutFullData    = UInt(1) //     .    .
  val PutPartialData = UInt(2) //     .    .
  val ArithmeticData = UInt(3) //     .    .
  val LogicalData    = UInt(4) //     .    .
  val Hint           = UInt(5) //     .    .
  val AccessAck      = UInt(0) //               .    .
  val AccessAckData  = UInt(1) //               .    .
  val AccessAckError = UInt(2) //               .    .
  val Acquire        = UInt(6) //     .
  val Probe          = UInt(6) //          .
  val ProbeAck       = UInt(3) //               .
  val ProbeAckData   = UInt(4) //               .
  val Release        = UInt(5) //               .
  val ReleaseData    = UInt(6) //               .
//val PutThroughData = UInt(7) //               .              // future extension
  val ReleaseAck     = UInt(3) //                    .
  val Grant          = UInt(4) //                    .
  val GrantData      = UInt(5) //                    .
  val GrantAck       = UInt(0) //                         .
 
  def isA(x: UInt) = x <= Acquire
  def isB(x: UInt) = x <= Probe
  def isC(x: UInt) = x <= ReleaseData
  def isD(x: UInt) = x <= GrantData
}

object TLPermissions
{
  // Cap types (Grant = new permissions, Probe = permisions <= target)
  val toT = UInt(0)
  val toB = UInt(1)
  val toN = UInt(2)
  def isCap(x: UInt) = x <= toN

  // Grow types (Acquire = permissions >= target)
  val NtoB = UInt(0)
  val NtoT = UInt(1)
  val BtoT = UInt(2)
  def isGrow(x: UInt) = x <= BtoT

  // Shrink types (ProbeAck, Release)
  val TtoB = UInt(0)
  val TtoN = UInt(1)
  val BtoN = UInt(2)
  def isShrink(x: UInt) = x <= BtoN

  // Report types (ProbeAck)
  val TtoT = UInt(3)
  val BtoB = UInt(4)
  val NtoN = UInt(5)
  def isReport(x: UInt) = x <= NtoN
}

object TLAtomics
{
  // Arithmetic types
  def isArithmetic(x: UInt) = Bool(true)

  // Logical types
  def isLogical(x: UInt) = Bool(true)
}

class TLBundleA(params: TLBundleParameters) extends TLBundleBase(params)
{
  val opcode  = UInt(width = 3)
  val param   = UInt(width = 3) // amo_opcode || perms || hint
  val size    = UInt(width = params.sizeBits)
  val source  = UInt(width = params.sourceBits)  // from
  val address = UInt(width = params.addressBits) // to
  val wmask   = UInt(width = params.dataBits/8)
  val data    = UInt(width = params.dataBits)
}

class TLBundleB(params: TLBundleParameters) extends TLBundleBase(params)
{
  val opcode  = UInt(width = 3)
  val param   = UInt(width = 3)
  val size    = UInt(width = params.sizeBits)
  val source  = UInt(width = params.sourceBits)  // to
  val address = UInt(width = params.addressBits) // from
  val wmask   = UInt(width = params.dataBits/8)
  val data    = UInt(width = params.dataBits)
}

class TLBundleC(params: TLBundleParameters) extends TLBundleBase(params)
{
  val opcode  = UInt(width = 3)
  val param   = UInt(width = 3)
  val size    = UInt(width = params.sizeBits)
  val source  = UInt(width = params.sourceBits)  // from
  val address = UInt(width = params.addressBits) // to
  val data    = UInt(width = params.dataBits)
}

class TLBundleD(params: TLBundleParameters) extends TLBundleBase(params)
{
  val opcode = UInt(width = 3)
  val param  = UInt(width = 2)
  val size   = UInt(width = params.sizeBits)
  val source = UInt(width = params.sourceBits) // to
  val sink   = UInt(width = params.sinkBits)   // from
  val data   = UInt(width = params.dataBits)
}

class TLBundleE(params: TLBundleParameters) extends TLBundleBase(params)
{
  val sink = UInt(width = params.sourceBits) // to
}

class TLBundle(params: TLBundleParameters) extends TLBundleBase(params)
{
  val a = Decoupled(new TLBundleA(params))
  val b = Decoupled(new TLBundleB(params)).flip
  val c = Decoupled(new TLBundleC(params))
  val d = Decoupled(new TLBundleD(params)).flip
  val e = Decoupled(new TLBundleE(params))
}
