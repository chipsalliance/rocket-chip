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

// common combos in lazy policy:
//   Put + Acquire
//   Release + AccessAck

object TLMessages 
{
  //                                  A    B    C    D    E
  val PutFullData    = UInt(0) //     .    .
  val PutPartialData = UInt(1) //     .    .
  val ArithmeticData = UInt(2) //     .    .
  val LogicalData    = UInt(3) //     .    .
  val Get            = UInt(4) //     .    .
  val Hint           = UInt(5) //     .    .
  val AccessAck      = UInt(0) //               .    .
  val AccessAckData  = UInt(1) //               .    .
  val AccessAckError = UInt(6) //               .    .
  val Acquire        = UInt(6) //     .
  val Probe          = UInt(6) //          .
  val ProbeAck       = UInt(2) //               .
  val ProbeAckData   = UInt(3) //               .
  val Release        = UInt(4) //               .
  val ReleaseData    = UInt(5) //               .
//val PutThroughData = UInt(7) //               .              // future extension ?
  val Grant          = UInt(2) //                    .
  val GrantData      = UInt(3) //                    .
  val ReleaseAck     = UInt(4) //                    .
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

trait HasTLOpcode
{
  // The data field in this message has value
  def hasData(x: Int=0): Bool
  // This message requires a response
  def hasFollowUp(x: Int=0): Bool
  // The size field of the opcode
  def size(x: Int=0): UInt
}

trait HasTLData extends HasTLOpcode
{
  def data(x: Int=0): UInt
  def wmask(x: Int=0): UInt
}

// !!! trait HasTLSource|Sink|Address
// !!! trait param: from and to perms

class TLBundleA(params: TLBundleParameters)
  extends TLBundleBase(params)
  with HasTLData
{
  val opcode  = UInt(width = 3)
  val param   = UInt(width = 3) // amo_opcode || perms || hint
  val size    = UInt(width = params.sizeBits)
  val source  = UInt(width = params.sourceBits)  // from
  val address = UInt(width = params.addressBits) // to
  val wmask   = UInt(width = params.dataBits/8)
  val data    = UInt(width = params.dataBits)

  def hasData(x: Int=0) = !opcode(2)
//    opcode === TLMessages.PutFullData    ||
//    opcode === TLMessages.PutPartialData ||
//    opcode === TLMessages.ArithmeticData ||
//    opcode === TLMessages.LogicalData
  def hasFollowUp(x: Int=0) = Bool(true)
  def size(x: Int=0) = size
  def data(x: Int=0) = data
  def wmask(x: Int=0) = wmask
}

class TLBundleB(params: TLBundleParameters)
  extends TLBundleBase(params)
  with HasTLData
{
  val opcode  = UInt(width = 3)
  val param   = UInt(width = 3)
  val size    = UInt(width = params.sizeBits)
  val source  = UInt(width = params.sourceBits)  // to
  val address = UInt(width = params.addressBits) // from
  val wmask   = UInt(width = params.dataBits/8)
  val data    = UInt(width = params.dataBits)

  def hasData(x: Int=0) = !opcode(2)
  def hasFollowUp(x: Int=0) = Bool(true)
  def size(x: Int=0) = size
  def data(x: Int=0) = data
  def wmask(x: Int=0) = wmask
}

class TLBundleC(params: TLBundleParameters)
  extends TLBundleBase(params)
  with HasTLData
{
  val opcode  = UInt(width = 3)
  val param   = UInt(width = 3)
  val size    = UInt(width = params.sizeBits)
  val source  = UInt(width = params.sourceBits)  // from
  val address = UInt(width = params.addressBits) // to
  val data    = UInt(width = params.dataBits)

  def hasData(x: Int=0) = opcode(0)
//    opcode === TLMessages.AccessAckData ||
//    opcode === TLMessages.ProbeAckData  ||
//    opcode === TLMessages.ReleaseData
  def hasFollowUp(x: Int=0) = opcode(2) && !opcode(1)
//    opcode === TLMessages.Release ||
//    opcode === TLMessages.ReleaseData
  def size(x: Int=0) = size
  def data(x: Int=0) = data
  def wmask(x: Int=0) = SInt(-1, width = params.dataBits/8).asUInt
}

class TLBundleD(params: TLBundleParameters)
  extends TLBundleBase(params)
  with HasTLData
{
  val opcode = UInt(width = 3)
  val param  = UInt(width = 2)
  val size   = UInt(width = params.sizeBits)
  val source = UInt(width = params.sourceBits) // to
  val sink   = UInt(width = params.sinkBits)   // from
  val data   = UInt(width = params.dataBits)

  def hasData(x: Int=0) = opcode(0)
//    opcode === TLMessages.AccessAckData ||
//    opcode === TLMessages.GrantData
  def hasFollowUp(x: Int=0) = !opcode(2) && opcode(1)
//    opcode === TLMessages.Grant     ||
//    opcode === TLMessages.GrantData
  def size(x: Int=0) = size
  def data(x: Int=0) = data
  def wmask(x: Int=0) = SInt(-1, width = params.dataBits/8).asUInt
}

class TLBundleE(params: TLBundleParameters)
  extends TLBundleBase(params)
  with HasTLOpcode
{
  val sink = UInt(width = params.sourceBits) // to

  def hasData(x: Int=0) = Bool(false)
  def hasFollowUp(x: Int=0) = Bool(false)
  def size(x: Int=0) = UInt(log2Up(params.dataBits/8))
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
