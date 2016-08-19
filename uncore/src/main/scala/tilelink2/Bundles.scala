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

class TLBundleA(params: TLBundleParameters) extends TLBundleBase(params)
{
  val opcode  = UInt(width = 3)
  val param   = UInt(width = 3) // amo_opcode || perms(req)
  val size    = UInt(width = params.sizeBits)
  val source  = UInt(width = params.sourceBits)  // from
  val address = UInt(width = params.addressBits) // to
  val wmask   = UInt(width = params.dataBits/8)
  val data    = UInt(width = params.dataBits)
}

class TLBundleB(params: TLBundleParameters) extends TLBundleBase(params)
{
  val opcode  = UInt(width = 3)
  val param   = UInt(width = 3) // amo_opcode || perms(req)
  val size    = UInt(width = params.sizeBits)
  val source  = UInt(width = params.sourceBits)  // to
  val address = UInt(width = params.addressBits) // from
  val wmask   = UInt(width = params.dataBits/8)
  val data    = UInt(width = params.dataBits)
}

class TLBundleC(params: TLBundleParameters) extends TLBundleBase(params)
{
  val opcode  = UInt(width = 3)
  val param   = UInt(width = 3) // perms(from=>to)
  val size    = UInt(width = params.sizeBits)
  val address = UInt(width = params.addressBits) // to
  val data    = UInt(width = params.dataBits)
  val error   = Bool()
}

class TLBundleD(params: TLBundleParameters) extends TLBundleBase(params)
{
  val opcode = UInt(width = 3)
  val param  = UInt(width = 3) // perms(to)
  val size   = UInt(width = params.sizeBits)
  val source = UInt(width = params.sourceBits) // to
  val sink   = UInt(width = params.sinkBits)   // from
  val data   = UInt(width = params.dataBits)
  val error  = Bool()
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
