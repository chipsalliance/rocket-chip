// See LICENSE.SiFive for license details.

package freechips.rocketchip.interrupts

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.util.{SynchronizerShiftReg, AsyncResetReg}

@deprecated("IntXing does not ensure interrupt source is glitch free. Use IntSyncSource and IntSyncSink", "rocket-chip 1.2")
class IntXing(sync: Int = 3)(implicit p: Parameters) extends LazyModule
{
  val intnode = IntAdapterNode()

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (intnode.in zip intnode.out) foreach { case ((in, _), (out, _)) =>
      out := SynchronizerShiftReg(in, sync)
    }
  }
}

object IntSyncCrossingSource
{
  def apply(alreadyRegistered: Boolean = false)(implicit p: Parameters) =
  {
    val intsource = LazyModule(new IntSyncCrossingSource(alreadyRegistered))
    intsource.node
  }
}


class IntSyncCrossingSource(alreadyRegistered: Boolean = false)(implicit p: Parameters) extends LazyModule
{
  val node = IntSyncSourceNode(alreadyRegistered)

  lazy val module = if (alreadyRegistered) (new ImplRegistered) else (new Impl)

  class Impl extends LazyModuleImp(this) {
    def outSize = node.out.headOption.map(_._1.sync.size).getOrElse(0)
    override def desiredName = s"IntSyncCrossingSource_n${node.out.size}x${outSize}"
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.sync := AsyncResetReg(Cat(in.reverse)).asBools
    }
  }
  class ImplRegistered extends LazyRawModuleImp(this) {
    def outSize = node.out.headOption.map(_._1.sync.size).getOrElse(0)
    override def desiredName = s"IntSyncCrossingSource_n${node.out.size}x${outSize}_Registered"
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.sync := in
    }
  }
}


object IntSyncCrossingSink
{
  @deprecated("IntSyncCrossingSink which used the `sync` parameter to determine crossing type is deprecated. Use IntSyncAsyncCrossingSink, IntSyncRationalCrossingSink, or IntSyncSyncCrossingSink instead for > 1, 1, and 0 sync values respectively", "rocket-chip 1.2")
  def apply(sync: Int = 3)(implicit p: Parameters) =
  {
    val intsink = LazyModule(new IntSyncAsyncCrossingSink(sync))
    intsink.node
  }
}


class IntSyncAsyncCrossingSink(sync: Int = 3)(implicit p: Parameters) extends LazyModule
{
  val node = IntSyncSinkNode(sync)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    override def desiredName = s"IntSyncAsyncCrossingSink_n${node.out.size}x${node.out.head._1.size}"
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out := SynchronizerShiftReg(in.sync, sync)
    }
  }
}

object IntSyncAsyncCrossingSink
{
  def apply(sync: Int = 3)(implicit p: Parameters) =
  {
    val intsink = LazyModule(new IntSyncAsyncCrossingSink(sync))
    intsink.node
  }
}

class IntSyncSyncCrossingSink()(implicit p: Parameters) extends LazyModule
{
  val node = IntSyncSinkNode(0)

  lazy val module = new Impl
  class Impl extends LazyRawModuleImp(this) {
    def outSize = node.out.headOption.map(_._1.size).getOrElse(0)
    override def desiredName = s"IntSyncSyncCrossingSink_n${node.out.size}x${outSize}"
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out := in.sync
    }
  }
}

object IntSyncSyncCrossingSink
{
  def apply()(implicit p: Parameters) =
  {
    val intsink = LazyModule(new IntSyncSyncCrossingSink())
    intsink.node
  }
}

class IntSyncRationalCrossingSink()(implicit p: Parameters) extends LazyModule
{
  val node = IntSyncSinkNode(1)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    def outSize = node.out.headOption.map(_._1.size).getOrElse(0)
    override def desiredName = s"IntSyncRationalCrossingSink_n${node.out.size}x${outSize}"
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out := RegNext(in.sync)
    }
  }
}

object IntSyncRationalCrossingSink
{
  def apply()(implicit p: Parameters) =
  {
    val intsink = LazyModule(new IntSyncRationalCrossingSink())
    intsink.node
  }
}
