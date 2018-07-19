// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tilelink._

abstract class TLPeripheralPuncher[T <: Data](
     val base:        BigInt,
     val devname:     String,
     val devcompat:   Seq[String],
     val nInterrupts: Int     = 0,
     val size:        BigInt  = 4096,
     val concurrency: Int     = 0,
     val beatBytes:   Int     = 4,
     val undefZero:   Boolean = true,
     val executable:  Boolean = false)
    (portBundle: T)(implicit p: Parameters)
  extends TLRegisterRouterBase(devname, devcompat, AddressSet(base, size-1), nInterrupts, concurrency, beatBytes, undefZero, executable)
  with LazyScope {

  protected val tlControlXing = new TLCrossingHelper(this)
  protected val intXing = new IntCrossingHelper(this)
  def crossControl(crossing: ClockCrossingType)(implicit p: Parameters): TLNode = node := tlControlXing.crossIn(crossing)
  def crossInt(crossing: ClockCrossingType)(implicit p: Parameters): IntNode = intXing.crossOut(crossing) := intnode

  val ioNode = BundleBridgeSource(() => portBundle.cloneType)
  val port = InModuleBody { ioNode.out.head._1 }

  val interrupts = InModuleBody { if (intnode.out.isEmpty) Vec(0, Bool()) else intnode.out(0)._1 }
}
