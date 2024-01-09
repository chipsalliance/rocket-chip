// See LICENSE.SiFive for license details.

package freechips.rocketchip.interrupts

import chisel3._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._

/** Useful for stubbing out parts of an interrupt interface where certain devices might be missing */
class NullIntSource(num: Int = 1, ports: Int = 1, sources: Int = 1)(implicit p: Parameters) extends LazyModule
{
  val intnode = IntSourceNode(IntSourcePortSimple(num, ports, sources))

  lazy val module = new Impl
  class Impl extends LazyRawModuleImp(this) {
    intnode.out.foreach { case (o, _) => o.foreach { _ := false.B } }
  }
}

object NullIntSource {
  def apply(num: Int = 1, ports: Int = 1, sources: Int = 1)(implicit p: Parameters): IntOutwardNode = {
    val null_int_source = LazyModule(new NullIntSource(num, ports, sources))
    null_int_source.intnode
  }
}

object NullIntSyncSource {
  def apply(num: Int = 1, ports: Int = 1, sources: Int = 1)(implicit p: Parameters): IntSyncOutwardNode = {
    IntSyncCrossingSource(alreadyRegistered = true) := NullIntSource(num, ports, sources)
  }
}
