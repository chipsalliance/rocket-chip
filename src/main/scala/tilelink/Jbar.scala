// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

class TLJbar(clientRatio: Int, managerRatio: Int, policy: TLArbiter.Policy = TLArbiter.roundRobin)(implicit p: Parameters) extends LazyModule
{
  val node = TLJunctionNode(clientRatio, managerRatio,
    clientFn  = { seq =>
      Seq.fill(managerRatio)(seq(0).copy(
        minLatency = seq.map(_.minLatency).min,
        clients = (TLXbar.mapInputIds(seq) zip seq) flatMap { case (range, port) =>
          port.clients map { client => client.copy(
            sourceId = client.sourceId.shift(range.start)
          )}
        }
      ))
    },
    managerFn = { seq =>
      val fifoIdFactory = TLXbar.relabeler()
      Seq.fill(clientRatio)(seq(0).copy(
        minLatency = seq.map(_.minLatency).min,
        endSinkId = TLXbar.mapOutputIds(seq).map(_.end).max,
        managers = seq.flatMap { port =>
          require (port.beatBytes == seq(0).beatBytes,
            s"Xbar data widths don't match: ${port.managers.map(_.name)} has ${port.beatBytes}B vs ${seq(0).managers.map(_.name)} has ${seq(0).beatBytes}B")
          val fifoIdMapper = fifoIdFactory()
          port.managers map { manager => manager.copy(
            fifoId = manager.fifoId.map(fifoIdMapper(_))
          )}
        }
      ))
    })

  lazy val module = new LazyModuleImp(this) {
    println(s"JBar info: ${node.in.size}/${clientRatio} vs ${node.out.size}/${managerRatio}")
    node.inoutGrouped.foreach { case (in, out) => TLXbar.circuit(policy, in, out) }
  }
}

object TLJbar
{
  def apply(clientRatio: Int, managerRatio: Int, policy: TLArbiter.Policy = TLArbiter.roundRobin)(implicit p: Parameters) = {
    val jbar = LazyModule(new TLJbar(clientRatio, managerRatio, policy))
    jbar.node
  }
}
