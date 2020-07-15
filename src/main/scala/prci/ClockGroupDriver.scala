// See LICENSE.SiFive for license details.
package freechips.rocketchip.prci

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{InModuleBody, ModuleValue, ValName}
import freechips.rocketchip.util.{HeterogeneousBag}

/** Used to parameterize the creation of simple clock group drivers */
case class ClockGroupDriverParameters(
  num: Int = 1,
  driveFn: ClockGroupDriver.DriveFn = ClockGroupDriver.driveFromImplicitClock
) {
  def drive(node: ClockGroupEphemeralNode)(implicit p: Parameters, vn: ValName): ModuleValue[HeterogeneousBag[ClockGroupBundle]] = {
    driveFn(node, num, p, vn)
  }
}

object ClockGroupDriver {
  type DriveFn = (ClockGroupEphemeralNode, Int, Parameters, ValName) => ModuleValue[HeterogeneousBag[ClockGroupBundle]]

  /** Drive all members of all groups from the Chisel implicit clock */
  def driveFromImplicitClock: DriveFn = { (groups, num, p, vn) =>
    implicit val pp = p
    val dummyClockGroupSourceNode: ClockGroupSourceNode = SimpleClockGroupSource(num)
    groups :*= dummyClockGroupSourceNode
    InModuleBody { HeterogeneousBag[ClockGroupBundle](Nil) }
  }

  /** Drive all members of all groups from cloned IOs */
  def driveFromIOs: DriveFn = { (groups, num, p, vn) =>
    implicit val pp = p
    val ioClockGroupSourceNode = ClockGroupSourceNode(List.fill(num) { ClockGroupSourceParameters() })
    groups :*= ioClockGroupSourceNode
    InModuleBody { ioClockGroupSourceNode.makeIOs()(vn) }
  }
}
