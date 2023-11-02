package org.chipsalliance.rocketchip.internal.tests

import chisel3.UInt
import freechips.rocketchip.diplomacy.{BundleBridgeSource, InModuleBody}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util.DontTouch
import org.chipsalliance.cde.config.Parameters

trait HaveExternalResetVector {
  this: ExampleRocketSystem =>
  // configure
  val resetVectorSourceNode = BundleBridgeSource[UInt]()
  tileResetVectorNexusNode := resetVectorSourceNode
  val resetVector = InModuleBody(resetVectorSourceNode.makeIO())
}

/** Example Top with periphery devices and ports, and a Rocket subsystem */
class ExampleRocketSystem(implicit p: Parameters) extends RocketSubsystem
  with CanHaveMasterAXI4MemPort
  with HaveExternalResetVector {
  override lazy val module = new ExampleRocketSystemModuleImp(this)
}

class ExampleRocketSystemModuleImp[+L <: ExampleRocketSystem](_outer: L) extends RocketSubsystemModuleImp(_outer)
  with HasRTCModuleImp
  with DontTouch
