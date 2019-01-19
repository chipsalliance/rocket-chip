// See LICENSE.SiFive for license details.
package freechips.rocketchip.clocks

import Chisel._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class ClockBundle(params: ClockBundleParameters) extends GenericParameterizedBundle(params)
{
  val clock = Clock()
  val reset = Bool()
}

class ClockGroupBundle(params: ClockGroupBundleParameters) extends GenericParameterizedBundle(params)
{
  val member = HeterogeneousBag(params.members.map(p => new ClockBundle(p)))
}
