// See LICENSE.SiFive for license details.
package freechips.rocketchip.prci

import chisel3._
import freechips.rocketchip.util.HeterogeneousBag

class ClockBundle(val params: ClockBundleParameters) extends Bundle
{
  val clock = Clock()
  val reset = Reset()
}

class ClockGroupBundle(val params: ClockGroupBundleParameters, val names: Option[Seq[String]] = None) extends Bundle
{
  val member = HeterogeneousBag(params.members.map(p => new ClockBundle(p)), names)
}
