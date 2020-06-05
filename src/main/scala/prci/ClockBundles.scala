// See LICENSE.SiFive for license details.
package freechips.rocketchip.prci

import chisel3._
import freechips.rocketchip.util.RecordMap
import scala.collection.immutable.ListMap


class ClockBundle(val params: ClockBundleParameters) extends Bundle
{
  val clock = Clock()
  val reset = Reset()
}

class ClockGroupBundle(val params: ClockGroupBundleParameters) extends Bundle
{
  val member: RecordMap[ClockBundle] = {
    val nameToBundleMap = ListMap(params.members.map{case (k: String, v: ClockBundleParameters) => k -> new ClockBundle(v)}.toMap)
    new RecordMap(nameToBundleMap)
  }
}
