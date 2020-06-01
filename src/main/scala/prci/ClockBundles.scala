// See LICENSE.SiFive for license details.
package freechips.rocketchip.prci

import chisel3._
import freechips.rocketchip.util.RecordListMap
import scala.collection.immutable.ListMap


class ClockBundle(val params: ClockBundleParameters) extends Bundle
{
  val clock = Clock()
  val reset = Reset()
}

class ClockGroupBundle(val params: ClockGroupBundleParameters) extends Bundle
{
  val member: RecordListMap[ClockBundle] = {
    val nameToBundleMap = ListMap(params.members.map{p => p.name -> new ClockBundle(p)}: _*)
    new RecordListMap(nameToBundleMap)
  }
}
