// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel

import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent

import scala.collection.mutable.ListBuffer

class DomCollector {
  def getComponent(): Option[OMComponent] = None
}

object DiplomaticObjectModel {
  val doms = ListBuffer[DomCollector]()

  def add(d: DomCollector): Unit = {
    doms += (d)
  }

  def getComponents(): Seq[OMComponent] = {
    doms.flatMap {
      case dc: DomCollector => dc.getComponent()
    }
  }
}
