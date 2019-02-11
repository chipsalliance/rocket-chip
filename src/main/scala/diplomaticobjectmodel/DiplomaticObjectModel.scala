// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel

import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent

import scala.collection.mutable.ListBuffer

class OMCollector {
  def getComponent(): Seq[OMComponent] = Nil
}

object DiplomaticObjectModel {
  val doms = ListBuffer[OMCollector]()

  def add(d: OMCollector): Unit = {
    doms += (d)
  }

  def getComponents(): Seq[OMComponent] = {
    doms.flatMap {
      case dc: OMCollector => dc.getComponent()
    }
  }
}
