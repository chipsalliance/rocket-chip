package diplomaticobjectmodel

import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent

import scala.collection.mutable.ListBuffer

object DiplomaticObjectModel {
  val components = new ListBuffer[OMComponent]()

  def add(c: OMComponent): Unit = {
    components.+=:(c)
  }
}
