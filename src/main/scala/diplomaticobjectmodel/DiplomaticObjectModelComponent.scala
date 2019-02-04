package freechips.rocketchip.diplomaticobjectmodel

import chisel3.experimental.RawModule
import diplomaticobjectmodel.DiplomaticObjectModel
import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent

abstract class DiplomaticObjectModelComponent {
  def getComponent(): OMComponent

  def add(c: OMComponent): Unit = {
    DiplomaticObjectModel.add(c)
  }
}
