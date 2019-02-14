// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import chisel3.experimental.RawModule
import freechips.rocketchip.diplomacy.ResourceBindingsMap
import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent

import scala.collection.mutable

case class OMEdge(
  parent: RawModule,
  child: RawModule
)

trait OMRegistrar {
  def parent: RawModule
  def child: RawModule
  def getEdge(f: () => OMEdge): OMEdge
  def getOMComponents(): Seq[OMComponent]
}

/**
  * Used to construct th logical tree from the Edge's
  * @param components
  * @param children
  */

case class OMVertex(
  components: List[OMComponent],
  children: List[OMVertex]
)

case object LogicalTreeRegistry {
  private var resourceBindingsMap: Option[ResourceBindingsMap] = None

  private val registryList: List[() => OMEdge] = List[() => OMEdge]()

  def setResourceBindingsMap(rbm: ResourceBindingsMap): Unit = resourceBindingsMap = Some(rbm)
  def getResourceBindingsMap(): ResourceBindingsMap = resourceBindingsMap.get

  def register(lr: OMRegistrar): Unit = {
    registryList.+:(lr)
  }

  def cycleCheck(): Boolean = false

  def makeTree(): Unit = {
    require(resourceBindingsMap != None)

  }

  def getObjectModel(): OMComponent = {

      registryList.map{
        case lr: OMRegistrar => lr.getOMComponents()
      }
    }
  }

}

class TopologicalSort {
  val stack = mutable.MutableList[OMVertex]()
  val visited = mutable.Set[OMVertex]()




}
