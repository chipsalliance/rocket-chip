// See LICENSE for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

//trait OMBaseType {
//  def getTypes: Seq[String]
//}
//
//trait OMCompoundType extends OMBaseType {
//  override def getTypes: Seq[String] = Seq[String]("CompoundType")
//}
//
//trait OMComponent extends OMCompoundType {
//  override def getTypes: Seq[String] = Seq[String]("Component") ++ super.getTypes
//}

trait OMBaseType {
  def getTypes: Seq[String] = Seq("Hello")// getInterfaces.map(_.getSimpleName) // Something like that
}

trait OMFoo extends OMBaseType

trait OMEnum extends OMFoo

trait OMCompoundType extends OMBaseType

trait OMComponent extends OMCompoundType

