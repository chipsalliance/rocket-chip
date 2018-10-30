// See LICENSE for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

trait OMBaseType {
  def getTypes: Seq[String]
}

trait OMCompoundType extends OMBaseType {
  override def getTypes: Seq[String] = Seq[String]("CompoundType")
}

trait OMComponent extends OMCompoundType {
  override def getTypes: Seq[String] = Seq[String]("Component") ++ super.getTypes
}

