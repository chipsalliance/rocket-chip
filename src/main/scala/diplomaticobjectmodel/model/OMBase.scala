// See LICENSE for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

trait OMBaseType

trait OMCompoundType extends OMBaseType {
  def getTypes: Seq[String] = Seq[String]("Base")
}

trait OMComponent extends OMCompoundType {
  override def getTypes: Seq[String] = Seq[String]("CompoundType") ++ super.getTypes
}

