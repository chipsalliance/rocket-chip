// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

sealed trait OMCoreComplexResetType extends OMEnum
case object CoreComplexResetTypeUnspecified extends OMCoreComplexResetType
case object CoreComplexResetTypeSingleSynchronous extends OMCoreComplexResetType
case object CoreComplexResetTypeSingleAsynchronous extends OMCoreComplexResetType
case object CoreComplexResetTypeSingleAsynchronousFull extends OMCoreComplexResetType
case object CoreComplexResetTypeSeparateCoreAndUncoreSynchronous extends OMCoreComplexResetType
case object CoreComplexResetTypeSeparateCoreAndUncoreAsynchronous extends OMCoreComplexResetType
case object CoreComplexResetTypeSeparateCoreAndUncoreAsynchronousFull extends OMCoreComplexResetType
case object CoreComplexResetTypeSeparateGPRAsynchronousFull extends OMCoreComplexResetType

case class OMCoreComplex(
  components: Seq[OMComponent],
  documentationName: String,
  resetType: Option[OMCoreComplexResetType],
  _types: Seq[String] = Seq("OMCoreComplex", "OMComponent", "OMCompoundType")
) extends OMComponent
