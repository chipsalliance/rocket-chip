// See LICENSE for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

sealed trait OMRegFieldRdAction
case object RFRA_CLEAR  extends OMRegFieldRdAction
case object RFRA_SET    extends OMRegFieldRdAction
case object RFRA_MODIFY extends OMRegFieldRdAction
