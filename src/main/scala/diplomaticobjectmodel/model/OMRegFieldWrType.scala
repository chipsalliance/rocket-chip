// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

/* The following enum names come from IP-XACT */
sealed trait OMRegFieldWrType extends OMEnum
case object RFWT_ONE_TO_CLEAR   extends OMRegFieldWrType
case object RFWT_ONE_TO_SET     extends OMRegFieldWrType
case object RFWT_ONE_TO_TOGGLE  extends OMRegFieldWrType
case object RFWT_ZERO_TO_CLEAR  extends OMRegFieldWrType
case object RFWT_ZERO_TO_SET    extends OMRegFieldWrType
case object RFWT_ZERO_TO_TOGGLE extends OMRegFieldWrType
case object RFWT_CLEAR          extends OMRegFieldWrType
case object RFWT_SET            extends OMRegFieldWrType
case object RFWT_MODIFY         extends OMRegFieldWrType

