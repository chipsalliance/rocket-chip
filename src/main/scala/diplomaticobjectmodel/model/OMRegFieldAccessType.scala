// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

sealed trait OMRegFieldAccessType extends OMEnum
case object R  extends OMRegFieldAccessType
case object W  extends OMRegFieldAccessType
case object RW extends OMRegFieldAccessType

