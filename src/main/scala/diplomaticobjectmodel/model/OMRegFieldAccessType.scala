// See LICENSE for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

sealed trait OMRegFieldAccessType
case object R extends OMRegFieldAccessType
case object W extends OMRegFieldAccessType
case object RW extends OMRegFieldAccessType

