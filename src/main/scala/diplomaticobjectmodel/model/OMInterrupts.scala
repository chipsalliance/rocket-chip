// See LICENSE for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMInterrupt(
    receiver: String, // TODO Reference
    numberAtReceiver: Int,
    name: String
)  extends OMCompoundType
