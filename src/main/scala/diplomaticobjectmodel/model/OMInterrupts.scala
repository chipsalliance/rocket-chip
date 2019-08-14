// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMInterrupt(
  receiver: String, // TODO Reference
  numberAtReceiver: BigInt,
  name: String,
  _types: Seq[String] = Seq("OMInterrupt", "OMCompoundType")
)  extends OMCompoundType

