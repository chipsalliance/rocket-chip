// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMSpecification(
  name: String,
  version: String,
  _types: Seq[String] = Seq("OMSpecification")
)
