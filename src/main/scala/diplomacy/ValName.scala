// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import scala.language.experimental.macros
import freechips.rocketchip.macros.ValNameImpl

case class ValName(name: String)

object ValName
{
  implicit def materialize(implicit x: ValNameImpl): ValName = ValName(x.name)
}
