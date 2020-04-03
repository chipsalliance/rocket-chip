// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

case object Unreachable {
  def apply(): Nothing = throw new AssertionError("unreachable code")
}
