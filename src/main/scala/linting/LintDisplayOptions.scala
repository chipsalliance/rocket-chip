// See LICENSE.SiFive for license details.

package freechips.rocketchip.linting

import firrtl.annotations.NoTargetAnnotation

case class LintDisplayOptions(
    level: String  = "strict",
    totalLimit: Option[Int] = None,
    perErrorLimit: Map[Int, Int] = Map.empty
) extends NoTargetAnnotation
