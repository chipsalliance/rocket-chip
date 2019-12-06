// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3.ExplicitCompileOptions.NotStrict

object CompileOptions {
  /** Compatibility mode semantics except Module implicit reset should be inferred instead of Bool */
  implicit val NotStrictInferReset = NotStrict.copy(inferModuleReset = true)
}
