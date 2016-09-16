// See LICENSE for license details.

package rocketchip

import firrtl._
import firrtl.ir._
import firrtl.passes.Pass
import java.io.Writer

// The Rocket Chip pass manager: a simple pass manager that allows various
// FIRRTL passes to be inserted at various points throughout the FIRRTL build
// process on the way to Verilog.  It's expected that users will extend this
// class in order to specialize their passes.
class RocketChipPassManager extends firrtl.Compiler {
  // These functions are designed to be overridden by users.  They return a
  // sequence of transformations that will be applied at various FIRRTL levels.
  // A transform that operates at a FIRRTL level is expected to both produce
  // and consume FIRRTL at that level.
  def operateMiddle(): Seq[Transform] = Seq()
  def operateLow():    Seq[Transform] = Seq()
  
  // You probably don't want to extend this.
  def transforms(writer: Writer): Seq[Transform] = Seq(
      new Chisel3ToHighFirrtl(),
      new IRToWorkingIR(),
      new ResolveAndCheck(),
      new HighFirrtlToMiddleFirrtl()
    ) ++ operateMiddle() ++
    Seq(
      new MiddleFirrtlToLowFirrtl()
    ) ++ operateLow() ++
    Seq(
      new EmitVerilogFromLowFirrtl(writer)
    )
}
