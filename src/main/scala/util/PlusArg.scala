// See LICENSE.SiFive for license details.

package util
import Chisel._

class plusarg_reader(format: String, default: Int) extends BlackBox(Map(
    "FORMAT"  -> chisel3.core.StringParam(format),
    "DEFAULT" -> chisel3.core.IntParam(default))) {
  val io = new Bundle {
    val out = UInt(OUTPUT, width = 32)
  }
}

object PlusArg
{
  // PlusArg("foo") will return 42 if the simulation is run with +foo=42
  // Do not use this as an initial register value. The value is set in an
  // initial block and thus accessing it from another initial is racey.
  def apply(name: String, default: Int = 0): UInt =
    Module(new plusarg_reader(name + "=%d", default)).io.out
}
