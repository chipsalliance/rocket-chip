// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._

class plusarg_reader(val format: String, val default: Int, val docstring: String) extends BlackBox(Map(
    "FORMAT"  -> chisel3.core.StringParam(format),
    "DEFAULT" -> chisel3.core.IntParam(default))) {
  val io = new Bundle {
    val out = UInt(OUTPUT, width = 32)
  }
}

object PlusArg
{
  // PlusArg("foo") will return 42.U if the simulation is run with +foo=42
  // Do not use this as an initial register value. The value is set in an
  // initial block and thus accessing it from another initial is racey.
  // Add a docstring to document the arg, which can be dumped in an elaboration
  // pass.

  def apply(name: String, default: Int = 0, docstring: String = ""): UInt =
    Module(new plusarg_reader(name + "=%d", default, docstring)).io.out
}
