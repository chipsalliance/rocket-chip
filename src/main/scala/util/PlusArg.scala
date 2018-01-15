// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._

case class PlusArgInfo(default: Int, docstring: String)

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

  def apply(name: String, default: Int = 0, docstring: String = ""): UInt = {
    PlusArgArtefacts.append(name, default, docstring)
    Module(new plusarg_reader(name + "=%d", default, docstring)).io.out
  }
}

object PlusArgArtefacts {
  private var artefacts: Map[String, PlusArgInfo] = Map.empty

  /* Add a new PlusArg */
  def append(name: String, default: Int, docstring: String): Unit =
    artefacts = artefacts ++ Map(name -> PlusArgInfo(default, docstring))

  /* From plus args, generate help text */
  private def serializeHelp_cHeader(tab: String = ""): String = artefacts
    .map{ case(arg, PlusArgInfo(default, docstring)) =>
      s"""|$tab+$arg=INT\\n\\
          |$tab${" "*20}$docstring\\n\\
          |$tab${" "*22}(default=$default)""".stripMargin }.toSeq
    .mkString("\\n\\\n") ++ "\""

  /* From plus args, generate a char array of their names */
  private def serializeArray_cHeader(tab: String = ""): String = {
    val prettyTab = tab + " " * 44 // Length of 'static const ...'
    s"${tab}static const char * verilog_plusargs [] = {\\\n" ++
      artefacts
        .map{ case(arg, _) => s"""$prettyTab"$arg",\\\n""" }
        .mkString("")++
    s"${prettyTab}0};"
  }

  /* Generate C code to be included in emulator.cc that helps with
   * argument parsing based on available Verilog PlusArgs */
  def serialize_cHeader(): String =
    s"""|#define PLUSARG_USAGE_OPTIONS \"EMULATOR VERILOG PLUSARGS\\n\\
        |${serializeHelp_cHeader(" "*7)}
        |${serializeArray_cHeader()}
        |""".stripMargin
}
