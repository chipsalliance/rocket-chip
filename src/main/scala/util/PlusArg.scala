// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.experimental._
import chisel3.util.HasBlackBoxResource

case class PlusArgInfo(default: BigInt, docstring: String)

class plusarg_reader(val format: String, val default: BigInt, val docstring: String, val width: Int) extends BlackBox(Map(
    "FORMAT"  -> StringParam(format),
    "DEFAULT" -> IntParam(default),
    "WIDTH" -> IntParam(width)
  )) with HasBlackBoxResource {
  val io = IO(new Bundle {
    val out = Output(UInt(width.W))
  })

  addResource("/vsrc/plusarg_reader.v")
}

/* This wrapper class has no outputs, making it clear it is a simulation-only construct */
class PlusArgTimeout(val format: String, val default: BigInt, val docstring: String, val width: Int) extends Module {
  val io = IO(new Bundle {
    val count = Input(UInt(width.W))
  })
  val max = Module(new plusarg_reader(format, default, docstring, width)).io.out
  when (max > 0.U) {
    assert (io.count < max, s"Timeout exceeded: $docstring")
  }
}

object PlusArg
{
  /** PlusArg("foo") will return 42.U if the simulation is run with +foo=42
    * Do not use this as an initial register value. The value is set in an
    * initial block and thus accessing it from another initial is racey.
    * Add a docstring to document the arg, which can be dumped in an elaboration
    * pass.
    */
  def apply(name: String, default: BigInt = 0, docstring: String = "", width: Int = 32): UInt = {
    PlusArgArtefacts.append(name, default, docstring)
    Module(new plusarg_reader(name + "=%d", default, docstring, width)).io.out
  }

  /** PlusArg.timeout(name, default, docstring)(count) will use chisel.assert
    * to kill the simulation when count exceeds the specified integer argument.
    * Default 0 will never assert.
    */
  def timeout(name: String, default: BigInt = 0, docstring: String = "", width: Int = 32)(count: UInt) {
    PlusArgArtefacts.append(name, default, docstring)
    Module(new PlusArgTimeout(name + "=%d", default, docstring, width)).io.count := count
  }
}

object PlusArgArtefacts {
  private var artefacts: Map[String, PlusArgInfo] = Map.empty

  /* Add a new PlusArg */
  def append(name: String, default: BigInt, docstring: String): Unit =
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
