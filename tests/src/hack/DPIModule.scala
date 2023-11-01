// TODO: this is DIRTY, the future plan for this is provide a DPIIntModule with parameters: trigger, guard, function signature.
package chisel3

import chisel3.experimental.ExtModule
import chisel3.internal.firrtl.{KnownWidth, UnknownWidth}
import chisel3.util.HasExtModuleInline

import scala.collection.mutable.ArrayBuffer

case class DPIElement[T <: Element](name: String, output: Boolean, data: T)

case class DPIReference[T <: Element](name: String, ref: T)

abstract class DPIModule extends ExtModule with HasExtModuleInline {

  // C Style
  override def desiredName: String = "[A-Z\\d]".r.replaceAllIn(
    super.desiredName,
    { m =>
      (if (m.end(0) == 1) "" else "_") + m.group(0).toLowerCase()
    }
  )

  def dpiTrigger[T <: Element](name: String, data: T) = bind(name, false, Input(data.cloneType))

  def dpi[T <: Element](name: String, data: T) = bind(name, true, data)

  val isImport: Boolean
  val references:    ArrayBuffer[DPIElement[_]] = scala.collection.mutable.ArrayBuffer.empty[DPIElement[_]]
  val dpiReferences: ArrayBuffer[DPIElement[_]] = scala.collection.mutable.ArrayBuffer.empty[DPIElement[_]]

  def bind[T <: Element](name: String, isDPIArg: Boolean, data: T) = {
    val ref = IO(data).suggestName(name)

    val ele = DPIElement(
      name,
      chisel3.reflect.DataMirror.directionOf(ref) match {
        case ActualDirection.Empty              => throw new Exception("no direction")
        case ActualDirection.Unspecified        => throw new Exception("no direction")
        case ActualDirection.Output             => true
        case ActualDirection.Input              => false
        case ActualDirection.Bidirectional(dir) => throw new Exception("unknown direction")
      },
      ref
    )
    require(!references.exists(ele => ele.name == name), s"$name already added.")
    references += ele
    if (isDPIArg) {
      dpiReferences += ele
    }
    DPIReference(name, ref)
  }

  val trigger:    String = ""
  val guard:      String = ""
  val exportBody: String = ""
  // Magic to execute post-hook
  private[chisel3] override def generateComponent() = {
    // return binding function and probe signals
    val localDefinition = "(" + references.map {
      case DPIElement(name, _, element) =>
        val output = chisel3.reflect.DataMirror.directionOf(element) match {
          case ActualDirection.Empty              => throw new Exception("no direction")
          case ActualDirection.Unspecified        => throw new Exception("no direction")
          case ActualDirection.Output             => true
          case ActualDirection.Input              => false
          case ActualDirection.Bidirectional(dir) => throw new Exception("unknown direction")
        }
        val width = chisel3.reflect.DataMirror.widthOf(element) match {
          case UnknownWidth()    => throw new Exception(s"$desiredName.$name width unknown")
          case KnownWidth(value) => value
        }
        val tpe = if (width != 1) s"bit[${width - 1}:0] " else ""
        s"${if (output) "output" else "input"} $tpe$name"
    }.mkString(", ") + ")"

    val dpiArg = dpiReferences.map {
      case DPIElement(name, output, element) =>
        val direction = if (output) "output " else "input "
        val width = chisel3.reflect.DataMirror.widthOf(element) match {
          case UnknownWidth()    => throw new Exception(s"$desiredName.$name width unknown")
          case KnownWidth(value) => value
        }
        val tpe = if (width != 1) s"bit[${width - 1}:0] " else ""
        s"$direction$tpe$name"
    }.mkString(", ")

    setInline(
      s"$desiredName.sv",
      s"""module $desiredName$localDefinition;
         |${if (isImport) s"""import "DPI-C" function void $desiredName($dpiArg);"""
      else s"""export "DPI-C" function $desiredName;"""}
         |${if (isImport) s"""$trigger ${if (guard.nonEmpty) s"if($guard)" else ""} $desiredName(${dpiReferences
                               .map(_.name)
                               .mkString(", ")});"""
      else ""}
         |${if (!isImport) exportBody else ""}
         |endmodule
         |""".stripMargin.lines().filter(_.nonEmpty).toArray.mkString("\n")
    )
    super.generateComponent()
  }
}
