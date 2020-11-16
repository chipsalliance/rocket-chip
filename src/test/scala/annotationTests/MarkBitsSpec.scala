// See LICENSE.SiFive for license details.

package freechips.rocketchip.annotationTests

import java.io.File

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import chisel3._
import chisel3.stage.ChiselStage
import freechips.rocketchip.util.{MarkBits, MarkBitsAnnotation}

/** Circuit for testing [[MarkBits]] API.
  *
  * Two 2:1 adders are instantiated within a top-level adder, which sums the
  * submodule adders' sum together. Constraints are placed on wires throughout.
  */
object MarkBitsTester {

  /** Example use case of [[MarkBits]] API. Exends [[MarkBitsAnnotation]] with
    * extra information.
    *
    * @param property property to be emitted for each marked [[Bits]]' constraint
    * @param fileName file to write each constraint to
    */
  case class ConstraintAnnotation(property: String, override val fileName: String = "constraints.tcl") extends MarkBitsAnnotation {
    override def markOutput(path: String): String = s"-constraint $path $property"
  }

  class AdderModule(width: Int) extends MultiIOModule {
    val io = IO(new Bundle {
      val a = Input(UInt(width.W))
      val b = Input(UInt(width.W))
      val sum = Output(UInt(width.W))
    })
    io.sum := io.a + io.b
    MarkBits.mark(io.b, ConstraintAnnotation("!= '1", "constraints_adderModule.tcl"))
  }

  class AdderTop extends MultiIOModule {
    val width = 5
    val io = IO(new Bundle {
      val a = Input(UInt(width.W))
      val b = Input(UInt(width.W))
      val sum = Output(UInt(width.W))
    })
    val adderA = Module(new AdderModule(width))
    val adderB = Module(new AdderModule(width))
    adderA.io.a := io.a
    adderA.io.b := io.b
    adderB.io.a := io.a
    adderB.io.b := io.b
    io.sum := adderA.io.sum + adderB.io.sum
    MarkBits.mark(io.a, ConstraintAnnotation("== '0"))
    MarkBits.mark(adderA.io.a, ConstraintAnnotation("== '1"))
  }
}

/** Test for [[MarkBits]].
  *
  * Checks that the constraints in the circuit above are written correctly into
  * the correct files.
  */
class MarkBitsSpec extends AnyFlatSpec {

  "constraints" should "get written to files" in {
    // create target directory
    val testDir = new File("target", "CutpointAnnotationSpec")
    testDir.mkdir()

    // emit Verilog and associated files
    (new ChiselStage).emitVerilog(new MarkBitsTester.AdderTop, Array("-td", testDir.getPath))

    // check files for correct constraint paths
    val constraintsFile = new File(testDir, "constraints.tcl")
    val constraintsAdderModuleFile = new File(testDir, "constraints_adderModule.tcl")

    testDir should exist
    constraintsFile should exist
    constraintsAdderModuleFile should exist

    val constraints = scala.io.Source.fromFile(constraintsFile).getLines.toList
    val constraintsAdderModule = scala.io.Source.fromFile(constraintsAdderModuleFile).getLines.toList

    constraints should contain allOf (
      "-constraint AdderTop.adderA.io_a == '1",
      "-constraint AdderTop.io_a == '0"
    )
    constraintsAdderModule should contain allOf (
      "-constraint AdderTop.adderB.io_b != '1",
      "-constraint AdderTop.adderA.io_b != '1"
    )
  }
}
