// See LICENSE.SiFive for license details.

package freechips.rocketchip.annotationTests

import java.io.File

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import chisel3._
import chisel3.stage.ChiselStage
import freechips.rocketchip.util.Cutpoint

/** Circuit for testing [[Cutpoint]].
  *
  * Two 2:1 adders are instantiated within a top-level adder, which sums the
  * submodule adders' sum together. Cutpoints are placed on wires throughout.
  */
object CutpointAnnotationTester {

  class AdderModule(width: Int) extends MultiIOModule {
    val io = IO(new Bundle {
      val a = Input(UInt(width.W))
      val b = Input(UInt(width.W))
      val sum = Output(UInt(width.W))
    })
    io.sum := io.a + io.b
    Cutpoint.cutpoint(io.b, "adderModule")
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
    Cutpoint.cutpoint(io.a)
    Cutpoint.cutpoint(adderA.io.a)
  }
}

/** Test for [[Cutpoint]].
  *
  * Checks that the cutpoints in the circuit above are written correctly into
  * the correct files.
  */
class CutpointAnnotationSpec extends AnyFlatSpec {

  "cutpoint annotations" should "appear" in {
    // create target directory
    val testDir = new File("target", "CutpointAnnotationSpec")
    testDir.mkdir()

    // emit verilog and associated files
    (new ChiselStage).emitVerilog(new CutpointAnnotationTester.AdderTop, Array("-td", testDir.getPath))

    // check cutpoint files for correct cutpoint paths
    val cutpointsFile = new File(testDir, "cutpoints.txt")
    val cutpointsAdderModuleFile = new File(testDir, "cutpoints.adderModule.txt")

    testDir should exist
    cutpointsFile should exist
    cutpointsAdderModuleFile should exist

    val cutpoints = scala.io.Source.fromFile(cutpointsFile).getLines.toList
    val cutpointAdderModule = scala.io.Source.fromFile(cutpointsAdderModuleFile).getLines.toList

    cutpoints should contain allOf ("AdderTop.adderA.io_a", "AdderTop.io_a")
    cutpointAdderModule should contain allOf ("AdderTop.adderB.io_b", "AdderTop.adderA.io_b")
  }
}
