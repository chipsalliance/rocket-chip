// See LICENSE for license details.

package freechips.rocketchip.annotationTests

import chisel3.testers.BasicTester
import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import freechips.rocketchip.util.CutpointAnnotation
import org.scalatest.FlatSpec
import freechips.rocketchip.util.Cutpoint

class CutpointTester extends RawModule {
  val in = IO(Input(Bool()))
  val out = IO(Output(Bool()))
  out := ~in
  Cutpoint.cutpoint(in)
  Cutpoint.cutpoint(out)
}

class CutpointAnnotationSpec extends FlatSpec {
  "cutpoint annotations" should "appear" in {
    val firrtl = (new ChiselStage).emitFirrtl( new CutpointTester)

    println(firrtl)
  }
}


