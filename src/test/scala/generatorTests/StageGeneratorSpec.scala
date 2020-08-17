// See LICENSE.SiFive for license details.

package generatorTests

import java.io.File

import chisel3.aop.injecting.InjectingAspect
import chisel3._
import firrtl.options.TargetDirAnnotation
import freechips.rocketchip.stage.{ConfigsAnnotation, TopModuleAnnotation}
import freechips.rocketchip.system.{RocketChipStage, TestHarness}
import org.scalatest.flatspec.AnyFlatSpec

/** run via SBT with
 *    > testOnly generatorTests.StageGeneratorSpec
 *
 *  Output can be viewed in the testbuild directory. The wire named "hello" should show up in the generated
 *  *.anno.json file.
 */
class StageGeneratorSpec extends AnyFlatSpec {

  val dummyAspect = InjectingAspect(
    {dut: TestHarness => Seq(dut.dut)},
    {dut: freechips.rocketchip.system.ExampleRocketSystemModuleImp[freechips.rocketchip.system.ExampleRocketSystem] =>
      val dummyWire = Wire(UInt(3.W)).suggestName("hello")
      dummyWire := 5.U
      dontTouch(dummyWire)
    }
  )

  "Test" should "pass" in {
    val dirName = System.getProperty("user.dir") + "/testbuild"
    val dir = new File(dirName)
    if (!dir.exists()) dir.mkdirs()

    new RocketChipStage().run(Seq(
      new TargetDirAnnotation(dirName),
      new TopModuleAnnotation(Class.forName("freechips.rocketchip.system.TestHarness")),
      new ConfigsAnnotation(Seq("freechips.rocketchip.system.DefaultConfig")),
      dummyAspect
    ))
  }

}
