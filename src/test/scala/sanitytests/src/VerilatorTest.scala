package sanitytests

import org.scalatest.funsuite.AnyFunSuite

/** software dependencies:
  * clang -> bootrom cross compiling / veriltor C compiling
  * verilator -> emulator generation
  * cmake -> simulation
  * ninja -> fast verilator build tool
  * spike -> isa behavior model linking in emulator
  */
class VerilatorTest extends AnyFunSuite {
  val outputDirectory = os.pwd / "out" / "rocketchip" / "sanitytests" / "VerilatorTest"
  test("build TestHarness emulator to run Hello World") {
    info("Building TestHarness emulator")
    val testHarness = classOf[freechips.rocketchip.system.TestHarness]
    val configs = Seq(classOf[TestConfig], classOf[freechips.rocketchip.system.DefaultConfig])
    val emulator = TestHarness(testHarness, configs, Some(outputDirectory)).emulator
//    val emulator: String = "/home/lucian/repo/playground/out/VerilatorTest/build/emulator"
    info("Running Hello World")
    os.proc(
      s"$emulator",
      s"${resourceOut("riscv64/pk")}",
      "hello",
    ).call(outputDirectory)

  }
}
