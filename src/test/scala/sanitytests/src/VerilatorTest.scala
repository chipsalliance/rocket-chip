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
  test("build TestHarness emulator") {
    val testHarness = classOf[freechips.rocketchip.system.TestHarness]
    val configs = Seq(classOf[TestConfig], classOf[freechips.rocketchip.system.DefaultConfig])
    val emulator = TestHarness(testHarness, configs, Some(outputDirectory)).emulator
    test("Hello World!") {
      os.proc(
        s"$emulator",
        s"${resource("riscv64/pk")}",
        "hello",
      ).call(outputDirectory)
    }
  }
}
