package sanitytests

import utest._

/** software dependencies:
  * clang -> bootrom cross compiling / veriltor C compiling
  * verilator -> emulator generation
  * cmake -> simulation
  * ninja -> fast verilator build tool
  * spike -> isa behavior model linking in emulator
  */
object VerilatorTest extends TestSuite {
  val outputDirectory = os.pwd / "out" / "VerilatorTest"
  os.remove.all(outputDirectory)
  os.makeDir(outputDirectory)
  val tests = Tests {
    test("build TestHarness emulator") {
      val testHarness = classOf[freechips.rocketchip.system.TestHarness]
      val configs = Seq(classOf[TestConfig], classOf[freechips.rocketchip.system.DefaultConfig])
      val emulator = TestHarness(testHarness, configs, Some(outputDirectory)).emulator
      test("build hello") {
        os.proc(
          "clang",
          "-o", "hello",
          s"${resource("csrc/hello.c")}",
          "--target=riscv64",
          "-mno-relax",
          "-nostdinc",
          s"-I${resource("riscv64/usr/include")}",
          "-fuse-ld=lld",
          "-nostdlib",
          s"${resource("riscv64/usr/lib/crt1.o")}",
          s"${resource("riscv64/usr/lib/crti.o")}",
          s"${resource("riscv64/usr/lib/riscv64/libclang_rt.builtins-riscv64.a")}",
          s"${resource("riscv64/usr/lib/libc.a")}",
          s"${resource("riscv64/usr/lib/crtn.o")}",
          "-static",
        ).call(outputDirectory)
        test("Hello World!") {
          os.proc(
            s"$emulator",
            s"${resource("riscv64/pk")}",
            "hello",
          ).call(outputDirectory)
        }
      }
    }
  }
}
