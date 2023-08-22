import mill._
import mill.scalalib._
import mill.scalalib.publish._
import coursier.maven.MavenRepository
import $file.hardfloat.common
import $file.cde.common
import $file.common

object v {
  val scala = "2.13.10"
  // the first version in this Map is the mainly supported version which will be used to run tests
  val chiselCrossVersions = Map(
    "3.6.0" -> (ivy"edu.berkeley.cs::chisel3:3.6.0", ivy"edu.berkeley.cs:::chisel3-plugin:3.6.0"),
    "5.0.0" -> (ivy"org.chipsalliance::chisel:5.0.0", ivy"org.chipsalliance:::chisel-plugin:5.0.0"),
  )
  val mainargs = ivy"com.lihaoyi::mainargs:0.5.0"
  val json4sJackson = ivy"org.json4s::json4s-jackson:4.0.5"
  val scalaReflect = ivy"org.scala-lang:scala-reflect:${scala}"
}

object macros extends Macros

trait Macros
  extends millbuild.common.MacrosModule
    with RocketChipPublishModule
    with SbtModule {

  def scalaVersion: T[String] = T(v.scala)

  def scalaReflectIvy = v.scalaReflect
}

object hardfloat extends mill.define.Cross[Hardfloat](v.chiselCrossVersions.keys.toSeq)

trait Hardfloat
  extends millbuild.hardfloat.common.HardfloatModule
    with RocketChipPublishModule
    with Cross.Module[String] {

  def scalaVersion: T[String] = T(v.scala)

  override def millSourcePath = os.pwd / "hardfloat" / "hardfloat"

  def chiselModule = None

  def chiselPluginJar = None

  def chiselIvy = Some(v.chiselCrossVersions(crossValue)._1)

  def chiselPluginIvy = Some(v.chiselCrossVersions(crossValue)._2)
}

object cde extends CDE

trait CDE
  extends millbuild.cde.common.CDEModule
    with RocketChipPublishModule
    with ScalaModule {

  def scalaVersion: T[String] = T(v.scala)

  override def millSourcePath = os.pwd / "cde" / "cde"
}

object rocketchip extends Cross[RocketChip](v.chiselCrossVersions.keys.toSeq)

trait RocketChip
  extends millbuild.common.RocketChipModule
    with RocketChipPublishModule
    with SbtModule
    with Cross.Module[String] {
  def scalaVersion: T[String] = T(v.scala)

  override def millSourcePath = super.millSourcePath / os.up

  def chiselModule = None

  def chiselPluginJar = None

  def chiselIvy = Some(v.chiselCrossVersions(crossValue)._1)

  def chiselPluginIvy = Some(v.chiselCrossVersions(crossValue)._2)

  def macrosModule = macros

  def hardfloatModule = hardfloat(crossValue)

  def cdeModule = cde

  def mainargsIvy = v.mainargs

  def json4sJacksonIvy = v.json4sJackson
}

trait RocketChipPublishModule
  extends PublishModule {
  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "org.chipsalliance",
    url = "http://github.com/chipsalliance/rocket-chip",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("chipsalliance", "rocket-chip"),
    developers = Seq(
      Developer("aswaterman", "Andrew Waterman", "https://aspire.eecs.berkeley.edu/author/waterman/")
    )
  )

  override def publishVersion: T[String] = T("1.6-SNAPSHOT")
}


// Tests
trait Emulator extends Cross.Module2[String, String] {
  val top: String = crossValue
  val config: String = crossValue2

  object generator extends Module {
    def elaborate = T {
      os.proc(
        mill.util.Jvm.javaExe,
        "-jar",
        rocketchip(v.chiselCrossVersions.keys.head).assembly().path,
        "--dir", T.dest.toString,
        "--top", top,
        config.split('_').flatMap(c => Seq("--config", c)),
      ).call()
      PathRef(T.dest)
    }

    def chiselAnno = T {
      os.walk(elaborate().path).collectFirst { case p if p.last.endsWith("anno.json") => p }.map(PathRef(_)).get
    }

    def chirrtl = T {
      os.walk(elaborate().path).collectFirst { case p if p.last.endsWith("fir") => p }.map(PathRef(_)).get
    }
  }

  object mfccompiler extends Module {
    def compile = T {
      os.proc("firtool",
        generator.chirrtl().path,
        s"--annotation-file=${generator.chiselAnno().path}",
        "-disable-infer-rw",
        "--disable-annotation-unknown",
        "-dedup",
        "-O=debug",
        "--split-verilog",
        "--preserve-values=named",
        "--output-annotation-file=mfc.anno.json",
        s"-o=${T.dest}"
      ).call(T.dest)
      PathRef(T.dest)
    }

    def rtls = T {
      os.read(compile().path / "filelist.f").split("\n").map(str =>
        try {
          os.Path(str)
        } catch {
          case e: IllegalArgumentException if e.getMessage.contains("is not an absolute path") =>
            compile().path / str.stripPrefix("./")
        }
      ).filter(p => p.ext == "v" || p.ext == "sv").map(PathRef(_)).toSeq
    }
  }

  object verilator extends Module {
    def spikeRoot = T {
      envByNameOrRiscv("SPIKE_ROOT")
    }

    def csrcDir = T {
      PathRef(os.pwd / "src" / "main" / "resources" / "csrc")
    }

    def vsrcDir = T {
      PathRef(os.pwd / "src" / "main" / "resources" / "vsrc")
    }

    def allCSourceFiles = T {
      Seq(
        "SimDTM.cc",
        "SimJTAG.cc",
        "debug_rob.cc",
        "emulator.cc",
        "remote_bitbang.cc",
      ).map(c => PathRef(csrcDir().path / c))
    }

    def CMakeListsString = T {
      // format: off
      s"""cmake_minimum_required(VERSION 3.20)
         |project(emulator)
         |include_directories(${csrcDir().path})
         |# plusarg is here
         |include_directories(${generator.elaborate().path})
         |link_directories(${spikeRoot() + "/lib"})
         |include_directories(${spikeRoot() + "/include"})
         |
         |set(CMAKE_BUILD_TYPE Release)
         |set(CMAKE_CXX_STANDARD 17)
         |set(CMAKE_C_COMPILER "clang")
         |set(CMAKE_CXX_COMPILER "clang++")
         |set(CMAKE_CXX_FLAGS
         |"$${CMAKE_CXX_FLAGS} -DVERILATOR -DTEST_HARNESS=VTestHarness -include VTestHarness.h -include verilator.h -include ${generator.elaborate().path / config + ".plusArgs"}")
         |set(THREADS_PREFER_PTHREAD_FLAG ON)
         |
         |find_package(verilator)
         |find_package(Threads)
         |
         |add_executable(emulator
         |${allCSourceFiles().map(_.path).mkString("\n")}
         |)
         |
         |target_link_libraries(emulator PRIVATE $${CMAKE_THREAD_LIBS_INIT})
         |target_link_libraries(emulator PRIVATE fesvr)
         |verilate(emulator
         |  SOURCES
         |  ${mfccompiler.rtls().map(_.path.toString).mkString("\n")}
         |  TOP_MODULE TestHarness
         |  PREFIX VTestHarness
         |  VERILATOR_ARGS ${verilatorArgs().mkString(" ")}
         |)
         |""".stripMargin
      // format: on
    }

    def verilatorArgs = T.input {
      Seq(
        // format: off
        "-Wno-UNOPTTHREADS", "-Wno-STMTDLY", "-Wno-LATCH", "-Wno-WIDTH", "--no-timing",
        "--x-assign unique",
        """+define+PRINTF_COND=\$c\(\"verbose\",\"&&\",\"done_reset\"\)""",
        """+define+STOP_COND=\$c\(\"done_reset\"\)""",
        "+define+RANDOMIZE_GARBAGE_ASSIGN",
        "--output-split 20000",
        "--output-split-cfuncs 20000",
        "--max-num-width 1048576",
        s"-I${vsrcDir().path}",
        // format: on
      )
    }

    def cmakefileLists = T.persistent {
      val path = T.dest / "CMakeLists.txt"
      os.write.over(path, CMakeListsString())
      PathRef(T.dest)
    }

    def elf = T.persistent {
      mill.util.Jvm.runSubprocess(Seq("cmake", "-G", "Ninja", "-S", cmakefileLists().path, "-B", T.dest.toString).map(_.toString), Map[String, String](), T.dest)
      mill.util.Jvm.runSubprocess(Seq("ninja", "-C", T.dest).map(_.toString), Map[String, String](), T.dest)
      PathRef(T.dest / "emulator")
    }
  }

  def elf = T {
    verilator.elf()
  }
}

/** object to elaborate verilated emulators. */
object emulator extends Cross[Emulator](
  // RocketSuiteA
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig"),
  // RocketSuiteB
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig"),
  // RocketSuiteC
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.TinyConfig"),
  // Unittest
  ("freechips.rocketchip.unittest.TestHarness", "freechips.rocketchip.unittest.AMBAUnitTestConfig"),
  ("freechips.rocketchip.unittest.TestHarness", "freechips.rocketchip.unittest.TLSimpleUnitTestConfig"),
  ("freechips.rocketchip.unittest.TestHarness", "freechips.rocketchip.unittest.TLWidthUnitTestConfig"),
  // DTM
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.WithJtagDTMSystem_freechips.rocketchip.system.WithDebugSBASystem_freechips.rocketchip.system.DefaultConfig"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.WithJtagDTMSystem_freechips.rocketchip.system.WithDebugSBASystem_freechips.rocketchip.system.DefaultRV32Config"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.WithJtagDTMSystem_freechips.rocketchip.system.DefaultConfig"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.WithJtagDTMSystem_freechips.rocketchip.system.DefaultRV32Config"),
  // Miscellaneous
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultSmallConfig"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DualBankConfig"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DualChannelConfig"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DualChannelDualBankConfig"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.RoccExampleConfig"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.Edge128BitConfig"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.Edge32BitConfig"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.QuadChannelBenchmarkConfig"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.EightChannelConfig"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DualCoreConfig"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.MemPortOnlyConfig"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.MMIOPortOnlyConfig"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.CloneTileConfig"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.HypervisorConfig"),
  //
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultFP16Config"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.BitManipCryptoConfig"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.BitManipCrypto32Config"),
)

object `runnable-riscv-test` extends mill.Cross[RiscvTest](
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "rv64mi-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "rv64mi-p-ld", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "rv64mi-p-lh", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "rv64mi-p-lw", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "rv64mi-p-sd", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "rv64mi-p-sh", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "rv64mi-p-sw", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "rv64si-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "rv64si-p-icache", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "rv64ua-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "rv64ua-v", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "rv64uc-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "rv64uc-v", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "rv64ud-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "rv64ud-v", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "rv64uf-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "rv64uf-v", "none"),
  // https://github.com/riscv-software-src/riscv-tests/issues/419
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "rv64ui-p", "ma_data"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "rv64ui-v", "ma_data"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "rv64um-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "rv64um-v", "none"),

  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig", "rv64mi-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig", "rv64mi-p-ld", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig", "rv64mi-p-lh", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig", "rv64mi-p-lw", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig", "rv64mi-p-sd", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig", "rv64mi-p-sh", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig", "rv64mi-p-sw", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig", "rv64si-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig", "rv64si-p-icache", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig", "rv64ua-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig", "rv64ua-v", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig", "rv64uc-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig", "rv64uc-v", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig", "rv64ud-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig", "rv64ud-v", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig", "rv64uf-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig", "rv64uf-v", "none"),
  // https://github.com/riscv-software-src/riscv-tests/issues/419
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig", "rv64ui-p", "ma_data"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig", "rv64ui-v", "ma_data"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig", "rv64um-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig", "rv64um-v", "none"),

  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config", "rv32mi-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config", "rv32mi-p-lh", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config", "rv32mi-p-lw", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config", "rv32mi-p-sh", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config", "rv32mi-p-sw", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config", "rv32si-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config", "rv32ua-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config", "rv32ua-v", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config", "rv32uc-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config", "rv32uc-v", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config", "rv32uf-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config", "rv32uf-v", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config", "rv32ui-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config", "rv32ui-v", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config", "rv32um-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config", "rv32um-v", "none"),

  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.TinyConfig", "rv32mi-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.TinyConfig", "rv32mi-p-lh", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.TinyConfig", "rv32mi-p-lw", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.TinyConfig", "rv32mi-p-sh", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.TinyConfig", "rv32mi-p-sw", "none"),
  // lsrc is not implemented if usingDataScratchpad
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.TinyConfig", "rv32ua-p", "lrsc"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.TinyConfig", "rv32uc-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.TinyConfig", "rv32ui-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.TinyConfig", "rv32um-p", "none"),

  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultFP16Config", "rv64uzfh-p", "none"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultFP16Config", "rv64uzfh-v", "none"),
)

object `runnable-arch-test` extends mill.Cross[ArchTest](
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "64", "RV64IMAFDCZicsr_Zifencei"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config", "32", "RV32IMAFCZicsr_Zifencei"),
  // For CI within reasonable time
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "64", "RV64IMACZicsr_Zifencei"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config", "32", "RV32IMACZicsr_Zifencei"),

  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.BitManipCryptoConfig", "64", "RV64IZba_Zbb_Zbc_Zbkb_Zbkc_Zbkx_Zbs_Zknd_Zkne_Zknh_Zksed_Zksh"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.BitManipCrypto32Config", "32", "RV32IZba_Zbb_Zbc_Zbkb_Zbkc_Zbkx_Zbs_Zknd_Zkne_Zknh_Zksed_Zksh"),
)

object `runnable-jtag-dtm-test` extends mill.Cross[JTAGDTMTest](
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.WithJtagDTMSystem_freechips.rocketchip.system.DefaultConfig", "off", "64", "DebugTest"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.WithJtagDTMSystem_freechips.rocketchip.system.DefaultConfig", "off", "64", "MemTest64"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.WithJtagDTMSystem_freechips.rocketchip.system.DefaultRV32Config", "off", "32", "DebugTest"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.WithJtagDTMSystem_freechips.rocketchip.system.DefaultRV32Config", "off", "32", "MemTest64"),
  // SBA
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.WithJtagDTMSystem_freechips.rocketchip.system.WithDebugSBASystem_freechips.rocketchip.system.DefaultConfig", "on", "64", "MemTest64"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.WithJtagDTMSystem_freechips.rocketchip.system.WithDebugSBASystem_freechips.rocketchip.system.DefaultConfig", "on", "64", "MemTest32"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.WithJtagDTMSystem_freechips.rocketchip.system.WithDebugSBASystem_freechips.rocketchip.system.DefaultRV32Config", "on", "32", "MemTest64"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.WithJtagDTMSystem_freechips.rocketchip.system.WithDebugSBASystem_freechips.rocketchip.system.DefaultRV32Config", "on", "32", "MemTest32"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.WithJtagDTMSystem_freechips.rocketchip.system.WithDebugSBASystem_freechips.rocketchip.system.DefaultRV32Config", "on", "32", "MemTest8"),
)

// TODO: split below into another file.
def envByNameOrRiscv(name: String): String = {
  sys.env.get(name) match {
    case Some(value) => value
    // TODO: if not found, give a warning
    case None => sys.env("RISCV")
  }
}

object `riscv-tests` extends Module {
  def testsRoot =
    os.Path(envByNameOrRiscv("RISCV_TESTS_ROOT")) / "riscv64-unknown-elf" / "share" / "riscv-tests"

  def allCases = T {
    os.walk(testsRoot).filterNot(p => p.last.endsWith("dump"))
  }

  object suite extends Cross[Suite](
    os.walk(testsRoot).map(_.last).filterNot(_.endsWith("dump")).map(_.split('-').dropRight(1).mkString("-")).filter(_ != "").toSet.toSeq.sorted
  )

  trait Suite extends Cross.Module[String] {
    val name: String = crossValue

    def description = T {
      s"test suite ${name} from riscv-tests"
    }

    def binaries = T {
      allCases().filter(p => p.last.startsWith(name)).map(PathRef(_))
    }
  }
}

// exclude defaults to "none" instead of "" because it is a file name
trait RiscvTest extends Cross.Module4[String, String, String, String] {
  val top: String = crossValue
  val config: String = crossValue2
  val suiteName: String = crossValue3
  val exclude: String = crossValue4

  def run = T {
    `riscv-tests`.suite(suiteName).binaries().map { bin =>
      val name = bin.path.last
      val toExclude = exclude.split("-").map(suiteName + "-" + _).exists(_ == name)
      if (toExclude) {
        PathRef(T.dest)
      } else {
        System.out.println(s"Running: ${emulator(top, config).elf().path} ${bin.path}")
        val p = os.proc(emulator(top, config).elf().path, bin.path).call(stdout = T.dest / s"$name.running.log", mergeErrIntoOut = true, check = false)
        PathRef(if (p.exitCode != 0) {
          os.move(T.dest / s"$name.running.log", T.dest / s"$name.failed.log")
          throw new Exception(s"Test $name failed with exit code ${p.exitCode}")
          T.dest / s"$name.failed.log"
        } else {
          os.move(T.dest / s"$name.running.log", T.dest / s"$name.passed.log")
          T.dest / s"$name.passed.log"
        })
      }
    }
  }
}

trait ArchTest extends Cross.Module4[String, String, String, String] {
  val top: String = crossValue
  val config: String = crossValue2
  val xlen: String = crossValue3
  val isa: String = crossValue4

  def ispecString = T {
    // format: off
    s"""hart_ids: [0]
       |hart0:
       |  ISA: ${isa}
       |  physical_addr_sz: 32
       |  User_Spec_Version: '2.3'
       |  supported_xlen: [${xlen}]
       |""".stripMargin
    // format: on
  }

  def pspecString = T {
    // format: off
    s"""mtime:
       |  implemented: true
       |  address: 0xbff8
       |mtimecmp:
       |  implemented: true
       |  address: 0x4000
       |nmi:
       |  label: nmi_vector
       |reset:
       |  label: reset_vector
       |""".stripMargin
    // format: on
  }

  def spikeRoot = T {
    envByNameOrRiscv("SPIKE_ROOT")
  }

  def CC = T {
    sys.env.get("RV64_TOOLCHAIN_ROOT") match {
      case Some(value) => value + "/bin/riscv64-none-elf-gcc" // nix uses a different name
      case None => sys.env("RISCV") + "/bin/riscv64-unknown-elf-gcc" // if not found, throws NoSuchElementException exception
    }
  }

  def configString = T {
    // format: off
    s"""[RISCOF]
       |ReferencePlugin=spike
       |ReferencePluginPath=spike
       |DUTPlugin=emulator
       |DUTPluginPath=emulator
       |
       |[spike]
       |pluginpath=spike
       |ispec=${ispecYaml().path}
       |pspec=${pspecYaml().path}
       |target_run=1
       |jobs=${Runtime.getRuntime().availableProcessors()}
       |PATH=${spikeRoot() + "/bin"}
       |CC=${CC()}
       |
       |[emulator]
       |pluginpath=emulator
       |ispec=${ispecYaml().path}
       |pspec=${pspecYaml().path}
       |target_run=1
       |jobs=${Runtime.getRuntime().availableProcessors()}
       |PATH=${emulator(top, config).elf().path / os.up}
       |CC=${CC()}
       |""".stripMargin
    // format: on
  }

  def ispecYaml = T.persistent {
    val path = T.dest / "ispec.yaml"
    os.write.over(path, ispecString())
    PathRef(path)
  }

  def pspecYaml = T.persistent {
    val path = T.dest / "pspec.yaml"
    os.write.over(path, pspecString())
    PathRef(path)
  }

  def configIni = T.persistent {
    val path = T.dest / "config.ini"
    os.write.over(path, configString())
    PathRef(T.dest)
  }

  def home = T {
    configIni()
  }

  def src = T {
    if (!os.exists(home().path / "riscv-arch-test")) {
      os.proc("riscof", "--verbose", "info", "arch-test", "--clone").call(home().path)
    }
    PathRef(T.dest)
  }

  def copy = T {
    os.copy.over(os.pwd / "scripts" / "arch-test" / "spike", home().path / "spike")
    os.copy.over(os.pwd / "scripts" / "arch-test" / "emulator", home().path / "emulator")
  }

  def run = T {
    src()
    copy()
    os.proc("riscof", "run", "--no-browser",
      s"--config=${configIni().path / "config.ini"}",
      "--suite=riscv-arch-test/riscv-test-suite/",
      "--env=riscv-arch-test/riscv-test-suite/env"
    ).call(home().path)
    val reportFile = configIni().path / "riscof_work" / "report.html"
    val report = os.read(reportFile)
    if (report.contains("0Failed")) {
      System.out.println(s"Arch Test $top $config $xlen $isa Succeeded")
    } else {
      throw new Exception(s"Arch Test $top $config $xlen $isa Failed")
    }
  }
}

trait JTAGDTMTest extends Cross.Module5[String, String, String, String, String] {
  val top: String = crossValue
  val config: String = crossValue2
  val sba: String = crossValue3
  val xlen: String = crossValue4
  val name: String = crossValue5

  def run = T {
    val gdbserver = os.Path(sys.env.get("RISCV_TESTS_ROOT").get) / "debug" / "gdbserver.py"
    val p = os.proc(
      gdbserver,
      "--print-failures",
      "--print-log-names",
      s"--sim_cmd=${emulator(top, config).elf().path} +jtag_rbb_enable=1 dummybin",
      "--server_cmd=openocd",
      "--gdb=riscv64-none-elf-gdb",
      s"--${xlen}",
      s"./scripts/RocketSim${xlen}.py",
      name,
    ).call(
      env = Map(
        "TERM" -> "", // otherwise readline issues on bracketed-paste
        "JTAG_DTM_ENABLE_SBA" -> sba,
      ),
      stdout = T.dest / s"$name.running.log",
      mergeErrIntoOut = true,
      check = false)
    PathRef(if (p.exitCode != 0) {
      os.move(T.dest / s"$name.running.log", T.dest / s"$name.failed.log")
      throw new Exception(s"Test $name failed with exit code ${p.exitCode}")
      T.dest / s"$name.failed.log"
    } else {
      os.move(T.dest / s"$name.running.log", T.dest / s"$name.passed.log")
      T.dest / s"$name.passed.log"
    })
  }
}
