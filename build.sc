import mill._
import mill.scalalib._
import mill.scalalib.publish._
import mill.modules.Util
import mill.define.{Sources, TaskModule}
import mill.scalalib.scalafmt._
import coursier.maven.MavenRepository
import $file.hardfloat.build
import $file.cde.common
import $file.common

object cdeRocket extends cde.common.CDEModule with PublishModule {
  override def millSourcePath = os.pwd / "cde" / "cde"

  override def scalaVersion = T {
    rocketchip.scalaVersion()
  }

  override def pomSettings = T {
    rocketchip.pomSettings()
  }

  override def publishVersion = T {
    rocketchip.publishVersion()
  }
}

object hardfloatRocket extends hardfloat.build.hardfloat {
  override def millSourcePath = os.pwd / "hardfloat"

  override def scalaVersion = T {
    rocketchip.scalaVersion()
  }

  // use same chisel version with RocketChip
  def chisel3IvyDeps = if(chisel3Module.isEmpty) Agg(
    common.getVersion("chisel3")
  ) else Agg.empty[Dep]

  def chisel3PluginIvyDeps = if(chisel3Module.isEmpty) Agg(
    common.getVersion("chisel3-plugin", cross = true)
  ) else Agg.empty[Dep]

  override def repositories = super.repositories ++ Seq(
    MavenRepository("https://oss.sonatype.org/content/repositories/snapshots"),
    MavenRepository("https://oss.sonatype.org/content/repositories/releases")
  )
}

object rocketchip extends common.CommonRocketChip {
  m =>
  override def scalaVersion: T[String] = T {
    "2.13.10"
  }
  override def ammoniteVersion: T[String] = T {
    "2.4.0"
  }

  def hardfloatModule = hardfloatRocket

  def cdeModule = cdeRocket
}

def envByNameOrRiscv(name: String): String = {
  sys.env.get(name) match {
    case Some(value) => value
    case None => sys.env("RISCV") // if not found, throws NoSuchElementException exception
  }
}

object emulator extends mill.Cross[Emulator](
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultBufferlessConfig"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.TinyConfig"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultFP16Config"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.BitManipCryptoConfig"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.BitManipCrypto32Config"),
  // Misc
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
)
class Emulator(top: String, config: String) extends Module {

  object generator extends Module {
    def elaborate = T {
      os.proc(
        mill.modules.Jvm.javaExe,
        "-jar",
        rocketchip.assembly().path,
        "freechips.rocketchip.system.Generator",
        "-td", T.dest.toString,
        "-T", top,
        "-C", config,
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
    def spikeRoot = T { envByNameOrRiscv("SPIKE_ROOT") }

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
        "-Wno-UNOPTTHREADS", "-Wno-STMTDLY", "-Wno-LATCH", "-Wno-WIDTH",
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
      mill.modules.Jvm.runSubprocess(Seq("cmake", "-G", "Ninja", "-S", cmakefileLists().path, "-B", T.dest.toString).map(_.toString), Map[String, String](), T.dest)
      mill.modules.Jvm.runSubprocess(Seq("ninja", "-C", T.dest).map(_.toString), Map[String, String](), T.dest)
      PathRef(T.dest / "emulator")
    }
  }

  def elf = T {
    verilator.elf()
  }
}

object `riscv-tests` extends Module {
  def testsRoot =
    os.Path(envByNameOrRiscv("RISCV_TESTS_ROOT")) / "riscv64-unknown-elf" / "share" / "riscv-tests"

  def allCases = T {
    os.walk(testsRoot).filterNot(p => p.last.endsWith("dump"))
  }

  object suite extends mill.Cross[Suite](
    os.walk(testsRoot).map(_.last).filterNot(_.endsWith("dump")).map(_.split('-').dropRight(1).mkString("-")).toSet.toSeq.sorted: _*
  )

  class Suite(name: String) extends Module {

    def description = T {
      s"test suite ${name} from riscv-tests"
    }

    def binaries = T {
      allCases().filter(p => p.last.startsWith(name)).map(PathRef(_))
    }
  }
}

object `runnable-test` extends mill.Cross[RunableTest](
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
// exclude defaults to "none" instead of "" because it is a file name
class RunableTest(top: String, config: String, suiteName: String, exclude: String) extends Module {
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

object `runnable-arch-test` extends mill.Cross[ArchTest](
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "64", "RV64IMAFDCZicsr_Zifencei"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config", "32", "RV32IMAFCZicsr_Zifencei"),
  // For CI within reasonable time
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultConfig", "64", "RV64IMACZicsr_Zifencei"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config", "32", "RV32IMACZicsr_Zifencei"),

  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.BitManipCryptoConfig", "64", "RV64IZba_Zbb_Zbc_Zbkb_Zbkc_Zbkx_Zbs_Zknd_Zkne_Zknh_Zksed_Zksh"),
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.BitManipCrypto32Config", "32", "RV32IZba_Zbb_Zbc_Zbkb_Zbkc_Zbkx_Zbs_Zknd_Zkne_Zknh_Zksed_Zksh"),
)
class ArchTest(top: String, config: String, xlen: String, isa: String) extends Module {
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

  def spikeRoot = T { envByNameOrRiscv("SPIKE_ROOT") }

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

  def home = T { configIni() }

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

object v {
  val scala = "2.12.16"
  val chisel3 = ivy"edu.berkeley.cs::chisel3:3.6-SNAPSHOT"
  val chisel3Plugin = ivy"edu.berkeley.cs::chisel3-plugin:3.6-SNAPSHOT"
  val chiseltest = ivy"edu.berkeley.cs::chiseltest:3.6-SNAPSHOT"
  val utest = ivy"com.lihaoyi::utest:latest.integration"
  val macroParadise = ivy"org.scalamacros:::paradise:2.1.1"
  val mainargs = ivy"com.lihaoyi::mainargs:0.3.0"
}
object tests extends Module{
  object cosim extends Module {
    object elaborate extends ScalaModule with ScalafmtModule {
      def scalaVersion = T {
        v.scala
      }

      //override def moduleDeps = Seq(diplomatic)

//      override def scalacOptions = T {
//        Seq("-Xsource:2.11", s"-Xplugin:${mychisel3.plugin.jar().path}")
//      }

      override def ivyDeps = Agg(
        v.mainargs
      )

      def elaborate = T.persistent {
        mill.modules.Jvm.runSubprocess(
          finalMainClass(),
          runClasspath().map(_.path),
          forkArgs(),
          forkEnv(),
          Seq(
            "--dir", T.dest.toString,
          ),
          workingDir = forkWorkingDir()
        )
        PathRef(T.dest)
      }

      def rtls = T.persistent {
        os.read(elaborate().path / "filelist.f").split("\n").map(str =>
          try {
            os.Path(str)
          } catch {
            case e: IllegalArgumentException if e.getMessage.contains("is not an absolute path") =>
              elaborate().path / str
          }
        ).filter(p => p.ext == "v" || p.ext == "sv").map(PathRef(_)).toSeq
      }

      def annos = T.persistent {
        os.walk(elaborate().path).filter(p => p.last.endsWith("anno.json")).map(PathRef(_))
      }
    }

    object myelaborate extends ScalaModule with ScalafmtModule {
      def scalaVersion = T {
        v.scala
      }

      //override def moduleDeps = Seq(diplomatic)

      //      override def scalacOptions = T {
      //        Seq("-Xsource:2.11", s"-Xplugin:${mychisel3.plugin.jar().path}")
      //      }

      override def ivyDeps = Agg(
        v.mainargs
      )

      def elaborate = T {
        mill.modules.Jvm.runSubprocess(
          finalMainClass(),
          runClasspath().map(_.path),
          forkArgs(),
          forkEnv(),
          Seq(
            "--dir", T.dest.toString,
            "--top", "tests.cosim.elaborate.DUT",
            "--config", "tests.cosim.elaborate.cosimConfig"
          ),
          workingDir = forkWorkingDir(),
        )
        PathRef(T.dest)
      }

      def chiselAnno = T {
        os.walk(elaborate().path).collectFirst { case p if p.last.endsWith("anno.json") => p }.map(PathRef(_)).get
      }

      def chirrtl = T {
        os.walk(elaborate().path).collectFirst { case p if p.last.endsWith("fir") => p }.map(PathRef(_)).get
      }

      def firtool = T {
        os.proc("firtool",
          chirrtl().path,
          s"--annotation-file=${chiselAnno().path}",
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
        val verilogs = os.read(firtool().path / "filelist.f").split("\n").map(str =>
          try {
            os.Path(str)
          } catch {
            case e: IllegalArgumentException if e.getMessage.contains("is not an absolute path") =>
              firtool().path / str.stripPrefix("./")
          }
        ).filter(p => p.ext == "v" || p.ext == "sv").map(PathRef(_)).toSeq
        T.log.info(s"RTL generated:\n${verilogs.map(_.path).mkString("\n")}")
        verilogs
      }

      def annotations = T {
        os.walk(firtool().path).filter(p => p.last.endsWith("mfc.anno.json")).map(PathRef(_))
      }
    }

    /** build emulator */
    object emulator extends Module {

      def csources = T.source {
        millSourcePath / "src"
      }

      def csrcDir = T {
        PathRef(millSourcePath / "src")
      }

      def vsrcs = T.persistent {
        elaborate.rtls().filter(p => p.path.ext == "v" || p.path.ext == "sv")
      }

      def allCSourceFiles = T {
        Lib.findSourceFiles(Seq(csrcDir()), Seq("S", "s", "c", "cpp", "cc")).map(PathRef(_))
      }

      val topName = "V"

      def verilatorConfig = T {
        val traceConfigPath = T.dest / "verilator.vlt"
        os.write(
          traceConfigPath,
          "`verilator_config\n" +
            ujson.read(cosim.elaborate.annos().collectFirst(f => os.read(f.path)).get).arr.flatMap {
              case anno if anno("class").str == "chisel3.experimental.Trace$TraceAnnotation" =>
                Some(anno("target").str)
              case _ => None
            }.toSet.map { t: String =>
              val s = t.split('|').last.split("/").last
              val M = s.split(">").head.split(":").last
              val S = s.split(">").last
              s"""//$t\npublic_flat_rd -module "$M" -var "$S""""
            }.mkString("\n")
        )
        PathRef(traceConfigPath)
      }

      def CMakeListsString = T {
        // format: off
        s"""cmake_minimum_required(VERSION 3.20)
           |set(CMAKE_CXX_STANDARD 17)
           |set(CMAKE_CXX_COMPILER_ID "clang")
           |set(CMAKE_C_COMPILER "clang")
           |set(CMAKE_CXX_COMPILER "clang++")
           |
           |project(emulator)
           |
           |find_package(args REQUIRED)
           |find_package(glog REQUIRED)
           |find_package(fmt REQUIRED)
           |find_package(libspike REQUIRED)
           |find_package(verilator REQUIRED)
           |find_package(Threads REQUIRED)
           |set(THREADS_PREFER_PTHREAD_FLAG ON)
           |
           |set(CMAKE_CXX_FLAGS "$${CMAKE_CXX_FLAGS} -DVERILATOR")
           |
           |add_executable(${topName}
           |${allCSourceFiles().map(_.path).mkString("\n")}
           |)
           |
           |target_include_directories(${topName} PUBLIC ${csources().path.toString})
           |
           |target_link_libraries(${topName} PUBLIC $${CMAKE_THREAD_LIBS_INIT})
           |target_link_libraries(${topName} PUBLIC libspike fmt glog)  # note that libargs is header only, nothing to link
           |
           |verilate(${topName}
           |  SOURCES
           |${vsrcs().map(_.path).mkString("\n")}
           |${verilatorConfig().path.toString}
           |  TRACE_FST
           |  TOP_MODULE DUT
           |  PREFIX V${topName}
           |  OPT_FAST
           |  THREADS 8
           |  VERILATOR_ARGS ${verilatorArgs().mkString(" ")}
           |)
           |""".stripMargin
        // format: on
      }

      def verilatorArgs = T.input {
        Seq(
          // format: off
          "-Wno-UNOPTTHREADS", "-Wno-STMTDLY", "-Wno-LATCH", "-Wno-WIDTH",
          "--x-assign unique",
          "+define+RANDOMIZE_GARBAGE_ASSIGN",
          "--output-split 20000",
          "--output-split-cfuncs 20000",
          "--max-num-width 1048576",
          "--vpi"
          // format: on
        )
      }

      def elf = T.persistent {
        val path = T.dest / "CMakeLists.txt"
        os.write.over(path, CMakeListsString())
        T.log.info(s"CMake project generated in $path,\nverilating...")
        os.proc(
          // format: off
          "cmake",
          "-G", "Ninja",
          T.dest.toString
          // format: on
        ).call(T.dest)
        T.log.info("compile rtl to emulator...")
        os.proc(
          // format: off
          "ninja"
          // format: on
        ).call(T.dest)
        val elf = T.dest / topName
        T.log.info(s"verilated exe generated: ${elf.toString}")
        PathRef(elf)
      }
    }
  }

  /** all the cases: entrance, smoketest and riscv-tests */
  object cases extends Module {
    trait Case extends Module {
      def name: T[String] = millSourcePath.last

      def sources = T.sources {
        millSourcePath
      }

      def allSourceFiles = T {
        Lib.findSourceFiles(sources(), Seq("S", "s", "c", "cpp")).map(PathRef(_))
      }

      def linkScript: T[PathRef] = T {
        os.write(T.ctx.dest / "linker.ld",
          s"""
             |SECTIONS
             |{
             |  . = 0x1000;
             |  .text.start : { *(.text.start) }
             |}
             |""".stripMargin)
        PathRef(T.ctx.dest / "linker.ld")
      }

      def compile: T[PathRef] = T {
        os.proc(Seq("clang-rv64", "-o", name() + ".elf", "--target=riscv64", "-march=rv64gc", "-mno-relax", s"-T${linkScript().path}") ++ allSourceFiles().map(_.path.toString)).call(T.ctx.dest)
        os.proc(Seq("llvm-objcopy", "-O", "binary", "--only-section=.text", name() + ".elf", name())).call(T.ctx.dest)
        T.log.info(s"${name()} is generated in ${T.dest},\n")
        PathRef(T.ctx.dest / name())
      }
    }

    object smoketest extends Case {
      override def linkScript: T[PathRef] = T {
        os.write(T.ctx.dest / "linker.ld",
          s"""
             |SECTIONS
             |{
             |  . = 0x80000000;
             |  .text.start : { *(.text.start) }
             |}
             |""".stripMargin)
        PathRef(T.ctx.dest / "linker.ld")
      }
    }

    object entrance extends Case

    object riscvtests extends Module {

      c =>
      trait Suite extends Module {
        def name: T[String]

        def description: T[String]

        def binaries: T[Seq[PathRef]]
      }

      object test extends Module {
        trait Suite extends c.Suite {
          def name = T {
            millSourcePath.last
          }

          def description = T {
            s"test suite ${name} from riscv-tests"
          }

          def target = T.persistent {
            os.walk(untar().path).filter(p => p.last.startsWith(name())).filterNot(p => p.last.endsWith("dump")).map(PathRef(_))
          }

          def init = T.persistent {
            target().map(bin => {
              os.proc("cp", bin.path, "./" + bin.path.last + ".elf").call(T.dest)
              os.proc("llvm-objcopy", "-O", "binary", bin.path.last + ".elf", bin.path.last).call(T.dest)
            })
            PathRef(T.dest)
          }

          def test = T {
            println("why")

            target.map(a =>
              println("hello"))
            PathRef(T.dest)
          }

          def binaries = T {
            os.walk(init().path).filter(p => p.last.startsWith(name())).filterNot(p => p.last.endsWith("elf")).map(PathRef(_))
          }
        }

        def commit = T.input {
          "047314c5b0525b86f7d5bb6ffe608f7a8b33ffdb"
        }

        def tgz = T.persistent {
          Util.download(s"https://github.com/ZenithalHourlyRate/riscv-tests-release/releases/download/tag-${commit()}/riscv-tests.tgz")
        }

        def untar = T.persistent {
          mill.modules.Jvm.runSubprocess(Seq("tar", "xzf", tgz().path).map(_.toString), Map[String, String](), T.dest)
          PathRef(T.dest)
        }

        object `rv32mi-p` extends Suite

        object `rv32mi-p-lh` extends Suite

        object `rv32mi-p-lw` extends Suite

        object `rv32mi-p-sh` extends Suite

        object `rv32mi-p-sw` extends Suite

        object `rv32si-p` extends Suite

        object `rv32ua-p` extends Suite

        object `rv32ua-v` extends Suite

        object `rv32uc-p` extends Suite

        object `rv32uc-v` extends Suite

        object `rv32ud-p` extends Suite

        object `rv32ud-v` extends Suite

        object `rv32uf-p` extends Suite

        object `rv32uf-v` extends Suite

        object `rv32ui-p` extends Suite

        object `rv32ui-v` extends Suite

        object `rv32um-p` extends Suite

        object `rv32um-v` extends Suite

        object `rv32uzfh-p` extends Suite

        object `rv32uzfh-v` extends Suite

        object `rv64mi-p` extends Suite

        object `rv64mi-p-ld` extends Suite

        object `rv64mi-p-lh` extends Suite

        object `rv64mi-p-lw` extends Suite

        object `rv64mi-p-sd` extends Suite

        object `rv64mi-p-sh` extends Suite

        object `rv64mi-p-sw` extends Suite

        object `rv64mzicbo-p` extends Suite

        object `rv64si-p` extends Suite

        object `rv64si-p-icache` extends Suite

        object `rv64ssvnapot-p` extends Suite

        object `rv64ua-p` extends Suite

        object `rv64ua-v` extends Suite

        object `rv64uc-p` extends Suite

        object `rv64uc-v` extends Suite

        object `rv64ud-p` extends Suite

        object `rv64ud-v` extends Suite

        object `rv64uf-p` extends Suite

        object `rv64uf-v` extends Suite

        object `rv64ui-p` extends Suite

        object `rv64ui-v` extends Suite

        object `rv64um-p` extends Suite

        object `rv64um-v` extends Suite

        object `rv64uzfh-p` extends Suite

        object `rv64uzfh-v` extends Suite

        object `rv64` extends Suite {
          override def binaries = T {
            os.walk(init().path).filter(p => p.last.startsWith(name())).filterNot(p => p.last.endsWith("elf")).filterNot(p => p.last.endsWith("rv64mi-p-csr")).filterNot(p => p.last.endsWith("rv64mi-p-breakpoint")).filterNot(p => p.last.startsWith("rv64um")).map(PathRef(_))
          }
        }
      }
    }
  }

  object tests extends Module() {
    object smoketest extends Module {
      trait Test extends TaskModule {
        override def defaultCommandName() = "run"

        def bin: cases.Case

        def run(args: String*) = T.command {
          val proc = os.proc(Seq(cosim.emulator.elf().path.toString(), "--entrance", cases.entrance.compile().path.toString(), "--bin", bin.compile().path.toString, "--wave", (T.dest / "wave").toString) ++ args)
          T.log.info(s"run test: ${bin.name} with:\n ${proc.command.map(_.value.mkString(" ")).mkString(" ")}")
          proc.call()
          PathRef(T.dest)
        }
      }

      object smoketest extends Test {
        def bin = cases.smoketest
      }

    }

    object riscvtests extends Module {

      trait Test extends TaskModule {
        override def defaultCommandName() = "run"

        def bin: T[Seq[PathRef]]

        def run(args: String*) = T.command {
          bin().map { c =>
            val name = c.path.last
            val proc = os.proc(Seq(cosim.emulator.elf().path.toString(), "--entrance", cases.entrance.compile().path.toString(), "--bin", c.path.toString, "--wave", (T.dest / "wave").toString) ++ args)
            T.log.info(s"run test: ${c.path.last} with:\n ${proc.command.map(_.value.mkString(" ")).mkString(" ")}")
            val p = proc.call(stdout = T.dest / s"$name.running.log", mergeErrIntoOut = true)

            PathRef(if (p.exitCode != 0) {
              os.move(T.dest / s"$name.running.log", T.dest / s"$name.failed.log")
              System.err.println(s"Test $name failed with exit code ${p.exitCode}")
              T.dest / s"$name.failed.log"
            } else {
              os.move(T.dest / s"$name.running.log", T.dest / s"$name.passed.log")
              T.dest / s"$name.passed.log"
            })
          }
        }

      }

      object smoketest extends Test {
        def bin = Seq(cases.smoketest.compile())
      }

      object `rv64` extends Test {
        def bin = cases.riscvtests.test.`rv64`.binaries
      }

      object `rv64si-p` extends Test {
        def bin = cases.riscvtests.test.`rv64si-p`.binaries
      }


      object `rv64mi-p` extends Test {
        def bin = cases.riscvtests.test.`rv64mi-p`.binaries
      }

      object `rv64ua-p` extends Test {
        def bin = cases.riscvtests.test.`rv64ua-p`.binaries
      }

      object `rv64ua-v` extends Test {
        def bin = cases.riscvtests.test.`rv64ua-v`.binaries
      }

      object `rv64uc-p` extends Test {
        def bin = cases.riscvtests.test.`rv64uc-p`.binaries
      }

      object `rv64uc-v` extends Test {
        def bin = cases.riscvtests.test.`rv64uc-v`.binaries
      }

      object `rv64ud-p` extends Test {
        def bin = cases.riscvtests.test.`rv64ud-p`.binaries
      }

      object `rv64ud-v` extends Test {
        def bin = cases.riscvtests.test.`rv64ud-v`.binaries
      }

      object `rv64uf-p` extends Test {
        def bin = cases.riscvtests.test.`rv64uf-p`.binaries
      }

      object `rv64uf-v` extends Test {
        def bin = cases.riscvtests.test.`rv64uf-v`.binaries
      }

      object `rv64ui-p` extends Test {
        def bin = cases.riscvtests.test.`rv64ui-p`.binaries
      }

      object `rv64ui-v` extends Test {
        def bin = cases.riscvtests.test.`rv64ui-v`.binaries
      }

      object `rv64uzfh-p` extends Test {
        def bin = cases.riscvtests.test.`rv64uzfh-p`.binaries
      }

      object `rv64uzfh-v` extends Test {
        def bin = cases.riscvtests.test.`rv64uzfh-p`.binaries
      }

      object `rv64um-p` extends Test {
        def bin = cases.riscvtests.test.`rv64um-p`.binaries
      }

      object `rv64um-v` extends Test {
        def bin = cases.riscvtests.test.`rv64um-v`.binaries
      }

    }

  }
}
