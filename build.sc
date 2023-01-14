import mill._
import mill.scalalib._
import mill.scalalib.publish._
import coursier.maven.MavenRepository
import $file.common
import $file.hardfloat.build
import $file.`api-config-chipsalliance`.`build-rules`.mill.build

object configRocket extends `api-config-chipsalliance`.`build-rules`.mill.build.config with PublishModule {
  override def millSourcePath = os.pwd / "api-config-chipsalliance" / "design" / "craft"

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

  def configModule = configRocket
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
)
class Emulator(top: String, config: String) extends ScalaModule {
  override def moduleDeps = Seq(rocketchip)

  override def scalaVersion: T[String] = T {
    "2.13.10"
  }

  def spikeRoot = T { envByNameOrRiscv("SPIKE_ROOT") }

  def generator = T {
    // class path for `moduleDeps` is only a directory, not a jar, which breaks the cache.
    // so we need to manually add the class files of `moduleDeps` here.
    upstreamCompileOutput()
    mill.modules.Jvm.runLocal(
      "freechips.rocketchip.system.Generator",
      runClasspath().map(_.path),
      Seq(
        "-td", T.dest.toString,
        "-T", top,
        "-C", config,
      ),
    )
    PathRef(T.dest)
  }

  def firrtl = T {
    val input = generator().path / (config + ".fir")
    val output = T.dest / (top + "-" + config + ".v")
    mill.modules.Jvm.runLocal(
      "firrtl.stage.FirrtlMain",
      runClasspath().map(_.path),
      Seq(
        "-i", input.toString,
        "-o", output.toString,
      ),
    )
    PathRef(output)
  }

  object verilator extends Module {
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
         |include_directories(${generator().path})
         |link_directories(${spikeRoot() + "/lib"})
         |include_directories(${spikeRoot() + "/include"})
         |
         |set(CMAKE_BUILD_TYPE Release)
         |set(CMAKE_CXX_STANDARD 17)
         |set(CMAKE_C_COMPILER "clang")
         |set(CMAKE_CXX_COMPILER "clang++")
         |set(CMAKE_CXX_FLAGS
         |"$${CMAKE_CXX_FLAGS} -DVERILATOR -DTEST_HARNESS=VTestHarness -include VTestHarness.h -include verilator.h -include ${generator().path / config + ".plusArgs"}")
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
         |${firrtl().path}
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
