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
    "2.12.10"
  }
  override def ammoniteVersion: T[String] = T {
    "2.4.0"
  }

  def hardfloatModule = hardfloatRocket

  def configModule = configRocket
}

object spike extends Module {
  def commit = T { "adfaef00e5cd57bef0aa6a9909b4bff5b3863c40" }

  def src = T.persistent {
    if (!os.exists(T.dest / "riscv-isa-sim")) {
      os.proc("git", "clone", "--depth=1", "https://github.com/riscv-software-src/riscv-isa-sim").call(T.dest)
    }
    os.proc("git", "checkout", "-f", commit()).call(T.dest / "riscv-isa-sim")
    PathRef(T.dest / "riscv-isa-sim")
  }

  def compile = T.persistent {
    os.proc(src().path / "configure", "--prefix", "/usr", "--without-boost", "--without-boost-asio", "--without-boost-regex").call(
      T.ctx.dest, Map(
        "CC" -> "clang",
        "CXX" -> "clang++",
        "AR" -> "llvm-ar",
        "RANLIB" -> "llvm-ranlib",
        "LD" -> "lld",
      )
    )
    os.proc("make", "-j", Runtime.getRuntime().availableProcessors()).call(T.ctx.dest)
    PathRef(T.dest)
  }

  def install = T.persistent {
    os.proc("make", "install", s"DESTDIR=${T.dest}").call(compile().path)
    PathRef(T.dest)
  }
}

trait Emulator extends ScalaModule {
  override def moduleDeps = Seq(rocketchip)

  override def scalaVersion: T[String] = T {
    "2.13.10"
  }

  def top = T { "freechips.rocketchip.system.TestHarness" };
  def config = T { "freechips.rocketchip.system.DefaultConfig" };

  def generator = T {
    mill.modules.Jvm.runSubprocess(
      "freechips.rocketchip.system.Generator",
      runClasspath().map(_.path),
      forkArgs(),
      forkEnv(),
      Seq(
        "-td", T.dest.toString,
        "-T", top(),
        "-C", config(),
      ),
      workingDir = forkWorkingDir()
    )
    PathRef(T.dest)
  }

  def firrtl = T {
    val input = generator().path / (config() + ".fir")
    val output = T.dest / (top() + "-" + config() + ".v")
    mill.modules.Jvm.runSubprocess(
      "firrtl.stage.FirrtlMain",
      runClasspath().map(_.path),
      forkArgs(),
      forkEnv(),
      Seq(
        "-i", input.toString,
        "-o", output.toString,
      ),
      workingDir = forkWorkingDir()
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

    def allVSourceFiles = T {
      Seq(
        "plusarg_reader.v",
        "SimDTM.v",
        "SimJTAG.v",
        "EICG_wrapper.v",
        ).map(v => PathRef(vsrcDir().path / v))
    }

    def CMakeListsString = T {
      // format: off
      s"""cmake_minimum_required(VERSION 3.20)
         |project(emulator)
         |include_directories(${csrcDir().path})
         |# plusarg is here
         |include_directories(${generator().path})
         |link_directories(${spike.install().path / "usr" / "lib"})
         |include_directories(${spike.install().path / "usr" / "include"})
         |
         |set(CMAKE_BUILD_TYPE Release)
         |set(CMAKE_CXX_STANDARD 17)
         |set(CMAKE_C_COMPILER "clang")
         |set(CMAKE_CXX_COMPILER "clang++")
         |set(CMAKE_CXX_FLAGS
         |"$${CMAKE_CXX_FLAGS} -DVERILATOR -DTEST_HARNESS=VTestHarness -include VTestHarness.h -include ${generator().path / config() + ".plusArgs"}")
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
         |${allVSourceFiles().map(_.path).mkString("\n")}
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
        "+define+RANDOMIZE_GARBAGE_ASSIGN",
        "--output-split 20000",
        "--output-split-cfuncs 20000",
        "--max-num-width 1048576"
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

object `emulator-default` extends Emulator {
  override def config = T {
    "freechips.rocketchip.system.DefaultConfig"
  }
}

object `emulator-default32` extends Emulator {
  override def config = T {
    "freechips.rocketchip.system.DefaultRV32Config"
  }
}

// riscv-tests
object `riscv-tests` extends Module {
  def commit = T { "907f1e5b67daedef0acbefb2102b82a23f9abfad" }

  def CC = T {
    "riscv64-none-elf-gcc"
    // FIXME: handle riscv64-unknown-elf-gcc
  }

  def src = T.persistent {
    if (!os.exists(T.dest / "riscv-tests")) {
      os.proc("git", "clone", "--depth=1", "https://github.com/riscv-software-src/riscv-tests").call(T.dest)
    }
    os.proc("git", "checkout", "-f", commit()).call(T.dest / "riscv-tests")
    os.proc("git", "submodule", "update", "--init", "--recursive").call(T.dest / "riscv-tests")
    os.proc("sed", "-i", "s/unknown-elf/none-elf/g", "benchmarks/Makefile").call(T.dest / "riscv-tests")
    os.proc("sed", "-i", "s/unknown-elf/none-elf/g", "isa/Makefile").call(T.dest / "riscv-tests")
    PathRef(T.dest / "riscv-tests")
  }

  def compile = T.persistent {
    os.proc("autoconf").call(src().path)
    os.proc(src().path / "configure").call(
      T.dest, Map(
        "CC" -> "riscv64-none-elf-gcc",
      )
    )
    os.proc("make", "-j", Runtime.getRuntime().availableProcessors(), "isa").call(T.dest)
    PathRef(T.dest)
  }

  def install = T.persistent {
    // TODO: handle benchmark
    os.proc("make", "install", s"DESTDIR=${T.dest}").call(compile().path)
    PathRef(T.dest)
  }

  object cases extends Module {
    trait SuiteCommon extends Module {
      def name: T[String]

      def description: T[String]

      def binaries: T[Seq[PathRef]]
    }

    lazy val allCases = T {
      val root = install().path / "usr" / "local" / "share" / "riscv-tests" / "isa"
      os.walk(root).filterNot(p => p.last.endsWith("dump"))
    }

    trait Suite extends SuiteCommon {
      def name = T {
        millSourcePath.last
      }

      def description = T {
        s"test suite ${name} from riscv-tests"
      }

      def binaries = T {
        allCases().filter(p => p.last.startsWith(name())).map(PathRef(_))
      }
    }

    object `rv32` extends Suite

    object `rv64` extends Suite
  }
}

trait RunableTest extends Module {
  def emulator: Emulator

  def testcases: T[Seq[PathRef]]

  def run = T {
    testcases().map { bin =>
      val name = bin.path.last
      System.out.println(s"Running: ${emulator.elf().path} ${bin.path}")
      val p = os.proc(emulator.elf().path, bin.path).call(stdout = T.dest / s"$name.running.log", mergeErrIntoOut = true)
      PathRef(if(p.exitCode != 0) {
        os.move(T.dest / s"$name.running.log", T.dest / s"$name.failed.log")
        System.err.println(s"Test $name failed with exit code ${p.exitCode}")
        T.dest / s"$name.failed.log"
      } else {
        os.move(T.dest / s"$name.running.log", T.dest / s"$name.passed.log")
        T.dest / s"$name.passed.log"
      })
    }
  }

  def report = T {
    val failed = run().filter(_.path.last.endsWith("failed.log"))
    assert(failed.isEmpty, s"tests failed in ${failed.map(_.path.last).mkString(", ")}")
  }
}

object `emulator-default-rv64` extends RunableTest {
  override def emulator = `emulator-default`
  override def testcases = `riscv-tests`.cases.`rv64`.binaries()
}

object `emulator-default32-rv32` extends RunableTest {
  override def emulator = `emulator-default32`
  override def testcases = `riscv-tests`.cases.`rv32`.binaries()
}

// riscv-arch-test
trait `Arch-test` extends Module {
  def emulator: Emulator

  def spikeISpec: String = "spike_isa.yaml"

  def emulatorISpec: String = "emulator_isa.yaml"

  def spikePSpec: String = "spike_platform.yaml"

  def emulatorPSpec: String = "emulator_platform.yaml"

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
       |ispec=spike/${spikeISpec}
       |pspec=spike/${spikePSpec}
       |target_run=1
       |jobs=${Runtime.getRuntime().availableProcessors()}
       |PATH=${spike.compile().path}
       |
       |[emulator]
       |pluginpath=emulator
       |ispec=emulator/${emulatorISpec}
       |pspec=emulator/${emulatorPSpec}
       |target_run=1
       |jobs=${Runtime.getRuntime().availableProcessors()}
       |PATH=${emulator.elf().path / os.up}
       |""".stripMargin
    // format: on
  }

  def config = T.persistent {
    val path = T.dest / "config.ini"
    os.write.over(path, configString())
    PathRef(T.dest)
  }

  def home = T { config() }

  def src = T {
    // FIXME: pin to specific commit
    if (!os.exists(home().path / "riscv-arch-test")) {
      os.proc("riscof", "--verbose", "info", "arch-test", "--clone").call(home().path)
    }
    PathRef(T.dest)
  }

  def copy = T {
    os.copy.over(os.pwd / "arch-test" / "spike", home().path / "spike")
    os.copy.over(os.pwd / "arch-test" / "emulator", home().path / "emulator")
  }

  def run = T {
    src()
    copy()
    os.proc("riscof", "run",
      s"--config=${config().path / "config.ini"}",
      "--suite=riscv-arch-test/riscv-test-suite/",
      "--env=riscv-arch-test/riscv-test-suite/env"
    ).call(home().path)
  }
}

object `emulator-default-arch-test-rv64i` extends `Arch-test` {
  override def emulator = `emulator-default`
  override def spikeISpec: String = "spike_isa_rv64i.yaml"
  override def emulatorISpec: String = "emulator_isa_rv64i.yaml"
}

object `emulator-default32-arch-test-rv32i` extends `Arch-test` {
  override def emulator = `emulator-default32`
  override def spikeISpec: String = "spike_isa_rv32i.yaml"
  override def emulatorISpec: String = "emulator_isa_rv32i.yaml"
}
