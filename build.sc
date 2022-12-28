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
  ("freechips.rocketchip.system.TestHarness", "freechips.rocketchip.system.DefaultRV32Config")
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
         |link_directories(${spikeRoot() + "/lib"})
         |include_directories(${spikeRoot() + "/include"})
         |
         |set(CMAKE_BUILD_TYPE Release)
         |set(CMAKE_CXX_STANDARD 17)
         |set(CMAKE_C_COMPILER "clang")
         |set(CMAKE_CXX_COMPILER "clang++")
         |set(CMAKE_CXX_FLAGS
         |"$${CMAKE_CXX_FLAGS} -DVERILATOR -DTEST_HARNESS=VTestHarness -include VTestHarness.h -include ${generator().path / config + ".plusArgs"}")
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
