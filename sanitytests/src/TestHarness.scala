package sanitytests

import chipsalliance.rocketchip.config.Config
import chisel3.RawModule
import firrtl.AnnotationSeq
import firrtl.options.TargetDirAnnotation
import firrtl.passes.memlib.{InferReadWriteAnnotation, GenVerilogMemBehaviorModelAnno}
import firrtl.stage.{FirrtlStage, OutputFileAnnotation, RunFirrtlTransformAnnotation}
import freechips.rocketchip.stage._
import freechips.rocketchip.system.RocketChipStage
import logger.LazyLogging
import os._

case class TestHarness[M <: RawModule](
  testHarness: Class[M],
  configs:     Seq[Class[_ <: Config]],
  targetDir:   Option[Path] = None) extends LazyLogging {
  /** compile [[testHarness]] with correspond [[configs]] to emulator.
    * return emulator [[Path]].
    */
  lazy val emulator: Path = {
    val outputDirectory: Path = targetDir.getOrElse(os.temp.dir(deleteOnExit = false))
    logger.warn(s"start to compile emulator in $outputDirectory")
    val annotations: AnnotationSeq = Seq(
      new RocketChipStage,
      new FirrtlStage
    ).foldLeft(
      AnnotationSeq(
        Seq(
          TargetDirAnnotation(outputDirectory.toString),
          new TopModuleAnnotation(testHarness),
          new ConfigsAnnotation(configs.map(_.getName)),
          InferReadWriteAnnotation,
          GenVerilogMemBehaviorModelAnno(false),
          RunFirrtlTransformAnnotation(new firrtl.passes.InlineInstances),
          new OutputBaseNameAnnotation("TestHarness")
        )
      )
    ) { case (annos, stage) => stage.transform(annos) }
    logger.warn(s"$testHarness with configs: ${configs.mkString("_")} generated.")
    val duts = annotations.collect {
      case OutputFileAnnotation(file) => outputDirectory / s"$file.v"
    }
    val blackbox =
      os.read.lines(outputDirectory / firrtl.transforms.BlackBoxSourceHelper.defaultFileListName).map(Path(_))
    val verilatorBuildDir = outputDirectory / "build"
    val cmakefilelist = verilatorBuildDir / "CMakeLists.txt"
    os.makeDir(verilatorBuildDir)
    val verilatorArgs = Seq(
      // format: off
      "-Wno-UNOPTTHREADS", "-Wno-STMTDLY", "-Wno-LATCH",
      "--x-assign unique",
      """+define+PRINTF_COND=\$c\(\"verbose\",\"&&\",\"done_reset\"\)""",
      """+define+STOP_COND=\$c\(\"done_reset\"\)""",
      "+define+RANDOMIZE_GARBAGE_ASSIGN",
      "--output-split 20000",
      "--output-split-cfuncs 20000",
      "--max-num-width 1048576"
      // format: on
    ).mkString(" ")
    val csrcs = Seq("csrc/emulator.cc", "csrc/SimDTM.cc", "csrc/SimJTAG.cc", "csrc/remote_bitbang.cc")
      .map(resource(_).toString)
      .mkString(" ")
    val vsrcs = (duts ++ blackbox)
      .filter(f => f.ext == "v" | f.ext == "sv")
      .map(_.toString)
      .mkString(" ")
    logger.warn("works fine util here")
    os.write(
      cmakefilelist,
      // format: off
      s"""cmake_minimum_required(VERSION 3.20)
         |project(emulator)
         |include_directories(${resource("usr/include")})
         |link_directories(${resource("usr/lib")})
         |find_package(verilator)
         |add_executable(emulator $csrcs)
         |set(CMAKE_C_COMPILER "clang")
         |set(CMAKE_CXX_COMPILER "clang++")
         |set(CMAKE_CXX_FLAGS "$${CMAKE_CXX_FLAGS} -DVERILATOR -DTEST_HARNESS=VTestHarness -std=c++11 -include ${resource("csrc/verilator.h")} -include ${outputDirectory / "TestHarness.plusArgs"} -include VTestHarness.h")
         |set(THREADS_PREFER_PTHREAD_FLAG ON)
         |find_package(Threads)
         |target_link_libraries(emulator PRIVATE $${CMAKE_THREAD_LIBS_INIT})
         |target_link_libraries(emulator PRIVATE fesvr)
         |verilate(emulator
         |  SOURCES $vsrcs
         |  TOP_MODULE TestHarness
         |  PREFIX VTestHarness
         |  VERILATOR_ARGS $verilatorArgs
         |)
         |""".stripMargin
      // format: on
    )
    logger.warn(s"compiling DUT with CMake:\n" + os.read(cmakefilelist))
    logger.warn(s"start to compile Verilog to C++")
    os.proc(
      // format: off
      "cmake",
      "-G", "Ninja",
      verilatorBuildDir.toString
      // format: on
    ).call(verilatorBuildDir)
    logger.warn(s"start to compile C++ to emulator")
    os.proc(
      // format: off
      "ninja"
      // format: on
    ).call(verilatorBuildDir)
    val emulatorBinary = verilatorBuildDir / "emulator"
    logger.warn(s"emulator location: $emulatorBinary")
    emulatorBinary
  }
}
