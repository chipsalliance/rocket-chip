// See LICENSE for license details.

package rocketchip

import Chisel._
import scala.collection.mutable.LinkedHashSet
import cde.{Parameters, ParameterDump, Config, Collector}

abstract class RocketTestSuite {
  val dir: String
  val makeTargetName: String
  val names: LinkedHashSet[String]
  def postScript = s"""

$$(addprefix $$(output_dir)/, $$(addsuffix .hex, $$($makeTargetName))): $$(output_dir)/%.hex: $dir/%.hex
\tmkdir -p $$(output_dir)
\tln -fs $$< $$@

$$(addprefix $$(output_dir)/, $$($makeTargetName)): $$(output_dir)/%: $dir/%
\tmkdir -p $$(output_dir)
\tln -fs $$< $$@

run-$makeTargetName: $$(addprefix $$(output_dir)/, $$(addsuffix .out, $$($makeTargetName)))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if /\\*{3}(.{8})\\*{3}(.*)/' $$^; echo;

run-$makeTargetName-debug: $$(addprefix $$(output_dir)/, $$(addsuffix .vpd, $$($makeTargetName)))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if /\\*{3}(.{8})\\*{3}(.*)/' $$(patsubst %.vpd,%.out,$$^); echo;
"""
}

class AssemblyTestSuite(makePrefix: String, toolsPrefix: String, val names: LinkedHashSet[String])(envName: String) extends RocketTestSuite {
  val dir = "$(base_dir)/riscv-tools/riscv-tests/isa"
  val makeTargetName = makePrefix + "-" + envName + "-asm-tests"
  override def toString = s"$makeTargetName = \\\n" + names.map(n => s"\t$toolsPrefix-$envName-$n").mkString(" \\\n") + postScript
}

class BenchmarkTestSuite(makePrefix: String, val dir: String, val names: LinkedHashSet[String]) extends RocketTestSuite {
  val makeTargetName = makePrefix + "-bmark-tests"
  override def toString = s"$makeTargetName = \\\n" + names.map(n => s"\t$n.riscv").mkString(" \\\n") + postScript
}

object TestGeneration extends FileSystemUtilities{
  import scala.collection.mutable.HashMap
  val asmSuites = new HashMap[String,AssemblyTestSuite]()
  val postscript = new HashMap[String,String]()
  val bmarkSuites = new  HashMap[String,BenchmarkTestSuite]()

  def addSuite(s: RocketTestSuite) {
    s match {
      case a: AssemblyTestSuite => asmSuites += (a.makeTargetName -> a)
      case b: BenchmarkTestSuite => bmarkSuites += (b.makeTargetName -> b)
    }
  }
  
  def addSuites(s: Seq[RocketTestSuite]) { s.foreach(addSuite) }

  def addVariable(name: String, value: String) { postscript += (name -> value) }

  def generateMakefrag(topModuleName: String, configClassName: String) {
    def gen(kind: String, s: Seq[RocketTestSuite]) = {
      if(s.length > 0) {
        val targets = s.map(t => s"$$(${t.makeTargetName})").mkString(" ") 
        s.map(_.toString).mkString("\n") + s"""
run-$kind-tests: $$(addprefix $$(output_dir)/, $$(addsuffix .out, $targets))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if /\\*{3}(.{8})\\*{3}(.*)/' $$^; echo;
run-$kind-tests-debug: $$(addprefix $$(output_dir)/, $$(addsuffix .vpd, $targets))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if /\\*{3}(.{8})\\*{3}(.*)/' $$(patsubst %.vpd,%.out,$$^); echo;
run-$kind-tests-fast: $$(addprefix $$(output_dir)/, $$(addsuffix .run, $targets))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if /\\*{3}(.{8})\\*{3}(.*)/' $$^; echo;
"""
      } else { "\n" }
    }

    val f = createOutputFile(s"$topModuleName.$configClassName.d")
    f.write(
      List(
        gen("asm", asmSuites.values.toSeq),
        gen("bmark", bmarkSuites.values.toSeq)
      ).mkString("\n") +
      postscript.map(p => p._1 + " = " + p._2).mkString("\n")
    )
    f.close
  }
}

object DefaultTestSuites {
  val rv32uiNames = LinkedHashSet(
    "simple", "add", "addi", "and", "andi", "auipc", "beq", "bge", "bgeu", "blt", "bltu", "bne", "fence_i", 
    "j", "jal", "jalr", "lb", "lbu", "lh", "lhu", "lui", "lw", "or", "ori", "sb", "sh", "sw", "sll", "slli",
    "slt", "slti", "sra", "srai", "srl", "srli", "sub", "xor", "xori")
  val rv32ui = new AssemblyTestSuite("rv32ui", "rv32ui", rv32uiNames)(_)

  val rv32umNames = LinkedHashSet("mul", "mulh", "mulhsu", "mulhu", "div", "divu", "rem", "remu")
  val rv32um = new AssemblyTestSuite("rv32um", "rv32ui", rv32umNames)(_)

  val rv32uaNames = LinkedHashSet("amoadd_w", "amoand_w", "amoor_w", "amoxor_w", "amoswap_w", "amomax_w", "amomaxu_w", "amomin_w", "amominu_w")
  val rv32ua = new AssemblyTestSuite("rv32ua", "rv32ui", rv32uaNames)(_)

  val rv64uiNames = LinkedHashSet("addw", "addiw", "ld", "lwu", "sd", "slliw", "sllw", "sltiu", "sltu", "sraiw", "sraw", "srliw", "srlw", "subw")
  val rv64ui = new AssemblyTestSuite("rv64ui", "rv64ui", rv32uiNames ++ rv64uiNames)(_)

  val rv64umNames = LinkedHashSet("divuw", "divw", "mulw", "remuw", "remw")
  val rv64um = new AssemblyTestSuite("rv64um", "rv64ui", rv32umNames ++ rv64umNames)(_)

  val rv64uaNames = rv32uaNames.map(_.replaceAll("_w","_d"))
  val rv64ua = new AssemblyTestSuite("rv64ua", "rv64ui", rv32uaNames ++ rv64uaNames)(_)

  val rv64ufNames = LinkedHashSet("ldst", "move", "fsgnj", "fcmp", "fcvt", "fcvt_w", "fclass", "fadd", "fdiv", "fmin", "fmadd", "structural")
  val rv64uf = new AssemblyTestSuite("rv64uf", "rv64uf", rv64ufNames)(_)
  val rv64ufNoDiv = new AssemblyTestSuite("rv64uf", "rv64uf", rv64ufNames - "fdiv")(_)

  val rv64siNames = LinkedHashSet("csr", "illegal", "ma_fetch", "ma_addr", "scall", "sbreak", "wfi")
  val rv64si = new AssemblyTestSuite("rv64si", "rv64si", rv64siNames)(_)

  val rv64miNames = LinkedHashSet("csr", "mcsr", "wfi", "dirty", "illegal", "ma_addr", "ma_fetch", "sbreak", "scall", "timer")
  val rv64mi = new AssemblyTestSuite("rv64mi", "rv64mi", rv64miNames)(_)

  // TODO: "rv64ui-pm-lrsc", "rv64mi-pm-ipi",

  val rv64u = List(rv64ui, rv64um, rv64ua)
  val rv64i = rv64u ++ List(rv64si, rv64mi)

  val bmarks = new BenchmarkTestSuite("basic", "$(base_dir)/riscv-tools/riscv-tests/benchmarks", LinkedHashSet(
    "median", "multiply", "qsort", "towers", "vvadd", "dhrystone", "spmv", "mt-vvadd", "mt-matmul", "pb-sgemm", "pb-spmv", "vec-daxpy", "vec-dgemm-opt", "vec-hsaxpy", "vec-hgemm-opt", "vec-hsgemm-opt", "vec-saxpy", "vec-sdaxpy", "vec-sdgemm-opt", "vec-sgemm-naive", "vec-sgemm-opt", "vec-vvadd"))

  val mtBmarks = new BenchmarkTestSuite("mt", "$(base_dir)/riscv-tools/riscv-tests/mt",
    LinkedHashSet(((0 to 4).map("vvadd"+_) ++
    List("ad","ae","af","ag","ai","ak","al","am","an","ap","aq","ar","at","av","ay","az",
         "bb","bc","bf","bh","bj","bk","bm","bo","br","bs","ce","cf","cg","ci","ck","cl",
         "cm","cs","cv","cy","dc","df","dm","do","dr","ds","du","dv").map(_+"_matmul")): _*))

  val zscaleBmarks = new BenchmarkTestSuite("zscale", "$(base_dir)/zscale/sw", LinkedHashSet(
    "led", "mbist"))
}

object TestGenerator extends App with FileSystemUtilities {
  val projectName = args(0)
  val topModuleName = args(1)
  val configClassName = args(2)
  val config = try {
      Class.forName(s"$projectName.$configClassName").newInstance.asInstanceOf[Config]
    } catch {
      case e: java.lang.ClassNotFoundException =>
        throwException(s"Could not find the cde.Config subclass you asked for " +
                        "(i.e. \"$configClassName\"), did you misspell it?", e)
    }
  val world = new Collector(config.topDefinitions,config.knobValues)
  val paramsFromConfig: Parameters = Parameters.root(world)
  config.topConstraints.foreach(c => paramsFromConfig.constrain(c))
  val gen = () => 
    Class.forName(s"$projectName.$topModuleName")
      .getConstructor(classOf[cde.Parameters])
      .newInstance(paramsFromConfig)
      .asInstanceOf[Module]

  chiselMain.run(args.drop(3), gen)
  //Driver.elaborate(gen, configName = configClassName)

  TestGeneration.generateMakefrag(topModuleName, configClassName)
  TestBenchGeneration.generateVerilogFragment(
    topModuleName, configClassName,
    paramsFromConfig(NMemoryChannels))
  TestBenchGeneration.generateCPPFragment(
    topModuleName, configClassName,
    paramsFromConfig(NMemoryChannels))

  val pdFile = createOutputFile(s"$topModuleName.$configClassName.prm")


  pdFile.write(ParameterDump.getDump)
  pdFile.close
  val v = createOutputFile(configClassName + ".knb")
  v.write(world.getKnobs)
  v.close
  val d = new java.io.FileOutputStream(Driver.targetDir + configClassName + ".dtb")
  d.write(paramsFromConfig(DeviceTree))
  d.close
  val w = createOutputFile(configClassName + ".cst")
  w.write(world.getConstraints)
  w.close
}
