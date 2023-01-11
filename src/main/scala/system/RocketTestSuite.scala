// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.system

import scala.collection.mutable.LinkedHashSet

abstract class RocketTestSuite {
  val dir: String
  val makeTargetName: String
  val names: LinkedHashSet[String]
  val envName: String
  def kind: String
  def postScript = s"""

$$(addprefix $$(output_dir)/, $$(addsuffix .hex, $$($makeTargetName))): $$(output_dir)/%.hex: $dir/%.hex
\tmkdir -p $$(output_dir)
\tln -fs $$< $$@

$$(addprefix $$(output_dir)/, $$($makeTargetName)): $$(output_dir)/%: $dir/%
\tmkdir -p $$(output_dir)
\tln -fs $$< $$@

run-$makeTargetName: $$(addprefix $$(output_dir)/, $$(addsuffix .out, $$($makeTargetName)))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if( /\\*{3}(.{8})\\*{3}(.*)/ || /ASSERTION (FAILED):(.*)/i )' $$^ /dev/null | perl -pe 'BEGIN { $$$$failed = 0 } $$$$failed = 1 if(/FAILED/i); END { exit($$$$failed) }'

run-$makeTargetName-debug: $$(addprefix $$(output_dir)/, $$(addsuffix .vpd, $$($makeTargetName)))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if( /\\*{3}(.{8})\\*{3}(.*)/ || /ASSERTION (FAILED):(.*)/i )' $$(patsubst %.vpd,%.out,$$^) /dev/null | perl -pe 'BEGIN { $$$$failed = 0 } $$$$failed = 1 if(/FAILED/i); END { exit($$$$failed) }'

run-$makeTargetName-fst: $$(addprefix $$(output_dir)/, $$(addsuffix .fst, $$($makeTargetName)))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if( /\\*{3}(.{8})\\*{3}(.*)/ || /ASSERTION (FAILED):(.*)/i )' $$(patsubst %.fst,%.out,$$^) /dev/null | perl -pe 'BEGIN { $$$$failed = 0 } $$$$failed = 1 if(/FAILED/i); END { exit($$$$failed) }'
"""
}

class AssemblyTestSuite(prefix: String, val names: LinkedHashSet[String])(val envName: String) extends RocketTestSuite {
  val dir = "$(RISCV)/riscv64-unknown-elf/share/riscv-tests/isa"
  val makeTargetName = prefix + "-" + envName + "-asm-tests"
  def kind = "asm"
  override def toString = s"$makeTargetName = \\\n" + names.map(n => s"\t$prefix-$envName-$n").mkString(" \\\n") + postScript
}

class BenchmarkTestSuite(makePrefix: String, val dir: String, val names: LinkedHashSet[String]) extends RocketTestSuite {
  val envName = ""
  val makeTargetName = makePrefix + "-bmark-tests"
  def kind = "bmark"
  override def toString = s"$makeTargetName = \\\n" + names.map(n => s"\t$n.riscv").mkString(" \\\n") + postScript
}

class RegressionTestSuite(val names: LinkedHashSet[String]) extends RocketTestSuite {
  val envName = ""
  val dir = "$(RISCV)/riscv64-unknown-elf/share/riscv-tests/isa"
  val makeTargetName = "regression-tests"
  def kind = "regression"
  override def toString = s"$makeTargetName = \\\n" + names.mkString(" \\\n")
}

object TestGeneration {
  private val suites = collection.mutable.ListMap[String, RocketTestSuite]()

  def addSuite(s: RocketTestSuite): Unit = { suites += (s.makeTargetName -> s) }
  
  def addSuites(s: Seq[RocketTestSuite]): Unit = { s.foreach(addSuite) }

  private[rocketchip] def gen(kind: String, s: Seq[RocketTestSuite]) = {
    if(s.length > 0) {
      val envs = s.groupBy(_.envName)
      val targets = s.map(t => s"$$(${t.makeTargetName})").mkString(" ")
      s.map(_.toString).mkString("\n") +
      envs.filterKeys(_ != "").map( {
                                     case (env,envsuites) => {
                                       val suites = envsuites.map(t => s"$$(${t.makeTargetName})").mkString(" ")
                                       s"""
run-$kind-$env-tests: $$(addprefix $$(output_dir)/, $$(addsuffix .out, $suites))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if( /\\*{3}(.{8})\\*{3}(.*)/ || /ASSERTION (FAILED):(.*)/i )' $$^ /dev/null | perl -pe 'BEGIN { $$$$failed = 0 } $$$$failed = 1 if(/FAILED/i); END { exit($$$$failed) }'
run-$kind-$env-tests-debug: $$(addprefix $$(output_dir)/, $$(addsuffix .vpd, $suites))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if( /\\*{3}(.{8})\\*{3}(.*)/ || /ASSERTION (FAILED):(.*)/i )' $$(patsubst %.vpd,%.out,$$^) /dev/null | perl -pe 'BEGIN { $$$$failed = 0 } $$$$failed = 1 if(/FAILED/i); END { exit($$$$failed) }'
run-$kind-$env-tests-fst: $$(addprefix $$(output_dir)/, $$(addsuffix .fst, $suites))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if( /\\*{3}(.{8})\\*{3}(.*)/ || /ASSERTION (FAILED):(.*)/i )' $$(patsubst %.fst,%.out,$$^) /dev/null | perl -pe 'BEGIN { $$$$failed = 0 } $$$$failed = 1 if(/FAILED/i); END { exit($$$$failed) }'
run-$kind-$env-tests-fast: $$(addprefix $$(output_dir)/, $$(addsuffix .run, $suites))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if( /\\*{3}(.{8})\\*{3}(.*)/ || /ASSERTION (FAILED):(.*)/i )' $$^ /dev/null | perl -pe 'BEGIN { $$$$failed = 0 } $$$$failed = 1 if(/FAILED/i); END { exit($$$$failed) }'
"""} } ).mkString("\n") + s"""
run-$kind-tests: $$(addprefix $$(output_dir)/, $$(addsuffix .out, $targets))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if( /\\*{3}(.{8})\\*{3}(.*)/ || /ASSERTION (FAILED):(.*)/i )' $$^ /dev/null | perl -pe 'BEGIN { $$$$failed = 0 } $$$$failed = 1 if(/FAILED/i); END { exit($$$$failed) }'
run-$kind-tests-debug: $$(addprefix $$(output_dir)/, $$(addsuffix .vpd, $targets))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if( /\\*{3}(.{8})\\*{3}(.*)/ || /ASSERTION (FAILED):(.*)/i )' $$(patsubst %.vpd,%.out,$$^) /dev/null | perl -pe 'BEGIN { $$$$failed = 0 } $$$$failed = 1 if(/FAILED/i); END { exit($$$$failed) }'
run-$kind-tests-fst: $$(addprefix $$(output_dir)/, $$(addsuffix .fst, $targets))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if( /\\*{3}(.{8})\\*{3}(.*)/ || /ASSERTION (FAILED):(.*)/i )' $$(patsubst %.fst,%.out,$$^) /dev/null | perl -pe 'BEGIN { $$$$failed = 0 } $$$$failed = 1 if(/FAILED/i); END { exit($$$$failed) }'
run-$kind-tests-fast: $$(addprefix $$(output_dir)/, $$(addsuffix .run, $targets))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if( /\\*{3}(.{8})\\*{3}(.*)/ || /ASSERTION (FAILED):(.*)/i )' $$^ /dev/null | perl -pe 'BEGIN { $$$$failed = 0 } $$$$failed = 1 if(/FAILED/i); END { exit($$$$failed) }'
"""
    } else { "\n" }
  }

  def generateMakeFrag: String = {
    suites.values.toSeq.groupBy(_.kind).map { case (kind, s) => gen(kind, s) }.mkString("\n")
  }

}

object DefaultTestSuites {
  val rv32uiNames = LinkedHashSet(
    "simple", "add", "addi", "and", "andi", "auipc", "beq", "bge", "bgeu", "blt", "bltu", "bne", "fence_i",
    "jal", "jalr", "lb", "lbu", "lh", "lhu", "lui", "lw", "or", "ori", "sb", "sh", "sw", "sll", "slli",
    "slt", "slti", "sra", "srai", "srl", "srli", "sub", "xor", "xori")
  val rv32ui = new AssemblyTestSuite("rv32ui", rv32uiNames)(_)

  val rv32ucNames = LinkedHashSet("rvc")
  val rv32uc = new AssemblyTestSuite("rv32uc", rv32ucNames)(_)

  val rv32umNames = LinkedHashSet("mul", "mulh", "mulhsu", "mulhu", "div", "divu", "rem", "remu")
  val rv32um = new AssemblyTestSuite("rv32um", rv32umNames)(_)

  val rv32uaSansLRSCNames = LinkedHashSet("amoadd_w", "amoand_w", "amoor_w", "amoxor_w", "amoswap_w", "amomax_w", "amomaxu_w", "amomin_w", "amominu_w")
  val rv32uaSansLRSC = new AssemblyTestSuite("rv32ua", rv32uaSansLRSCNames)(_)

  val rv32uaNames = rv32uaSansLRSCNames + "lrsc"
  val rv32ua = new AssemblyTestSuite("rv32ua", rv32uaNames)(_)

  val rv32siNames = LinkedHashSet("csr", "ma_fetch", "scall", "sbreak", "wfi", "dirty")
  val rv32si = new AssemblyTestSuite("rv32si", rv32siNames)(_)

  val rv32miNames = LinkedHashSet("csr", "mcsr", "illegal", "ma_addr", "ma_fetch", "sbreak", "scall", "breakpoint", "lh-misaligned", "lw-misaligned", "shamt", "sh-misaligned", "sw-misaligned", "zicntr")
  val rv32mi = new AssemblyTestSuite("rv32mi", rv32miNames)(_)

  val rv32u = List(rv32ui, rv32um)
  val rv32i = List(rv32ui, rv32si, rv32mi)
  val rv32pi = List(rv32ui, rv32mi)

  val rv64uiNames = LinkedHashSet("addw", "addiw", "ld", "lwu", "sd", "slliw", "sllw", "sltiu", "sltu", "sraiw", "sraw", "srliw", "srlw", "subw")
  val rv64ui = new AssemblyTestSuite("rv64ui", rv32uiNames ++ rv64uiNames)(_)

  val rv64uiMaDataNames = LinkedHashSet("ma_data")
  val rv64uiMaData = new AssemblyTestSuite("rv64ui", rv64uiMaDataNames)(_)

  val rv64umNames = LinkedHashSet("divuw", "divw", "mulw", "remuw", "remw")
  val rv64um = new AssemblyTestSuite("rv64um", rv32umNames ++ rv64umNames)(_)

  val rv64uaSansLRSCNames = rv32uaSansLRSCNames.map(_.replaceAll("_w","_d"))
  val rv64uaSansLRSC = new AssemblyTestSuite("rv64ua", rv32uaSansLRSCNames ++ rv64uaSansLRSCNames)(_)

  val rv64uaNames = rv64uaSansLRSCNames + "lrsc"
  val rv64ua = new AssemblyTestSuite("rv64ua", rv32uaNames ++ rv64uaNames)(_)

  val rv64ucNames = rv32ucNames
  val rv64uc = new AssemblyTestSuite("rv64uc", rv64ucNames)(_)

  val rv64ufNames = LinkedHashSet("ldst", "move", "fcmp", "fcvt", "fcvt_w", "fclass", "fadd", "fdiv", "fmin", "fmadd", "recoding")
  val rv64uf = new AssemblyTestSuite("rv64uf", rv64ufNames)(_)

  val rv32uf = new AssemblyTestSuite("rv32uf", rv64ufNames)(_)
  val rv32ud = new AssemblyTestSuite("rv32ud", rv64ufNames - "move")(_)

  val rv64udNames = rv64ufNames + "structural"
  val rv64ud = new AssemblyTestSuite("rv64ud", rv64udNames)(_)

  val rv32uzfhNames = rv64ufNames
  val rv32uzfh = new AssemblyTestSuite("rv32uzfh", rv32uzfhNames)(_)

  val rv64uzfhNames = rv64ufNames
  val rv64uzfh = new AssemblyTestSuite("rv64uzfh", rv64uzfhNames)(_)


  val rv64siNames = rv32siNames
  val rv64si = new AssemblyTestSuite("rv64si", rv64siNames)(_)

  val rv64miNames = rv32miNames + "breakpoint" + "access" + "icache-alias"
  val rv64mi = new AssemblyTestSuite("rv64mi", rv64miNames)(_)

  val groundtestNames = LinkedHashSet("simple")
  val groundtest64 = new AssemblyTestSuite("rv64ui", groundtestNames)(_)
  val groundtest32 = new AssemblyTestSuite("rv32ui", groundtestNames)(_)

  val rv64u = List(rv64ui, rv64um)
  val rv64i = List(rv64ui, rv64si, rv64mi)
  val rv64pi = List(rv64ui, rv64mi)

  val benchmarks = new BenchmarkTestSuite("rvi", "$(RISCV)/riscv64-unknown-elf/share/riscv-tests/benchmarks", LinkedHashSet(
    "median", "multiply", "qsort", "rsort", "pmp", "towers", "vvadd", "dhrystone", "mt-matmul"))

  val rv32udBenchmarks = new BenchmarkTestSuite("rvd", "$(RISCV)/riscv64-unknown-elf/share/riscv-tests/benchmarks", LinkedHashSet(
    "mm", "spmv", "mt-vvadd"))

  val emptyBmarks = new BenchmarkTestSuite("empty",
    "$(RISCV)/riscv64-unknown-elf/share/riscv-tests/benchmarks", LinkedHashSet.empty)

  val singleRegression = new RegressionTestSuite(LinkedHashSet("rv64ui-p-simple"))
}
