package rocketchip

import Chisel._
import uncore._
import rocket._
import hwacha._

object HwachaTestSuites {
  import DefaultTestSuites._
  val rv64uvNames = Set(
    "wakeup", "fence", "vsetcfgi", "vsetcfg", "vsetvl", "keepcfg",
    "vmsa", "vmss", "vssd", "vssw", "vssh", "vssb",
    "vlsd", "vlsw", "vlswu", "vlsh", "vlshu", "vlsb", "vlsbu", 
    "vsad", "vsaw", "vsah", "vsab", "vlad", "vlaw", "vlawu", "vlah", "vlahu", "vlab", "vlabu",
    "vld", "vlw", "vlwu", "vlh", "vlhu", "vlb", "vlbu", "vlxd", "vlxw", "vlxwu", "vlxh", "vlxhu", "vlxb", "vlxbu", 
    "vsd", "vsw", "vsh", "vsb", "vsxd", "vsxw", "vsxh", "vsxb",
    "eidx", "imul", "fcvt", "vvadd_d", "vvadd_w", "vvadd_fd", "vvadd_fw", "vvmul_d",
    "overlap", "sched_sreg_xbar", "sched_fadd", "sched_waw", "sched_war") ++
    rv32uaNames ++ rv64uaNames 
  val rv64uvBasic = new AssemblyTestSuite("rv64uv", "rv64uv", rv64uvNames)(_)
  
  val rv64uiVecNames = Set(
    "addi", "add", "addiw", "addw", "and", "andi", "div", "divu", "divuw", "divw", 
    "mul", "mulw", "mulh", "mulhu", "mulhsu", "or", "ori", "rem", "remu", "remuw", "remw",
    "sll", "slli", "slliw", "sllw", "slt", "slti", "sltiu", "sltu",
    "sra", "srai", "sraiw", "sraw", "srl", "srli", "srliw", "srlw", "sub", "subw", "xor", "xori").map("vec-" + _)
  val rv64uiVec = new AssemblyTestSuite("rv64ui-vec", "rv64ui", rv64uiVecNames)(_)

  val rv64uiScalarVecNames = rv64uiVecNames.map("s"+_)
  val rv64uiScalarVecNamesV4 = rv64uiScalarVecNames -- rv64umNames.map("svec-" + _) --
  Set("svec-div", "svec-divu", "svec-divuw", "svec-divw", "svec-mul", "svec-mulw", "svec-mulh", "svec-mulhu", "svec-mulhsu",
      "svec-rem", "svec-remu", "svec-remuw", "svec-remw")
  val rv64uiScalerVec = new AssemblyTestSuite("rv64ui-svec", "rv64ui", rv64uiScalarVecNamesV4)(_)

  val rv64ufVecNames = Set(
    "fadd", "fcmp", "fdiv", "fclass", "fcvt", "fcvt_w", "fmadd", "fmin", "fsgnj").map("vec-" + _)
  val rv64ufVecNamesV4 = rv64ufVecNames -- Set("vec-fcmp", "vec-fcvt")
  val rv64ufVec = new AssemblyTestSuite("rv64uf-vec", "rv64uf", rv64ufVecNamesV4)(_) 

  val rv64ufScalarVecNames = rv64ufVecNames.map("s"+_)
  val rv64ufScalarVecNamesV4 = rv64ufScalarVecNames -- Set("svec-fdiv", "svec-fcmp", "svec-fcvt", "svec-fcvt_w")
  val rv64ufScalarVec = new AssemblyTestSuite("rv64uf-svec", "rv64uf", rv64ufScalarVecNamesV4)(_) 

  val rv64uv = List(rv64ufScalarVec, rv64ufVec, rv64uiScalerVec, rv64uiVec, rv64uvBasic)

  val rv64svNames = Set(
    "illegal_inst", "illegal_vt_inst", "illegal_vt_regid", "ma_utld", "ma_utsd", "ma_vld", "ma_vsd", "ma_vt_inst", "privileged_inst")
    val rv64svNamesV4 = rv64svNames -- Set(
    "illegal_inst", "illegal_vt_inst", "illegal_vt_regid", "ma_utld", "ma_utsd", "ma_vld", "ma_vsd", "ma_vt_inst", "privileged_inst")
  val rv64sv = new AssemblyTestSuite("rv64sv", "rv64sv", rv64svNamesV4)(_)
}

import HwachaTestSuites._
class WithHwachaTests extends ChiselConfig(
  (pname,site,here) => pname match {
    case BuildRoCC => {
      TestGeneration.addSuites(rv64uv.map(_("p")))
      // no excep or vm in v4 yet
      //TestGeneration.addSuites((if(site(UseVM)) List("pt","v") else List("pt")).flatMap(env => rv64uv.map(_(env))))
      TestGeneration.addSuite(rv64sv("p"))
      TestGeneration.addVariable("SRC_EXTENSION", "$(base_dir)/hwacha/$(src_path)/*.scala")
      TestGeneration.addVariable("DISASM_EXTENSION", "--extension=hwacha")
      Some(() => (Module(new Hwacha, { case CoreName => "Hwacha" })))
    }
  }
)

class HwachaVLSIConfig extends ChiselConfig(new WithHwachaTests ++ new DefaultHwachaConfig ++ new DefaultL2VLSIConfig)
class HwachaFPGAConfig extends ChiselConfig(new WithHwachaTests ++ new DefaultHwachaConfig ++ new DefaultL2FPGAConfig) 
class HwachaCPPConfig extends ChiselConfig(new WithHwachaTests ++ new DefaultHwachaConfig ++ new DefaultL2CPPConfig) 

class EOS24Config extends ChiselConfig(new With4Banks ++ new WithL2Capacity256 ++ new HwachaVLSIConfig)
class EOS24FPGAConfig extends ChiselConfig(new FPGAConfig ++ new EOS24Config)
