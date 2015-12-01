package rocketchip

import Chisel.Module
import cde._
import cde.Implicits._
import scala.collection.mutable.LinkedHashSet
import uncore._
import rocket._
import hwacha._

object HwachaTestSuites {
  import DefaultTestSuites._
  val rv64uvNames = LinkedHashSet(
    "wakeup", "fence", "keepcfg",
    "vmsa", "vmss", "vssd", "vssw", "vssh", "vssb",
    "vlsd", "vlsw", "vlswu", "vlsh", "vlshu", "vlsb", "vlsbu", 
    "vsad", "vsaw", "vsah", "vsab", "vlad", "vlaw", "vlawu", "vlah", "vlahu", "vlab", "vlabu",
    "vld", "vlw", "vlwu", "vlh", "vlhu", "vlb", "vlbu", "vlxd", "vlxw", "vlxwu", "vlxh", "vlxhu", "vlxb", "vlxbu", 
    "vsd", "vsw", "vsh", "vsb", "vsxd", "vsxw", "vsxh", "vsxb",
    "eidx", "imul", "fcvt", "vvadd_d", "vvadd_w", "vvadd_fd", "vvadd_fw", "vvmul_d",
    "overlap", "sched_sreg_xbar", "sched_fadd", "sched_waw", "sched_war", "pointer", "vcjal", "vfirst", "vfence",
    "vl_empty", "vs_empty", "vlx_empty", "vsx_empty", "vamo_empty", "eidx_empty") ++
    rv32uaNames ++ rv64uaNames 
  val rv64uvBasic = new AssemblyTestSuite("rv64uv", "rv64uv", rv64uvNames)(_)
  
  val rv64uiVecNames = LinkedHashSet(
    "addi", "add", "addiw", "addw", "and", "andi", "div", "divu", "divuw", "divw", 
    "mul", "mulw", "mulh", "mulhu", "mulhsu", "or", "ori", "rem", "remu", "remuw", "remw",
    "sll", "slli", "slliw", "sllw", "slt", "slti", "sltiu", "sltu",
    "sra", "srai", "sraiw", "sraw", "srl", "srli", "srliw", "srlw", "sub", "subw", "xor", "xori").map("vec-" + _)
  val rv64uiVec = new AssemblyTestSuite("rv64ui-vec", "rv64ui", rv64uiVecNames)(_)

  val rv64uiScalarVecNames = rv64uiVecNames.map("s"+_)
  val rv64uiScalerVec = new AssemblyTestSuite("rv64ui-svec", "rv64ui", rv64uiScalarVecNames)(_)

  val rv64ufVecNames = LinkedHashSet(
    "fadd", "fcmp", "fdiv", "fclass", "fcvt", "fcvt_w", "fmadd", "fmin", "fsgnj").map("vec-" + _)
  val rv64ufVecNamesV4 = rv64ufVecNames
  val rv64ufVec = new AssemblyTestSuite("rv64uf-vec", "rv64uf", rv64ufVecNamesV4)(_) 

  val rv64ufScalarVecNames = rv64ufVecNames.map("s"+_)
  val rv64ufScalarVecNamesV4 = rv64ufScalarVecNames -- Set("svec-fdiv", "svec-fcmp")
  val rv64ufScalarVec = new AssemblyTestSuite("rv64uf-svec", "rv64uf", rv64ufScalarVecNamesV4)(_) 

  val rv64uv = List(rv64ufScalarVec, rv64ufVec, rv64uiScalerVec, rv64uiVec, rv64uvBasic)

  val rv64svNames = LinkedHashSet(
    "illegal_inst", "illegal_vt_inst", "illegal_vt_regid", "ma_utld", "ma_utsd", "ma_vld", "ma_vsd", "ma_vt_inst", "privileged_inst")
    val rv64svNamesV4 = rv64svNames -- Set(
    "illegal_inst", "illegal_vt_inst", "illegal_vt_regid", "ma_utld", "ma_utsd", "ma_vld", "ma_vsd", "ma_vt_inst", "privileged_inst")
  val rv64sv = new AssemblyTestSuite("rv64sv", "rv64sv", rv64svNamesV4)(_)
}

import HwachaTestSuites._
class WithHwachaTests extends Config(
  (pname,site,here) => pname match {
    case BuildRoCC => {
      TestGeneration.addSuites(rv64uv.map(_("p")))
      // no excep or vm in v4 yet
      //TestGeneration.addSuites((if(site(UseVM)) List("pt","v") else List("pt")).flatMap(env => rv64uv.map(_(env))))
      TestGeneration.addSuite(rv64sv("p"))
      TestGeneration.addVariable("SRC_EXTENSION", "$(base_dir)/hwacha/$(src_path)/*.scala")
      TestGeneration.addVariable("DISASM_EXTENSION", "--extension=hwacha")
      Seq((p: Parameters) => (Module(new Hwacha()(p.alterPartial({ case CoreName => "Hwacha" })))))
    }
  }
)

class HwachaVLSIConfig extends Config(new WithHwachaTests ++ new DefaultHwachaConfig ++ new DefaultL2VLSIConfig)
class HwachaFPGAConfig extends Config(new WithHwachaTests ++ new DefaultHwachaConfig ++ new DefaultL2FPGAConfig) 
class HwachaCPPConfig extends Config(new WithHwachaTests ++ new DefaultHwachaConfig ++ new DefaultL2CPPConfig) 

class EOS24Config extends Config(new With4BanksPerMemChannel ++ new WithL2Capacity256 ++ new HwachaVLSIConfig)
class EOS24FPGAConfig extends Config(new FPGAConfig ++ new EOS24Config)

class WithoutBackupMemoryPort extends Config(
  (pname,site,here) => pname match {
    case UseBackupMemoryPort => false
  }
)

class With4L2AcquireXacts extends Config(
  (pname,site,here) => pname match {
    case NAcquireTransactors => 4
  }
)

class With8L2AcquireXacts extends Config(
  (pname,site,here) => pname match {
    case NAcquireTransactors => 8
  }
)

class With16L2AcquireXacts extends Config(
  (pname,site,here) => pname match {
    case NAcquireTransactors => 16
  }
)


class With2Lanes extends Config(
  (pname,site,here) => pname match {
    case HwachaNLanes => 2
  }
)

class With4Lanes extends Config(
  (pname,site,here) => pname match {
    case HwachaNLanes => 4
  }
)

class With32BtbEntires extends Config(
  (pname,site,here) => pname match {
    case BtbKey => BtbParameters(nEntries = 32)
  }
)

class Process28nmConfig extends Config(
  (pname,site,here) => pname match {
    case SFMALatency => 3
    case DFMALatency => 4
  }
)

class WithoutConfPrec extends Config(
  (pname,site,here) => pname match {
    case HwachaConfPrec => false
  }
)

class VRU10Outstanding extends Config(
  (pname,site,here) => pname match {
    case HwachaVRUThrottle => 10
  }
)

class ISCA2016Config extends Config(
  new Process28nmConfig ++
  new WithoutBackupMemoryPort ++ new With2MemoryChannels ++ new With4BanksPerMemChannel ++
  new With8L2AcquireXacts ++ new WithL2Capacity256 ++ new With32BtbEntires ++ new HwachaVLSIConfig)
{
  override val knobValues:Any=>Any = {
    case "HWACHA_NSRAMRF_ENTRIES" => 256
    case "HWACHA_BUILD_VRU" => true
    // WithoutBackupMemoryPort not included here because it doesn't have knobs.
    case x => (new Config(new With2MemoryChannels ++ new With4BanksPerMemChannel ++ new WithL2Capacity256 ++ new HwachaVLSIConfig)).knobValues(x)
  }

  override val topConstraints:List[ViewSym=>Ex[Boolean]] = {
    List(
      {ex => (ex(HwachaNSRAMRFEntries) === 256)},
      {ex => (ex(HwachaBuildVRU) === true || ex(HwachaBuildVRU) === false)}
    )
  }
}

class ISCA2016L2Config extends Config(new With2Lanes ++ new ISCA2016Config)
class ISCA2016L4Config extends Config(new With4Lanes ++ new ISCA2016Config)

class ISCA2016LOVB4Config extends Config(new VRU10Outstanding ++ new WithoutConfPrec ++ new With2BanksPerMemChannel ++ new ISCA2016Config)
class ISCA2016LOVB8Config extends Config(new WithoutConfPrec ++ new ISCA2016Config)
class ISCA2016HOVB4Config extends Config(new VRU10Outstanding ++ new With2BanksPerMemChannel ++ new ISCA2016Config)
class ISCA2016HOVB8Config extends Config(new ISCA2016Config)
