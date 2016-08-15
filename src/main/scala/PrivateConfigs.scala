package rocketchip

import Chisel.Module
import cde._
import cde.Implicits._
import scala.collection.mutable.LinkedHashSet
import uncore.tilelink._
import uncore.agents._
import rocket._
import hwacha._

class VectorAssemblyTestSuite(prefix: String, names: LinkedHashSet[String])(env: String) extends AssemblyTestSuite(prefix, names)(env + "-vec")
class ScalarVectorAssemblyTestSuite(prefix: String, names: LinkedHashSet[String])(env: String) extends AssemblyTestSuite(prefix, names)(env + "-svec")

object HwachaTestSuites {
  import DefaultTestSuites._
  val rv64uvNames = LinkedHashSet(
    "wakeup", "fence", "keepcfg",
    "vmca", "vmcs", "vssd", "vssw", "vssh", "vssb",
    "vlsd", "vlsw", "vlswu", "vlsh", "vlshu", "vlsb", "vlsbu",
    "vsad", "vsaw", "vsah", "vsab", "vlad", "vlaw", "vlawu", "vlah", "vlahu", "vlab", "vlabu",
    "vld", "vlw", "vlwu", "vlh", "vlhu", "vlb", "vlbu", "vlxd", "vlxw", "vlxwu", "vlxh", "vlxhu", "vlxb", "vlxbu",
    "vsd", "vsw", "vsh", "vsb", "vsxd", "vsxw", "vsxh", "vsxb",
    "eidx", "imul", "fcvt", "vvadd_d", "vvadd_w", "vvadd_fd", "vvadd_fw", "vvmul_d",
    "overlap", "sched_sreg_xbar", "sched_fadd", "sched_waw", "sched_war", "pointer", "vcjal", "vfirst", "vfence",
    "vl_empty", "vs_empty", "vlx_empty", "vsx_empty", "vamo_empty", "eidx_empty") ++
    (rv32uaNames -- Set("lrsc")) ++ (rv64uaNames -- Set("lrsc"))
  val rv64uvBasic = new AssemblyTestSuite("rv64uv", rv64uvNames)(_)

  val rv64uiVecNames = rv32uiNames ++ rv64uiNames -- Set("simple", "auipc", "lui", "fence_i",
    "beq", "bge", "bgeu", "blt", "bltu", "bne", "jal", "jalr",
    "lb", "lbu", "lh", "lhu", "lw", "lwu", "ld", "sb", "sh", "sw", "sd")
  val rv64uiVec = new VectorAssemblyTestSuite("rv64ui", rv64uiVecNames)(_)
  val rv64uiScalarVec = new ScalarVectorAssemblyTestSuite("rv64ui", rv64uiVecNames)(_)

  val rv64umVec = new VectorAssemblyTestSuite("rv64um", rv64umNames)(_)
  val rv64umScalarVec = new ScalarVectorAssemblyTestSuite("rv64um", rv64umNames)(_)

  val rv64ufVecNames = rv64ufNames -- Set("ldst", "move")
  val rv64ufVec = new VectorAssemblyTestSuite("rv64uf", rv64ufVecNames)(_)
  val rv64udVec = new VectorAssemblyTestSuite("rv64ud", rv64ufVecNames)(_)

  val rv64ufScalarVecNames = rv64ufVecNames -- Set("fdiv", "fcmp") // unsupported by current scalar unit
  val rv64ufScalarVec = new ScalarVectorAssemblyTestSuite("rv64uf", rv64ufScalarVecNames)(_)
  val rv64udScalarVec = new ScalarVectorAssemblyTestSuite("rv64ud", rv64ufScalarVecNames)(_)

  val rv64uv = List(rv64ufScalarVec, rv64ufVec, rv64udScalarVec, rv64udVec, rv64uiScalarVec, rv64uiVec, rv64umScalarVec, rv64umVec, rv64uvBasic)

  val rv64svNames = LinkedHashSet(
    "illegal_inst", "illegal_vt_inst", "illegal_vt_regid", "ma_utld", "ma_utsd", "ma_vld", "ma_vsd", "ma_vt_inst", "privileged_inst")
    val rv64svNamesV4 = rv64svNames -- Set(
    "illegal_inst", "illegal_vt_inst", "illegal_vt_regid", "ma_utld", "ma_utsd", "ma_vld", "ma_vsd", "ma_vt_inst", "privileged_inst")
  val rv64sv = new AssemblyTestSuite("rv64sv", rv64svNamesV4)(_)

  val hwachaBmarks = new BenchmarkTestSuite("hwacha", "$(RISCV)/riscv64-unknown-elf/share/riscv-tests/benchmarks", LinkedHashSet(
    "pb-spmv", "vec-daxpy", "vec-dgemm-opt", "vec-hsaxpy", "vec-hgemm-opt", "vec-hsgemm-opt", "vec-saxpy", "vec-sdaxpy", "vec-sdgemm-opt", "vec-sgemm-naive", "vec-sgemm-opt", "vec-vvadd"))

}

import HwachaTestSuites._
class WithHwachaTests extends Config(
  (pname,site,here) => pname match {
    case TLKey("L1toL2") => site(TLKey("DefaultL1toL2")).copy(dataBeats = 4)
    case TLKey("L2toMC") => site(TLKey("DefaultL2toMC")).copy(dataBeats = 4)
    case TLKey("L2toMMIO") => site(TLKey("DefaultL2toMMIO")).copy(dataBeats = 4)
    case BuildRoCC => {
      TestGeneration.addSuites(rv64uv.map(_("p")))
      TestGeneration.addSuites(rv64uv.map(_("vp")))
      // no excep or vm in v4 yet
      //TestGeneration.addSuites((if(site(UseVM)) List("pt","v") else List("pt")).flatMap(env => rv64uv.map(_(env))))
      TestGeneration.addSuite(rv64sv("p"))
      TestGeneration.addSuite(hwachaBmarks)
      TestGeneration.addVariable("SRC_EXTENSION", "$(base_dir)/hwacha/$(src_path)/*.scala")
      TestGeneration.addVariable("DISASM_EXTENSION", "--extension=hwacha")
      Seq(RoccParameters(
        opcodes = OpcodeSet.custom0 | OpcodeSet.custom1,
        generator = (p: Parameters) =>
          (Module(new Hwacha()(p.alterPartial({ case CoreName => "Hwacha" })))),
        nMemChannels = site(HwachaNLanes),
        nPTWPorts = 2 + site(HwachaNLanes), // icache + vru + vmus
        useFPU = true))
    }
    case _ => throw new CDEMatchError
  }
)

class HwachaConfig extends Config(new WithHwachaTests ++ new DefaultHwachaConfig ++ new DefaultL2Config)
class HwachaFPGAConfig extends Config(new WithHwachaTests ++ new DefaultHwachaConfig ++ new DefaultL2FPGAConfig)

class EOS24Config extends Config(new WithNBanksPerMemChannel(4) ++ new WithL2Capacity(256) ++ new HwachaConfig)
class EOS24FPGAConfig extends Config(new FPGAConfig ++ new EOS24Config)

class With5L2AcquireXacts extends Config(
  (pname,site,here) => pname match {
    case NAcquireTransactors => 4
    case _ => throw new CDEMatchError
  }
)

class With9L2AcquireXacts extends Config(
  (pname,site,here) => pname match {
    case NAcquireTransactors => 9
    case _ => throw new CDEMatchError
  }
)

class With16L2AcquireXacts extends Config(
  (pname,site,here) => pname match {
    case NAcquireTransactors => 16
    case _ => throw new CDEMatchError
  }
)


class With2Lanes extends Config(
  (pname,site,here) => pname match {
    case HwachaNLanes => 2
    case _ => throw new CDEMatchError
  }
)

class With4Lanes extends Config(
  (pname,site,here) => pname match {
    case HwachaNLanes => 4
    case _ => throw new CDEMatchError
  }
)

class With32BtbEntires extends Config(
  (pname,site,here) => pname match {
    case BtbKey => BtbParameters(nEntries = 32)
    case _ => throw new CDEMatchError
  }
)

class Process28nmConfig extends Config(
  (pname,site,here) => pname match {
    case SFMALatency => 3
    case DFMALatency => 4
    case _ => throw new CDEMatchError
  }
)

class WithoutConfPrec extends Config(
  (pname,site,here) => pname match {
    case HwachaConfPrec => false
    case _ => throw new CDEMatchError
  }
)

class WithSmallPredRF extends Config(
  (pname,site,here) => pname match {
    case HwachaNPredRFEntries => 128
    case _ => throw new CDEMatchError
  }
)

class ISCA2016Config extends Config(
  new Process28nmConfig ++
  new WithNMemoryChannels(2) ++ new WithNBanksPerMemChannel(4) ++
  new With5L2AcquireXacts ++ new WithL2Capacity(256) ++ new With32BtbEntires ++ new HwachaConfig)
{
  override val knobValues:Any=>Any = {
    case "HWACHA_NSRAMRF_ENTRIES" => 256
    case "HWACHA_BUILD_VRU" => true
    case x => (new Config(new WithNMemoryChannels(2) ++ new WithNBanksPerMemChannel(4) ++ new WithL2Capacity(256) ++ new HwachaConfig)).knobValues(x)
    case _ => throw new CDEMatchError
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

class ISCA2016HOVB4Config extends Config(new With9L2AcquireXacts ++ new WithNBanksPerMemChannel(2) ++ new ISCA2016Config)
class ISCA2016HOVB8Config extends Config(new ISCA2016Config)
class ISCA2016LOVB4Config extends Config(new WithoutConfPrec ++ new ISCA2016HOVB4Config)
class ISCA2016LOVB8Config extends Config(new WithoutConfPrec ++ new ISCA2016HOVB8Config)

class ISCA2016HOVL2B4Config extends Config(new With2Lanes ++ new ISCA2016HOVB4Config)
class ISCA2016HOVL2B8Config extends Config(new With2Lanes ++ new ISCA2016HOVB8Config)
class ISCA2016LOVL2B4Config extends Config(new With2Lanes ++ new ISCA2016LOVB4Config)
class ISCA2016LOVL2B8Config extends Config(new With2Lanes ++ new ISCA2016LOVB8Config)

class ISCA2016HOVL4B4Config extends Config(new With4Lanes ++ new ISCA2016HOVB4Config)
class ISCA2016HOVL4B8Config extends Config(new With4Lanes ++ new ISCA2016HOVB8Config)
class ISCA2016LOVL4B4Config extends Config(new With4Lanes ++ new ISCA2016LOVB4Config)
class ISCA2016LOVL4B8Config extends Config(new With4Lanes ++ new ISCA2016LOVB8Config)
