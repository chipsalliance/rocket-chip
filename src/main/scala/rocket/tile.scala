// See LICENSE for license details.

package rocket

import Chisel._
import diplomacy._
import uncore.tilelink._
import uncore.tilelink2._
import uncore.agents._
import uncore.converters._
import uncore.devices._
import util._
import cde.{Parameters, Field}
import scala.collection.mutable.ListBuffer

case object BuildRoCC extends Field[Seq[RoccParameters]]
case object TileId extends Field[Int]

case class RoccParameters(
  opcodes: OpcodeSet,
  generator: Parameters => RoCC,
  nMemChannels: Int = 0,
  nPTWPorts : Int = 0,
  useFPU: Boolean = false)

class RocketTile(tileId: Int)(implicit p: Parameters) extends LazyModule {
  val dcacheParams = p.alterPartial({
    case CacheName => "L1D"
    case TLId => "L1toL2"
    case TileId => tileId // TODO using this messes with Heirarchical P&R
  })
  val icacheParams = p.alterPartial({
    case CacheName => "L1I"
    case TLId => "L1toL2"
  })

  //TODO val intNode = IntInputNode()
  val slaveNode = if (p(DataScratchpadSize) == 0) None else Some(TLInputNode())
  val scratch = if (p(DataScratchpadSize) == 0) None else Some(LazyModule(new ScratchpadSlavePort()(dcacheParams)))
  val dcache = HellaCache(p(DCacheKey))(dcacheParams)
  val ucLegacy = LazyModule(new TLLegacy()(icacheParams))

  val cachedOut = TLOutputNode()
  val uncachedOut = TLOutputNode()
  cachedOut := dcache.node
  uncachedOut := ucLegacy.node
  val masterNodes = List(cachedOut, uncachedOut)
  
  (slaveNode zip scratch) foreach { case (node, lm) => lm.node := TLFragmenter(p(XLen)/8, p(CacheBlockBytes))(node) }
  
  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val cached = cachedOut.bundleOut
      val uncached = uncachedOut.bundleOut
      val slave = slaveNode.map(_.bundleIn)
      val hartid = UInt(INPUT, p(XLen))
      val interrupts = new TileInterrupts().asInput
      val resetVector = UInt(INPUT, p(XLen))
    }

    val buildRocc = p(BuildRoCC)
    val usingRocc = !buildRocc.isEmpty
    val nRocc = buildRocc.size
    val nFPUPorts = buildRocc.filter(_.useFPU).size

    val core = Module(new Rocket()(dcacheParams))
    val icache = Module(new Frontend()(icacheParams))

    val ptwPorts = ListBuffer(icache.io.ptw, dcache.module.io.ptw)
    val dcPorts = ListBuffer(core.io.dmem)
    val uncachedArbPorts = ListBuffer(icache.io.mem)
    core.io.interrupts := io.interrupts
    core.io.hartid := io.hartid
    icache.io.cpu <> core.io.imem
    icache.io.resetVector := io.resetVector

    val fpuOpt = p(FPUKey).map(cfg => Module(new FPU(cfg)))
    fpuOpt.foreach(fpu => core.io.fpu <> fpu.io)

    if (usingRocc) {
      val respArb = Module(new RRArbiter(new RoCCResponse, nRocc))
      core.io.rocc.resp <> respArb.io.out

      val roccOpcodes = buildRocc.map(_.opcodes)
      val cmdRouter = Module(new RoccCommandRouter(roccOpcodes))
      cmdRouter.io.in <> core.io.rocc.cmd

      val roccs = buildRocc.zipWithIndex.map { case (accelParams, i) =>
        val rocc = accelParams.generator(p.alterPartial({
          case RoccNMemChannels => accelParams.nMemChannels
          case RoccNPTWPorts => accelParams.nPTWPorts
        }))
        val dcIF = Module(new SimpleHellaCacheIF()(dcacheParams))
        rocc.io.cmd <> cmdRouter.io.out(i)
        rocc.io.exception := core.io.rocc.exception
        dcIF.io.requestor <> rocc.io.mem
        dcPorts += dcIF.io.cache
        uncachedArbPorts += rocc.io.autl
        rocc
      }

      if (nFPUPorts > 0) {
        fpuOpt.foreach { fpu =>
          val fpArb = Module(new InOrderArbiter(new FPInput, new FPResult, nFPUPorts))
          val fp_roccs = roccs.zip(buildRocc)
            .filter { case (_, params) => params.useFPU }
            .map { case (rocc, _) => rocc.io }
          fpArb.io.in_req <> fp_roccs.map(_.fpu_req)
          fp_roccs.zip(fpArb.io.in_resp).foreach {
            case (rocc, fpu_resp) => rocc.fpu_resp <> fpu_resp
          }
          fpu.io.cp_req <> fpArb.io.out_req
          fpArb.io.out_resp <> fpu.io.cp_resp
        }
      }

      core.io.rocc.busy := cmdRouter.io.busy || roccs.map(_.io.busy).reduce(_ || _)
      core.io.rocc.interrupt := roccs.map(_.io.interrupt).reduce(_ || _)
      respArb.io.in <> roccs.map(rocc => Queue(rocc.io.resp))

      ptwPorts ++= roccs.flatMap(_.io.ptw)
      uncachedArbPorts ++= roccs.flatMap(_.io.utl) // TODO no difference between io.autl and io.utl for now
    }

    val uncachedArb = Module(new ClientUncachedTileLinkIOArbiter(uncachedArbPorts.size)(icacheParams))
    uncachedArb.io.in <> uncachedArbPorts
    ucLegacy.module.io.legacy <> uncachedArb.io.out

    if (p(UseVM)) {
      val ptw = Module(new PTW(ptwPorts.size)(dcacheParams))
      ptw.io.requestor <> ptwPorts
      ptw.io.mem +=: dcPorts
      core.io.ptw <> ptw.io.dpath
    }

    scratch.foreach { lm => lm.module.io.dmem +=: dcPorts }

    require(dcPorts.size == core.dcacheArbPorts)
    val dcArb = Module(new HellaCacheArbiter(dcPorts.size)(dcacheParams))
    dcArb.io.requestor <> dcPorts
    dcache.module.io.cpu <> dcArb.io.mem

    if (nFPUPorts == 0) {
      fpuOpt.foreach { fpu =>
        fpu.io.cp_req.valid := Bool(false)
        fpu.io.cp_resp.ready := Bool(false)
      }
    }
  }
}
