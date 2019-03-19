// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.logicaltree

import freechips.rocketchip.diplomacy.{LazyModule, ResourceBindingsMap, SimpleDevice}
import freechips.rocketchip.diplomaticobjectmodel.model._
import freechips.rocketchip.rocket.{DCacheParams, Frontend, ICacheParams, ScratchpadSlavePort}
import freechips.rocketchip.tile.{RocketTileParams, TileParams, XLen}


class ICacheLogicalTreeNode(device: SimpleDevice, icacheParams: Option[ICacheParams]) extends LogicalTreeNode {
  def getOMICacheFromBindings(resourceBindingsMap: ResourceBindingsMap): Option[OMICache] = {
    icacheParams.map(i => getOMComponents(resourceBindingsMap) match {
      case Seq() => throw new IllegalArgumentException
      case Seq(h) => h.asInstanceOf[OMICache]
      case _ => throw new IllegalArgumentException
    })
  }

  override def getOMComponents(resourceBindingsMap: ResourceBindingsMap, children: Seq[OMComponent] = Nil): Seq[OMComponent] = {
    val resourceBindings = resourceBindingsMap.map.get(device)
    icacheParams.map{ x => Seq[OMComponent](OMCaches.icache(x, resourceBindings))}.getOrElse(Nil)
  }
}

class RocketLogicalTreeNode(
  device: SimpleDevice,
  rocketParams: RocketTileParams,
//  icacheLTN: ICacheLogicalTreeNode,
  dtim_adapter: Option[ScratchpadSlavePort],
  XLen: Int) extends LogicalTreeNode {

//  def getOMICache(resourceBindingsMap: ResourceBindingsMap): OMICache = { // TODO
//    icacheLTN.getOMComponents(resourceBindingsMap) match {
//      case Seq() => throw new IllegalArgumentException
//      case Seq(h) => h.asInstanceOf[OMICache]
//      case _ => throw new IllegalArgumentException
//    }
//  }

  def getOMDCacheFromBindings(dCacheParams: DCacheParams, resourceBindingsMap: ResourceBindingsMap): Option[OMDCache] = {
    val omDTIM: Option[OMDCache] = dtim_adapter.map(_.device.getMemory(dCacheParams, resourceBindingsMap))
    val omDCache: Option[OMDCache] = rocketParams.dcache.filterNot(_.scratch.isDefined).map(OMCaches.dcache(_, None))
    require(!(omDTIM.isDefined && omDCache.isDefined))

    omDTIM.orElse(omDCache)
  }

  def getInterruptTargets(): Seq[OMInterruptTarget] = {
    Seq(OMInterruptTarget(
      hartId = rocketParams.hartId,
      modes = OMModes.getModes(rocketParams.core.useVM)
    ))
  }

  override def  getOMComponents(resourceBindingsMap: ResourceBindingsMap, components: Seq[OMComponent]): Seq[OMComponent] = {
    val coreParams = rocketParams.core

    val omICache: OMICache = OMICache(Nil,Nil, 1,1,1,1, None, None, 1, 1) // TODO getOMICache(resourceBindingsMap)

    val omDCache = rocketParams.dcache.flatMap{ getOMDCacheFromBindings(_, resourceBindingsMap)}

    Seq(OMRocketCore(
      isa = OMISA.rocketISA(coreParams, XLen),
      mulDiv =  coreParams.mulDiv.map{ md => OMMulDiv.makeOMI(md, XLen)},
      fpu = coreParams.fpu.map{f => OMFPU(fLen = f.fLen)},
      performanceMonitor = PerformanceMonitor.perfmon(coreParams),
      pmp = OMPMP.pmp(coreParams),
      documentationName = rocketParams.name.getOrElse("rocket"),
      hartIds = Seq(rocketParams.hartId),
      hasVectoredInterrupts = true,
      interruptLatency = 4,
      nLocalInterrupts = coreParams.nLocalInterrupts,
      nBreakpoints = coreParams.nBreakpoints,
      branchPredictor = rocketParams.btb.map(OMBTB.makeOMI),
      dcache = omDCache,
      icache = Some(omICache)
    ))
  }
}
