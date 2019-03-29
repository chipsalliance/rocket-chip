// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.logicaltree

import freechips.rocketchip.diplomacy.{LazyModule, ResourceBindingsMap, SimpleDevice}
import freechips.rocketchip.diplomaticobjectmodel.model._
import freechips.rocketchip.rocket.{DCacheParams, Frontend, ICacheParams, ScratchpadSlavePort}
import freechips.rocketchip.tile.{RocketTileParams, TileParams, XLen}


class ICacheLogicalTreeNode(device: SimpleDevice, icacheParams: ICacheParams) extends LogicalTreeNode {
  def getOMICacheFromBindings(resourceBindingsMap: ResourceBindingsMap): OMICache = {
    getOMComponents(resourceBindingsMap) match {
      case Seq() => throw new IllegalArgumentException
      case Seq(h) => h.asInstanceOf[OMICache]
      case _ => throw new IllegalArgumentException
    }
  }

  override def getOMComponents(resourceBindingsMap: ResourceBindingsMap, children: Seq[OMComponent] = Nil): Seq[OMComponent] = {
    val resourceBindings = resourceBindingsMap.map.get(device)
    Seq[OMComponent](OMCaches.icache(icacheParams, resourceBindings))
  }

  def iCache(resourceBindingsMap: ResourceBindingsMap): OMICache = {
    val resourceBindings = resourceBindingsMap.map.get(device)
    OMCaches.icache(icacheParams, resourceBindings)
  }
}

class RocketLogicalTreeNode(
  device: SimpleDevice,
  rocketParams: RocketTileParams,
  dtim_adapter: Option[ScratchpadSlavePort],
  XLen: Int,
  icacheLTN: ICacheLogicalTreeNode
) extends LogicalTreeNode {

  def getOMDCacheFromBindings(dCacheParams: DCacheParams, resourceBindingsMap: ResourceBindingsMap): Option[OMDCache] = {
    val omDTIM: Option[OMDCache] = dtim_adapter.map(_.device.getMemory(dCacheParams, resourceBindingsMap))
    val omDCache: Option[OMDCache] = rocketParams.dcache.filterNot(_.scratch.isDefined).map(OMCaches.dcache(_, None))
    require(!(omDTIM.isDefined && omDCache.isDefined))

    omDTIM.orElse(omDCache)
  }

  def getOMInterruptTargets(): Seq[OMInterruptTarget] = {
    Seq(OMInterruptTarget(
      hartId = rocketParams.hartId,
      modes = OMModes.getModes(rocketParams.core.useVM)
    ))
  }

  override def getOMComponents(resourceBindingsMap: ResourceBindingsMap, components: Seq[OMComponent]): Seq[OMComponent] = {
    val coreParams = rocketParams.core

    val omDCache = rocketParams.dcache.flatMap{ getOMDCacheFromBindings(_, resourceBindingsMap)}

    val omICache = icacheLTN.iCache(resourceBindingsMap)

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

class RocketTileLogicalTreeNode(
  getOMRocketInterruptTargets: () => Seq[OMInterruptTarget]) extends LogicalTreeNode {

  def getIndex(cs: Seq[OMComponent]): Seq[(OMComponent, Int)] = {
    cs.zipWithIndex.filter(_._1.isInstanceOf[OMPLIC])
  }

  def updatePlic(plic: OMPLIC): OMPLIC = {
    val omRocketInterruptTargets: Seq[OMInterruptTarget] = getOMRocketInterruptTargets()

    plic.copy(targets = omRocketInterruptTargets)
  }

  def addIntsToPlic(resourceBindingsMap: ResourceBindingsMap, components: Seq[OMComponent]): Seq[OMComponent] = {
    val cs = getIndex(components)

    require(cs.size <= 1, "Too many Plic's")

    cs.flatMap {
      case (plic, index) =>
        val omplic = plic.asInstanceOf[OMPLIC]
        val updatedPlic = updatePlic(omplic)
        components.updated(index, updatedPlic)
    }
  }

  override def getOMComponents(resourceBindingsMap: ResourceBindingsMap, components: Seq[OMComponent]): Seq[OMComponent] = {
    components
  }
}

