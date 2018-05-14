// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._
import scala.math.max

class TLFIFOFixer(policy: TLFIFOFixer.Policy = TLFIFOFixer.all)(implicit p: Parameters) extends LazyModule
{
  private def fifoMap(seq: Seq[TLManagerParameters]) = {
    val (flatManagers, keepManagers) = seq.partition(policy)
    // We need to be careful if one flatManager and one keepManager share an existing domain
    // Erring on the side of caution, we will also flatten the keepManager in this case
    val flatDomains = Set(flatManagers.flatMap(_.fifoId):_*) // => ID 0
    val keepDomains = Set(keepManagers.flatMap(_.fifoId):_*) -- flatDomains // => IDs compacted
    // Calculate what the FIFO domains look like after the fixer is applied
    val flatMap = flatDomains.map { x => (x, 0) }.toMap
    val keepMap = keepDomains.scanLeft((-1,0)) { case ((_,s),x) => (x, s+1) }.toMap
    val map = flatMap ++ keepMap
    val fixMap = seq.map { m => m.fifoId match {
      case None => if (policy(m)) Some(0) else None
      case Some(id) => Some(map(id)) // also flattens some who did not ask
    } }
    // Compress the FIFO domain space of those we are combining
    val reMap = flatDomains.scanLeft((-1,-1)) { case ((_,s),x) => (x, s+1) }.toMap
    val splatMap = seq.map { m => m.fifoId match {
      case None => None
      case Some(id) => reMap.lift(id)
    } }
    (fixMap, splatMap)
  }

  val node = TLAdapterNode(
    clientFn  = { cp => cp },
    managerFn = { mp =>
      val (fixMap, _) = fifoMap(mp.managers)
      mp.copy(managers = (fixMap zip mp.managers) map { case (id, m) => m.copy(fifoId = id) })
    })

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val (fixMap, splatMap) = fifoMap(edgeOut.manager.managers)

      // Do we need to serialize the request to this manager?
      val a_notFIFO = edgeIn.manager.fastProperty(in.a.bits.address, _.fifoId != Some(0), (b:Boolean) => Bool(b))
      // Compact the IDs of the cases we serialize
      val compacted = ((fixMap zip splatMap) zip edgeOut.manager.managers) flatMap {
        case ((f, s), m) => if (f == Some(0)) Some(m.copy(fifoId = s)) else None
      }
      val sinks = if (compacted.exists(_.supportsAcquireB)) edgeOut.manager.endSinkId else 0
      val a_id = if (compacted.isEmpty) UInt(0) else
        edgeOut.manager.copy(managers = compacted, endSinkId = sinks).findFifoIdFast(in.a.bits.address)
      val a_noDomain = a_id === UInt(0)

      if (false) {
        println(s"FIFOFixer for: ${edgeIn.client.clients.map(_.name).mkString(", ")}")
        println(s"make FIFO: ${edgeIn.manager.managers.filter(_.fifoId==Some(0)).map(_.name).mkString(", ")}")
        println(s"not  FIFO: ${edgeIn.manager.managers.filter(_.fifoId!=Some(0)).map(_.name).mkString(", ")}")
        println(s"domains: ${compacted.groupBy(_.name).mapValues(_.map(_.fifoId))}")
        println("")
      }

      // Count beats
      val a_first = edgeIn.first(in.a)
      val d_first = edgeOut.first(out.d) && out.d.bits.opcode =/= TLMessages.ReleaseAck

      // Keep one bit for each source recording if there is an outstanding request that must be made FIFO
      // Sources unused in the stall signal calculation should be pruned by DCE
      val flight = RegInit(Vec.fill(edgeIn.client.endSourceId) { Bool(false) })
      when (a_first && in.a.fire()) { flight(in.a.bits.source) := !a_notFIFO }
      when (d_first && in.d.fire()) { flight(in.d.bits.source) := Bool(false) }

      val stalls = edgeIn.client.clients.filter(c => c.requestFifo && c.sourceId.size > 1).map { c =>
        val a_sel = c.sourceId.contains(in.a.bits.source)
        val id    = RegEnable(a_id, in.a.fire() && a_sel && !a_notFIFO)
        val track = flight.slice(c.sourceId.start, c.sourceId.end)

        a_sel && a_first && track.reduce(_ || _) && (a_noDomain || id =/= a_id)
      }

      val stall = stalls.foldLeft(Bool(false))(_||_)

      out.a <> in.a
      in.d <> out.d
      out.a.valid := in.a.valid && (a_notFIFO || !stall)
      in.a.ready := out.a.ready && (a_notFIFO || !stall)

      if (edgeOut.manager.anySupportAcquireB && edgeOut.client.anySupportProbe) {
        in .b <> out.b
        out.c <> in .c
        out.e <> in .e
      } else {
        in.b.valid := Bool(false)
        in.c.ready := Bool(true)
        in.e.ready := Bool(true)
        out.b.ready := Bool(true)
        out.c.valid := Bool(false)
        out.e.valid := Bool(false)
      }

//Functional cover properties
     
      cover(in.a.valid && stall, "COVER FIFOFIXER STALL", "Cover: Stall occured for a valid transaction")

      val SourceIdFIFOed = RegInit(UInt(0, width = edgeIn.client.endSourceId))
      val SourceIdSet = Wire(init = UInt(0, width = edgeIn.client.endSourceId))
      val SourceIdClear = Wire(init = UInt(0, width = edgeIn.client.endSourceId))

      when (a_first && in.a.fire() && !a_notFIFO)  {
        SourceIdSet := UIntToOH(in.a.bits.source)
      }
      when (d_first && in.d.fire())  {
        SourceIdClear := UIntToOH(in.d.bits.source)
      }

      SourceIdFIFOed := SourceIdFIFOed | SourceIdSet
      val allIDs_FIFOed = SourceIdFIFOed===Fill(SourceIdFIFOed.getWidth, 1.U)

      cover(allIDs_FIFOed, "COVER all sources", "Cover: FIFOFIXER covers all Source IDs")
    //cover(flight.reduce(_ && _), "COVER full", "Cover: FIFO is full with all Source IDs")
      cover(!(flight.reduce(_ || _)), "COVER empty", "Cover: FIFO is empty")
      cover(SourceIdSet > 0.U, "COVER at least one push", "Cover: At least one Source ID is pushed")
      cover(SourceIdClear > 0.U, "COVER at least one pop", "Cover: At least one Source ID is popped")

    }
  }
}

object TLFIFOFixer
{
  // Which slaves should have their FIFOness combined?
  // NOTE: this transformation is still only applied for masters with requestFifo
  type Policy = TLManagerParameters => Boolean
  import RegionType._

  val all:            Policy = m => true
  val allFIFO:        Policy = m => m.fifoId.isDefined
  val allUncacheable: Policy = m => m.regionType <= UNCACHEABLE

  def apply(policy: Policy = all)(implicit p: Parameters): TLNode =
  {
    val fixer = LazyModule(new TLFIFOFixer(policy))
    fixer.node
  }
}
