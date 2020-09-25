// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.{BusMemoryLogicalTreeNode, LogicalModuleTree, LogicalTreeNode}
import freechips.rocketchip.diplomaticobjectmodel.model.AXI4_Lite
import freechips.rocketchip.util._
import freechips.rocketchip.amba._

// Setting wcorrupt=true is not enough to enable the w.user field
// You must also list AMBACorrupt in your master's requestFields
class AXI4RAM(
    address: AddressSet,
    cacheable: Boolean = true,
    parentLogicalTreeNode: Option[LogicalTreeNode] = None,
    executable: Boolean = true,
    beatBytes: Int = 4,
    devName: Option[String] = None,
    errors: Seq[AddressSet] = Nil,
    wcorrupt: Boolean = true)
  (implicit p: Parameters) extends DiplomaticSRAM(address, beatBytes, devName)
{
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address       = List(address) ++ errors,
      resources     = resources,
      regionType    = if (cacheable) RegionType.UNCACHED else RegionType.IDEMPOTENT,
      executable    = executable,
      supportsRead  = TransferSizes(1, beatBytes),
      supportsWrite = TransferSizes(1, beatBytes),
      interleavedId = Some(0))),
    beatBytes  = beatBytes,
    requestKeys = if (wcorrupt) Seq(AMBACorrupt) else Seq(),
    minLatency = 1)))

  private val outer = this

  lazy val module = new LazyModuleImp(this) with HasJustOneSeqMem {
    val (in, edgeIn) = node.in(0)
    val laneDataBits = 8
    val (mem, omSRAM, omMem) = makeSinglePortedByteWriteSeqMem(
      size = BigInt(1) << mask.filter(b=>b).size,
      lanes = beatBytes,
      bits = laneDataBits)
    val eccCode = None
    val address = outer.address

    parentLogicalTreeNode.map {
      case parentLTN =>
        def sramLogicalTreeNode = new BusMemoryLogicalTreeNode(
          device = device,
          omSRAMs = Seq(omSRAM),
          busProtocol = new AXI4_Lite(None),
          dataECC = None,
          hasAtomics = None,
          busProtocolSpecification = None)
        LogicalModuleTree.add(parentLTN, sramLogicalTreeNode)
    }

    val corrupt = if (edgeIn.bundle.requestFields.contains(AMBACorrupt)) Some(SeqMem(1 << mask.filter(b=>b).size, UInt(width=2))) else None

    val r_addr = Cat((mask zip (in.ar.bits.addr >> log2Ceil(beatBytes)).asBools).filter(_._1).map(_._2).reverse)
    val w_addr = Cat((mask zip (in.aw.bits.addr >> log2Ceil(beatBytes)).asBools).filter(_._1).map(_._2).reverse)
    val r_sel0 = address.contains(in.ar.bits.addr)
    val w_sel0 = address.contains(in.aw.bits.addr)

    val w_full = RegInit(Bool(false))
    val w_id   = Reg(UInt())
    val w_echo = Reg(BundleMap(in.params.echoFields))
    val r_sel1 = Reg(r_sel0)
    val w_sel1 = Reg(w_sel0)

    when (in. b.fire()) { w_full := Bool(false) }
    when (in.aw.fire()) { w_full := Bool(true) }

    when (in.aw.fire()) {
      w_id := in.aw.bits.id
      w_sel1 := w_sel0
      w_echo :<= in.aw.bits.echo
    }

    val wdata = Vec.tabulate(beatBytes) { i => in.w.bits.data(8*(i+1)-1, 8*i) }
    when (in.aw.fire() && w_sel0) {
      mem.write(w_addr, wdata, in.w.bits.strb.asBools)
      corrupt.foreach { _.write(w_addr, in.w.bits.user(AMBACorrupt).asUInt) }
    }

    in. b.valid := w_full
    in.aw.ready := in. w.valid && (in.b.ready || !w_full)
    in. w.ready := in.aw.valid && (in.b.ready || !w_full)

    in.b.bits.id   := w_id
    in.b.bits.resp := Mux(w_sel1, AXI4Parameters.RESP_OKAY, AXI4Parameters.RESP_DECERR)
    in.b.bits.echo :<= w_echo

    val r_full = RegInit(Bool(false))
    val r_id   = Reg(UInt())
    val r_echo = Reg(BundleMap(in.params.echoFields))

    when (in. r.fire()) { r_full := Bool(false) }
    when (in.ar.fire()) { r_full := Bool(true) }

    when (in.ar.fire()) {
      r_id := in.ar.bits.id
      r_sel1 := r_sel0
      r_echo :<= in.ar.bits.echo
    }

    val ren = in.ar.fire()
    val rdata = mem.readAndHold(r_addr, ren)
    val rcorrupt = corrupt.map(_.readAndHold(r_addr, ren)(0)).getOrElse(Bool(false))

    in. r.valid := r_full
    in.ar.ready := in.r.ready || !r_full

    in.r.bits.id   := r_id
    in.r.bits.resp := Mux(r_sel1, Mux(rcorrupt, AXI4Parameters.RESP_SLVERR, AXI4Parameters.RESP_OKAY), AXI4Parameters.RESP_DECERR)
    in.r.bits.data := Cat(rdata.reverse)
    in.r.bits.echo :<= r_echo
    in.r.bits.last := Bool(true)
  }
}

object AXI4RAM
{
  def apply(
    address: AddressSet,
    cacheable: Boolean = true,
    parentLogicalTreeNode: Option[LogicalTreeNode] = None,
    executable: Boolean = true,
    beatBytes: Int = 4,
    devName: Option[String] = None,
    errors: Seq[AddressSet] = Nil,
    wcorrupt: Boolean = true)
  (implicit p: Parameters) =
  {
    val axi4ram = LazyModule(new AXI4RAM(
      address = address,
      cacheable = cacheable,
      executable = executable,
      beatBytes = beatBytes,
      devName = devName,
      errors = errors,
      wcorrupt = wcorrupt))
    axi4ram.node
  }
}
