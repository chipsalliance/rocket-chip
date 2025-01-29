// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

import freechips.rocketchip.diplomacy.{AddressSet, RegionType, TransferSizes}
import freechips.rocketchip.resources.{DiplomaticSRAM, HasJustOneSeqMem}
import freechips.rocketchip.tilelink.LFSRNoiseMaker
import freechips.rocketchip.util.{MaskGen, DataToAugmentedData, SeqMemToAugmentedSeqMem, PlusArg}

class AHBRAM(
    address: AddressSet,
    cacheable: Boolean = true,
    executable: Boolean = true,
    beatBytes: Int = 4,
    fuzzHreadyout: Boolean = false,
    devName: Option[String] = None,
    errors: Seq[AddressSet] = Nil)
  (implicit p: Parameters) extends DiplomaticSRAM(address, beatBytes, devName)
{
  val node = AHBSubordinateSinkNode(Seq(AHBSubordinatePortParameters(
    Seq(AHBSubordinateParameters(
      address       = List(address) ++ errors,
      resources     = resources,
      regionType    = if (cacheable) RegionType.UNCACHED else RegionType.IDEMPOTENT,
      executable    = executable,
      supportsRead  = TransferSizes(1, beatBytes * AHBParameters.maxTransfer),
      supportsWrite = TransferSizes(1, beatBytes * AHBParameters.maxTransfer))),
    beatBytes  = beatBytes,
    lite = true)))

  private val outer = this

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with HasJustOneSeqMem {
    val (in, edge) = node.in(0)
    val laneDataBits = 8
    val mem = makeSinglePortedByteWriteSeqMem(
      size = BigInt(1) << mask.filter(b=>b).size,
      lanes = beatBytes,
      bits = laneDataBits)
    val eccCode = None
    val address = outer.address

    // The mask and address during the address phase
    val a_access    = in.htrans === AHBParameters.TRANS_NONSEQ || in.htrans === AHBParameters.TRANS_SEQ
    val a_request   = in.hready && in.hsel && a_access
    val a_mask      = MaskGen(in.haddr, in.hsize, beatBytes)
    val a_address   = Cat((mask zip (in.haddr >> log2Ceil(beatBytes)).asBools).filter(_._1).map(_._2).reverse)
    val a_write     = in.hwrite
    val a_legal     = address.contains(in.haddr)

    // The data phase signals
    val d_wdata = VecInit.tabulate(beatBytes) { i => in.hwdata(8*(i+1)-1, 8*i) }

    // AHB writes must occur during the data phase; this poses a structural
    // hazard with reads which must occur during the address phase. To solve
    // this problem, we delay the writes until there is a free cycle.
    //
    // The idea is to record the address information from address phase and
    // then as soon as possible flush the pending write. This cannot be done
    // on a cycle when there is an address phase read, but on any other cycle
    // the write will execute. In the case of reads following a write, the
    // result must bypass data from the pending write into the read if they
    // happen to have matching address.

    // Pending write?
    val p_valid     = RegInit(false.B)
    val p_address   = Reg(UInt())
    val p_mask      = Reg(UInt(a_mask.getWidth.W))
    val p_latch_d   = Reg(Bool())
    val p_wdata     = d_wdata holdUnless p_latch_d

    // Decide if the SRAM port is used for reading or (potentially) writing
    val read = a_request && !a_write
    // In case we choose to stall, we need to hold the read data
    val d_rdata = mem.readAndHold(a_address, read)
    val d_legal = RegEnable(a_legal, in.hreadyout)
    // Whenever the port is not needed for reading, execute pending writes
    when (!read && p_valid) {
      p_valid := false.B
      mem.write(p_address, p_wdata, p_mask.asBools)
    }

    // Record the request for later?
    p_latch_d := a_request && a_write
    when (a_request && a_write && a_legal) {
      p_valid   := true.B
      p_address := a_address
      p_mask    := a_mask
    }

    // Does the read need to be muxed with the previous write?
    val a_bypass = a_address === p_address && p_valid
    val d_bypass = RegEnable(a_bypass, a_request)

    // Mux in data from the pending write
    val muxdata = VecInit((p_mask.asBools zip (p_wdata zip d_rdata))
                      map { case (m, (p, r)) => Mux(d_bypass && m, p, r) })

    // Don't fuzz hready when not in data phase
    val d_request = RegInit(false.B)
    when (in.hready) { d_request := false.B }
    when (a_request)  { d_request := true.B }

    val disable_ahb_fuzzing = PlusArg("disable_ahb_fuzzing", default = 0, "1:Disabled 0:Enabled.")(0)

    // Finally, the outputs
    in.hreadyout := Mux(disable_ahb_fuzzing, true.B, { if(fuzzHreadyout) { !d_request || LFSRNoiseMaker(1)(0) }  else { true.B }} )
    in.hresp     := Mux(!d_request || d_legal || !in.hreadyout, AHBParameters.RESP_OKAY, AHBParameters.RESP_ERROR)
    in.hrdata    := Mux(in.hreadyout, muxdata.asUInt, 0.U)
  }
}
