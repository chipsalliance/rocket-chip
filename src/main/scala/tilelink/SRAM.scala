// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property

class TLRAMErrors(val params: ECCParams, val addrBits: Int) extends Bundle with CanHaveErrors {
  val correctable   = (params.code.canCorrect && params.notifyErrors).option(Valid(UInt(addrBits.W)))
  val uncorrectable = (params.code.canDetect  && params.notifyErrors).option(Valid(UInt(addrBits.W)))
}

class TLRAM(
    address: AddressSet,
    cacheable: Boolean = true,
    executable: Boolean = true,
    atomics: Boolean = false,
    beatBytes: Int = 4,
    ecc: ECCParams = ECCParams(),
    sramReg: Boolean = false, // drive SRAM data output directly into a register => 1 cycle longer response
    val devName: Option[String] = None,
    val dtsCompat: Option[Seq[String]] = None,
    val devOverride: Option[Device with DeviceRegName] = None
  )(implicit p: Parameters) extends DiplomaticSRAM(address, beatBytes, devName, dtsCompat, devOverride)
{
  val eccBytes = ecc.bytes
  val code = ecc.code
  require (eccBytes  >= 1 && isPow2(eccBytes))
  require (beatBytes >= 1 && isPow2(beatBytes))
  require (eccBytes <= beatBytes, s"TLRAM eccBytes (${eccBytes}) > beatBytes (${beatBytes}). Use a WidthWidget=>Fragmenter=>SRAM if you need high density and narrow ECC; it will do bursts efficiently")

  val node = TLManagerNode(Seq(TLSlavePortParameters.v1(
    Seq(TLSlaveParameters.v1(
      address            = List(address),
      resources          = resources,
      regionType         = if (cacheable) RegionType.UNCACHED else RegionType.IDEMPOTENT,
      executable         = executable,
      supportsGet        = TransferSizes(1, beatBytes),
      supportsPutPartial = TransferSizes(1, beatBytes),
      supportsPutFull    = TransferSizes(1, beatBytes),
      supportsArithmetic = if (atomics) TransferSizes(1, beatBytes) else TransferSizes.none,
      supportsLogical    = if (atomics) TransferSizes(1, beatBytes) else TransferSizes.none,
      fifoId             = Some(0))), // requests are handled in order
    beatBytes  = beatBytes,
    minLatency = 1))) // no bypass needed for this device

  val notifyNode = ecc.notifyErrors.option(BundleBridgeSource(() => new TLRAMErrors(ecc, log2Ceil(address.max)).cloneType))

  private val outer = this

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with HasJustOneSeqMem {
    val (in, edge) = node.in(0)

    val indexBits = (outer.address.mask & ~(beatBytes-1)).bitCount
    val width = code.width(eccBytes*8)
    val lanes = beatBytes/eccBytes
    val mem = makeSinglePortedByteWriteSeqMem(
      size = BigInt(1) << indexBits,
      lanes = lanes,
      bits = width)
    val eccCode = Some(ecc.code)
    val address = outer.address
    val laneDataBits = eccBytes * 8

    /* This block has a three-stage pipeline
     * Stage A is the combinational request from TileLink A channel
     * Stage R corresponds to an accepted request
     * Stage D registers the result of an SRAM read (if any)
     *
     * The TileLink D channel response comes from
     *   - stage D for corected reads or AMOs
     *   - stage R for everything else
     *   - However, to increase maximum operating frequency, the
     *     stage R responses can be configured to come from stage D
     *
     * For sub-ECC granule writes and atomic operations:
     *   - stage A sets up the read for the old data value
     *   - stage R is used to gather the result from SRAM to registers
     *   - stage D corrects ECC, applies the ALU, and sets up SRAM write
     *
     * For super-ECC granule writes:
     *   - stage A sets up the write
     *
     * For reads:
     *   - stage A sets up the read
     *   - stage R drives the uncorrected data with valid based on ECC validity
     *   - stage D sets up the correction, if any
     *
     * When stage D needs to perform a write (AMO, sub-ECC write, or ECC correction):
     *   - there is a WaW or WaR hazard vs. the operation in stage R
     *     - for sub-ECC writes and atomics, we ensure stage R has a bubble
     *     - for ECC correction, we cause stage R to be replayed (and reject stage A twice)
     *   - there is a structural hazard competing with stage A for SRAM access
     *     - stage D always wins (stage A is rejected)
     *   - on ECC correction, there is a structural hazard competing with stage R for the response channel
     *     - stage D always wins (stage R is replayed)
     */

    // D stage registers from R
    val d_full      = RegInit(false.B)
    val d_respond   = Reg(Bool())
    val d_opcode    = Reg(UInt(3.W))
    val d_param     = Reg(UInt(3.W))
    val d_size      = Reg(UInt(edge.bundle.sizeBits.W))
    val d_source    = Reg(UInt(edge.bundle.sourceBits.W))
    val d_read      = Reg(Bool())
    val d_atomic    = Reg(Bool())
    val d_sublane   = Reg(Bool())
    val d_address   = Reg(UInt(edge.bundle.addressBits.W))
    val d_mask      = Reg(UInt(beatBytes.W))
    val d_rmw_data  = Reg(UInt((8*beatBytes).W))
    val d_poison    = Reg(Bool())
    val d_raw_data  = Reg(Vec(lanes, Bits(width.W)))

    // R stage registers from A
    val r_full      = RegInit(false.B)
    val r_opcode    = Reg(UInt(3.W))
    val r_param     = Reg(UInt(3.W))
    val r_size      = Reg(UInt(edge.bundle.sizeBits.W))
    val r_source    = Reg(UInt(edge.bundle.sourceBits.W))
    val r_read      = Reg(Bool())
    val r_atomic    = Reg(Bool())
    val r_sublane   = Reg(Bool())
    val r_address   = Reg(UInt(edge.bundle.addressBits.W))
    val r_mask      = Reg(UInt(beatBytes.W))
    val r_rmw_data  = Reg(UInt((8*beatBytes).W))
    val r_poison    = Reg(Bool())
    val r_raw_data  = Wire(Vec(lanes, Bits(width.W)))

    // Decode raw SRAM output
    val d_decoded       = d_raw_data.map(lane => code.decode(lane))
    val d_corrected     = Cat(d_decoded.map(_.corrected).reverse)
    val d_uncorrected   = Cat(d_decoded.map(_.uncorrected).reverse)
    val d_correctable   = d_decoded.map(_.correctable)
    val d_uncorrectable = d_decoded.map(_.uncorrectable)
    val d_need_fix      = d_correctable.reduce(_ || _)
    val d_lanes         = Cat(Seq.tabulate(lanes) { i => d_mask(eccBytes*(i+1)-1, eccBytes*i).orR }.reverse)
    val d_lane_error    = Cat(d_uncorrectable.reverse) & d_lanes
    val d_error         = d_lane_error.orR

    val r_decoded       = r_raw_data.map(lane => code.decode(lane))
    val r_corrected     = Cat(r_decoded.map(_.corrected).reverse)
    val r_uncorrected   = Cat(r_decoded.map(_.uncorrected).reverse)
    val r_correctable   = r_decoded.map(_.correctable)
    val r_uncorrectable = r_decoded.map(_.uncorrectable)
    val r_need_fix      = r_correctable.reduce(_ || _)
    val r_lanes         = Cat(Seq.tabulate(lanes) { i => r_mask(eccBytes*(i+1)-1, eccBytes*i).orR }.reverse)
    val r_lane_error    = Cat(r_uncorrectable.reverse) & r_lanes
    val r_error         = r_lane_error.orR

    // Out-of-band notification of any faults
    notifyNode.foreach { nnode =>
      nnode.bundle.correctable.foreach { c =>
        c.valid := d_need_fix && d_full && (d_atomic || d_read || d_sublane)
        c.bits  := d_address
      }
      nnode.bundle.uncorrectable.foreach { u =>
        u.valid := d_error && d_full && (d_atomic || d_read || d_sublane)
        u.bits  := d_address
      }
    }

    // What does D-stage want to write-back?
    // Make an ALU if we need one
    val d_updated = if (atomics) {
      val alu = Module(new Atomics(edge.bundle))
      alu.io.write     := false.B
      alu.io.a.opcode  := d_opcode
      alu.io.a.param   := d_param
      alu.io.a.size    := d_size
      alu.io.a.source  := 0.U
      alu.io.a.address := 0.U
      alu.io.a.data    := d_rmw_data
      alu.io.a.mask    := d_mask
      alu.io.a.corrupt := false.B
      alu.io.data_in   := d_corrected
      alu.io.data_out
    } else {
      Cat(Seq.tabulate(beatBytes) { i =>
        val upd = d_mask(i) && !d_read
        val rmw = d_rmw_data (8*(i+1)-1, 8*i)
        val fix = d_corrected(8*(i+1)-1, 8*i) // safe to use, because D-stage write-back always wins arbitration
        Mux(upd, rmw, fix)
      }.reverse)
    }

    // Stage D always wins control of the response channel
    val d_win = d_full && d_respond
    val d_mux = if (sramReg) true.B else d_win
    val out_aad = Mux(d_mux, d_read || d_atomic, r_read || r_atomic)
    in.d.bits.opcode  := Mux(out_aad, TLMessages.AccessAckData, TLMessages.AccessAck)
    in.d.bits.param   := 0.U
    in.d.bits.size    := Mux(d_mux, d_size,   r_size)
    in.d.bits.source  := Mux(d_mux, d_source, r_source)
    in.d.bits.sink    := 0.U
    in.d.bits.denied  := false.B
    in.d.bits.data    := Mux(d_mux, d_corrected, r_uncorrected)
    in.d.bits.corrupt := Mux(d_mux, d_error, r_error) && out_aad

    val mem_active_valid = Seq(property.CoverBoolean(in.d.valid, Seq("mem_active")))
    val data_error = Seq(
      property.CoverBoolean(!d_need_fix && !d_error , Seq("no_data_error")),
      property.CoverBoolean(d_need_fix && !in.d.bits.corrupt, Seq("data_correctable_error_not_reported")),
      property.CoverBoolean(d_error && in.d.bits.corrupt, Seq("data_uncorrectable_error_reported")))

    val error_cross_covers = new property.CrossProperty(Seq(mem_active_valid, data_error), Seq(), "Ecc Covers")
    property.cover(error_cross_covers)

    // Does the D stage want to perform a write?
    // It's important this reduce to false.B when eccBytes=1 && atomics=false && canCorrect=false
    val d_wb = d_full && (d_sublane || d_atomic || (d_read && d_need_fix))
    // Formulate an R response unless there is a data output fix to perform
    // It's important this reduce to false.B for sramReg and true.B for !code.canCorrect
    val r_respond = !sramReg.B && (!r_need_fix || !(r_read || r_atomic))
    // Resolve WaW and WaR hazard when D performs an update (only happens on ECC correction)
    // It's important this reduce to false.B unless code.canDetect
    val r_replay = RegNext(r_full && d_full && d_read && d_need_fix)
    // r_full && d_wb => read ecc fault (we insert a buble for atomic/sublane)
    assert (!(r_full && d_wb) || (d_full && d_read && d_need_fix))

    // Pipeline control
    in.d.valid := (d_full && d_respond) || (r_full && r_respond && !d_wb && !r_replay)
    val d_ready = !d_respond || in.d.ready
    val r_ready = !d_wb && !r_replay && (!d_full || d_ready) && (!r_respond || (!d_win && in.d.ready))
    in.a.ready := !(d_full && d_wb) && (!r_full || r_ready) && (!r_full || !(r_atomic || r_sublane))

    val a_sublane = if (eccBytes == 1) false.B else
      in.a.bits.opcode === TLMessages.PutPartialData ||
      in.a.bits.size < log2Ceil(eccBytes).U
    val a_atomic = if (!atomics) false.B else
      in.a.bits.opcode === TLMessages.ArithmeticData ||
      in.a.bits.opcode === TLMessages.LogicalData
    val a_read = in.a.bits.opcode === TLMessages.Get

    // Forward pipeline stage from R to D
    when (d_ready) { d_full := false.B }
    when (r_full && r_ready) {
      d_full     := true.B
      d_respond  := !r_respond
      d_opcode   := r_opcode
      d_param    := r_param
      d_size     := r_size
      d_source   := r_source
      d_read     := r_read
      d_atomic   := r_atomic
      d_sublane  := r_sublane
      d_address  := r_address
      d_mask     := r_mask
      d_rmw_data := r_rmw_data
      d_poison   := r_poison
      d_raw_data := r_raw_data
    }

    // Forward pipeline stage from A to R
    when (r_ready) { r_full := false.B }
    when (in.a.fire) {
      r_full     := true.B
      r_sublane  := a_sublane
      r_opcode   := in.a.bits.opcode
      r_param    := in.a.bits.param
      r_size     := in.a.bits.size
      r_source   := in.a.bits.source
      r_read     := a_read
      r_atomic   := a_atomic
      r_sublane  := a_sublane
      r_address  := in.a.bits.address
      r_poison   := in.a.bits.corrupt
      r_mask     := in.a.bits.mask
      when (!a_read) { r_rmw_data := in.a.bits.data }
    }

    // Split data into eccBytes-sized chunks:
    val a_data = VecInit(Seq.tabulate(lanes) { i => in.a.bits.data(eccBytes*8*(i+1)-1, eccBytes*8*i) })
    val r_data = VecInit(Seq.tabulate(lanes) { i => r_rmw_data(eccBytes*8*(i+1)-1, eccBytes*8*i) })
    val d_data = VecInit(Seq.tabulate(lanes) { i => d_updated(8*eccBytes*(i+1)-1, 8*eccBytes*i) })

    // Which data chunks get poisoned
    val a_poisonv = VecInit(Seq.fill(lanes) { in.a.bits.corrupt })
    val r_poisonv = VecInit(Seq.fill(lanes) { r_poison })
    val d_poisonv = VecInit(Seq.tabulate(lanes) { i =>
      val upd = d_mask(eccBytes*(i+1)-1, eccBytes*i)
      (!upd.andR && d_uncorrectable(i)) || d_poison // sub-lane writes should not correct uncorrectable
    })

    val a_lanes = Cat(Seq.tabulate(lanes) { i => in.a.bits.mask(eccBytes*(i+1)-1, eccBytes*i).orR }.reverse)

    // SRAM arbitration
    val a_fire = in.a.fire
    val a_ren = a_read || a_atomic || a_sublane
    val r_ren = r_read || r_atomic || r_sublane
    val wen = d_wb || Mux(r_replay, !r_ren, a_fire && !a_ren)
    val ren = !wen && (a_fire || r_replay) // help Chisel infer a RW-port

    val addr   = Mux(d_wb, d_address, Mux(r_replay, r_address, in.a.bits.address))
    val sel    = Mux(d_wb, d_lanes,   Mux(r_replay, r_lanes,   a_lanes))
    val dat    = Mux(d_wb, d_data,    Mux(r_replay, r_data,    a_data))
    val poison = Mux(d_wb, d_poisonv, Mux(r_replay, r_poisonv, a_poisonv))
    val coded  = VecInit((dat zip poison) map { case (d, p) =>
      if (code.canDetect) code.encode(d, p) else code.encode(d)
    })

    val index = Cat(mask.zip((addr >> log2Ceil(beatBytes)).asBools).filter(_._1).map(_._2).reverse)
    r_raw_data := mem.read(index, ren) holdUnless RegNext(ren)
    when (wen) { mem.write(index, coded, sel.asBools) }

    // Tie off unused channels
    in.b.valid := false.B
    in.c.ready := true.B
    in.e.ready := true.B
  }
}

object TLRAM
{
  def apply(
    address: AddressSet,
    cacheable: Boolean = true,
    executable: Boolean = true,
    atomics: Boolean = false,
    beatBytes: Int = 4,
    ecc: ECCParams = ECCParams(),
    sramReg: Boolean = false,
    devName: Option[String] = None,
  )(implicit p: Parameters): TLInwardNode =
  {
    val ram = LazyModule(new TLRAM(address, cacheable, executable, atomics, beatBytes, ecc, sramReg, devName))
    ram.node
  }
}

// Synthesizable unit testing
import freechips.rocketchip.unittest._

class TLRAMSimple(ramBeatBytes: Int, sramReg: Boolean, txns: Int)(implicit p: Parameters) extends LazyModule {
  val fuzz = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("SRAMSimple"))
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x3ff), beatBytes = ramBeatBytes, sramReg = sramReg))

  ram.node := TLDelayer(0.25) := model.node := fuzz.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class TLRAMSimpleTest(ramBeatBytes: Int, sramReg: Boolean, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new TLRAMSimple(ramBeatBytes, sramReg, txns)).module)
  dut.io.start := io.start
  io.finished := dut.io.finished
}

class TLRAMECC(ramBeatBytes: Int, eccBytes: Int, sramReg: Boolean, txns: Int)(implicit p: Parameters) extends LazyModule {
  val fuzz = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("SRAMSimple"))
  val ram  = LazyModule(new TLRAM(
    AddressSet(0x0, 0x3ff),
    atomics = true,
    beatBytes = ramBeatBytes,
    ecc = ECCParams(bytes = eccBytes, code = new SECDEDCode),
    sramReg = sramReg))

  ram.node := TLDelayer(0.25) := model.node := fuzz.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class TLRAMECCTest(ramBeatBytes: Int, eccBytes: Int, sramReg: Boolean, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new TLRAMECC(ramBeatBytes, eccBytes, sramReg, txns)).module)
  dut.io.start := io.start
  io.finished := dut.io.finished
}
