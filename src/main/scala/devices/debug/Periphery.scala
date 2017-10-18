// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.debug

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.coreplex.HasPeripheryBus
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.jtag._
import freechips.rocketchip.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.regmapper._

/** A knob selecting one of the two possible debug interfaces */
case object IncludeJtagDTM extends Field[Boolean]
case object IncludeAXIDTM extends Field[Boolean]

/** A wrapper bundle containing one of the two possible debug interfaces */
class DebugIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val clockeddmi = (!p(IncludeJtagDTM) && !p(IncludeAXIDTM)).option(new ClockedDMIIO().flip)
  val systemjtag = (p(IncludeJtagDTM)).option(new SystemJTAGIO)
  val ndreset    = Bool(OUTPUT)
  val dmactive   = Bool(OUTPUT)
}

/** Either adds a JTAG DTM to system, and exports a JTAG interface,
  * or exports the Debug Module Interface (DMI), based on a global parameter.
  */
trait HasPeripheryDebug extends HasPeripheryBus {
  val module: HasPeripheryDebugModuleImp

  val debug = LazyModule(new TLDebugModule())
  debug.node := pbus.toVariableWidthSlaves

  val stream = LazyModule(new DMIAXInterface())

  val axi4InputNode = AXI4BlindInputNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(name = "AXI4 DMI Port",
      id   = IdRange(0, 2))))))
  stream.node := axi4InputNode
}
// MSB indicates full status
object NonBlockingEnqueue {
  def apply[T <: Data](enq: DecoupledIO[T], regWidth: Int = 32): Seq[RegField] = {
    val enqWidth = enq.bits.asUInt.getWidth
    val quash = Wire(Bool())
    require(enqWidth > 0)
    require(regWidth > enqWidth)
    Seq(
      RegField(enqWidth,
        RegReadFn(UInt(0)),
        RegWriteFn((valid, data) => {
          enq.valid := valid && !quash
          enq.bits := data.asTypeOf(enq.bits)
          Bool(true)
        })),
      RegField(regWidth - enqWidth - 1),
      RegField(1,
        !enq.ready,
        RegWriteFn((valid, data) =>  {
          quash := valid && data(0)
          Bool(true)
        })))
  }
}

// MSB indicates empty status
object NonBlockingDequeue {
  def apply[T <: Data](deq: DecoupledIO[T], regWidth: Int = 32): Seq[RegField] = {
    val deqWidth = deq.bits.asUInt.getWidth
    require(deqWidth > 0)
    require(regWidth > deqWidth)
    Seq(
      RegField.r(deqWidth,
        RegReadFn(ready => {
          deq.ready := ready
          (Bool(true), deq.bits.asUInt)
        })),
      RegField(regWidth - deqWidth - 1),
      RegField.r(1, !deq.valid))
  }
}

trait HasDMIIO extends Bundle {
  implicit val p: Parameters
  val dmi = new DMIIO()(p)
}

trait HasAXIDMIModuleContents extends Module with HasRegMap {
  val io: HasDMIIO
  implicit val p: Parameters
  regmap(
    0 -> NonBlockingEnqueue(io.dmi.req, 64),
    8 -> NonBlockingDequeue(io.dmi.resp, 64)
  )
}

class DMIAXInterface(implicit p: Parameters) extends AXI4RegisterRouter(0x1000, 0, 4096, 0, 8)(
  new AXI4RegBundle((), _) with HasDMIIO)(
  new AXI4RegModule((), _, _) with HasAXIDMIModuleContents)

trait HasPeripheryDebugBundle {
  implicit val p: Parameters

  val debug: DebugIO
  val debug_axi: HeterogeneousBag[AXI4Bundle]

  def connectDebug(c: Clock, r: Bool, out: Bool) {
    if(p(IncludeAXIDTM)) {
    debug_axi.foreach { d =>
      d.ar.valid := false.B
      d.aw.valid := false.B
      d.w.valid := false.B
      d.r.ready := true.B
      d.b.ready := true.B

      val dtm = Module(new SimDTM)
      val dmi = Wire(new ClockedDMIIO())
      dmi.dmi.resp.valid := false.B
      dmi.dmi.req.ready := false.B

      val req_valid = RegInit(false.B)
      val req_ready = RegInit(false.B)
      val req_data = Reg(dmi.dmi.req.bits)
      val resp_ar_valid = RegInit(false.B)
      dtm.connect(c, r, dmi, out)

      //response
      when(!r) {
        resp_ar_valid := true.B
        when(d.r.valid && (d.r.bits.id === 0.U) && !d.r.bits.data(63)) {
          //valid response data 
          dmi.dmi.resp.valid  := true.B
          dmi.dmi.resp.bits := d.r.bits.data(62,0).asTypeOf(dmi.dmi.resp.bits)
          d.r.ready := true.B // resp_q ready
        }
      }
      //request
      when(!r) {
        when(dmi.dmi.req.valid && !req_valid) {
          //valid request
          req_valid := true.B
          req_data := dmi.dmi.req.bits
        }
        when(d.r.valid && d.r.bits.id === 1.U && !d.r.bits.data(63)) {
          // ready for request
          req_ready := true.B
          d.r.ready := true.B
          dmi.dmi.req.ready := true.B
        }
        when(req_valid && req_ready) {
          // assumes we can do aw and w in same cycle
          d.aw.valid := true.B
          d.aw.bits.addr := 4096.U
          d.aw.bits.id := 1.U
          d.w.valid := true.B
          d.w.bits.data := req_data.asUInt
          d.w.bits.strb := ~0.U(8.W)
          // reset
          req_valid := false.B
          dmi.dmi.req.ready := true.B
        }
      }
      // arbiter between req and resp ar
      when(!r) {
        when(req_valid && req_ready) {
          d.ar.valid :=false.B
        }.elsewhen(req_valid) {
          d.ar.valid := true.B
          d.ar.bits.id := 1.U
          d.ar.bits.addr := 4096.U
          //TODO: what should len, size, and burst be set to
        }.elsewhen(resp_ar_valid) {
          d.ar.valid := true.B
          d.ar.bits.id := 0.U
          d.ar.bits.addr := 4104.U
        }.otherwise {
          d.ar.valid := false.B
          d.aw.valid := false.B
        }
      }
    }
    }
    debug.clockeddmi.foreach { d =>
      val dtm = Module(new SimDTM).connect(c, r, d, out)
    }
    debug.systemjtag.foreach { sj =>
      val jtag = Module(new JTAGVPI).connect(sj.jtag, sj.reset, r, out)
      sj.mfr_id := p(JtagDTMKey).idcodeManufId.U(11.W)
    }
  }
}

trait HasPeripheryDebugModuleImp extends LazyMultiIOModuleImp with HasPeripheryDebugBundle {
  val outer: HasPeripheryDebug

  val debug = IO(new DebugIO)
  val debug_axi = IO(outer.axi4InputNode.bundleIn)
  chisel3.experimental.dontTouch(debug_axi)

  debug.clockeddmi.foreach { dbg => outer.debug.module.io.dmi <> dbg }

  if(p(IncludeAXIDTM)) {
    outer.stream.module.io.in <> debug_axi
    //override dmi interface
    outer.debug.module.io.dmi.dmi.req <> outer.stream.module.io.dmi.req
    // add dmi resp queue
    outer.stream.module.io.dmi.resp <> Queue(outer.debug.module.io.dmi.dmi.resp, 4)
    outer.debug.module.io.dmi.dmiClock := clock
    outer.debug.module.io.dmi.dmiReset := reset
  }

  val dtm = debug.systemjtag.map { sj =>
    val dtm = Module(new DebugTransportModuleJTAG(p(DebugModuleParams).nDMIAddrSize, p(JtagDTMKey)))
    dtm.io.jtag <> sj.jtag

    dtm.clock          := sj.jtag.TCK
    dtm.io.jtag_reset  := sj.reset
    dtm.io.jtag_mfr_id := sj.mfr_id
    dtm.reset          := dtm.io.fsmReset

    outer.debug.module.io.dmi.dmi <> dtm.io.dmi
    outer.debug.module.io.dmi.dmiClock := sj.jtag.TCK
    outer.debug.module.io.dmi.dmiReset := ResetCatchAndSync(sj.jtag.TCK, sj.reset, "dmiResetCatch")
    dtm
  }

  debug.ndreset  := outer.debug.module.io.ctrl.ndreset
  debug.dmactive := outer.debug.module.io.ctrl.dmactive

  // TODO in inheriting traits: Set this to something meaningful, e.g. "component is in reset or powered down"
  outer.debug.module.io.ctrl.debugUnavail.foreach { _ := Bool(false) }
}

class SimDTM(implicit p: Parameters) extends BlackBox {
  val io = new Bundle {
    val clk = Clock(INPUT)
    val reset = Bool(INPUT)
    val debug = new DMIIO
    val exit = UInt(OUTPUT, 32)
  }

  def connect(tbclk: Clock, tbreset: Bool, dutio: ClockedDMIIO, tbsuccess: Bool) = {
    io.clk := tbclk
    io.reset := tbreset
    dutio.dmi <> io.debug
    dutio.dmiClock := tbclk
    dutio.dmiReset := tbreset

    tbsuccess := io.exit === UInt(1)
    when (io.exit >= UInt(2)) {
      printf("*** FAILED *** (exit code = %d)\n", io.exit >> UInt(1))
      stop(1)
    }
  }
}

class JTAGVPI(implicit val p: Parameters) extends BlackBox {
  val io = new Bundle {
    val jtag = new JTAGIO(hasTRSTn = false)
    val enable = Bool(INPUT)
    val init_done = Bool(INPUT)
  }

  def connect(dutio: JTAGIO, jtag_reset: Bool, tbreset: Bool, tbsuccess: Bool) = {
    dutio <> io.jtag

    dutio.TRSTn.foreach{ _:= false.B}
    jtag_reset := tbreset

    io.enable    := ~tbreset
    io.init_done := ~tbreset

    // Success is determined by the gdbserver
    // which is controlling this simulation.
    tbsuccess := Bool(false)
  }
}
