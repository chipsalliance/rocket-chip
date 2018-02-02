// See LICENSE.jtag for license details.

package freechips.rocketchip.jtag

import scala.collection.SortedMap

// !!! See Issue #1160.
// import chisel3._
import Chisel._
import chisel3.core.{Input, Output}
import chisel3.util._
import chisel3.experimental.withReset

import freechips.rocketchip.config.Parameters

/** JTAG signals, viewed from the master side
  */
class JTAGIO(hasTRSTn: Boolean = false) extends Bundle {
  val TRSTn = if (hasTRSTn) Some(Output(Bool())) else None
  val TCK   = Output(Clock())
  val TMS   = Output(Bool())
  val TDI   = Output(Bool())
  val TDO   = Input(new Tristate())

  override def cloneType = new JTAGIO(hasTRSTn).asInstanceOf[this.type]
}

/** JTAG block output signals.
  */
class JtagOutput(irLength: Int) extends Bundle {
  val state = Output(JtagState.State.chiselType())  // state, transitions on TCK rising edge
  val instruction = Output(UInt(irLength.W))  // current active instruction
  val reset = Output(Bool())  // synchronous reset asserted in Test-Logic-Reset state, should NOT hold the FSM in reset

  override def cloneType = new JtagOutput(irLength).asInstanceOf[this.type]
}

class JtagControl extends Bundle {
  val jtag_reset = Input(Bool())
}

/** Aggregate JTAG block IO.
  */
class JtagBlockIO(irLength: Int, hasIdcode:Boolean = true) extends Bundle {

  val jtag = Flipped(new JTAGIO())
  val control = new JtagControl
  val output = new JtagOutput(irLength)
  val idcode = if (hasIdcode) Some(Input(new JTAGIdcodeBundle())) else None

  override def cloneType = new JtagBlockIO(irLength, hasIdcode).asInstanceOf[this.type]
}

/** Internal controller block IO with data shift outputs.
  */
class JtagControllerIO(irLength: Int) extends JtagBlockIO(irLength, false) {
  val dataChainOut = Output(new ShifterIO)
  val dataChainIn = Input(new ShifterIO)

  override def cloneType = new JtagControllerIO(irLength).asInstanceOf[this.type]
}

/** JTAG TAP controller internal block, responsible for instruction decode and data register chain
  * control signal generation.
  *
  * Misc notes:
  * - Figure 6-3 and 6-4 provides examples with timing behavior
  */
class JtagTapController(irLength: Int, initialInstruction: BigInt)(implicit val p: Parameters) extends Module {
  require(irLength >= 2)  // 7.1.1a

  val io = IO(new JtagControllerIO(irLength))

  val tdo = Wire(Bool())  // 4.4.1c TDI should appear here uninverted after shifting
  val tdo_driven = Wire(Bool())
  io.jtag.TDO.data := NegEdgeReg(clock, tdo, name = Some("tdoReg"))  // 4.5.1a TDO changes on falling edge of TCK, 6.1.2.1d driver active on first TCK falling edge in ShiftIR and ShiftDR states
  io.jtag.TDO.driven := NegEdgeReg(clock, tdo_driven, name = Some("tdoeReg"))

  //
  // JTAG state machine
  //

  val currState = Wire(JtagState.State.chiselType)

  // At this point, the TRSTn should already have been
  // combined with any POR, and it should also be
  // synchronized to TCK.
  require(!io.jtag.TRSTn.isDefined, "TRSTn should be absorbed into jtckPOReset outside of JtagTapController.")
  withReset(io.control.jtag_reset) {
    val stateMachine = Module(new JtagStateMachine)
    stateMachine.suggestName("stateMachine")
    stateMachine.io.tms := io.jtag.TMS
    currState := stateMachine.io.currState
    io.output.state := stateMachine.io.currState
  }

  //
  // Instruction Register
  //
  // 7.1.1d IR shifter two LSBs must be b01 pattern
  // TODO: 7.1.1d allow design-specific IR bits, 7.1.1e (rec) should be a fixed pattern
  // 7.2.1a behavior of instruction register and shifters
  val irChain = Module(CaptureUpdateChain(UInt(irLength.W)))
  irChain.suggestName("irChain")
  irChain.io.chainIn.shift := currState === JtagState.ShiftIR.U
  irChain.io.chainIn.data := io.jtag.TDI
  irChain.io.chainIn.capture := currState === JtagState.CaptureIR.U
  irChain.io.chainIn.update := currState === JtagState.UpdateIR.U
  irChain.io.capture.bits := "b01".U

  val updateInstruction = Wire(Bool())

  val nextActiveInstruction = Wire(UInt(irLength.W))
  val activeInstruction = NegEdgeReg(clock, nextActiveInstruction, updateInstruction, name = Some("irReg"))   // 7.2.1d active instruction output latches on TCK falling edge

  when (reset.toBool) {
    nextActiveInstruction := initialInstruction.U(irLength.W)
    updateInstruction := true.B
  } .elsewhen (currState === JtagState.UpdateIR.U) {
    nextActiveInstruction := irChain.io.update.bits
    updateInstruction := true.B
  } .otherwise {
    //!!! Needed when using chisel3._ (See #1160)
    // nextActiveInstruction := DontCare
    updateInstruction := false.B
  }
  io.output.instruction := activeInstruction

  io.output.reset := currState === JtagState.TestLogicReset.U

  //
  // Data Register
  //
  io.dataChainOut.shift := currState === JtagState.ShiftDR.U
  io.dataChainOut.data := io.jtag.TDI
  io.dataChainOut.capture := currState === JtagState.CaptureDR.U
  io.dataChainOut.update := currState === JtagState.UpdateDR.U

  //
  // Output Control
  //
  when (currState === JtagState.ShiftDR.U) {
    tdo := io.dataChainIn.data
    tdo_driven := true.B
  } .elsewhen (currState === JtagState.ShiftIR.U) {
    tdo := irChain.io.chainOut.data
    tdo_driven := true.B
  } .otherwise {
    //!!! Needed when using chisel3._ (See #1160)
    //tdo := DontCare
    tdo_driven := false.B
  }
}

object JtagTapGenerator {
  /** JTAG TAP generator, enclosed module must be clocked from TCK and reset from output of this
    * block.
    *
    * @param irLength length, in bits, of instruction register, must be at least 2
    * @param instructions map of instruction codes to data register chains that select that data
    * register; multiple instructions may map to the same data chain
    * @param idcode optional idcode instruction. idcode UInt will come from outside this core.
    *
    * @note all other instruction codes (not part of instructions or idcode) map to BYPASS
    * @note initial instruction is idcode (if supported), otherwise all ones BYPASS
    *
    * Usage notes:
    * - 4.3.1b TMS must appear high when undriven
    * - 4.3.1c (rec) minimize load presented by TMS
    * - 4.4.1b TDI must appear high when undriven
    * - 4.5.1b TDO must be inactive except when shifting data (undriven? 6.1.2)
    * - 6.1.3.1b TAP controller must not be (re-?)initialized by system reset (allows
    *   boundary-scan testing of reset pin)
    *   - 6.1 TAP controller can be initialized by a on-chip power on reset generator, the same one
    *     that would initialize system logic
    *
    * TODO:
    * - support concatenated scan chains
    */
  def apply(irLength: Int, instructions: Map[BigInt, Chain], icode: Option[BigInt] = None)(implicit p: Parameters): JtagBlockIO = {

    val internalIo = Wire(new JtagBlockIO(irLength, icode.isDefined))


    // Create IDCODE chain if needed
    val allInstructions = icode match {
      case (Some(icode)) => {
        require(!(instructions contains icode), "instructions may not contain IDCODE")
        val idcodeChain = Module(CaptureChain(new JTAGIdcodeBundle()))
        idcodeChain.suggestName("idcodeChain")
        val i = internalIo.idcode.get.asUInt()
        assert(i % 2.U === 1.U, "LSB must be set in IDCODE, see 12.1.1d")
        assert(((i >> 1) & ((1.U << 11) - 1.U)) =/= JtagIdcode.dummyMfrId.U,
          "IDCODE must not have 0b00001111111 as manufacturer identity, see 12.2.1b")
        idcodeChain.io.capture.bits := internalIo.idcode.get
        instructions + (icode -> idcodeChain)

      }
      case None => instructions
    }

    val bypassIcode = (BigInt(1) << irLength) - 1  // required BYPASS instruction
    val initialInstruction = icode.getOrElse(bypassIcode) // 7.2.1e load IDCODE or BYPASS instruction after entry into TestLogicReset

    require(!(allInstructions contains bypassIcode), "instructions may not contain BYPASS code")

    val controllerInternal = Module(new JtagTapController(irLength, initialInstruction))

    val unusedChainOut = Wire(new ShifterIO)  // De-selected chain output
    unusedChainOut.shift := false.B
    unusedChainOut.data := false.B
    unusedChainOut.capture := false.B
    unusedChainOut.update := false.B

    val bypassChain = Module(JtagBypassChain())

    // The Great Data Register Chain Mux
    bypassChain.io.chainIn := controllerInternal.io.dataChainOut  // for simplicity, doesn't visibly affect anything else
    require(allInstructions.size > 0, "Seriously? JTAG TAP with no instructions?")

    // Need to ensure that this mapping is ordered to produce deterministic verilog,
    // and neither Map nor groupBy are deterministic.
    // Therefore, we first sort by IDCODE, then sort the groups by the first IDCODE in each group.
    val chainToIcode = (SortedMap(allInstructions.toList:_*).groupBy { case (icode, chain) => chain } map {
      case (chain, icodeToChain) => chain -> icodeToChain.keys
    }).toList.sortBy(_._2.head)

    val chainToSelect = chainToIcode map {
      case (chain, icodes) => {
        assume(icodes.size > 0)
        val icodeSelects = icodes map { controllerInternal.io.output.instruction === _.asUInt(irLength.W) }
        chain -> icodeSelects.reduceLeft(_||_)
      }
    }

    def foldOutSelect(res: WhenContext, x: (Chain, Bool)): WhenContext = {
      val (chain, select) = x
      // Continue the WhenContext with if this chain is selected
      res.elsewhen(select) {
        controllerInternal.io.dataChainIn := chain.io.chainOut
      }
    }

    val emptyWhen = when (false.B) { }  // Empty WhenContext to start things off
    chainToSelect.toSeq.foldLeft(emptyWhen)(foldOutSelect).otherwise {
      controllerInternal.io.dataChainIn := bypassChain.io.chainOut
    }

    def mapInSelect(x: (Chain, Bool)) {
      val (chain, select) = x
      when (select) {
        chain.io.chainIn := controllerInternal.io.dataChainOut
      } .otherwise {
        chain.io.chainIn := unusedChainOut
      }
    }

    chainToSelect.map(mapInSelect)

    controllerInternal.io.jtag <> internalIo.jtag
    internalIo.control <> controllerInternal.io.control
    internalIo.output <> controllerInternal.io.output

    internalIo
  }

}
