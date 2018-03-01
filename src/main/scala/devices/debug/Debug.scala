// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.debug

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.rocket.Instructions
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._

/** Constant values used by both Debug Bus Response & Request
  */

object DMIConsts{

  def dmiDataSize = 32

  def dmiOpSize = 2
  def dmi_OP_NONE            = "b00".U
  def dmi_OP_READ            = "b01".U
  def dmi_OP_WRITE           = "b10".U

  def dmiRespSize = 2
  def dmi_RESP_SUCCESS     = "b00".U
  def dmi_RESP_FAILURE     = "b01".U
  def dmi_RESP_HW_FAILURE  = "b10".U
  // This is used outside this block
  // to indicate 'busy'.
  def dmi_RESP_RESERVED    = "b11".U

  def dmi_haltStatusAddr   = 0x40
}

object DsbBusConsts {

  def sbAddrWidth = 12
  def sbIdWidth   = 10 

}

object DsbRegAddrs{

  // These are used by the ROM.
  def HALTED       = 0x100
  def GOING        = 0x104
  def RESUMING     = 0x108
  def EXCEPTION    = 0x10C

  def WHERETO      = 0x300
  // This needs to be aligned for up to lq/sq

  
  // This shows up in HartInfo, and needs to be aligned
  // to enable up to LQ/SQ instructions.
  def DATA         = 0x380

  // We want DATA to immediately follow PROGBUF so that we can
  // use them interchangeably. Leave another slot if there is an
  // implicit ebreak.
  def PROGBUF(cfg:DebugModuleParams) = {
    val tmp = DATA - (cfg.nProgramBufferWords * 4)
    if (cfg.hasImplicitEbreak) (tmp - 4) else tmp
  }
  // This is unused if hasImpEbreak is false, and just points to the end of the PROGBUF.
  def IMPEBREAK(cfg: DebugModuleParams) = { DATA - 4 }

  // We want abstract to be immediately before PROGBUF
  // because we auto-generate 2 instructions.
  def ABSTRACT(cfg:DebugModuleParams) = PROGBUF(cfg) - 8

  def FLAGS        = 0x400
  def ROMBASE      = 0x800
 
}

/** Enumerations used both in the hardware
  * and in the configuration specification.
  */

object DebugModuleAccessType extends scala.Enumeration {
  type DebugModuleAccessType = Value
  val Access8Bit, Access16Bit, Access32Bit, Access64Bit, Access128Bit = Value
}
import DebugModuleAccessType._

object DebugAbstractCommandError extends scala.Enumeration {
  type DebugAbstractCommandError = Value
  val Success, ErrBusy, ErrNotSupported, ErrException, ErrHaltResume = Value
}
import DebugAbstractCommandError._

object DebugAbstractCommandType extends scala.Enumeration {
  type DebugAbstractCommandType = Value
  val AccessRegister, QuickAccess  = Value
}
import DebugAbstractCommandType._

/** Parameters exposed to the top-level design, set based on
  * external requirements, etc.
  *
  *  This object checks that the parameters conform to the 
  *  full specification. The implementation which receives this
  *  object can perform more checks on what that implementation
  *  actually supports.
  *  nComponents : The number of components to support debugging.
  *  nDMIAddrSize : Size of the Debug Bus Address
  *  nAbstractDataWords: Number of 32-bit words for Abstract Commands
  *  nProgamBufferWords: Number of 32-bit words for Program Buffer
  *  hasBusMaster: Whethr or not a bus master should be included
  *    The size of the accesses supported by the Bus Master. 
  *  supportQuickAccess : Whether or not to support the quick access command.
  *  supportHartArray : Whether or not to implement the hart array register.
  *  hartIdToHartSel: For systems where hart ids are not 1:1 with hartsel, provide the mapping.
  *  hartSelToHartId: Provide inverse mapping of the above
  *  hasImplicitEbreak: There is an additional RO program buffer word containing an ebreak
  **/

case class DebugModuleParams (
  nDMIAddrSize  : Int = 7,
  nProgramBufferWords: Int = 16,
  nAbstractDataWords : Int = 4,
  nScratch : Int = 1,
  //TODO: Use diplomacy to decide if you want this.
  hasBusMaster : Boolean = false,
  hasAccess128 : Boolean = false,
  hasAccess64  : Boolean = false,
  hasAccess32  : Boolean = false,
  hasAccess16  : Boolean = false,
  hasAccess8   : Boolean = false,
  supportQuickAccess : Boolean = false,
  supportHartArray   : Boolean = false,
  hartIdToHartSel : (UInt) => UInt = (x:UInt) => x,
  hartSelToHartId : (UInt) => UInt = (x:UInt) => x,
  hasImplicitEbreak : Boolean = false
) {

  if (hasBusMaster == false){
    require (hasAccess128 == false, "No Bus mastering support in Debug Module yet")
    require (hasAccess64  == false, "No Bus mastering support in Debug Module yet")
    require (hasAccess32  == false, "No Bus mastering support in Debug Module yet")
    require (hasAccess16  == false, "No Bus mastering support in Debug Module yet")
    require (hasAccess8   == false, "No Bus mastering support in Debug Module yet")
  }

  require ((nDMIAddrSize >= 7) && (nDMIAddrSize <= 32), s"Legal DMIAddrSize is 7-32, not ${nDMIAddrSize}")

  require ((nAbstractDataWords  > 0)  && (nAbstractDataWords  <= 16), s"Legal nAbstractDataWords is 0-16, not ${nAbstractDataWords}")
  require ((nProgramBufferWords >= 0) && (nProgramBufferWords <= 16), s"Legal nProgramBufferWords is 0-16, not ${nProgramBufferWords}")

  if (supportQuickAccess) {
    // TODO: Check that quick access requirements are met.
  }

}

object DefaultDebugModuleParams {

  def apply(xlen:Int /*TODO , val configStringAddr: Int*/): DebugModuleParams = {
    new DebugModuleParams().copy(
      nAbstractDataWords  = (if (xlen == 32) 1 else if (xlen == 64) 2 else 4)
    )
  }
}


case object DebugModuleParams extends Field[DebugModuleParams]

// *****************************************
// Module Interfaces
// 
// *****************************************

/** Structure to define the contents of a Debug Bus Request
  */
class DMIReq(addrBits : Int) extends Bundle {
  val addr = UInt(addrBits.W)
  val data = UInt(DMIConsts.dmiDataSize.W)
  val op   = UInt(DMIConsts.dmiOpSize.W)

  override def cloneType = new DMIReq(addrBits).asInstanceOf[this.type]
}

/** Structure to define the contents of a Debug Bus Response
  */
class DMIResp( ) extends Bundle {
  val data = UInt(DMIConsts.dmiDataSize.W)
  val resp = UInt(DMIConsts.dmiRespSize.W)
}

/** Structure to define the top-level DMI interface 
  *  of DebugModule.
  *  DebugModule is the consumer of this interface.
  *  Therefore it has the 'flipped' version of this.
  */
class DMIIO(implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val req = new  DecoupledIO(new DMIReq(p(DebugModuleParams).nDMIAddrSize))
  val resp = new DecoupledIO(new DMIResp).flip()
}

/* structure for passing hartsel between the "Outer" and "Inner"
 */

class DebugInternalBundle ()(implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val resumereq    = Bool()
  val hartsel      = UInt(10.W)
  val ackhavereset = Bool()
}

/* structure for top-level Debug Module signals which aren't the bus interfaces.
 */

class DebugCtrlBundle (nComponents: Int)(implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val debugUnavail    = Vec(nComponents, Bool()).asInput
  val ndreset         = Bool(OUTPUT)
  val dmactive        = Bool(OUTPUT)
}

// *****************************************
// Debug Module 
// 
// *****************************************

/** Parameterized version of the Debug Module defined in the
  *  RISC-V Debug Specification 
  *  
  *  DebugModule is a slave to two asynchronous masters:
  *    The Debug Bus (DMI) -- This is driven by an external debugger
  *  
  *    The System Bus -- This services requests from the cores. Generally
  *                      this interface should only be active at the request
  *                      of the debugger, but the Debug Module may also 
  *                      provide the default MTVEC since it is mapped
  *                      to address 0x0.
  *  
  *  DebugModule is responsible for control registers and RAM, and
  *  Debug ROM. It runs partially off of the dmiClk (e.g. TCK) and
  *  the TL clock. Therefore, it is divided into "Outer" portion (running
  *  of off dmiClock and dmiReset) and "Inner" (running off tlClock and tlReset).
  *  This allows DMCONTROL.haltreq, hartsel, dmactive, and ndreset to be
  *  modified even while the Core is in reset or not being clocked. 
  *  Not all reads from the Debugger to the Debug Module will actually complete
  *  in these scenarios either, they will just block until tlClock and tlReset
  *  allow them to complete. This is not strictly necessary for 
  *  proper debugger functionality.
  */

// Local reg mapper function : Notify when written, but give the value as well.  
object WNotifyWire {
  def apply(n: Int, value: UInt, set: Bool, name: String, desc: String) : RegField = {
    RegField(n, UInt(0), RegWriteFn((valid, data) => {
      set := valid
      value := data
      Bool(true)
    }), Some(RegFieldDesc(name = name, desc = desc, access = RegFieldAccessType.WSPECIAL)))
  }
}

// Local reg mapper function : Notify when accessed either as read or write.
object RWNotify {
    def apply (n: Int, rVal: UInt, wVal: UInt, rNotify: Bool, wNotify: Bool, desc: Option[RegFieldDesc] = None): RegField = {
      RegField(n,
        RegReadFn ((ready)       => {rNotify := ready ; (Bool(true), rVal)}),
        RegWriteFn((valid, data) => {
          wNotify := valid
          when (valid) {wVal := data}
          Bool(true)
        }
        ), desc)
    }
}

class TLDebugModuleOuter(device: Device)(implicit p: Parameters) extends LazyModule {

  // For Shorter Register Names
  import DMI_RegAddrs._

  val intnode = IntNexusNode(
    sourceFn       = { _ => IntSourcePortParameters(Seq(IntSourceParameters(1, Seq(Resource(device, "int"))))) },
    sinkFn         = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
    outputRequiresInput = false)

  val dmiNode = TLRegisterNode (
    address = AddressSet.misaligned(DMI_DMCONTROL << 2, 4),
    device = device,
    beatBytes = 4,
    executable = false
  )

  lazy val module = new LazyModuleImp(this) {
    require (intnode.edges.in.size == 0, "Debug Module does not accept interrupts")

    val nComponents = intnode.out.size

    val io = IO(new Bundle {
      val ctrl = (new DebugCtrlBundle(nComponents))
      val innerCtrl = new DecoupledIO(new DebugInternalBundle())
    })

    //----DMCONTROL (The whole point of 'Outer' is to maintain this register on dmiClock (e.g. TCK) domain, so that it
    //               can be written even if 'Inner' is not being clocked or is in reset. This allows halting
    //               harts while the rest of the system is in reset. It doesn't really allow any other
    //               register accesses, which will keep returning 'busy' to the debugger interface.

    val DMCONTROLReset = Wire(init = (new DMCONTROLFields().fromBits(0.U)))
    val DMCONTROLNxt = Wire(init = new DMCONTROLFields().fromBits(0.U))

    val DMCONTROLReg = Wire(init = new DMCONTROLFields().fromBits(AsyncResetReg(updateData = DMCONTROLNxt.asUInt,
      resetData = BigInt(0),
      enable = true.B,
      name = "DMCONTROL"
    )))

    val DMCONTROLRdData = Wire(init = DMCONTROLReg)

    val DMCONTROLWrDataVal = Wire(init = 0.U(32.W))
    val DMCONTROLWrData = (new DMCONTROLFields()).fromBits(DMCONTROLWrDataVal)
    val DMCONTROLWrEn   = Wire(init = false.B)
    val DMCONTROLRdEn   = Wire(init = false.B)

    val dmactive = DMCONTROLReg.dmactive

    DMCONTROLNxt := DMCONTROLReg
    when (~dmactive) {
      DMCONTROLNxt := DMCONTROLReset
    } .otherwise {
      when (DMCONTROLWrEn) {
        DMCONTROLNxt.ndmreset     := DMCONTROLWrData.ndmreset
        DMCONTROLNxt.hartsel      := DMCONTROLWrData.hartsel
        DMCONTROLNxt.haltreq      := DMCONTROLWrData.haltreq
        DMCONTROLNxt.resumereq    := DMCONTROLWrData.resumereq
        DMCONTROLNxt.ackhavereset := DMCONTROLWrData.ackhavereset
      }
    }

    // Put this last to override its own effects.
    when (DMCONTROLWrEn) {
      DMCONTROLNxt.dmactive := DMCONTROLWrData.dmactive
    }

    // DMCONTROL is the only register, so it's at offset 0.
    dmiNode.regmap(
      0 -> Seq(RWNotify(32, DMCONTROLRdData.asUInt(),
        DMCONTROLWrDataVal, DMCONTROLRdEn, DMCONTROLWrEn, Some(RegFieldDesc("dmi_dmcontrol", "", reset=Some(0)))))
    )

    //--------------------------------------------------------------
    // Interrupt Registers
    //--------------------------------------------------------------

    val debugIntNxt = Wire(init = Vec.fill(nComponents){false.B})
    val debugIntRegs = Wire(init = Vec(AsyncResetReg(updateData = debugIntNxt.asUInt,
      resetData = 0,
      enable = true.B,
      name = "debugInterrupts").toBools))

    debugIntNxt := debugIntRegs

    val (intnode_out, _) = intnode.out.unzip
    for (component <- 0 until nComponents) {
      intnode_out(component)(0) := debugIntRegs(component)
    }

    // Halt request registers are set & cleared by writes to DMCONTROL.haltreq
    // resumereq also causes the core to execute a 'dret',
    // so resumereq is passed through to Inner.
    // hartsel must also be used by the DebugModule state machine,
    // so it is passed to Inner.
    // It is true that there is no backpressure -- writes
    // which occur 'too fast' will be dropped.

    for (component <- 0 until nComponents) {
      when (~dmactive) {
        debugIntNxt(component) := false.B
      }. otherwise {
        when (DMCONTROLWrEn && DMCONTROLWrData.hartsel === component.U) {
          debugIntNxt(component) := DMCONTROLWrData.haltreq
        }
      }
    }

    io.innerCtrl.valid := DMCONTROLWrEn
    io.innerCtrl.bits.hartsel      := DMCONTROLWrData.hartsel
    io.innerCtrl.bits.resumereq    := DMCONTROLWrData.resumereq
    io.innerCtrl.bits.ackhavereset := DMCONTROLWrData.ackhavereset 

    io.ctrl.ndreset := DMCONTROLReg.ndmreset
    io.ctrl.dmactive := DMCONTROLReg.dmactive

  }
}

class TLDebugModuleOuterAsync(device: Device)(implicit p: Parameters) extends LazyModule {

  val dmi2tl = LazyModule(new DMIToTL())
  val dmiXbar = LazyModule (new TLXbar())

  val dmOuter = LazyModule( new TLDebugModuleOuter(device))
  val intnode = IntSyncCrossingSource(alreadyRegistered = true) :*= dmOuter.intnode

  val dmiInnerNode = TLAsyncCrossingSource() := dmiXbar.node

  dmiXbar.node := dmi2tl.node
  dmOuter.dmiNode := dmiXbar.node
  
  lazy val module = new LazyModuleImp(this) {

    val nComponents = dmOuter.intnode.edges.out.size

    val io = IO(new Bundle {
      val dmi   = new DMIIO()(p).flip()
      val ctrl = new DebugCtrlBundle(nComponents)
      val innerCtrl = new AsyncBundle(depth=1, new DebugInternalBundle())
    })

    dmi2tl.module.io.dmi <> io.dmi

    io.ctrl <> dmOuter.module.io.ctrl
    io.innerCtrl := ToAsyncBundle(dmOuter.module.io.innerCtrl, depth=1)

  }
}

class TLDebugModuleInner(device: Device, getNComponents: () => Int, beatBytes: Int)(implicit p: Parameters) extends LazyModule
{

  val dmiNode = TLRegisterNode(
    address = AddressSet.misaligned(0, DMI_RegAddrs.DMI_DMCONTROL << 2) ++
              AddressSet.misaligned((DMI_RegAddrs.DMI_DMCONTROL + 1) << 2, (0x200 - ((DMI_RegAddrs.DMI_DMCONTROL + 1) << 2))),
    device = device,
    beatBytes = 4,
    executable = false
  )

  val tlNode = TLRegisterNode(
    address=Seq(AddressSet(0, 0xFFF)), // This is required for correct functionality, it's not configurable.
    device=device,
    beatBytes=beatBytes,
    executable=true
  )

  lazy val module = new LazyModuleImp(this){

    val cfg = p(DebugModuleParams)

    val nComponents = getNComponents()

    val io = IO(new Bundle {
      val dmactive = Bool(INPUT)
      val innerCtrl = (new DecoupledIO(new DebugInternalBundle())).flip
      val debugUnavail = Vec(nComponents, Bool()).asInput
    })

    //--------------------------------------------------------------
    // Import constants for shorter variable names
    //--------------------------------------------------------------

    import DMI_RegAddrs._
    import DsbRegAddrs._
    import DsbBusConsts._
    import DMIConsts._

    //--------------------------------------------------------------
    // Sanity Check Configuration For this implementation.
    //--------------------------------------------------------------

    require (cfg.hasBusMaster == false, "No Bus Mastering support yet")
    require (cfg.supportQuickAccess == false, "No Quick Access support yet")
    require (cfg.supportHartArray == false, "No Hart Array support yet")

    //--------------------------------------------------------------
    // Register & Wire Declarations (which need to be pre-declared)
    //--------------------------------------------------------------

    val haltedBitRegs    = RegInit(Vec.fill(nComponents){false.B})
    val resumeReqRegs    = RegInit(Vec.fill(nComponents){false.B})
    val haveResetBitRegs = RegInit(Vec.fill(nComponents){true.B})

    // --- regmapper outputs

    val hartHaltedWrEn       = Wire(Bool())
    val hartHaltedId         = Wire(UInt(sbIdWidth.W))
    val hartGoingWrEn        = Wire(Bool())
    val hartGoingId          = Wire(UInt(sbIdWidth.W))
    val hartResumingWrEn     = Wire(Bool())
    val hartResumingId       = Wire(UInt(sbIdWidth.W))
    val hartExceptionWrEn    = Wire(Bool())
    val hartExceptionId      = Wire(UInt(sbIdWidth.W))

    val dmiProgramBufferRdEn = Wire(init = Vec.fill(cfg.nProgramBufferWords * 4){false.B})
    val dmiProgramBufferAccessLegal = Wire(init = false.B)
    val dmiProgramBufferWrEnMaybe = Wire(init = Vec.fill(cfg.nProgramBufferWords * 4){false.B})

    val dmiAbstractDataRdEn = Wire(init = Vec.fill(cfg.nAbstractDataWords * 4){false.B})
    val dmiAbstractDataAccessLegal = Wire (init = false.B)
    val dmiAbstractDataWrEnMaybe = Wire(init = Vec.fill(cfg.nAbstractDataWords * 4){false.B})

    //--------------------------------------------------------------
    // Registers coming from 'CONTROL' in Outer
    //--------------------------------------------------------------

    val selectedHartReg = RegInit(0.U(10.W))

    when (io.innerCtrl.fire()){
      selectedHartReg := io.innerCtrl.bits.hartsel
    }

    io.innerCtrl.ready := true.B

    //--------------------------------------------------------------
    // DMI Registers
    //--------------------------------------------------------------

    //----DMSTATUS

    val DMSTATUSRdData = Wire(init = (new DMSTATUSFields()).fromBits(0.U))
    DMSTATUSRdData.authenticated := true.B // Not implemented
    DMSTATUSRdData.version       := 2.U    // Version 0.13

    when (selectedHartReg >= nComponents.U) {
      DMSTATUSRdData.allnonexistent := true.B
      DMSTATUSRdData.anynonexistent := true.B
    }.elsewhen (io.debugUnavail(selectedHartReg)) {
      DMSTATUSRdData.allunavail := true.B
      DMSTATUSRdData.anyunavail := true.B
    }.elsewhen (haltedBitRegs(selectedHartReg)) {
      DMSTATUSRdData.allhalted := true.B
      DMSTATUSRdData.anyhalted := true.B
    }.otherwise {
      DMSTATUSRdData.allrunning := true.B
      DMSTATUSRdData.anyrunning := true.B
    }
    DMSTATUSRdData.allhavereset := haveResetBitRegs(selectedHartReg)
    DMSTATUSRdData.anyhavereset := haveResetBitRegs(selectedHartReg)

    val resumereq = io.innerCtrl.fire() && io.innerCtrl.bits.resumereq

    when (io.innerCtrl.fire()){
      when (io.innerCtrl.bits.ackhavereset) {
        haveResetBitRegs(io.innerCtrl.bits.hartsel) := false.B
      }
    }
 
    DMSTATUSRdData.allresumeack := ~resumeReqRegs(selectedHartReg) && ~resumereq
    DMSTATUSRdData.anyresumeack := ~resumeReqRegs(selectedHartReg) && ~resumereq

    //TODO
    DMSTATUSRdData.devtreevalid := false.B

    DMSTATUSRdData.impebreak := (cfg.hasImplicitEbreak).B

    //----HARTINFO

    val HARTINFORdData = Wire (init = (new HARTINFOFields()).fromBits(0.U))
    HARTINFORdData.dataaccess  := true.B
    HARTINFORdData.datasize    := cfg.nAbstractDataWords.U
    HARTINFORdData.dataaddr    := DsbRegAddrs.DATA.U
    HARTINFORdData.nscratch    := cfg.nScratch.U

    //----HALTSUM (and halted registers)
    val numHaltedStatus = ((nComponents - 1) / 32) + 1
    val haltedStatus   = Wire(Vec(numHaltedStatus, Bits(width = 32)))

    for (ii <- 0 until numHaltedStatus) {
      haltedStatus(ii) := Cat(haltedBitRegs.slice(ii * 32, (ii + 1) * 32).reverse)
    }

    val haltedSummary = Cat(haltedStatus.map(_.orR).reverse)
    val HALTSUMRdData = (new HALTSUMFields()).fromBits(haltedSummary)

    //----ABSTRACTCS

    val ABSTRACTCSReset = Wire(init = (new ABSTRACTCSFields()).fromBits(0.U))
    ABSTRACTCSReset.datacount   := cfg.nAbstractDataWords.U
    ABSTRACTCSReset.progbufsize := cfg.nProgramBufferWords.U

    val ABSTRACTCSReg       = Reg(new ABSTRACTCSFields())
    val ABSTRACTCSWrDataVal = Wire(init = 0.U(32.W))
    val ABSTRACTCSWrData    = (new ABSTRACTCSFields()).fromBits(ABSTRACTCSWrDataVal)
    val ABSTRACTCSRdData    = Wire(init = ABSTRACTCSReg)

    val ABSTRACTCSRdEn = Wire(init = false.B)
    val ABSTRACTCSWrEnMaybe = Wire(init = false.B)

    val ABSTRACTCSWrEnLegal = Wire(init = false.B)
    val ABSTRACTCSWrEn      = ABSTRACTCSWrEnMaybe && ABSTRACTCSWrEnLegal

    val errorBusy        = Wire(init = false.B)
    val errorException   = Wire(init = false.B)
    val errorUnsupported = Wire(init = false.B)
    val errorHaltResume  = Wire(init = false.B)

    when(~io.dmactive){
      ABSTRACTCSReg := ABSTRACTCSReset
    }.otherwise {
      when (errorBusy){
        ABSTRACTCSReg.cmderr := DebugAbstractCommandError.ErrBusy.id.U
      }.elsewhen (errorException) {
        ABSTRACTCSReg.cmderr := DebugAbstractCommandError.ErrException.id.U
      }.elsewhen (errorUnsupported) {
        ABSTRACTCSReg.cmderr := DebugAbstractCommandError.ErrNotSupported.id.U
      }.elsewhen (errorHaltResume) {
        ABSTRACTCSReg.cmderr := DebugAbstractCommandError.ErrHaltResume.id.U
      }.otherwise {
        when (ABSTRACTCSWrEn){
          ABSTRACTCSReg.cmderr := ABSTRACTCSReg.cmderr & ~(ABSTRACTCSWrData.cmderr);
        }
      }
    }

    // For busy, see below state machine.
    val abstractCommandBusy = Wire(init = true.B)
    ABSTRACTCSRdData.busy := abstractCommandBusy

    //---- ABSTRACTAUTO

    val ABSTRACTAUTOReset     = Wire(init = (new ABSTRACTAUTOFields()).fromBits(0.U))
    val ABSTRACTAUTOReg       = Reg(new ABSTRACTAUTOFields())
    val ABSTRACTAUTOWrDataVal = Wire(init = 0.U(32.W))
    val ABSTRACTAUTOWrData    = (new ABSTRACTAUTOFields()).fromBits(ABSTRACTAUTOWrDataVal)
    val ABSTRACTAUTORdData    = Wire(init = ABSTRACTAUTOReg)

    val ABSTRACTAUTORdEn = Wire(init = false.B)
    val ABSTRACTAUTOWrEnMaybe = Wire(init = false.B)

    val ABSTRACTAUTOWrEnLegal = Wire(init = false.B)
    val ABSTRACTAUTOWrEn      = ABSTRACTAUTOWrEnMaybe && ABSTRACTAUTOWrEnLegal

    when (~io.dmactive) {
      ABSTRACTAUTOReg := ABSTRACTAUTOReset
    }.elsewhen (ABSTRACTAUTOWrEn) {
      ABSTRACTAUTOReg.autoexecprogbuf := ABSTRACTAUTOWrData.autoexecprogbuf & ( (1 << cfg.nProgramBufferWords) - 1).U
      ABSTRACTAUTOReg.autoexecdata := ABSTRACTAUTOWrData.autoexecdata & ( (1 << cfg.nAbstractDataWords) - 1).U
    }

    val dmiAbstractDataAccessVec  = Wire(init = Vec.fill(cfg.nAbstractDataWords * 4){false.B})
    dmiAbstractDataAccessVec := (dmiAbstractDataWrEnMaybe zip dmiAbstractDataRdEn).map{ case (r,w) => r | w}

    val dmiProgramBufferAccessVec  = Wire(init = Vec.fill(cfg.nProgramBufferWords * 4){false.B})
    dmiProgramBufferAccessVec := (dmiProgramBufferWrEnMaybe zip dmiProgramBufferRdEn).map{ case (r,w) => r | w}

    val dmiAbstractDataAccess  = dmiAbstractDataAccessVec.reduce(_ || _ )
    val dmiProgramBufferAccess = dmiProgramBufferAccessVec.reduce(_ || _)

    // This will take the shorter of the lists, which is what we want.
    val autoexecData  = Wire(init = Vec.fill(cfg.nAbstractDataWords){false.B})
    val autoexecProg  = Wire(init = Vec.fill(cfg.nProgramBufferWords){false.B})
      (autoexecData zip ABSTRACTAUTOReg.autoexecdata.toBools).zipWithIndex.foreach {case (t, i) => t._1 := dmiAbstractDataAccessVec(i * 4) && t._2 }
      (autoexecProg zip ABSTRACTAUTOReg.autoexecprogbuf.toBools).zipWithIndex.foreach {case (t, i) => t._1 := dmiProgramBufferAccessVec(i * 4) && t._2}

    val autoexec = autoexecData.reduce(_ || _) || autoexecProg.reduce(_ || _)

    //---- COMMAND

    val COMMANDReset = Wire(init = (new COMMANDFields()).fromBits(0.U))
    val COMMANDReg = Reg(new COMMANDFields())

    val COMMANDWrDataVal    = Wire(init = 0.U(32.W))
    val COMMANDWrData       = Wire(init = (new COMMANDFields()).fromBits(COMMANDWrDataVal))
    val COMMANDWrEnMaybe    = Wire(init = false.B)
    val COMMANDWrEnLegal    = Wire(init = false.B)
    val COMMANDRdEn  = Wire(init = false.B)

    val COMMANDWrEn = COMMANDWrEnMaybe && COMMANDWrEnLegal
    val COMMANDRdData = COMMANDReg

    when (~io.dmactive) {
      COMMANDReg := COMMANDReset
    }.otherwise {
      when (COMMANDWrEn) {
        COMMANDReg := COMMANDWrData
      }
    }

    // --- Abstract Data

    // These are byte addressible, s.t. the Processor can use
    // byte-addressible instructions to store to them.
    val abstractDataMem       = Reg(Vec(cfg.nAbstractDataWords*4, UInt(8.W)))
    val abstractDataNxt       = Wire(init = abstractDataMem)

    // --- Program Buffer
    val programBufferMem    = Reg(Vec(cfg.nProgramBufferWords*4, UInt(8.W)))
    val programBufferNxt    = Wire(init = programBufferMem)

    //--------------------------------------------------------------
    // These bits are implementation-specific bits set
    // by harts executing code.
    //--------------------------------------------------------------

    for (component <- 0 until nComponents) {
      when (~io.dmactive) {
        haltedBitRegs(component) := false.B
        resumeReqRegs(component) := false.B
      }.otherwise {
        // Hart Halt Notification Logic
        when (hartHaltedWrEn) {
          when (cfg.hartIdToHartSel(hartHaltedId) === component.U) {
            haltedBitRegs(component) := true.B
          }
        }.elsewhen (hartResumingWrEn) {
          when (cfg.hartIdToHartSel(hartResumingId) === component.U) {
            haltedBitRegs(component) := false.B
          }
        }

        // Hart Resume Req Logic
        // If you request a hart to resume at the same moment
        // it actually does resume, then the request wins.
        // So don't try to write resumereq more than once
        when (hartResumingWrEn) {
          when (cfg.hartIdToHartSel(hartResumingId) === component.U) {
            resumeReqRegs(component) := false.B
          }
        }
        when(resumereq) {
          resumeReqRegs(io.innerCtrl.bits.hartsel) := true.B
        }
      }
    }

    //--------------------------------------------------------------
    // Program Buffer Access (DMI ... System Bus can override)
    //--------------------------------------------------------------
    dmiNode.regmap(
      (DMI_DMSTATUS    << 2) -> Seq(RegField.r(32, DMSTATUSRdData.asUInt(), RegFieldDesc("dmi_dmstatus", ""))),
      //TODO (DMI_CFGSTRADDR0 << 2) -> cfgStrAddrFields,
      (DMI_HARTINFO    << 2) -> Seq(RegField.r(32, HARTINFORdData.asUInt(), RegFieldDesc("dmi_hartinfo", "" /*, reset=Some(HARTINFORdData.litValue)*/))),
      (DMI_HALTSUM     << 2) -> Seq(RegField.r(32, HALTSUMRdData.asUInt(), RegFieldDesc("dmi_haltsum", ""))),
      (DMI_ABSTRACTCS  << 2) -> Seq(RWNotify(32, ABSTRACTCSRdData.asUInt(), ABSTRACTCSWrDataVal, ABSTRACTCSRdEn, ABSTRACTCSWrEnMaybe,
        Some(RegFieldDesc("dmi_abstractcs", "" /*, reset=Some(ABSTRACTCSReset.litValue)*/)))),
      (DMI_ABSTRACTAUTO<< 2) -> Seq(RWNotify(32, ABSTRACTAUTORdData.asUInt(), ABSTRACTAUTOWrDataVal, ABSTRACTAUTORdEn, ABSTRACTAUTOWrEnMaybe,
        Some(RegFieldDesc("dmi_abstractauto", "", reset=Some(0))))),
      (DMI_COMMAND     << 2) -> Seq(RWNotify(32, COMMANDRdData.asUInt(), COMMANDWrDataVal, COMMANDRdEn, COMMANDWrEnMaybe,
        Some(RegFieldDesc("dmi_command", "", reset=Some(0))))),
      (DMI_DATA0       << 2) -> RegFieldGroup("dmi_data", None, abstractDataMem.zipWithIndex.map{case (x, i) => RWNotify(8, x, abstractDataNxt(i),
        dmiAbstractDataRdEn(i),
        dmiAbstractDataWrEnMaybe(i),
        Some(RegFieldDesc(s"dmi_data_$i", "", reset = Some(0))))}),
      (DMI_PROGBUF0    << 2) -> RegFieldGroup("dmi_progbuf", None, programBufferMem.zipWithIndex.map{case (x, i) => RWNotify(8, x, programBufferNxt(i),
        dmiProgramBufferRdEn(i),
        dmiProgramBufferWrEnMaybe(i),
        Some(RegFieldDesc(s"dmi_progbuf_$i", "", reset = Some(0))))}),
      (DMIConsts.dmi_haltStatusAddr << 2) -> RegFieldGroup("dmi_halt_status", None, haltedStatus.zipWithIndex.map{case (x, i) => RegField.r(32, x, RegFieldDesc(s"halt_status_$i", ""))})
    )

    abstractDataMem.zipWithIndex.foreach { case (x, i) =>
      when (dmiAbstractDataWrEnMaybe(i) && dmiAbstractDataAccessLegal) {
        x := abstractDataNxt(i)
      }
    }

    programBufferMem.zipWithIndex.foreach { case (x, i) =>
      when (dmiProgramBufferWrEnMaybe(i) && dmiProgramBufferAccessLegal) {
        x := programBufferNxt(i)
      }
    }

    //--------------------------------------------------------------
    // "Variable" ROM Generation
    //--------------------------------------------------------------

    val goReg        = Reg(Bool())
    val goAbstract   = Wire(init = false.B)
    val jalAbstract  = Wire(init = (new GeneratedUJ()).fromBits(Instructions.JAL.value.U))
    jalAbstract.setImm(ABSTRACT(cfg) - WHERETO)

    when (~io.dmactive){
      goReg := false.B
    }.otherwise {
      when (goAbstract) {
        goReg := true.B
      }.elsewhen (hartGoingWrEn){
        assert(hartGoingId === 0.U, "Unexpected 'GOING' hart.")//Chisel3 #540 %x, expected %x", hartGoingId, 0.U)
        goReg := false.B
      }
    }

    class flagBundle extends Bundle {
      val reserved = UInt(6.W)
      val resume = Bool()
      val go = Bool()
    }

    val flags = Wire(init = Vec.fill(nComponents){new flagBundle().fromBits(0.U)})
    assert ((cfg.hartSelToHartId(selectedHartReg) < 1024.U),
      "HartSel to HartId Mapping is illegal for this Debug Implementation, because HartID must be < 1024 for it to work.");
    flags(cfg.hartSelToHartId(selectedHartReg)).go := goReg
    for (component <- 0 until nComponents) {
      val componentSel = Wire(init = component.U)
      flags(cfg.hartSelToHartId(componentSel)).resume := resumeReqRegs(component)
    }

    //----------------------------
    // Abstract Command Decoding & Generation
    //----------------------------

    val accessRegisterCommandWr  = Wire(init = (new ACCESS_REGISTERFields()).fromBits(COMMANDWrData.asUInt()))
    val accessRegisterCommandReg = Wire(init = (new ACCESS_REGISTERFields()).fromBits(COMMANDReg.asUInt()))

    // TODO: Quick Access

    class GeneratedI extends Bundle {
      val imm    = UInt(12.W)
      val rs1    = UInt(5.W)
      val funct3 = UInt(3.W)
      val rd     = UInt(5.W)
      val opcode = UInt(7.W)
    }

    class GeneratedS extends Bundle {
      val immhi  = UInt(7.W)
      val rs2    = UInt(5.W)
      val rs1    = UInt(5.W)
      val funct3 = UInt(3.W)
      val immlo  = UInt(5.W)
      val opcode = UInt(7.W)
    }

    class GeneratedUJ extends Bundle {
      val imm3    = UInt(1.W)
      val imm0    = UInt(10.W)
      val imm1    = UInt(1.W)
      val imm2    = UInt(8.W)
      val rd      = UInt(5.W)
      val opcode  = UInt(7.W)

      def setImm(imm: Int) : Unit = {
        // TODO: Check bounds of imm.

        require(imm % 2 == 0, "Immediate must be even for UJ encoding.")
        val immWire = Wire(init = imm.S(21.W))
        val immBits = Wire(init = Vec(immWire.toBools))

        imm0 := immBits.slice(1,  1  + 10).asUInt()
        imm1 := immBits.slice(11, 11 + 11).asUInt()
        imm2 := immBits.slice(12, 12 + 8).asUInt()
        imm3 := immBits.slice(20, 20 + 1).asUInt()
      }
    }

    val abstractGeneratedMem = Reg(Vec(2, (UInt(32.W))))
    val abstractGeneratedI = Wire(new GeneratedI())
    val abstractGeneratedS = Wire(new GeneratedS())
    val nop = Wire(new GeneratedI())

    abstractGeneratedI.opcode := ((new GeneratedI()).fromBits(Instructions.LW.value.U)).opcode
    abstractGeneratedI.rd     := (accessRegisterCommandReg.regno & 0x1F.U)
    abstractGeneratedI.funct3 := accessRegisterCommandReg.size
    abstractGeneratedI.rs1    := 0.U
    abstractGeneratedI.imm    := DATA.U

    abstractGeneratedS.opcode := ((new GeneratedS()).fromBits(Instructions.SW.value.U)).opcode
    abstractGeneratedS.immlo  := (DATA & 0x1F).U
    abstractGeneratedS.funct3 := accessRegisterCommandReg.size
    abstractGeneratedS.rs1    := 0.U
    abstractGeneratedS.rs2    := (accessRegisterCommandReg.regno & 0x1F.U)
    abstractGeneratedS.immhi  := (DATA >> 5).U

    nop := ((new GeneratedI()).fromBits(Instructions.ADDI.value.U))
    nop.rd   := 0.U
    nop.rs1  := 0.U
    nop.imm  := 0.U

    when (goAbstract) {
      abstractGeneratedMem(0) := Mux(accessRegisterCommandReg.transfer,
        Mux(accessRegisterCommandReg.write,
          // To write a register, we need to do LW.
          abstractGeneratedI.asUInt(),
          // To read a register, we need to do SW.
          abstractGeneratedS.asUInt()),
        nop.asUInt()
      )
      abstractGeneratedMem(1) := Mux(accessRegisterCommandReg.postexec,
        nop.asUInt(),
        Instructions.EBREAK.value.U)
    }

    //--------------------------------------------------------------
    // System Bus Access
    //--------------------------------------------------------------

    tlNode.regmap(
      // This memory is writable.
      HALTED      -> Seq(WNotifyWire(sbIdWidth, hartHaltedId, hartHaltedWrEn,
        "debug_hart_halted", "Debug ROM Causes hart to write its hartID here when it is in Debug Mode.")),
      GOING       -> Seq(WNotifyWire(sbIdWidth, hartGoingId,  hartGoingWrEn,
        "debug_hart_going", "Debug ROM causes hart to write 0 here when it begins executing Debug Mode instructions.")),
      RESUMING    -> Seq(WNotifyWire(sbIdWidth, hartResumingId,  hartResumingWrEn,
        "debug_hart_resuming", "Debug ROM causes hart to write 0 here when it leaves Debug Mode.")),
      EXCEPTION   -> Seq(WNotifyWire(sbIdWidth, hartExceptionId,  hartExceptionWrEn,
        "debug_hart_exception", "Debug ROM causes hart to write 0 here if it gets an exception in Debug Mode.")),
      DATA        -> RegFieldGroup("debug_data", Some("Data used to communicate with Debug Module"),
        abstractDataMem.zipWithIndex.map {case (x, i) => RegField(8, x, RegFieldDesc(s"debug_data_$i", ""))}),
      PROGBUF(cfg)-> RegFieldGroup("debug_progbuf", Some("Program buffer used to communicate with Debug Module"),
        programBufferMem.zipWithIndex.map {case (x, i) => RegField(8, x, RegFieldDesc(s"debug_progbuf_$i", ""))}),

      // These sections are read-only.
      IMPEBREAK(cfg)-> {if (cfg.hasImplicitEbreak) Seq(RegField.r(32,  Instructions.EBREAK.value.U, RegFieldDesc("debug_impebreak", "Debug Implicit EBREAK"))) else Nil},
      WHERETO       -> Seq(RegField.r(32, jalAbstract.asUInt, RegFieldDesc("debug_whereto", "Instruction filled in by Debug Module to control hart in Debug Mode"))),
      ABSTRACT(cfg) -> RegFieldGroup("debug_abstract", Some("Instructions generated by Debug Module"),
        abstractGeneratedMem.zipWithIndex.map{ case (x,i) => RegField.r(32, x, RegFieldDesc(s"debug_abstract_$i", ""))}),
      FLAGS         -> RegFieldGroup("debug_flags", Some("Memory region used to control hart going/resuming in Debug Mode"),
        flags.zipWithIndex.map{case(x, i) => RegField.r(8, x.asUInt(), RegFieldDesc(s"debug_flags_${i}", ""))}),
      ROMBASE       -> RegFieldGroup("debug_rom", Some("Debug ROM"),
        DebugRomContents().zipWithIndex.map{case (x, i) => RegField.r(8, (x & 0xFF).U(8.W),
          RegFieldDesc(s"debug_rom_$i", "", reset=Some(x)))})
    )

    // Override System Bus accesses with dmactive reset.
    when (~io.dmactive){
      abstractDataMem.foreach  {x => x := 0.U}
      programBufferMem.foreach {x => x := 0.U}
    }

    //--------------------------------------------------------------
    // Abstract Command State Machine
    //--------------------------------------------------------------

    object CtrlState extends scala.Enumeration {
      type CtrlState = Value
      val Waiting, CheckGenerate, Exec = Value

      def apply( t : Value) : UInt = {
        t.id.U(log2Up(values.size).W)
      }
    }
    import CtrlState._

    // This is not an initialization!
    val ctrlStateReg = Reg(CtrlState(Waiting))

    val hartHalted   = haltedBitRegs(selectedHartReg)
    val ctrlStateNxt = Wire(init = ctrlStateReg)

    //------------------------
    // DMI Register Control and Status

    abstractCommandBusy := (ctrlStateReg != CtrlState(Waiting))

    ABSTRACTCSWrEnLegal   := (ctrlStateReg === CtrlState(Waiting))
    COMMANDWrEnLegal      := (ctrlStateReg === CtrlState(Waiting))
    ABSTRACTAUTOWrEnLegal := (ctrlStateReg === CtrlState(Waiting))
    dmiAbstractDataAccessLegal  := (ctrlStateReg === CtrlState(Waiting))
    dmiProgramBufferAccessLegal := (ctrlStateReg === CtrlState(Waiting))

    errorBusy := (ABSTRACTCSWrEnMaybe    && ~ABSTRACTCSWrEnLegal)        ||
                 (ABSTRACTAUTOWrEnMaybe  && ~ABSTRACTAUTOWrEnLegal)      ||
                 (COMMANDWrEnMaybe       && ~COMMANDWrEnLegal)           ||
                 (dmiAbstractDataAccess  && ~dmiAbstractDataAccessLegal) ||
                 (dmiProgramBufferAccess && ~dmiProgramBufferAccessLegal)

    // TODO: Maybe Quick Access
    val commandWrIsAccessRegister = (COMMANDWrData.cmdtype === DebugAbstractCommandType.AccessRegister.id.U)
    val commandRegIsAccessRegister = (COMMANDReg.cmdtype === DebugAbstractCommandType.AccessRegister.id.U)

    val commandWrIsUnsupported = COMMANDWrEn && !commandWrIsAccessRegister;

    val commandRegIsUnsupported = Wire(init = true.B)
    val commandRegBadHaltResume = Wire(init = false.B)
    when (commandRegIsAccessRegister) {
      when (!accessRegisterCommandReg.transfer || (accessRegisterCommandReg.regno >= 0x1000.U && accessRegisterCommandReg.regno <= 0x101F.U)){
        commandRegIsUnsupported := false.B
        commandRegBadHaltResume := ~hartHalted
      }
    }

    val wrAccessRegisterCommand  = COMMANDWrEn && commandWrIsAccessRegister  && (ABSTRACTCSReg.cmderr === 0.U)
    val regAccessRegisterCommand = autoexec    && commandRegIsAccessRegister && (ABSTRACTCSReg.cmderr === 0.U)

    //------------------------
    // Variable ROM STATE MACHINE
    // -----------------------

    when (ctrlStateReg === CtrlState(Waiting)){
      when (wrAccessRegisterCommand || regAccessRegisterCommand) {
        ctrlStateNxt := CtrlState(CheckGenerate)
      }.elsewhen (commandWrIsUnsupported) { // These checks are really on the command type.
        errorUnsupported := true.B
      }.elsewhen (autoexec && commandRegIsUnsupported) {
        errorUnsupported := true.B
      }
    }.elsewhen (ctrlStateReg === CtrlState(CheckGenerate)){

      // We use this state to ensure that the COMMAND has been
      // registered by the time that we need to use it, to avoid
      // generating it directly from the COMMANDWrData.
      // This 'commandRegIsUnsupported' is really just checking the
      // AccessRegisterCommand parameters (regno)
      when (commandRegIsUnsupported) {
        errorUnsupported := true.B
        ctrlStateNxt := CtrlState(Waiting)
      }.elsewhen (commandRegBadHaltResume){
        errorHaltResume := true.B
        ctrlStateNxt := CtrlState(Waiting)
      }.otherwise {
        ctrlStateNxt := CtrlState(Exec)
        goAbstract := true.B
      }
    
    }.elsewhen (ctrlStateReg === CtrlState(Exec)) {

      // We can't just look at 'hartHalted' here, because
      // hartHaltedWrEn is overloaded to mean 'got an ebreak'
      // which may have happened when we were already halted.
      when(goReg === false.B && hartHaltedWrEn && (cfg.hartIdToHartSel(hartHaltedId) === selectedHartReg)){
        ctrlStateNxt := CtrlState(Waiting)
      }
      when(hartExceptionWrEn) {
        assert(hartExceptionId === 0.U, "Unexpected 'EXCEPTION' hart")//Chisel3 #540, %x, expected %x", hartExceptionId, 0.U)
          ctrlStateNxt := CtrlState(Waiting)
        errorException := true.B
      }
    }

    when (~io.dmactive) {
      ctrlStateReg := CtrlState(Waiting)
    }.otherwise {
      ctrlStateReg := ctrlStateNxt
    }
  }
}


// Wrapper around TL Debug Module Inner and an Async DMI Sink interface.
// Handles the synchronization of dmactive, which is used as a synchronous reset
// inside the Inner block.
// Also is the Sink side of hartsel & resumereq fields of DMCONTROL.
class TLDebugModuleInnerAsync(device: Device, getNComponents: () => Int, beatBytes: Int)(implicit p: Parameters) extends LazyModule{

  val dmInner = LazyModule(new TLDebugModuleInner(device, getNComponents, beatBytes))
  val dmiXing = LazyModule(new TLAsyncCrossingSink(depth=1))
  val dmiNode = dmiXing.node
  val tlNode = dmInner.tlNode

  dmInner.dmiNode := dmiXing.node

  lazy val module = new LazyModuleImp(this) {

    val io = IO(new Bundle {
      // These are all asynchronous and come from Outer
      val dmactive = Bool(INPUT)
      val innerCtrl = new AsyncBundle(1, new DebugInternalBundle()).flip
      // This comes from tlClk domain.
      val debugUnavail    = Vec(getNComponents(), Bool()).asInput
      val psd = new PSDTestMode().asInput
    })

    dmInner.module.io.innerCtrl := FromAsyncBundle(io.innerCtrl)
    dmInner.module.io.dmactive := ~ResetCatchAndSync(clock, ~io.dmactive, "dmactiveSync", io.psd)
    dmInner.module.io.debugUnavail := io.debugUnavail
  }
}

/** Create a version of the TLDebugModule which includes a synchronization interface
  * internally for the DMI. This is no longer optional outside of this module
  *  because the Clock must run when tlClock isn't running or tlReset is asserted.
  */

class TLDebugModule(beatBytes: Int)(implicit p: Parameters) extends LazyModule {

  val device = new SimpleDevice("debug-controller", Seq("sifive,debug-013","riscv,debug-013")){
    override val alwaysExtended = true
  }

  val dmOuter = LazyModule(new TLDebugModuleOuterAsync(device)(p))
  val dmInner = LazyModule(new TLDebugModuleInnerAsync(device, () => {dmOuter.dmOuter.intnode.edges.out.size}, beatBytes)(p))

  val node = dmInner.tlNode
  val intnode = dmOuter.intnode

  dmInner.dmiNode := dmOuter.dmiInnerNode

  lazy val module = new LazyModuleImp(this) {
    val nComponents = dmOuter.dmOuter.intnode.edges.out.size

    val io = IO(new Bundle {
      val ctrl = new DebugCtrlBundle(nComponents)
      val dmi = new ClockedDMIIO().flip
      val psd = new PSDTestMode().asInput
    })

    dmOuter.module.io.dmi <> io.dmi.dmi
    dmOuter.module.reset := io.dmi.dmiReset
    dmOuter.module.clock := io.dmi.dmiClock

    dmInner.module.io.innerCtrl    := dmOuter.module.io.innerCtrl
    dmInner.module.io.dmactive     := dmOuter.module.io.ctrl.dmactive
    dmInner.module.io.debugUnavail := io.ctrl.debugUnavail

    dmInner.module.io.psd <> io.psd

    io.ctrl <> dmOuter.module.io.ctrl

  }
}

/** This includes the clock and reset as these are passed through the
  *  hierarchy until the Debug Module is actually instantiated. 
  *  
  */

class ClockedDMIIO(implicit val p: Parameters) extends ParameterizedBundle()(p){
  val dmi      = new DMIIO()(p)
  val dmiClock = Clock(OUTPUT)
  val dmiReset = Bool(OUTPUT)
}

/** Convert DMI to TL. Avoids using special DMI synchronizers and register accesses
  *  
  */

class DMIToTL(implicit p: Parameters) extends LazyModule {

  val node = TLClientNode(Seq(TLClientPortParameters(Seq(TLClientParameters("debug")))))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val dmi = new DMIIO()(p).flip()
    })

    val (tl, edge) = node.out(0)

    val src  = Wire(init = 0.U)
    val addr = Wire(init = (io.dmi.req.bits.addr << 2))
    val size = (log2Ceil(DMIConsts.dmiDataSize / 8)).U

    val (_,  gbits) = edge.Get(src, addr, size)
    val (_, pfbits) = edge.Put(src, addr, size, io.dmi.req.bits.data)

    // We force DMI NOPs to go to CONTROL register because
    // Inner  may be in reset / not have a clock,
    // so we force address to be the one that goes to Outer.
    // Therefore for a NOP we don't really need to pay the penalty to go
    // across the CDC.

    val (_, nbits)  = edge.Put(src, toAddress = (DMI_RegAddrs.DMI_DMCONTROL << 2).U, size, data=0.U, mask = 0.U)

    when (io.dmi.req.bits.op === DMIConsts.dmi_OP_WRITE)       { tl.a.bits := pfbits
    }.elsewhen  (io.dmi.req.bits.op === DMIConsts.dmi_OP_READ) { tl.a.bits := gbits
    }.otherwise {                                                tl.a.bits := nbits
    }

    tl.a.valid       := io.dmi.req.valid
    io.dmi.req.ready := tl.a.ready

    io.dmi.resp.valid      := tl.d.valid
    tl.d.ready             := io.dmi.resp.ready
    io.dmi.resp.bits.resp  := Mux(tl.d.bits.error, DMIConsts.dmi_RESP_FAILURE, DMIConsts.dmi_RESP_SUCCESS)
    io.dmi.resp.bits.data  := tl.d.bits.data

    // Tie off unused channels
    tl.b.ready := false.B
    tl.c.valid := false.B
    tl.e.valid := false.B

  }
}
