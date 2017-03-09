// See LICENSE.SiFive for license details.

package uncore.devices

import Chisel._
import junctions._
import util._
import regmapper._
import tile.XLen
import uncore.tilelink2._
import config._

// *****************************************
// Constants which are interesting even
// outside of this module
// *****************************************

object DbRegAddrs{

  def DMRAMBASE    = UInt(0x0)
  def DMCONTROL    = UInt(0x10)

  def DMINFO       = UInt(0x11)
  def AUTHDATA0    = UInt(0x12)
  def AUTHDATA1    = UInt(0x13)
  def SERDATA      = UInt(0x14)
  def SERSTATUS    = UInt(0x15)
  def SBUSADDRESS0 = UInt(0x16)
  def SBUSADDRESS1 = UInt(0x17)
  def SBDATA0      = UInt(0x18)
  def SBDATA1      = UInt(0x19)
  //1a
  def HALTSUM      = UInt(0x1B)
  //1c - 3b are the halt notification registers.
  def SBADDRESS2    = UInt(0x3d)
  // 3c
  def SBDATA2       = UInt(0x3e)
  def SBDATA3       = UInt(0x3f)
}

/** Constant values used by both Debug Bus Response & Request
  */

object DbBusConsts{

  def dbDataSize = 34

  def dbRamWordBits = 32

  def dbOpSize = 2
  def db_OP_NONE            = UInt("b00")
  def db_OP_READ            = UInt("b01")
  def db_OP_READ_WRITE      = UInt("b10")
  def db_OP_READ_COND_WRITE = UInt("b11")

  def dbRespSize = 2
  def db_RESP_SUCCESS     = UInt("b00")
  def db_RESP_FAILURE     = UInt("b01")
  def db_RESP_HW_FAILURE  = UInt("b10")
  // This is used outside this block
  // to indicate 'busy'.
  def db_RESP_RESERVED    = UInt("b11")

}

object DsbBusConsts {

  def sbAddrWidth = 12
  def sbIdWidth   = 10 

  //These are the default ROM contents, which support RV32 and RV64.
  // See $RISCV/riscv-tools/riscv-isa-sim/debug_rom/debug_rom.h/S
  // The code assumes 64 bytes of Debug RAM.

  def xlenAnyRomContents : Array[Byte] = Array(
    0x6f, 0x00, 0xc0, 0x04, 0x6f, 0x00, 0xc0, 0x00, 0x13, 0x04, 0xf0, 0xff,
    0x6f, 0x00, 0x80, 0x00, 0x13, 0x04, 0x00, 0x00, 0x0f, 0x00, 0xf0, 0x0f,
    0xf3, 0x24, 0x00, 0xf1, 0x63, 0xc6, 0x04, 0x00, 0x83, 0x24, 0xc0, 0x43,
    0x6f, 0x00, 0x80, 0x00, 0x83, 0x34, 0x80, 0x43, 0x23, 0x2e, 0x80, 0x42,
    0x73, 0x24, 0x40, 0xf1, 0x23, 0x20, 0x80, 0x10, 0x73, 0x24, 0x00, 0x7b,
    0x13, 0x74, 0x84, 0x00, 0x63, 0x12, 0x04, 0x04, 0x73, 0x24, 0x20, 0x7b,
    0x73, 0x00, 0x20, 0x7b, 0x73, 0x10, 0x24, 0x7b, 0x73, 0x24, 0x00, 0x7b,
    0x13, 0x74, 0x04, 0x1c, 0x13, 0x04, 0x04, 0xf4, 0x63, 0x1e, 0x04, 0x00,
    0x73, 0x24, 0x00, 0xf1, 0x63, 0x46, 0x04, 0x00, 0x23, 0x2e, 0x90, 0x42,
    0x67, 0x00, 0x00, 0x40, 0x23, 0x3c, 0x90, 0x42, 0x67, 0x00, 0x00, 0x40,
    0x73, 0x24, 0x40, 0xf1, 0x23, 0x26, 0x80, 0x10, 0x73, 0x60, 0x04, 0x7b,
    0x73, 0x24, 0x00, 0x7b, 0x13, 0x74, 0x04, 0x02, 0xe3, 0x0c, 0x04, 0xfe,
    0x6f, 0xf0, 0x1f, 0xfd).map(_.toByte)

  // These ROM contents support only RV32 
  // See $RISCV/riscv-tools/riscv-isa-sim/debug_rom/debug_rom.h/S
  // The code assumes only 28 bytes of Debug RAM.

  def xlen32OnlyRomContents : Array[Byte] = Array(
    0x6f, 0x00, 0xc0, 0x03, 0x6f, 0x00, 0xc0, 0x00, 0x13, 0x04, 0xf0, 0xff,
    0x6f, 0x00, 0x80, 0x00, 0x13, 0x04, 0x00, 0x00, 0x0f, 0x00, 0xf0, 0x0f,
    0x83, 0x24, 0x80, 0x41, 0x23, 0x2c, 0x80, 0x40, 0x73, 0x24, 0x40, 0xf1,
    0x23, 0x20, 0x80, 0x10, 0x73, 0x24, 0x00, 0x7b, 0x13, 0x74, 0x84, 0x00,
    0x63, 0x1a, 0x04, 0x02, 0x73, 0x24, 0x20, 0x7b, 0x73, 0x00, 0x20, 0x7b,
    0x73, 0x10, 0x24, 0x7b, 0x73, 0x24, 0x00, 0x7b, 0x13, 0x74, 0x04, 0x1c,
    0x13, 0x04, 0x04, 0xf4, 0x63, 0x16, 0x04, 0x00, 0x23, 0x2c, 0x90, 0x40,
    0x67, 0x00, 0x00, 0x40, 0x73, 0x24, 0x40, 0xf1, 0x23, 0x26, 0x80, 0x10,
    0x73, 0x60, 0x04, 0x7b, 0x73, 0x24, 0x00, 0x7b, 0x13, 0x74, 0x04, 0x02,
    0xe3, 0x0c, 0x04, 0xfe, 0x6f, 0xf0, 0x1f, 0xfe).map(_.toByte)

  // These ROM contents support only RV64
  // See $RISCV/riscv-tools/riscv-isa-sim/debug_rom/debug_rom.h/S
  // The code assumes 64 bytes of Debug RAM.

  def xlen64OnlyRomContents : Array[Byte] = Array(
    0x6f, 0x00, 0xc0, 0x03, 0x6f, 0x00, 0xc0, 0x00, 0x13, 0x04, 0xf0, 0xff,
    0x6f, 0x00, 0x80, 0x00, 0x13, 0x04, 0x00, 0x00, 0x0f, 0x00, 0xf0, 0x0f,
    0x83, 0x34, 0x80, 0x43, 0x23, 0x2e, 0x80, 0x42, 0x73, 0x24, 0x40, 0xf1,
    0x23, 0x20, 0x80, 0x10, 0x73, 0x24, 0x00, 0x7b, 0x13, 0x74, 0x84, 0x00,
    0x63, 0x1a, 0x04, 0x02, 0x73, 0x24, 0x20, 0x7b, 0x73, 0x00, 0x20, 0x7b,
    0x73, 0x10, 0x24, 0x7b, 0x73, 0x24, 0x00, 0x7b, 0x13, 0x74, 0x04, 0x1c,
    0x13, 0x04, 0x04, 0xf4, 0x63, 0x16, 0x04, 0x00, 0x23, 0x3c, 0x90, 0x42,
    0x67, 0x00, 0x00, 0x40, 0x73, 0x24, 0x40, 0xf1, 0x23, 0x26, 0x80, 0x10,
    0x73, 0x60, 0x04, 0x7b, 0x73, 0x24, 0x00, 0x7b, 0x13, 0x74, 0x04, 0x02,
    0xe3, 0x0c, 0x04, 0xfe, 0x6f, 0xf0, 0x1f, 0xfe).map(_.toByte)
}



object DsbRegAddrs{

  def CLEARDEBINT  = 0x100
  def SETHALTNOT   = 0x10C
  def SERINFO      = 0x110
  def SERBASE      = 0x114
  // For each serial, there are
  // 3 registers starting here:
  // SERSEND0
  // SERRECEIVE0
  // SERSTATUS0
  // ...
  // SERSTATUS7
  def SERTX_OFFSET   = 0
  def SERRX_OFFSET   = 4
  def SERSTAT_OFFSET = 8
  def RAMBASE      = 0x400
  def ROMBASE      = 0x800

}


// *****************************************
// Configuration & Parameters for this Module
// 
// *****************************************

/** Enumerations used both in the hardware
  * and in the configuration specification.
  */

object DebugModuleAuthType extends scala.Enumeration {
  type DebugModuleAuthType = Value
  val None, Password, ChallengeResponse, Reserved = Value
}
import DebugModuleAuthType._

object DebugModuleAccessType extends scala.Enumeration {
  type DebugModuleAccessType = Value
  val Access8Bit, Access16Bit, Access32Bit, Access64Bit, Access128Bit = Value
}
import DebugModuleAccessType._


/** Parameters exposed to the top-level design, set based on
  * external requirements, etc.
  *
  *  This object checks that the parameters conform to the 
  *  full specification. The implementation which receives this
  *  object can perform more checks on what that implementation
  *  actually supports.
  *  nComponents : The number of components to support debugging.
  *  nDebugBusAddrSize : Size of the Debug Bus Address
  *  nDebugRam Bytes: Size of the Debug RAM (depends on the XLEN of the machine).
  *  debugRomContents: Optional Sequence of bytes which form the Debug ROM contents. 
  *  hasBusMaster: Whether or not a bus master should be included
  *    The size of the accesses supported by the Bus Master. 
  *  nSerialPorts : Number of serial ports to instantiate
  *  authType : The Authorization Type
  *  Number of cycles to assert ndreset when pulsed. 
  **/


case class DebugModuleConfig (
  nComponents       : Int,
  nDebugBusAddrSize : Int,
  nDebugRamBytes    : Int,
  debugRomContents  : Option[Seq[Byte]],
  hasBusMaster : Boolean,
  hasAccess128 : Boolean,
  hasAccess64  : Boolean,
  hasAccess32  : Boolean,
  hasAccess16  : Boolean,
  hasAccess8   : Boolean,
  nSerialPorts : Int,
  authType : DebugModuleAuthType,
  nNDResetCycles : Int
) {

  if (hasBusMaster == false){
    require (hasAccess128 == false)
    require (hasAccess64  == false)
    require (hasAccess32  == false)
    require (hasAccess16  == false)
    require (hasAccess8   == false)
  }

  require (nSerialPorts <= 8)

  require ((nDebugBusAddrSize >= 5) && (nDebugBusAddrSize <= 7))

  private val maxComponents = nDebugBusAddrSize match {
    case 5 => (32*4)
    case 6 => (32*32)
    case 7 => (32*32)
  }
  require (nComponents > 0 && nComponents <= maxComponents)

  private val maxRam = nDebugBusAddrSize match {
    case 5 => (4 * 16)
    case 6 => (4 * 16)
    case 7 => (4 * 64)
  }

  require (nDebugRamBytes > 0 && nDebugRamBytes <= maxRam)

  val hasHaltSum = (nComponents > 64) || (nSerialPorts > 0)

  val hasDebugRom = debugRomContents.nonEmpty

  if (hasDebugRom) {
    require (debugRomContents.get.size > 0)
    require (debugRomContents.get.size <= 512)
  }

  require (nNDResetCycles > 0)

}

class DefaultDebugModuleConfig (val ncomponents : Int, val xlen:Int)
    extends DebugModuleConfig(
      nComponents = ncomponents,
      nDebugBusAddrSize = 5,
      // While smaller numbers are theoretically
      // possible as noted in the Spec,
      // the ROM image would need to be
      // adjusted accordingly.
      nDebugRamBytes = xlen match{
        case 32  => 28 
        case 64  => 64
        case 128 => 64
      },
      debugRomContents = xlen match {
        case 32  => Some(DsbBusConsts.xlen32OnlyRomContents)
        case 64  => Some(DsbBusConsts.xlen64OnlyRomContents)
      },
      hasBusMaster = false,
      hasAccess128 = false, 
      hasAccess64 = false, 
      hasAccess32 = false, 
      hasAccess16 = false, 
      hasAccess8 = false, 
      nSerialPorts = 0,
      authType = DebugModuleAuthType.None,
      nNDResetCycles = 1)

case object DMKey extends Field[DebugModuleConfig]

// *****************************************
// Module Interfaces
// 
// *****************************************


/** Structure to define the contents of a Debug Bus Request
  */

class DebugBusReq(addrBits : Int) extends Bundle {
  val addr = UInt(width = addrBits)
  val data = UInt(width = DbBusConsts.dbDataSize)
  val op   = UInt(width = DbBusConsts.dbOpSize)

  override def cloneType = new DebugBusReq(addrBits).asInstanceOf[this.type]
}


/** Structure to define the contents of a Debug Bus Response
  */
class DebugBusResp( ) extends Bundle {
  val data = UInt(width = DbBusConsts.dbDataSize)
  val resp = UInt(width = DbBusConsts.dbRespSize)

}

/** Structure to define the top-level DebugBus interface 
  *  of DebugModule.
  *  DebugModule is the consumer of this interface.
  *  Therefore it has the 'flipped' version of this.
  */

class DebugBusIO(implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val req = new  DecoupledIO(new DebugBusReq(p(DMKey).nDebugBusAddrSize))
  val resp = new DecoupledIO(new DebugBusResp).flip()
}

class AsyncDebugBusIO(implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val req  = new AsyncBundle(1, new DebugBusReq(p(DMKey).nDebugBusAddrSize))
  val resp = new AsyncBundle(1, new DebugBusResp).flip
}

object FromAsyncDebugBus
{
  def apply(x: AsyncDebugBusIO) = {
    val out = Wire(new DebugBusIO()(x.p))
    out.req <> FromAsyncBundle(x.req)
    x.resp <> ToAsyncBundle(out.resp, 1)
    out
  }
}

object ToAsyncDebugBus
{
  def apply(x: DebugBusIO) = {
    val out = Wire(new AsyncDebugBusIO()(x.p))
    out.req <> ToAsyncBundle(x.req, 1)
    x.resp <> FromAsyncBundle(out.resp)
    out
  }
}

trait HasDebugModuleParameters {
  implicit val p: Parameters
  val cfg = p(DMKey)
}

/** Debug Module I/O, with the exclusion of the RegisterRouter
  *  Access interface.
  */

trait DebugModuleBundle extends Bundle with HasDebugModuleParameters {
  val db = new DebugBusIO()(p).flip()
  val debugInterrupts = Vec(cfg.nComponents, Bool()).asOutput
  val ndreset    = Bool(OUTPUT)
  val fullreset  = Bool(OUTPUT)
}


// *****************************************
// The Module 
// 
// *****************************************

/** Parameterized version of the Debug Module defined in the
  *  RISC-V Debug Specification 
  *  
  *  DebugModule is a slave to two masters:
  *    The Debug Bus -- implemented as a generic Decoupled IO with request
  *                   and response channels
  *    The System Bus -- implemented as generic RegisterRouter
  *  
  *  DebugModule is responsible for holding registers, RAM, and ROM
  *      to support debug interactions, as well as driving interrupts
  *      to a configurable number of components in the system.
  *      It is also responsible for some reset lines.
  */

trait DebugModule extends Module with HasDebugModuleParameters with HasRegMap {

  val io: DebugModuleBundle

  //--------------------------------------------------------------
  // Import constants for shorter variable names
  //--------------------------------------------------------------

  import DbRegAddrs._
  import DsbRegAddrs._
  import DsbBusConsts._
  import DbBusConsts._

  //--------------------------------------------------------------
  // Sanity Check Configuration For this implementation.
  //--------------------------------------------------------------

  require (cfg.nComponents <= 128)
  require (cfg.nSerialPorts == 0)
  require (cfg.hasBusMaster == false)
  require (cfg.nDebugRamBytes <= 64)
  require (cfg.authType == DebugModuleAuthType.None)
  require((DbBusConsts.dbRamWordBits % 8) == 0)

  //--------------------------------------------------------------
  // Private Classes (Register Fields)
  //--------------------------------------------------------------

  class RAMFields() extends Bundle {
    val interrupt     = Bool()
    val haltnot       = Bool()
    val data          = Bits(width = 32)

    override def cloneType = new RAMFields().asInstanceOf[this.type]    
  }

  class CONTROLFields() extends Bundle {
    val interrupt     = Bool()
    val haltnot       = Bool()
    val reserved0     = Bits(width = 31-22 + 1)
    val buserror      = Bits(width = 3)
    val serial        = Bits(width = 3)
    val autoincrement = Bool()
    val access        = UInt(width = 3) 
    val hartid        = Bits(width = 10)
    val ndreset       = Bool()         
    val fullreset     = Bool()

    override def cloneType = new CONTROLFields().asInstanceOf[this.type]
    
  }

  class DMINFOFields() extends Bundle {
    val reserved0     = Bits(width = 2)
    val abussize      = UInt(width = 7)
    val serialcount   = UInt(width = 4)
    val access128     = Bool()
    val access64      = Bool()
    val access32      = Bool()
    val access16      = Bool()
    val accesss8      = Bool()
    val dramsize      = UInt(width = 6)
    val haltsum       = Bool()
    val reserved1     = Bits(width = 3)
    val authenticated = Bool()
    val authbusy      = Bool()
    val authtype      = UInt(width = 2)
    val version       = UInt(width = 2)

    override def cloneType = new DMINFOFields().asInstanceOf[this.type]
    
  }

  class HALTSUMFields() extends Bundle {
    val serialfull     = Bool()
    val serialvalid    = Bool()
    val acks           = Bits(width = 32)

    override def cloneType = new HALTSUMFields().asInstanceOf[this.type]
    
  }


  //--------------------------------------------------------------
  // Register & Wire Declarations
  //--------------------------------------------------------------

  // --- Debug Bus Registers
  val CONTROLReset = Wire(new CONTROLFields())
  val CONTROLWrEn = Wire(Bool())
  val CONTROLReg = Reg(new CONTROLFields())
  val CONTROLWrData = Wire (new CONTROLFields())
  val CONTROLRdData = Wire (new CONTROLFields())
  val ndresetCtrReg = Reg(UInt(cfg.nNDResetCycles))

  val DMINFORdData = Wire (new DMINFOFields())

  val HALTSUMRdData = Wire (new HALTSUMFields())

  val RAMWrData = Wire (new RAMFields())
  val RAMRdData = Wire (new RAMFields())

  // --- System Bus Registers
  
  val SETHALTNOTWrEn = Wire(Bool())
  val SETHALTNOTWrData = Wire(UInt(width = sbIdWidth))
  val CLEARDEBINTWrEn = Wire(Bool())
  val CLEARDEBINTWrData = Wire(UInt(width = sbIdWidth))

  // --- Interrupt & Halt Notification Registers

  val interruptRegs = Reg(init=Vec.fill(cfg.nComponents){Bool(false)})

  val haltnotRegs     = Reg(init=Vec.fill(cfg.nComponents){Bool(false)})
  val numHaltnotStatus = ((cfg.nComponents - 1) / 32) + 1

  val haltnotStatus = Wire(Vec(numHaltnotStatus, Bits(width = 32)))
  val rdHaltnotStatus = Wire(Bits(width = 32))

  val haltnotSummary = Cat(haltnotStatus.map(_.orR).reverse)

  // --- Debug RAM

  val ramDataWidth    = DbBusConsts.dbRamWordBits
  val ramDataBytes    = ramDataWidth / 8;
  val ramAddrWidth    = log2Up(cfg.nDebugRamBytes / ramDataBytes)

  val ramMem    = Reg(init = Vec.fill(cfg.nDebugRamBytes){UInt(0, width = 8)})

  val dbRamAddr   = Wire(UInt(width=ramAddrWidth))
  val dbRamAddrValid   = Wire(Bool())
  val dbRamRdData = Wire (UInt(width=ramDataWidth))
  val dbRamWrData = Wire(UInt(width=ramDataWidth))
  val dbRamWrEn   = Wire(Bool())
  val dbRamRdEn   = Wire(Bool())
  val dbRamWrEnFinal   = Wire(Bool())
  val dbRamRdEnFinal   = Wire(Bool())
  require((cfg.nDebugRamBytes % ramDataBytes) == 0)
  val dbRamDataOffset = log2Up(ramDataBytes)


  // --- Debug Bus Accesses

  val dbRdEn   = Wire(Bool())
  val dbWrEn   = Wire(Bool())
  val dbRdData = Wire(UInt(width = DbBusConsts.dbDataSize))

  val s_DB_READY :: s_DB_RESP :: Nil = Enum(Bits(), 2)
  val dbStateReg = Reg(init = s_DB_READY)

  val dbResult  = Wire(io.db.resp.bits)

  val dbReq     = Wire(io.db.req.bits)
  val dbRespReg = Reg(io.db.resp.bits) 

  val rdCondWrFailure = Wire(Bool())
  val dbWrNeeded = Wire(Bool())

  //--------------------------------------------------------------
  // Interrupt Registers
  //--------------------------------------------------------------
  
  for (component <- 0 until cfg.nComponents) {
    io.debugInterrupts(component) := interruptRegs(component)
  }

  // Interrupt Registers are written by write to CONTROL or debugRAM addresses
  // for Debug Bus, and cleared by writes to CLEARDEBINT by System Bus.
  // It is "unspecified" what should happen if both
  // SET and CLEAR happen at the same time. In this
  // implementation, the SET wins.

  for (component <- 0 until cfg.nComponents) {
    when (CONTROLWrEn) {
      when (CONTROLWrData.hartid === UInt(component)) {
        interruptRegs(component) := interruptRegs(component) | CONTROLWrData.interrupt
      }
    }.elsewhen (dbRamWrEn) {
      when (CONTROLReg.hartid === UInt(component)){
        interruptRegs(component) := interruptRegs(component)  | RAMWrData.interrupt
      }
    }.elsewhen (CLEARDEBINTWrEn){
      when (CLEARDEBINTWrData === UInt(component, width = sbIdWidth)) {
        interruptRegs(component) := Bool(false)
      }
    }
  }

  //--------------------------------------------------------------
  // Halt Notification Registers
  //--------------------------------------------------------------

  // Halt Notifications Registers are cleared by zero write to CONTROL or debugRAM addresses
  // for Debug Bus, and set by write to SETHALTNOT by System Bus.
  // It is "unspecified" what should happen if both
  // SET and CLEAR happen at the same time. In this
  // implementation, the SET wins.
  
  for (component <- 0 until cfg.nComponents) {
    when (SETHALTNOTWrEn){
      when (SETHALTNOTWrData === UInt(component, width = sbIdWidth)) {
        haltnotRegs(component) := Bool(true)
      }
    } .elsewhen (CONTROLWrEn) {
      when (CONTROLWrData.hartid === UInt(component)) {
        haltnotRegs(component) := haltnotRegs(component) &  CONTROLWrData.haltnot
      }
    }.elsewhen (dbRamWrEn) {
      when (CONTROLReg.hartid === UInt(component)){
        haltnotRegs(component) := haltnotRegs(component)  &  RAMWrData.haltnot
      }
    }
  }

  for (ii <- 0 until numHaltnotStatus) {
    haltnotStatus(ii) := Cat(haltnotRegs.slice(ii * 32, (ii + 1) * 32).reverse)
  }

  //--------------------------------------------------------------
  // Other Registers 
  //--------------------------------------------------------------

  CONTROLReset.interrupt     := Bool(false) 
  CONTROLReset.haltnot       := Bool(false) 
  CONTROLReset.reserved0     := Bits(0)     
  CONTROLReset.buserror      := Bits(0)
  CONTROLReset.serial        := Bits(0)
  CONTROLReset.autoincrement := Bool(false)
  CONTROLReset.access        := UInt(DebugModuleAccessType.Access32Bit.id)
  CONTROLReset.hartid        := Bits(0)
  CONTROLReset.ndreset       := Bool(false)
  CONTROLReset.fullreset     := Bool(false)

  // Because this version of DebugModule doesn't
  // support authentication, this entire register is
  // Read-Only constant wires. 
  DMINFORdData.reserved0     := Bits(0)
  DMINFORdData.abussize      := UInt(0) // Not Implemented.
  DMINFORdData.serialcount   := UInt(cfg.nSerialPorts)
  DMINFORdData.access128     := Bool(cfg.hasAccess128)
  DMINFORdData.access64      := Bool(cfg.hasAccess64)
  DMINFORdData.access32      := Bool(cfg.hasAccess32)
  DMINFORdData.access16      := Bool(cfg.hasAccess16)
  DMINFORdData.accesss8      := Bool(cfg.hasAccess8)
  DMINFORdData.dramsize      := Bits((cfg.nDebugRamBytes >> 2) - 1) // Size in 32-bit words minus 1.
  DMINFORdData.haltsum       := Bool(cfg.hasHaltSum)
  DMINFORdData.reserved1     := Bits(0)
  DMINFORdData.authenticated := Bool(true)  // Not Implemented.
  DMINFORdData.authbusy      := Bool(false) // Not Implemented.
  DMINFORdData.authtype      := UInt(cfg.authType.id)
  DMINFORdData.version       := UInt(1)     // Conforms to RISC-V Debug Spec

  HALTSUMRdData.serialfull  := Bool(false) // Not Implemented
  HALTSUMRdData.serialvalid := Bool(false) // Not Implemented
  HALTSUMRdData.acks        := haltnotSummary

  //--------------------------------------------------------------
  // Debug RAM Access (Debug Bus ... System Bus can override)
  //--------------------------------------------------------------

  dbReq := io.db.req.bits
  // Debug Bus RAM Access
  // From Specification: Debug RAM is 0x00 - 0x0F
  //                                  0x40 - 0x6F Not Implemented
  dbRamAddr   := dbReq.addr( ramAddrWidth-1 , 0)
  dbRamWrData := dbReq.data
  dbRamAddrValid := (dbReq.addr(3,0) <= UInt((cfg.nDebugRamBytes/ramDataBytes)))

  val dbRamRdDataFields = List.tabulate(cfg.nDebugRamBytes / ramDataBytes) { ii =>
    val slice = ramMem.slice(ii * ramDataBytes, (ii+1)*ramDataBytes)
    slice.reduce[UInt]{ case (x: UInt, y: UInt) => Cat(y, x)}
  }

  dbRamRdData := dbRamRdDataFields(dbRamAddr)

  when (dbRamWrEnFinal) {
    for (ii <- 0 until ramDataBytes) {
      ramMem((dbRamAddr << UInt(dbRamDataOffset)) + UInt(ii)) := dbRamWrData((8*(ii+1)-1), (8*ii))
    }
  }
  
  //--------------------------------------------------------------
  // Debug Bus Access
  //--------------------------------------------------------------

  // 0x00 - 0x0F Debug RAM
  // 0x10 - 0x1B Registers
  // 0x1C - 0x3B Halt Notification Registers
  // 0x3C - 0x3F Registers
  // 0x40 - 0x6F Debug RAM


  // -----------------------------------------
  // DB Access Write Decoder

  CONTROLWrData := new CONTROLFields().fromBits(dbReq.data)
  RAMWrData     := new RAMFields().fromBits(dbReq.data)

  dbRamWrEn       := Bool(false)
  dbRamWrEnFinal  := Bool(false)
  CONTROLWrEn     := Bool(false)
  when ((dbReq.addr >> 4) === Bits(0)) {  // 0x00 - 0x0F Debug RAM
    dbRamWrEn := dbWrEn
    when (dbRamAddrValid) {
      dbRamWrEnFinal := dbWrEn
    }
  }.elsewhen (dbReq.addr === DMCONTROL) {
    CONTROLWrEn  := dbWrEn
  }.otherwise {
    //Other registers/RAM are Not Implemented.
  }

  when (reset) {
    CONTROLReg := CONTROLReset
    ndresetCtrReg := UInt(0)
  }.elsewhen (CONTROLWrEn) {
    // interrupt handled in other logic
    // haltnot   handled in other logic
    if (cfg.hasBusMaster){
      // buserror is set 'until 0 is written to any bit in this field'. 
      CONTROLReg.buserror      := Mux(CONTROLWrData.buserror.andR, CONTROLReg.buserror, UInt(0))
      CONTROLReg.autoincrement := CONTROLWrData.autoincrement
      CONTROLReg.access        := CONTROLWrData.access
    }
    if (cfg.nSerialPorts > 0){
      CONTROLReg.serial        := CONTROLWrData.serial
    }
    CONTROLReg.hartid        := CONTROLWrData.hartid
    CONTROLReg.fullreset     := CONTROLReg.fullreset | CONTROLWrData.fullreset
    when (CONTROLWrData.ndreset){
      ndresetCtrReg := UInt(cfg.nNDResetCycles)
    }.otherwise {
      ndresetCtrReg := Mux(ndresetCtrReg === UInt(0) , UInt(0), ndresetCtrReg - UInt(1)) 
    }
  }.otherwise {
    ndresetCtrReg := Mux(ndresetCtrReg === UInt(0) , UInt(0), ndresetCtrReg - UInt(1))
  }

  // -----------------------------------------
  // DB Access Read Mux
 
  CONTROLRdData := CONTROLReg;
  CONTROLRdData.interrupt := interruptRegs(CONTROLReg.hartid)
  CONTROLRdData.haltnot   := haltnotRegs(CONTROLReg.hartid)
  CONTROLRdData.ndreset   := ndresetCtrReg.orR

  RAMRdData.interrupt := interruptRegs(CONTROLReg.hartid)
  RAMRdData.haltnot   := haltnotRegs(CONTROLReg.hartid)
  RAMRdData.data      := dbRamRdData

  dbRdData := UInt(0)

  // Higher numbers of numHaltnotStatus Not Implemented.
  // This logic assumes only up to 128 components.
  rdHaltnotStatus := Bits(0)
  for (ii <- 0 until numHaltnotStatus) {
    when (dbReq.addr(1, 0) === UInt(ii)) {
      rdHaltnotStatus := haltnotStatus(ii)
    }
  }

  dbRamRdEn      := Bool(false)
  dbRamRdEnFinal := Bool(false)
  when ((dbReq.addr >> 4) === Bits(0)) { // 0x00 - 0x0F Debug RAM
    dbRamRdEn := dbRdEn
    when (dbRamAddrValid) {
      dbRdData  := RAMRdData.asUInt
      dbRamRdEnFinal := dbRdEn
    }
  }.elsewhen (dbReq.addr === DMCONTROL) {
    dbRdData := CONTROLRdData.asUInt
  }.elsewhen (dbReq.addr === DMINFO) {
    dbRdData := DMINFORdData.asUInt
  }.elsewhen (dbReq.addr === HALTSUM) {
    if (cfg.hasHaltSum){
      dbRdData := HALTSUMRdData.asUInt
    } else {
      dbRdData := UInt(0)
    }
  }.elsewhen ((dbReq.addr >> 2) === UInt(7)) { // 0x1C - 0x1F Haltnot
    dbRdData := rdHaltnotStatus
  } .otherwise {
    //These Registers are not implemented in this version of DebugModule:
    // AUTHDATA0
    // AUTHDATA1
    // SERDATA
    // SERSTATUS
    // SBUSADDRESS0
    // SBUSADDRESS1
    // SBDATA0     
    // SBDATA1     
    // SBADDRESS2  
    // SBDATA2     
    // SBDATA3
    // 0x20 - 0x3B haltnot
    // Upper bytes of Debug RAM.
    dbRdData := UInt(0)
  }
    
  // Conditional write fails if MSB is set of the read data.
  rdCondWrFailure := dbRdData(dbDataSize - 1 ) &&
  (dbReq.op === db_OP_READ_COND_WRITE)

  dbWrNeeded := (dbReq.op === db_OP_READ_WRITE) ||
  ((dbReq.op === db_OP_READ_COND_WRITE) && ~rdCondWrFailure)

  // This is only relevant at end of s_DB_READ.
  dbResult.resp := Mux(rdCondWrFailure,
    db_RESP_FAILURE,
    db_RESP_SUCCESS)
  dbResult.data := dbRdData

  // -----------------------------------------
  // DB Access State Machine Decode (Combo)
  io.db.req.ready := (dbStateReg === s_DB_READY) ||
  (dbStateReg === s_DB_RESP && io.db.resp.fire())

  io.db.resp.valid := (dbStateReg === s_DB_RESP)
  io.db.resp.bits  := dbRespReg

  dbRdEn := io.db.req.fire()
  dbWrEn := dbWrNeeded && io.db.req.fire()

  // -----------------------------------------
  // DB Access State Machine Update (Seq)

  when (dbStateReg === s_DB_READY){
    when (io.db.req.fire()){
      dbStateReg := s_DB_RESP
      dbRespReg := dbResult
    }
  } .elsewhen (dbStateReg === s_DB_RESP){
    when (io.db.req.fire()){
      dbStateReg := s_DB_RESP
      dbRespReg := dbResult
    }.elsewhen (io.db.resp.fire()){
      dbStateReg := s_DB_READY
    }
  }

  
  //--------------------------------------------------------------
  // Debug ROM
  //--------------------------------------------------------------

  val romRegFields  =  if (cfg.hasDebugRom) {
    cfg.debugRomContents.get.map( x => RegField.r(8, UInt(x.toInt & 0xFF)))
  } else {
    Seq(RegField(8))
  }

  //--------------------------------------------------------------
  // System Bus Access
  //--------------------------------------------------------------

  // Local reg mapper function : Notify when written, but give the value.
  def wValue (n: Int, value: UInt, set: Bool) : RegField = {
    RegField(n, value, RegWriteFn((valid, data) => {set := valid ; value := data; Bool(true)}))
  }

  regmap(
    CLEARDEBINT -> Seq(wValue(sbIdWidth, CLEARDEBINTWrData, CLEARDEBINTWrEn)),
    SETHALTNOT  -> Seq(wValue(sbIdWidth, SETHALTNOTWrData, SETHALTNOTWrEn)),
    RAMBASE     -> ramMem.map(x => RegField(8, x)),
    ROMBASE     -> romRegFields
  )

  //--------------------------------------------------------------
  // Misc. Outputs
  //--------------------------------------------------------------

  io.ndreset   := ndresetCtrReg.orR
  io.fullreset := CONTROLReg.fullreset

}

/** Create a concrete TL2 Slave for the DebugModule RegMapper interface.
  *  
  */

class TLDebugModule(address: BigInt = 0)(implicit p: Parameters)
  extends TLRegisterRouter(address, "debug", Nil, beatBytes=p(XLen)/8, executable=true)(
  new TLRegBundle((), _ )    with DebugModuleBundle)(
  new TLRegModule((), _, _)  with DebugModule)


/** Synchronizers for DebugBus
  *  
  */


object AsyncDebugBusCrossing {
  // takes from_source from the 'from' clock domain to the 'to' clock domain
  def apply(from_clock: Clock, from_reset: Bool, from_source: DebugBusIO, to_clock: Clock, to_reset: Bool, depth: Int = 1, sync: Int = 3) = {
    val to_sink = Wire(new DebugBusIO()(from_source.p))
    to_sink.req <> AsyncDecoupledCrossing(from_clock, from_reset, from_source.req, to_clock, to_reset, depth, sync)
    from_source.resp <> AsyncDecoupledCrossing(to_clock, to_reset, to_sink.resp, from_clock, from_reset, depth, sync)
    to_sink // is now to_source
  }
}


object AsyncDebugBusFrom { // OutsideClockDomain
  // takes from_source from the 'from' clock domain and puts it into your clock domain
  def apply(from_clock: Clock, from_reset: Bool, from_source: DebugBusIO, depth: Int = 1, sync: Int = 3): DebugBusIO = {
    val scope = AsyncScope()
    AsyncDebugBusCrossing(from_clock, from_reset, from_source, scope.clock, scope.reset, depth, sync)
  }
}

object AsyncDebugBusTo { // OutsideClockDomain
  // takes source from your clock domain and puts it into the 'to' clock domain
  def apply(to_clock: Clock, to_reset: Bool, source: DebugBusIO, depth: Int = 1, sync: Int = 3): DebugBusIO = {
    val scope = AsyncScope()
    AsyncDebugBusCrossing(scope.clock, scope.reset, source, to_clock, to_reset, depth, sync)
  }
}
