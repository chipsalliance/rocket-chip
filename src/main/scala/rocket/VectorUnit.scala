package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._

case class RocketCoreVectorParams(
  build: Parameters => RocketVectorUnit,
  vLen: Int,
  eLen: Int,
  vfLen: Int,
  vfh: Boolean,
  vMemDataBits: Int,
  decoder: Parameters => RocketVectorDecoder,
  useDCache: Boolean,
  issueVConfig: Boolean,
  vExts: Seq[String]
)

class VectorCoreIO(implicit p: Parameters) extends CoreBundle()(p) {
  val status = Input(new MStatus)
  val ex = new Bundle {
    val valid = Input(Bool())
    val ready = Output(Bool())
    val inst = Input(UInt(32.W))
    val pc = Input(UInt(vaddrBitsExtended.W))
    val vconfig = Input(new VConfig)
    val vstart = Input(UInt(log2Ceil(maxVLMax).W))
    val rs1 = Input(UInt(xLen.W))
    val rs2 = Input(UInt(xLen.W))
  }
  val killm = Input(Bool())
  val mem = new Bundle {
    val frs1 = Input(UInt(fLen.W))
    val block_mem = Output(Bool())
    val block_all = Output(Bool())
  }

  val wb = new Bundle {
    val store_pending = Input(Bool())
    val replay = Output(Bool())
    val retire = Output(Bool())
    val inst = Output(UInt(32.W))
    val rob_should_wb = Output(Bool()) // debug
    val rob_should_wb_fp = Output(Bool()) // debug
    val pc = Output(UInt(vaddrBitsExtended.W))
    val xcpt = Output(Bool())
    val cause = Output(UInt(log2Ceil(Causes.all.max).W))
    val tval = Output(UInt(coreMaxAddrBits.W))
    val vxrm = Input(UInt(2.W))
    val frm = Input(UInt(3.W))
  }

  val resp = Decoupled(new Bundle {
    val fp = Bool()
    val size = UInt(2.W)
    val rd = UInt(5.W)
    val data = UInt((xLen max fLen).W)
  })

  val set_vstart = Valid(UInt(log2Ceil(maxVLMax).W))
  val set_vxsat = Output(Bool())
  val set_vconfig = Valid(new VConfig)
  val set_fflags = Valid(UInt(5.W))

  val trap_check_busy = Output(Bool())
  val backend_busy = Output(Bool())
}

abstract class RocketVectorUnit(implicit p: Parameters) extends LazyModule {
  val module: RocketVectorUnitModuleImp
  val tlNode: TLNode = TLIdentityNode()
  val atlNode: TLNode = TLIdentityNode()
}

class RocketVectorUnitModuleImp(outer: RocketVectorUnit) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val core = new VectorCoreIO
    val tlb = Flipped(new DCacheTLBPort)
    val dmem = new HellaCacheIO

    val fp_req = Decoupled(new FPInput())
    val fp_resp = Flipped(Decoupled(new FPResult()))
  })
}

abstract class RocketVectorDecoder(implicit p: Parameters) extends CoreModule()(p) {
  val io = IO(new Bundle {
    val inst = Input(UInt(32.W))
    val vconfig = Input(new VConfig)
    val vector = Output(Bool()) // this is a vector instruction
    val legal = Output(Bool()) // this is a legal vector instruction given vconfig
    val fp = Output(Bool())
    val read_rs1 = Output(Bool())
    val read_rs2 = Output(Bool())
    val read_frs1 = Output(Bool())
    val write_rd = Output(Bool())
    val write_frd = Output(Bool())
  })
}
