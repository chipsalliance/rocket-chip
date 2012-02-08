package Top

import Chisel._
import Node._;
import Constants._
import Instructions._

class rocketFPUDecoder extends Component
{
  val io = new Bundle {
    val inst = Bits(32, INPUT)
    val valid = Bool(OUTPUT)
    val wen = Bool(OUTPUT)
    val ren1 = Bool(OUTPUT)
    val ren2 = Bool(OUTPUT)
    val ren3 = Bool(OUTPUT)
  }
//  val fp =
//  ListLookup(io.dpath.inst,
//    List(FPU_N, FPU_N, FPU_N, FPU_N, FPU_N),
//     Array(
//        FMOVZ -> List(Bool(true)),
//        FMOVN -> List(Bool(true)),
//        FADD_S -> List(Bool(true)),
//        FSUB_S -> List(Bool(true)),
//        FMUL_S -> List(Bool(true)),
//        FDIV_S -> List(Bool(true)),
//        FSQRT_S -> List(Bool(true)),
//        FSGNJ_S -> List(Bool(true)),
//        FSGNJN_S -> List(Bool(true)),
//        FSGNJX_S -> List(Bool(true)),
//        FADD_D -> List(Bool(true)),
//        FSUB_D -> List(Bool(true)),
//        FMUL_D -> List(Bool(true)),
//        FDIV_D -> List(Bool(true)),
//        FSQRT_D -> List(Bool(true)),
//        FSGNJ_D -> List(Bool(true)),
//        FSGNJN_D -> List(Bool(true)),
//        FSGNJX_D -> List(Bool(true)),
//        FCVT_L_S -> List(Bool(true)),
//        FCVT_LU_S -> List(Bool(true)),
//        FCVT_W_S -> List(Bool(true)),
//        FCVT_WU_S -> List(Bool(true)),
//        FCVT_L_D -> List(Bool(true)),
//        FCVT_LU_D -> List(Bool(true)),
//        FCVT_W_D -> List(Bool(true)),
//        FCVT_WU_D -> List(Bool(true)),
//        FCVT_S_L -> List(Bool(true)),
//        FCVT_S_LU -> List(Bool(true)),
//        FCVT_S_W -> List(Bool(true)),
//        FCVT_S_WU -> List(Bool(true)),
//        FCVT_D_L -> List(Bool(true)),
//        FCVT_D_LU -> List(Bool(true)),
//        FCVT_D_W -> List(Bool(true)),
//        FCVT_D_WU -> List(Bool(true)),
//        FCVT_S_D -> List(Bool(true)),
//        FCVT_D_S -> List(Bool(true)),
//        FEQ_S -> List(Bool(true)),
//        FLT_S -> List(Bool(true)),
//        FLE_S -> List(Bool(true)),
//        FEQ_D -> List(Bool(true)),
//        FLT_D -> List(Bool(true)),
//        FLE_D -> List(Bool(true)),
//        FMIN_S -> List(Bool(true)),
//        FMAX_S -> List(Bool(true)),
//        FMIN_D -> List(Bool(true)),
//        FMAX_D -> List(Bool(true)),
//        MFTX_S -> List(Bool(true)),
//        MFTX_D -> List(Bool(true)),
//        MFFSR -> List(Bool(true)),
//        MXTF_S -> List(Bool(true)),
//        MXTF_D -> List(Bool(true)),
//        MTFSR -> List(Bool(true)),
//        FLW -> List(FPU_Y, FPU_Y, FPU_N, FPU_N, FPU_N),
//        FLD -> List(FPU_Y, FPU_Y, FPU_N, FPU_N, FPU_N),
//        FSW -> List(FPU_Y, FPU_N, FPU_N, FPU_Y, FPU_N),
//        FSD -> List(FPU_Y, FPU_N, FPU_N, FPU_Y, FPU_N)
//        FMADD_S -> List(Bool(true)),
//        FMSUB_S -> List(Bool(true)),
//        FNMSUB_S -> List(Bool(true)),
//        FNMADD_S -> List(Bool(true)),
//        FMADD_D -> List(Bool(true)),
//        FMSUB_D -> List(Bool(true)),
//        FNMSUB_D -> List(Bool(true)),
//        FNMADD_D -> List(Bool(true))
//     ));

  val N = Bool(false)
  val Y = Bool(true)
  val decoder = ListLookup(io.inst,
    List                  (N, N, N, N, N),
    Array(FLW      -> List(Y, Y, N, N, N),
          FLD      -> List(Y, Y, N, N, N),
          FSW      -> List(Y, N, N, Y, N),
          FSD      -> List(Y, N, N, Y, N)))
  val valid :: wen :: ren1 :: ren2 :: ren3 :: Nil = decoder

  io.valid := valid.toBool
  io.wen := wen.toBool
  io.ren1 := ren1.toBool
  io.ren2 := ren2.toBool
  io.ren3 := ren3.toBool
}

class ioDpathFPU extends Bundle {
  val store_data = Bits(64, INPUT)
}

class rocketFPU extends Component
{
  val io = new Bundle {
    val req_valid = Bool(INPUT)
    val req_ready = Bool(OUTPUT)
    val req_cmd = Bits(6, INPUT)
    val req_inst = Bits(32, INPUT)
  
    val killx = Bool(INPUT)
    val killm = Bool(INPUT)
  
    val dmem = new ioDmem(List("resp_val", "resp_tag", "resp_data"))
    val dpath = new ioDpathFPU().flip()
  }

  val ex_reg_inst = Reg() { Bits() }
  when (io.req_valid) {
    ex_reg_inst <== io.req_inst
  }

  // load response
  val dmem_resp_val_fpu = io.dmem.resp_val && io.dmem.resp_tag(0).toBool
  val load_wb = Reg(dmem_resp_val_fpu, resetVal = Bool(false))
  val load_wb_data = Reg() { Bits() }
  val load_wb_tag = Reg() { UFix() }
  when (dmem_resp_val_fpu) {
    load_wb_data <== io.dmem.resp_data
    load_wb_tag <== io.dmem.resp_tag.toUFix >> UFix(1)
  }

  // regfile
  val regfile = Mem4(32, load_wb_data);
  regfile.setReadLatency(0);
  regfile.setTarget('inst);
  regfile.write(load_wb_tag, load_wb_data, load_wb);

  io.req_ready := Bool(true)

  io.dpath.store_data := regfile(ex_reg_inst(21,17))
}
