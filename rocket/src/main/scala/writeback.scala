package Top
{

import Chisel._
import Node._;
import Constants._;

class ioWriteback extends Bundle()
{
  val dmem_resp_val     = Bool('input);
  val dmem_resp_data    = UFix(64, 'input);
  val dmem_resp_tag     = UFix(12, 'input);
  val wb_waddr          = UFix(5, 'output);
  val wb_wen            = Bool('output);
  val wb_wdata          = Bits(64, 'output);
}

class rocketWriteback extends Component
{
  override val io = new ioWriteback();
  
  val r_dmem_resp_val  = Reg(io.dmem_resp_val);
  val r_dmem_resp_data = Reg(io.dmem_resp_data);
  val r_dmem_resp_tag  = Reg(io.dmem_resp_tag);

  val dmem_resp_xf    = r_dmem_resp_tag(11);
  val dmem_resp_type  = r_dmem_resp_tag(10, 8);
  val dmem_resp_pos   = r_dmem_resp_tag(7, 5);
  val dmem_resp_waddr = r_dmem_resp_tag(4, 0);
  val dmem_resp_xval  = r_dmem_resp_val & ~dmem_resp_xf;
  val dmem_resp_fval  = r_dmem_resp_val &  dmem_resp_xf;

  val dmem_resp_data_w = 
    Mux(dmem_resp_pos(2).toBool, r_dmem_resp_data(63, 32), r_dmem_resp_data(31, 0));
  val dmem_resp_data_h = 
    Mux(dmem_resp_pos(1).toBool, dmem_resp_data_w(31, 16),  dmem_resp_data_w(15, 0));
  val dmem_resp_data_b = 
    Mux(dmem_resp_pos(0).toBool, dmem_resp_data_h(15, 8),   dmem_resp_data_h(7, 0));

  val dmem_resp_data_final =
    Mux(dmem_resp_type === MT_B,  Cat(Fill(56, dmem_resp_data_b(7)), dmem_resp_data_b),
    Mux(dmem_resp_type === MT_BU, Cat(UFix(0, 56), dmem_resp_data_b),
    Mux(dmem_resp_type === MT_H,  Cat(Fill(48, dmem_resp_data_h(15)), dmem_resp_data_h),
    Mux(dmem_resp_type === MT_HU, Cat(UFix(0, 48), dmem_resp_data_h),
    Mux(dmem_resp_type === MT_W,  Cat(Fill(32, dmem_resp_data_w(31)), dmem_resp_data_w),
    Mux(dmem_resp_type === MT_WU, Cat(UFix(0, 32), dmem_resp_data_w),
    Mux(dmem_resp_type === MT_D,  r_dmem_resp_data,
        UFix(0, 64))))))));

  io.wb_wen   := dmem_resp_xval.toBool;
  io.wb_waddr := dmem_resp_waddr;
  io.wb_wdata := dmem_resp_data_final;

}

}
