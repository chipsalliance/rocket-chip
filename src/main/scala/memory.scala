package Top
{

import Chisel._
import Node._;

import queues._;
import Constants._;

class ioMemory extends Bundle()
{
  val mem_mrq_val    = Bool('input);
  val mem_mrq_cmd    = Bits(4, 'input);
  val mem_mrq_type   = Bits(3, 'input);
  val mem_xsdq_rdy   = Bool('output);
  val mem_xsdq_val   = Bool('input);
  val mem_mrq_deq    = Bool('output);
  val dpath_rs2      = Bits(64, 'input);
  val dpath_waddr    = UFix(5, 'input);
  val dpath_alu_out  = UFix(64, 'input);
  val dmem_req_val   = Bool('output);
  val dmem_req_rdy   = Bool('input);
  val dmem_req_op    = Bits(4, 'output);
  val dmem_req_addr  = UFix(32, 'output);
  val dmem_req_data  = Bits(64, 'output);
  val dmem_req_wmask = Bits(8, 'output);
  val dmem_req_tag   = Bits(12, 'output);
}

class rocketMemory extends Component
{
  override val io = new ioMemory();
  val mrq_enq_xf
    = (io.mem_mrq_cmd === M_FRD || io.mem_mrq_cmd === M_FWR);

  val mrq_enq_op
    = Mux(io.mem_mrq_cmd === M_FRD, M_XRD,
      Mux(io.mem_mrq_cmd === M_FWR, M_XWR,
          io.mem_mrq_cmd));

  val mrq_enq_type = io.mem_mrq_type;

  val mrq  = new queueSimplePF(45, 4, 2);
  val xsdq = new queueSimplePF(64, 4, 2);

  mrq.io.q_reset  := Bool(false);
  mrq.io.enq_bits := Cat(mrq_enq_xf,mrq_enq_op,mrq_enq_type,io.dpath_waddr,io.dpath_alu_out(31,0));
  mrq.io.enq_val  ^^ io.mem_mrq_val;
  // mrq.io.enq_rdy  <> (); issue logic takes care of this

  val mrq_deq_xf    = Wire(){Bits(width = 1)};
  val mrq_deq_op    = Wire(){Bits(width = 4)};
  val mrq_deq_type  = Wire(){Bits(width = 3)};
  val mrq_deq_waddr = Wire(){Bits(width = 5)};
  val mrq_deq_addr  = Wire(){Bits(width = 32)};
  val mrq_deq_bits  = mrq.io.deq_bits;
  mrq_deq_bits.Match(Array(mrq_deq_xf, mrq_deq_op, mrq_deq_type, mrq_deq_waddr, mrq_deq_addr));
  val mrq_deq_val   = mrq.io.deq_val;

  xsdq.io.q_reset := Bool(false);
  xsdq.io.enq_bits ^^ io.dpath_rs2;
  xsdq.io.enq_val  ^^ io.mem_xsdq_val;
  xsdq.io.enq_rdy  ^^ io.mem_xsdq_rdy;

  val mrq_deq_flush = mrq_deq_op === M_FLA;
  val mrq_deq_load  = mrq_deq_op === M_XRD;
  val mrq_deq_xstore = mrq_deq_op === M_XWR & ~mrq_deq_xf & xsdq.io.deq_val;

  val mrq_deq_rdy = io.dmem_req_rdy & (mrq_deq_load | mrq_deq_xstore | mrq_deq_flush);
  io.mem_mrq_deq := (mrq_deq_val & mrq_deq_rdy).toBool;
  mrq.io.deq_rdy  := mrq_deq_rdy.toBool;
  val xsdq_deq_rdy = io.dmem_req_rdy & mrq_deq_val & mrq_deq_op === M_XWR & ~mrq_deq_xf;
  xsdq.io.deq_rdy  := xsdq_deq_rdy.toBool;

  val wdata = xsdq.io.deq_bits;

  val wmask_b =
    Mux(mrq_deq_addr(2,0) === UFix(0, 3), Bits("b0000_0001", 8),
    Mux(mrq_deq_addr(2,0) === UFix(1, 3), Bits("b0000_0010", 8),
    Mux(mrq_deq_addr(2,0) === UFix(2, 3), Bits("b0000_0100", 8),
    Mux(mrq_deq_addr(2,0) === UFix(3, 3), Bits("b0000_1000", 8),
    Mux(mrq_deq_addr(2,0) === UFix(4, 3), Bits("b0001_0000", 8),
    Mux(mrq_deq_addr(2,0) === UFix(5, 3), Bits("b0010_0000", 8),
    Mux(mrq_deq_addr(2,0) === UFix(6, 3), Bits("b0100_0000", 8),
    Mux(mrq_deq_addr(2,0) === UFix(7, 3), Bits("b1000_0000", 8),
        UFix(0, 8)))))))));

  val wmask_h =
    Mux(mrq_deq_addr(2,1) === UFix(0, 2), Bits("b0000_0011", 8),
    Mux(mrq_deq_addr(2,1) === UFix(1, 2), Bits("b0000_1100", 8),
    Mux(mrq_deq_addr(2,1) === UFix(2, 2), Bits("b0011_0000", 8),
    Mux(mrq_deq_addr(2,1) === UFix(3, 2), Bits("b1100_0000", 8),
        UFix(0, 8)))));

  val wmask_w =
    Mux(mrq_deq_addr(2) === UFix(0, 1), Bits("b0000_1111", 8),
    Mux(mrq_deq_addr(2) === UFix(1, 1), Bits("b1111_0000", 8),
        UFix(0, 8)));

  val wmask_d =
    Bits("b1111_1111", 8);

  io.dmem_req_val  := (mrq_deq_val & (mrq_deq_load | mrq_deq_xstore | mrq_deq_flush)).toBool;
  io.dmem_req_op   := mrq_deq_op;
  io.dmem_req_addr := Cat(mrq_deq_addr(31,3), UFix(0, 3)).toUFix;

 io.dmem_req_data :=
   Mux(mrq_deq_type === MT_B, Fill(8, wdata( 7,0)),
   Mux(mrq_deq_type === MT_H, Fill(4, wdata(15,0)),
   Mux(mrq_deq_type === MT_W, Fill(2, wdata(31,0)),
   Mux(mrq_deq_type === MT_D, wdata,
       UFix(0, 64)))));

  io.dmem_req_wmask :=
    Mux(mrq_deq_type === MT_B, wmask_b,
    Mux(mrq_deq_type === MT_H, wmask_h,
    Mux(mrq_deq_type === MT_W, wmask_w,
    Mux(mrq_deq_type === MT_D, wmask_d,
        UFix(0, 8)))));

  io.dmem_req_tag := Cat(mrq_deq_xf,mrq_deq_type,mrq_deq_addr(2,0),mrq_deq_waddr);
}

}
