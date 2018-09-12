package freechips.rocketchip.DRAMModel

import Chisel._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._

/*
class DRAMSystemWrapper(conf: MemoryControllerConfiguration)(implicit p: Parameters) extends LazyModule{
  lazy val module = new DRAMSystemWrapperImp(this, conf)
}
*/

class DRAMSystemWrapper(config: MemoryParameters)(implicit p : Parameters) extends LazyModule {
    implicit val conf = config
    println(s"into DRAMSystemWrapper\n")
    lazy val module = new LazyModuleImp(this) {
        val io = IO(new Bundle {
            val memResp = Decoupled(new MemResp)
            val memReqCmd = Decoupled(new MemReqCmd).flip()
            val memReqData = Decoupled(new MemData).flip()
            val errors = new DRAMModelErrorIO
            val params = new DRAMModelParameterIO().flip
            val mem = new MemIO
        })
        //mem model and mem controller declarations

        println(s"after instantiate dram controller\n")
        val DRAMModel = Module(new DRAMTimingModel())
        val memController = Module(new MemController())
        //val memController = new RekallWrapper()

        val fireTgtClk = Wire(Bool())

        io.errors <> DRAMModel.io.errors
        DRAMModel.io.params <> io.params
        DRAMModel.io.ctrlFSM.fireTgtCycle := fireTgtClk
        memController.io.DRAMModel <> DRAMModel.io.memController
        memController.io.fireTgtCycle := fireTgtClk
        memController.io.params <> io.params
        memController.io.mem_cmd_queue.valid := io.memReqCmd.valid
        memController.io.mem_cmd_queue.bits := io.memReqCmd.bits
        io.memReqCmd.ready := memController.io.mem_cmd_queue.ready && fireTgtClk
        memController.io.mem_data_queue.valid := io.memReqData.valid
        memController.io.mem_data_queue.bits := io.memReqData.bits
        io.memReqData.ready := memController.io.mem_data_queue.ready && fireTgtClk
        io.memResp.valid := memController.io.mem_resp_queue.valid && fireTgtClk
        io.memResp.bits.tag := memController.io.mem_resp_queue.bits.tag
        io.memResp.bits.data := memController.io.mem_resp_queue.bits.data
        memController.io.mem_resp_queue.ready := io.memResp.ready
        //backing mem side IO
        io.mem.req_cmd.valid := Bool(false)
        io.mem.req_cmd.bits.rw := Bool(false)
        io.mem.req_cmd.bits.tag := Bits(0)
        io.mem.req_cmd.bits.addr := UInt(0)
        io.mem.req_data.valid := Bool(false)
        io.mem.req_data.bits.data := UInt(0)
        io.mem.resp.ready := Bool(false)

        //read data regs
        val readDataRegs = Vec.fill(3) {
            Reg(new MemData)
        }
        val readTagReg = Reg(Bits())
        readTagReg := io.mem.resp.bits.tag.toBits
        //ctrl_fsm
        val idle :: send_read_cmd :: wait_for_read_data0 :: wait_for_read_data1 :: wait_for_read_data2 :: wait_for_read_data3 :: send_write_cmd :: send_write_data0 :: send_write_data1 :: send_write_data2 :: send_write_data3 :: executeTgtCycle1 :: executeTgtCycle2 :: Nil = Enum(UInt(), 13)

        val current_state = Reg(init = idle)
        val next_state = Wire(UInt());
        next_state := current_state
        current_state := next_state

        fireTgtClk := Bool(false)
        DRAMModel.io.ctrlFSM.readData.valid := Bool(false)
        for (i <- 0 until 3) {
            DRAMModel.io.ctrlFSM.readData.data(i) := readDataRegs(i).data
        }
        DRAMModel.io.ctrlFSM.readData.data(3) := io.mem.resp.bits.data
        //io.ctrlFSMReadData := DRAMModel.io.ctrlFSM.readData.data
        //io.memControllerReadDataIn := memController.io.debug_out
        //io.memControllerReadDataConcat := memController.io.debug_readData
    }
}

