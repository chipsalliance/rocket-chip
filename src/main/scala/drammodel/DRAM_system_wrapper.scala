package freechips.rocketchip.DRAMModel

import Chisel._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._

class DRAMModel(config: MemoryParameters) extends Module {
  implicit val conf = config
  val io = IO(new Bundle{
    val mem = new MemIO().flip
  })
  val die = Mem(1024, Bits(width = config.memIF.dataBits))
  io.mem.resp.valid := Bool(false)
  when(io.mem.req_cmd.valid && io.mem.req_cmd.bits.rw){
    die(io.mem.req_cmd.bits.addr) := io.mem.req_data.bits.data
    io.mem.resp.valid := Bool(true)
    io.mem.resp.bits.tag := io.mem.req_cmd.bits.tag
  }
  when(RegNext(io.mem.req_cmd.valid) && ~RegNext(io.mem.req_cmd.bits.rw)){
    io.mem.resp.bits.data := die(io.mem.req_cmd.bits.addr)
    io.mem.resp.bits.tag := io.mem.req_cmd.bits.tag
    io.mem.resp.valid := Bool(true)
  }

  io.mem.req_cmd.ready := Bool(true)
}

class DRAMSystem(config: MemoryParameters)(implicit p : Parameters) extends LazyModule {
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

      when(io.memReqCmd.valid) {
        printf("get memReqCmd addr = %x \n", io.memReqCmd.bits.addr)
      }
      println(s"after instantiate dram controller\n")
      val DRAMModel = Module(new DRAMTimingModel())
      val memController = Module(new MemController())
      //val memController = new RekallWrapper()

      val fireTgtClk = Wire(Bool(false))
      val executeTgtCycle = Wire(Bool()); executeTgtCycle := fireTgtClk

      io.errors <> DRAMModel.io.errors
      DRAMModel.io.params <> io.params
      DRAMModel.io.ctrlFSM.fireTgtCycle := fireTgtClk
      memController.io.DRAMModel <> DRAMModel.io.memController
      memController.io.fireTgtCycle := fireTgtClk
      memController.io.params <> io.params
      memController.io.mem_cmd_queue.valid := io.memReqCmd.valid
      memController.io.mem_cmd_queue.bits := io.memReqCmd.bits
      io.memReqCmd.ready := memController.io.mem_cmd_queue.ready //&& fireTgtClk
      memController.io.mem_data_queue.valid := io.memReqData.valid
      memController.io.mem_data_queue.bits := io.memReqData.bits
      io.memReqData.ready := memController.io.mem_data_queue.ready //&& fireTgtClk
      io.memResp.valid := memController.io.mem_resp_queue.valid //&& fireTgtClk
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

      val readDataRegs = Reg(init = Vec.fill(3){UInt(0, width = conf.memIF.dataBits)})
      val readTagReg = Reg(Bits())
      readTagReg := io.mem.resp.bits.tag.toBits
      //ctrl_fsm
      val idle :: send_read_cmd :: wait_for_read_data0 :: wait_for_read_data1 :: wait_for_read_data2 :: wait_for_read_data3 :: send_write_cmd :: send_write_data0 :: send_write_data1 :: send_write_data2 :: send_write_data3 :: executeTgtCycle1 :: executeTgtCycle2 :: Nil = Enum(UInt(), 13)

      val ctrlFSMAddr = Reg(init = UInt(0))
      val current_state = Reg(init = idle)
      val next_state = Wire(UInt());
      next_state := current_state
      current_state := next_state
      fireTgtClk := Bool(false)
      executeTgtCycle := Bool(false)

      DRAMModel.io.ctrlFSM.readData.valid := Bool(false)
      for (i <- 0 until 3) {
        DRAMModel.io.ctrlFSM.readData.data(i) := readDataRegs(i).data
      }
      DRAMModel.io.ctrlFSM.readData.data(3) := io.mem.resp.bits.data
      //io.ctrlFSMReadData := DRAMModel.io.ctrlFSM.readData.data
      //io.memControllerReadDataIn := memController.io.debug_out
      //io.memControllerReadDataConcat := memController.io.debug_readData
      when(current_state === idle) {
        fireTgtClk := io.memResp.ready
        executeTgtCycle := Bool(true)
        next_state := executeTgtCycle1
        when(DRAMModel.io.ctrlFSM.doWrite) {
          //fireTgtClk := Bool(false)
          next_state := send_write_cmd
          ctrlFSMAddr := DRAMModel.io.ctrlFSM.writeAddr
        }.elsewhen(DRAMModel.io.ctrlFSM.doRead) {
          //fireTgtClk := Bool(false)
          next_state := send_read_cmd
          ctrlFSMAddr := DRAMModel.io.ctrlFSM.readAddr
        }
      }.elsewhen(current_state === send_read_cmd) {
        when(io.mem.req_cmd.ready) {
          io.mem.req_cmd.valid := Bool(true)
          io.mem.req_cmd.bits.rw := Bool(false)
          io.mem.req_cmd.bits.addr := ctrlFSMAddr
          next_state := wait_for_read_data0
        }
      }.elsewhen(current_state === wait_for_read_data0) {
        when(io.mem.resp.valid) {
          io.mem.resp.ready := Bool(true)
          readDataRegs(0).data := io.mem.resp.bits.data
          next_state := wait_for_read_data1
        }
      }.elsewhen(current_state === wait_for_read_data1) {
        when(io.mem.resp.valid) {
          io.mem.resp.ready := Bool(true)
          readDataRegs(1).data := io.mem.resp.bits.data
          next_state := wait_for_read_data2
        }
      }.elsewhen(current_state === wait_for_read_data2) {
        when(io.mem.resp.valid) {
          io.mem.resp.ready := Bool(true)
          readDataRegs(2).data := io.mem.resp.bits.data
          next_state := wait_for_read_data3
        }
      }.elsewhen(current_state === wait_for_read_data3) {
        when(io.mem.resp.valid && io.memResp.ready) {
          io.mem.resp.ready := Bool(true)
          fireTgtClk := io.memResp.ready
          executeTgtCycle := Bool(true)
          DRAMModel.io.ctrlFSM.readData.valid := Bool(true)
          next_state := executeTgtCycle1
        }
      }.elsewhen(current_state === send_write_cmd) {
        when(io.mem.req_cmd.ready) {
          io.mem.req_cmd.valid := Bool(true)
          io.mem.req_cmd.bits.rw := Bool(true)
          io.mem.req_cmd.bits.addr := ctrlFSMAddr
          next_state := send_write_data0
        }
      }.elsewhen(current_state === send_write_data0) {
        when(io.mem.req_data.ready) {
          io.mem.req_data.valid := Bool(true)
          io.mem.req_data.bits.data := DRAMModel.io.ctrlFSM.writeData(0)
          next_state := send_write_data1
        }
      }.elsewhen(current_state === send_write_data1) {
        when(io.mem.req_data.ready) {
          io.mem.req_data.valid := Bool(true)
          io.mem.req_data.bits.data := DRAMModel.io.ctrlFSM.writeData(1)
          next_state := send_write_data2
        }
      }.elsewhen(current_state === send_write_data2) {
        when(io.mem.req_data.ready) {
          io.mem.req_data.valid := Bool(true)
          io.mem.req_data.bits.data := DRAMModel.io.ctrlFSM.writeData(2)
          next_state := send_write_data3
        }
      }.elsewhen(current_state === send_write_data3) {
        when(DRAMModel.io.ctrlFSM.doRead) {
          when(io.mem.req_data.ready) {
            io.mem.req_data.valid := Bool(true)
            io.mem.req_data.bits.data := DRAMModel.io.ctrlFSM.writeData(3)
            next_state := send_read_cmd
          }
        }.otherwise {
          when(io.mem.req_data.ready && io.memResp.ready) {
            io.mem.req_data.valid := Bool(true)
            io.mem.req_data.bits.data := DRAMModel.io.ctrlFSM.writeData(3)
            executeTgtCycle := Bool(true)
            fireTgtClk := io.memReqCmd.valid && io.memReqData.valid && io.memResp.ready
            next_state := executeTgtCycle1
          }
        }
      }.elsewhen(current_state === executeTgtCycle1) {
        executeTgtCycle := Bool(true)
        next_state := executeTgtCycle2
      }.elsewhen(current_state === executeTgtCycle2) {
        executeTgtCycle := Bool(true)
        next_state := idle
      }
    }
}

class DRAMSystemWrapper(config: MemoryParameters)(implicit p: Parameters) extends LazyModule{
  implicit val conf = config
  val DRAMSys = LazyModule(new DRAMSystem(config))
  lazy val module = new LazyModuleImp(this) {
    lazy val io = IO(new Bundle {
      val memResp = Decoupled(new MemResp)
      val memReqCmd = Decoupled(new MemReqCmd).flip()
      val memReqData = Decoupled(new MemData).flip()
      val errors = new DRAMModelErrorIO
      val params = new DRAMModelParameterIO().flip
    })
    val DRAMModel = Module(new MockMemory())
    DRAMModel.io.mem <> DRAMSys.module.io.mem
    io.memResp <> DRAMSys.module.io.memResp
    DRAMSys.module.io.memReqCmd <> io.memReqCmd
    DRAMSys.module.io.memReqData <> io.memReqData
    io.errors <> DRAMSys.module.io.errors
    DRAMSys.module.io.params <> io.params
  }
}

