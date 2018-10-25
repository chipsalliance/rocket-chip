package freechips.rocketchip.DRAMModel

import Chisel._

class MockMemory()(implicit conf: MemoryParameters) extends Module {
  val io = IO(new Bundle {
    val mem = new MemIO().flip
  })
  val memoryLines = scala.math.pow(2, conf.memIF.addrBits)
  val backingMemory = Mem(Bits(width = conf.memIF.dataBits*conf.memIF.dataBeats), 2048)
  

  val readAddr = io.mem.req_cmd.bits.addr
  val writeAddr = io.mem.req_cmd.bits.addr

  val writeData = Wire(Bits(0))
  val writeEn = Wire(Bool())
  writeEn := Bool(false)

  val readData = backingMemory.read(readAddr)
  when(writeEn){
    backingMemory.write(writeAddr, writeData)
  }

  val receiveWriteData = Wire(Bool())
  receiveWriteData := Bool(false)
  val writeDataReg0 = Reg(init = Bits(0))
  val writeDataReg1 = Reg(init = Bits(0))
  val writeDataReg2 = Reg(init = Bits(0))
  when(receiveWriteData){
    writeDataReg0 := io.mem.req_data.bits.data
    writeDataReg1 := writeDataReg0
    writeDataReg2 := writeDataReg1
  }

  val idle :: read0 :: read1 :: read2 :: read3 :: write0 :: write1 :: write2 :: write3 :: Nil = Enum(UInt(), 9)
  val currentState = Reg(init = idle)
  
  io.mem.req_cmd.ready := Bool(true)
  io.mem.req_data.ready := Bool(true)
  io.mem.resp.valid := Bool(false)
  io.mem.resp.bits.data := Bits(0)
  io.mem.resp.bits.tag := Bits(0)
  when(currentState === idle){
    when(io.mem.req_cmd.valid){
      when(io.mem.req_cmd.bits.rw){
        currentState := write0
      }.otherwise{
        currentState := read0
      }
    }
  }.elsewhen(currentState === read0){
    io.mem.resp.bits.tag := io.mem.req_cmd.bits.tag
    io.mem.resp.bits.data := readData(conf.memIF.dataBits - 1, 0)
    io.mem.resp.valid := Bool(true)
    when(io.mem.resp.ready){
      currentState := read1
    }
  }.elsewhen(currentState === read1){
    io.mem.resp.bits.tag := io.mem.req_cmd.bits.tag
    io.mem.resp.bits.data := readData(conf.memIF.dataBits*2 - 1, conf.memIF.dataBits)
    io.mem.resp.valid := Bool(true)
    when(io.mem.resp.ready){
      currentState := read2
    }
  }.elsewhen(currentState === read2){
    io.mem.resp.bits.tag := io.mem.req_cmd.bits.tag
    io.mem.resp.bits.data := readData(conf.memIF.dataBits*3 - 1, conf.memIF.dataBits*2)
    io.mem.resp.valid := Bool(true)
    when(io.mem.resp.ready){
      currentState := read3
    }
  }.elsewhen(currentState === read3){
    io.mem.resp.bits.tag := io.mem.req_cmd.bits.tag
    io.mem.resp.bits.data := readData(conf.memIF.dataBits*4 - 1, conf.memIF.dataBits*3)
    io.mem.resp.valid := Bool(true)
    when(io.mem.resp.ready){
      io.mem.req_cmd.ready := Bool(true)
      currentState := idle
    }
  }.elsewhen(currentState === write0){
    io.mem.req_data.ready := Bool(true)
    when(io.mem.req_data.valid){
      receiveWriteData := Bool(true)
      currentState := write1
    }
  }.elsewhen(currentState === write1){
    io.mem.req_data.ready := Bool(true)
    when(io.mem.req_data.valid){
      receiveWriteData := Bool(true)
      currentState := write2
    }
  }.elsewhen(currentState === write2){
    io.mem.req_data.ready := Bool(true)
    when(io.mem.req_data.valid){
      receiveWriteData := Bool(true)
      currentState := write3
    }
  }.elsewhen(currentState === write3){
    io.mem.req_data.ready := Bool(true)
    when(io.mem.req_data.valid){
      writeEn := Bool(true)
      writeData := Cat(io.mem.req_data.bits.data, writeDataReg0, writeDataReg1, writeDataReg2 )
      io.mem.req_cmd.ready := Bool(true)
      currentState := idle
    }
  }
}
