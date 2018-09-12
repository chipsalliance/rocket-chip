package freechips.rocketchip.DRAMModel

import Chisel._

class MockMemory()(implicit conf: MemoryParameters) extends Module {
  val io = new MemIO().flip
  val memoryLines = scala.math.pow(2, conf.memIF.addrBits)
  val backingMemory = Mem(Bits(width = conf.memIF.dataBits*conf.memIF.dataBeats), 2048)
  
  val readAddr = Bits()
  val readData = Bits()
  val writeAddr = Bits()
  val writeData = Bits()
  val writeEn = Bool()
  
  readAddr := io.req_cmd.bits.addr
  writeAddr := io.req_cmd.bits.addr
  writeData := Bits(0)
  writeEn := Bool(false)

  readData := backingMemory.read(readAddr)
  when(writeEn){
    backingMemory.write(writeAddr, writeData)
  }

  val receiveWriteData = Bool()
  receiveWriteData := Bool(false)
  val writeDataReg0 = Reg(init = Bits(0))
  val writeDataReg1 = Reg(init = Bits(0))
  val writeDataReg2 = Reg(init = Bits(0))
  when(receiveWriteData){
    writeDataReg0 := io.req_data.bits.data
    writeDataReg1 := writeDataReg0
    writeDataReg2 := writeDataReg1
  }

  val idle :: read0 :: read1 :: read2 :: read3 :: write0 :: write1 :: write2 :: write3 :: Nil = Enum(UInt(), 9)
  val currentState = Reg(init = idle)
  
  io.req_cmd.ready := Bool(false)
  io.req_data.ready := Bool(false)
  io.resp.valid := Bool(false)
  io.resp.bits.data := Bits(0)
  io.resp.bits.tag := Bits(0)
  when(currentState === idle){
    when(io.req_cmd.valid){
      when(io.req_cmd.bits.rw){
        currentState := write0
      }.otherwise{
        currentState := read0
      }
    }
  }.elsewhen(currentState === read0){
    io.resp.bits.tag := io.req_cmd.bits.tag
    io.resp.bits.data := readData(conf.memIF.dataBits - 1, 0)
    io.resp.valid := Bool(true)
    when(io.resp.ready){  
      currentState := read1
    }
  }.elsewhen(currentState === read1){
    io.resp.bits.tag := io.req_cmd.bits.tag
    io.resp.bits.data := readData(conf.memIF.dataBits*2 - 1, conf.memIF.dataBits)
    io.resp.valid := Bool(true)
    when(io.resp.ready){
      currentState := read2
    }
  }.elsewhen(currentState === read2){
    io.resp.bits.tag := io.req_cmd.bits.tag
    io.resp.bits.data := readData(conf.memIF.dataBits*3 - 1, conf.memIF.dataBits*2)
    io.resp.valid := Bool(true)
    when(io.resp.ready){
      currentState := read3
    }
  }.elsewhen(currentState === read3){
    io.resp.bits.tag := io.req_cmd.bits.tag
    io.resp.bits.data := readData(conf.memIF.dataBits*4 - 1, conf.memIF.dataBits*3)
    io.resp.valid := Bool(true)
    when(io.resp.ready){
      io.req_cmd.ready := Bool(true)
      currentState := idle
    }
  }.elsewhen(currentState === write0){
    io.req_data.ready := Bool(true)
    when(io.req_data.valid){
      receiveWriteData := Bool(true)
      currentState := write1
    }
  }.elsewhen(currentState === write1){
    io.req_data.ready := Bool(true)
    when(io.req_data.valid){
      receiveWriteData := Bool(true)
      currentState := write2
    }
  }.elsewhen(currentState === write2){
    io.req_data.ready := Bool(true)
    when(io.req_data.valid){
      receiveWriteData := Bool(true)
      currentState := write3
    }
  }.elsewhen(currentState === write3){
    io.req_data.ready := Bool(true)
    when(io.req_data.valid){
      writeEn := Bool(true)
      writeData := Cat(io.req_data.bits.data, writeDataReg0, writeDataReg1, writeDataReg2 )
      io.req_cmd.ready := Bool(true)
      currentState := idle
    }
  }
}
