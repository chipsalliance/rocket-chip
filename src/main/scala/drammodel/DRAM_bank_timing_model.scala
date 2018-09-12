package freechips.rocketchip.DRAMModel

import Chisel._

class BankCmdIO(implicit val conf: MemoryParameters) extends Bundle {
  val valid = Bool(INPUT)
  val activate = Bool(INPUT)
  val read = Bool(INPUT)
  val write = Bool(INPUT)
  val precharge = Bool(INPUT)
  val bankAddr = UInt(INPUT, width = conf.memConst.DRAM_BANK_ADDR_WIDTH)
  val rowAddr = UInt(INPUT, width = conf.memConst.DRAM_ROW_ADDR_WIDTH)
  val colAddr = UInt(INPUT, width = conf.memConst.DRAM_COL_ADDR_WIDTH)
}

class BankToCtrlFSMIO(implicit val conf:MemoryParameters) extends Bundle {
  val rowAddr = UInt(OUTPUT, width = conf.memConst.DRAM_ROW_ADDR_WIDTH)
}

class BankErrorIO extends Bundle {
  val invalidCmd = Bool()
  val tRAS_violation = Bool()
  val tRCD_violation = Bool()
  val tRP_violation = Bool()
  val tCCD_violation = Bool()
  val tRTP_violation = Bool()
  val tWTR_violation = Bool()
  val tWR_violation = Bool()
}

class BankParameterIO(implicit val conf: MemoryParameters) extends Bundle {
  val tRAS = UInt(width = conf.memConst.TIMING_COUNTER_WIDTH)
  val tRCD = UInt(width = conf.memConst.TIMING_COUNTER_WIDTH)
  val tRP = UInt(width = conf.memConst.TIMING_COUNTER_WIDTH)
  val tCCD = UInt(width = conf.memConst.TIMING_COUNTER_WIDTH)
  val tRTP = UInt(width = conf.memConst.TIMING_COUNTER_WIDTH)
  val tWTR = UInt(width = conf.memConst.TIMING_COUNTER_WIDTH)
  val tWR = UInt(width = conf.memConst.TIMING_COUNTER_WIDTH)
}

class DRAMBank(implicit val conf: MemoryParameters)extends Module {
  val io = IO(new Bundle {
    val params = new DRAMModelParameterIO().flip
    val cmds = new BankCmdIO()
    val errors = new DRAMModelErrorIO()
    val ctrl = new BankToCtrlFSMIO()
    val fireTgtCycle = Bool(INPUT)
  })
  val tRAS = io.params.tRAS - UInt(1)
  val tRCD = io.params.tRCD - UInt(1)
  val tRP = io.params.tRP - UInt(1)
  val tCCD = io.params.tCCD - UInt(1)
  val tRTP = io.params.tRTP - UInt(1)
  val tWTR = io.params.tWTR - UInt(1)
  val tWR = io.params.tWR - UInt(1)
  //row and col state registers
  val prevWriteColAddr = Reg(init = UInt(0))
  val prevWriteColAddrVal = Reg(init = Bool(false))
  val prevWriteColAddrNext = Wire(UInt())
  val prevWriteColAddrValNext = Wire(Bool())
  prevWriteColAddrNext := prevWriteColAddr
  prevWriteColAddrValNext := prevWriteColAddrVal
  val currentRowAddr = Reg(init = UInt(0))
  val nextRowAddr = Wire(UInt())
  nextRowAddr := currentRowAddr
  when(io.fireTgtCycle){
    prevWriteColAddr := prevWriteColAddrNext
    prevWriteColAddrVal := prevWriteColAddrValNext
    currentRowAddr := nextRowAddr
  }
  io.ctrl.rowAddr := currentRowAddr
  //timing counters
  val tRAS_counter = Reg(init = UInt(0, conf.memConst.TIMING_COUNTER_WIDTH))
  val tRCD_counter = Reg(init = UInt(0, conf.memConst.TIMING_COUNTER_WIDTH))
  val tRP_counter= Reg(init = UInt(0, conf.memConst.TIMING_COUNTER_WIDTH))
  val tCCD_counter = Reg(init = UInt(0, conf.memConst.TIMING_COUNTER_WIDTH))
  val tRTP_counter = Reg(init = UInt(0, conf.memConst.TIMING_COUNTER_WIDTH))
  val tWTR_counter = Reg(init = UInt(0, conf.memConst.TIMING_COUNTER_WIDTH))
  val tWR_counter = Reg(init = UInt(0, conf.memConst.TIMING_COUNTER_WIDTH))
  
  val tRAS_increment = Wire(Bool(false))
  val tRCD_increment = Wire(Bool(false))
  val tRP_increment = Wire(Bool(false))
  val tCCD_increment = Wire(Bool(false))
  val tRTP_increment = Wire(Bool(false))
  val tWTR_increment = Wire(Bool(false))
  val tWR_increment = Wire(Bool(false))
  
  val tRAS_reset = Wire(Bool(false))
  val tRCD_reset = Wire(Bool(false))
  val tRP_reset = Wire(Bool(false))
  val tCCD_reset = Wire(Bool(false))
  val tRTP_reset = Wire(Bool(false))
  val tWTR_reset = Wire(Bool(false))
  val tWR_reset = Wire(Bool(false))
  
  when(io.fireTgtCycle){
    when(tRAS_reset){
      tRAS_counter := UInt(0)
    }.otherwise{
      when(tRAS_increment && tRAS_counter < tRAS){
        tRAS_counter := tRAS_counter + UInt(1)
      }
    }
    when(tRCD_reset){
      tRCD_counter := UInt(0)
    }.otherwise{
      when(tRCD_increment && tRCD_counter < tRCD){
        tRCD_counter := tRCD_counter + UInt(1)
      }
    }
    when(tRP_reset){
      tRP_counter := UInt(0)
    }.otherwise{
      when(tRP_increment && tRP_counter < tRP){
        tRP_counter := tRP_counter + UInt(1)
      }
    }
    when(tCCD_reset){
      tCCD_counter := UInt(0)
    }.otherwise{
      when(tCCD_increment && tCCD_counter < tCCD){
        tCCD_counter := tCCD_counter + UInt(1)
      }
    }
    when(tRTP_reset){
      tRTP_counter := UInt(0)
    }.otherwise{
      when(tRTP_increment && tRTP_counter < tRTP){
        tRTP_counter := tRTP_counter + UInt(1)
      }
    }
    when(tWTR_reset){
      tWTR_counter := UInt(0)
    }.otherwise{
      when(tWTR_increment && tWTR_counter < tWTR){
        tWTR_counter := tWTR_counter + UInt(1)
      }
    }
    when(tWR_reset){
      tWR_counter := UInt(0)
    }.otherwise{
      when(tWR_increment && tWR_counter < tWR){
        tWR_counter := tWR_counter + UInt(1)
      }
    }
  }
  
  //Bank state FSM
  val rowClosed :: rowOpened :: colOpenedRead :: colOpenedWrite :: invalidCmd :: tRAS_violation :: tRCD_violation :: tRP_violation :: tCCD_violation :: tRTP_violation :: tWTR_violation :: tWR_violation :: Nil = Enum(UInt(), 12) 
  val currentState = Reg(init = rowClosed)
  val nextState = Wire(UInt())
  nextState := currentState
  when(io.fireTgtCycle){
    currentState := nextState
  }
  
  io.errors.invalidCmd := Bool(false)
  io.errors.tRAS_violation := Bool(false)
  io.errors.tRCD_violation := Bool(false)
  io.errors.tRP_violation := Bool(false)
  io.errors.tCCD_violation := Bool(false)
  io.errors.tRTP_violation := Bool(false)
  io.errors.tWTR_violation := Bool(false)
  io.errors.tWR_violation := Bool(false)
  when(currentState === rowClosed){
    //counter increments
    tRP_increment := Bool(true)
    when(io.cmds.valid){
      //counter resets
      tRP_reset := Bool(true)
      //next state logic
      when(io.cmds.activate){
        when(tRP_counter === tRP){
          nextState := rowOpened
          nextRowAddr := io.cmds.rowAddr
        }.otherwise{
          nextState := tRP_violation
        }
      }.elsewhen(io.cmds.precharge){
        nextState := rowClosed
      }
    }
  }.elsewhen(currentState === rowOpened){
    //counter increments
    tRAS_increment := Bool(true)
    tRCD_increment := Bool(true)
    when(io.cmds.valid){
      //counter resets
      tRCD_reset := Bool(true)
      when(nextState === rowClosed){
        tRAS_reset := Bool(true)
      }
      when(nextState === colOpenedRead){
        tRTP_reset := Bool(true)
      }
      when(nextState === colOpenedWrite){
        tWR_reset := Bool(true)
      }
      //next state logic
      when(io.cmds.precharge){
        when(tRAS_counter === tRAS){
          nextState := rowClosed
        }.otherwise{
          nextState := tRAS_violation
        }
      }.elsewhen(io.cmds.read){
        when(tRCD_counter === tRCD){
          nextState := colOpenedRead
        }.otherwise{
          nextState := tRCD_violation
        }
      }.elsewhen(io.cmds.write){
        when(tRCD_counter === tRCD){
          nextState := colOpenedWrite
          prevWriteColAddrNext := io.cmds.colAddr
          prevWriteColAddrValNext := Bool(true)
        }.otherwise{
          nextState := tRCD_violation
        }
      }
    }
  }.elsewhen(currentState === colOpenedRead){
    //counter increments
    tRAS_increment := Bool(true)
    tCCD_increment := Bool(true)
    tRTP_increment := Bool(true)
    when(io.cmds.valid){
      //counter resets
      tCCD_reset := Bool(true)
      when(nextState === rowClosed){
        tRAS_reset := Bool(true)
      }
      when(nextState === colOpenedRead){
        tRTP_reset := Bool(true)
      }
      when(nextState === colOpenedWrite){
        tWR_reset := Bool(true)
      }
      //next state logic
      when(io.cmds.precharge){
        when(tRAS_counter === tRAS && tRTP_counter === tRTP){
          nextState := rowClosed
        }.elsewhen(tRTP_counter != io.params.tRTP){
          nextState := tRTP_violation
        }.otherwise{
          nextState := tRAS_violation
        }
      }.elsewhen(io.cmds.read){
        when(tCCD_counter === tCCD){
          nextState := colOpenedRead
        }.otherwise{
          nextState := tCCD_violation
        }
      }.elsewhen(io.cmds.write){
        when(tCCD_counter === tCCD){
          nextState := colOpenedWrite
          prevWriteColAddrNext := io.cmds.colAddr
          prevWriteColAddrValNext := Bool(true)
        }.otherwise{
          nextState := tCCD_violation
        }
      }     
    }
  }.elsewhen(currentState === colOpenedWrite){
    //counter increments
    tRAS_increment := Bool(true)
    tCCD_increment := Bool(true)
    tWTR_increment := Bool(true)
    tWR_increment := Bool(true)
    when(io.cmds.valid){
      //counter resets
      tCCD_reset := Bool(true)
      tWTR_reset := Bool(true)
      when(nextState === rowClosed){
        tRAS_reset := Bool(true)
      }
      when(nextState === colOpenedRead){
        tRTP_reset := Bool(true)
      }
      when(nextState === colOpenedWrite){
        tWR_reset := Bool(true)
      }
      //next state logic
      when(io.cmds.precharge){
        when(tRAS_counter === tRAS && tWR_counter === tWR){
          nextState := rowClosed
        }.elsewhen(tWR_counter != io.params.tWR){
          nextState := tWR_violation
          prevWriteColAddrValNext := Bool(false)
        }.otherwise{
          nextState := tRAS_violation
        }
      }.elsewhen(io.cmds.write){
        when(tCCD_counter === tCCD){
          nextState := colOpenedWrite
          prevWriteColAddrNext := io.cmds.colAddr
          prevWriteColAddrValNext := Bool(true)
        }.otherwise{
          nextState := tCCD_violation
        }
      }.elsewhen(io.cmds.read){
        when(prevWriteColAddrVal && prevWriteColAddr === io.cmds.colAddr){
          when(tWTR_counter === tWTR && tCCD_counter === tCCD){
            nextState := colOpenedRead
            prevWriteColAddrValNext := Bool(false)
          }.elsewhen(tWTR_counter != io.params.tWTR){
            nextState := tWTR_violation
          }.otherwise{
            nextState := tCCD_violation
          }
        }.otherwise{
          when(tCCD_counter === tCCD){
            nextState := colOpenedRead
          }.otherwise{
            nextState := tCCD_violation
          }
        }
      }   
    }
  }.elsewhen(currentState === invalidCmd){
    io.errors.invalidCmd := Bool(true)
  }.elsewhen(currentState === tRAS_violation){
    io.errors.tRAS_violation := Bool(true)
  }.elsewhen(currentState === tRCD_violation){
    io.errors.tRCD_violation := Bool(true)
  }.elsewhen(currentState === tRP_violation){
    io.errors.tRP_violation := Bool(true)
  }.elsewhen(currentState === tCCD_violation){
    io.errors.tCCD_violation := Bool(true)
  }.elsewhen(currentState === tRTP_violation){
    io.errors.tRTP_violation := Bool(true)
  }.elsewhen(currentState === tWTR_violation){
    io.errors.tWTR_violation := Bool(true)
  }.elsewhen(currentState === tWTR_violation){
    io.errors.tWTR_violation := Bool(true)
  }
}
