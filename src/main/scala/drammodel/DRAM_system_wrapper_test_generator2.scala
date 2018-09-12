/*package freechips.rocketchip.DRAMModel

import Chisel._
import collection.mutable.{ArrayBuffer}
import collection.mutable.{HashMap}

object genDRAMSystemTestHarness2{
  def apply(cmdQueueHostVal: Bool, cmdQueueHostRdy: Bool, cmdQueueValid: Bool, cmdQueueReady: Bool, cmdQueueBitsRW: Bool, cmdQueueBitsAddr: UInt, cmdQueueBitsTag: Bits, dataQueueHostVal : Bool, dataQueueHostRdy: Bool, dataQueueValid: Bool, dataQueueReady: Bool, dataQueueBitsData: Bits, respQueueHostVal: Bool, respQueueHostRdy: Bool, respQueueValid: Bool, respQueueReady: Bool, respQueueBitsTag: Bits, respQueueBitsData: Bits, passed: Bool, cmds: ArrayBuffer[(String, Int, Int, (Int, Int, Int, Int))]) = {
    val fireTgtCycle = Bool()
    fireTgtCycle := cmdQueueHostRdy && dataQueueHostRdy && respQueueHostVal
    cmdQueueHostVal := fireTgtCycle
    dataQueueHostVal := fireTgtCycle
    respQueueHostRdy := fireTgtCycle
    
    cmdQueueValid := Bool(false)
    cmdQueueBitsRW := Bool(false)
    cmdQueueBitsAddr := UInt(0)
    cmdQueueBitsTag := Bits(1)
    dataQueueValid := Bool(false)
    dataQueueBitsData := UInt(0)
    
    var nextCmdStateNum = 0
    val currentState = Reg(init = UInt(0, log2Up(5*cmds.length + 1)))
    val nextState = UInt(); nextState := currentState
    when(fireTgtCycle){
      currentState := nextState
    }
    for(i <- 0 until cmds.length){
      if(cmds(i)._1 == "read"){
        when(currentState === UInt(nextCmdStateNum)){
          when(cmdQueueReady){
            nextState := UInt(nextCmdStateNum + 1)
            cmdQueueValid := Bool(true)
            cmdQueueBitsRW := Bool(false)
            cmdQueueBitsAddr := UInt(cmds(i)._2)
            cmdQueueBitsTag := UInt(cmds(i)._3)
          }
        }
        nextCmdStateNum = nextCmdStateNum + 1
      } else {
        when(currentState === UInt(nextCmdStateNum)){
          when(cmdQueueReady){
            nextState := UInt(nextCmdStateNum + 1)
            cmdQueueValid := Bool(true)
            cmdQueueBitsRW := Bool(true)
            cmdQueueBitsAddr := UInt(cmds(i)._2)
            cmdQueueBitsTag := UInt(cmds(i)._3)
          }
        }
        when(currentState === UInt(nextCmdStateNum + 1)){
          when(dataQueueReady){
            nextState := UInt(nextCmdStateNum + 2)
            dataQueueValid := Bool(true)
            dataQueueBitsData := UInt(cmds(i)._4._1)
          }
        }
        when(currentState === UInt(nextCmdStateNum + 2)){
          when(dataQueueReady){
            nextState := UInt(nextCmdStateNum + 3)
            dataQueueValid := Bool(true)
            dataQueueBitsData := UInt(cmds(i)._4._2)
          }
        }
        when(currentState === UInt(nextCmdStateNum + 3)){
          when(dataQueueReady){
            nextState := UInt(nextCmdStateNum + 4)
            dataQueueValid := Bool(true)
            dataQueueBitsData := UInt(cmds(i)._4._3)
          }
        }
        when(currentState === UInt(nextCmdStateNum + 4)){
          when(dataQueueReady){
            nextState := UInt(nextCmdStateNum + 5)
            dataQueueValid := Bool(true)
            dataQueueBitsData := UInt(cmds(i)._4._4)
          }
        }
        nextCmdStateNum = nextCmdStateNum + 5
      }
    }
    val expectedReadData = new HashMap[Int, (Int, Int, Int, Int)]()
    for(i <- 0 until cmds.length){
      var prevWrite = -1
      if(cmds(i)._1 == "read"){
        for(j <- 0 until i){
          if(cmds(j)._1 == "write" && cmds(j)._2 == cmds(i)._2){
            prevWrite = j
          }
        }
        expectedReadData(i) = cmds(prevWrite)._4
      }
    }
    var nextCompStateNum = 0
    val compCurrentState = Reg(init = UInt(0, log2Up(4*cmds.length + 1)))
    val compNextState = UInt(); compNextState := compCurrentState
    when(fireTgtCycle){
      compCurrentState := compNextState
    }
    passed := Bool(false)
    respQueueReady := Bool(false)
    for(i <- 0 until cmds.length){
      if(cmds(i)._1 == "read"){
        when(compCurrentState === UInt(nextCompStateNum)){
          when(respQueueValid && respQueueBitsTag === UInt(cmds(i)._3) && respQueueBitsData === UInt(expectedReadData(i)._1)){
            respQueueReady := Bool(true)
            compNextState := UInt(nextCompStateNum + 1)
          }
        }
        when(compCurrentState === UInt(nextCompStateNum + 1)){
          when(respQueueValid && respQueueBitsTag === UInt(cmds(i)._3) && respQueueBitsData === UInt(expectedReadData(i)._2)){
            respQueueReady := Bool(true)
            compNextState := UInt(nextCompStateNum + 2)
          }
        }
        when(compCurrentState === UInt(nextCompStateNum + 2)){
          when(respQueueValid && respQueueBitsTag === UInt(cmds(i)._3) && respQueueBitsData === UInt(expectedReadData(i)._3)){
            respQueueReady := Bool(true)
            compNextState := UInt(nextCompStateNum + 3)
          }
        }
        when(compCurrentState === UInt(nextCompStateNum + 3)){
          when(respQueueValid && respQueueBitsTag === UInt(cmds(i)._3) && respQueueBitsData === UInt(expectedReadData(i)._4)){
            respQueueReady := Bool(true)
            compNextState := UInt(nextCompStateNum + 4)
          }
        }
        nextCompStateNum = nextCompStateNum + 4
      }
    }
    when(compCurrentState === UInt(nextCompStateNum)){
      passed:= Bool(true)
    }
  }
}
*/
