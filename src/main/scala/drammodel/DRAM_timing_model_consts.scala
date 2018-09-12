package freechips.rocketchip.DRAMModel

import Chisel._
import scala.math._

case class MemoryControllerConfiguration(deviceWidth: Int, addrOffsetWidth: Int, mif: MemoryIFConfiguration)

case class MemoryIFConfiguration(
                                  addrBits: Int = 32,
                                  dataBits: Int = 64,
                                  tagBits: Int = 4,
                                  dataBeats: Int = 4)

case class MemoryParameters(
                           memConst : MemModelConstants = MemModelConstants(),
                           memIF : MemoryIFConfiguration = MemoryIFConfiguration()
                           ){
  val deviceWidth = memConst.ROW_WIDTH/memIF.dataBits
  val addrOffsetWidth = if(deviceWidth == 4) 0 else log2Up(deviceWidth/4)
}

case class MemModelConstants(
  NUM_BANKS : Int = 8,
  NUM_ROWS : Int = 16384,
  NUM_COLS : Int = 512,
  DATABUS_WIDTH : Int = 64,
  ROW_WIDTH : Int = 512){
  val BURST_LENGTH = ROW_WIDTH/DATABUS_WIDTH
  val DRAM_BANK_ADDR_WIDTH = log2Up(NUM_BANKS)
  val DRAM_ROW_ADDR_WIDTH = log2Up(NUM_ROWS)
  val DRAM_COL_ADDR_WIDTH = log2Up(NUM_COLS)

  val TIMING_COUNTER_WIDTH = 8
  val tCL = 4
  //Predef.assert(tCL > 0)
  val tWL = 3
  //Predef.assert(tWL > 0)
  val NUM_CMDS = 5
  val activate_cmd :: read_cmd :: write_cmd :: precharge_cmd :: other_cmd :: Nil=Enum(UInt(), NUM_CMDS)
  val CMD_WIDTH = log2Up(NUM_CMDS)
}
