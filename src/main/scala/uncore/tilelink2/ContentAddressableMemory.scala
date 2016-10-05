package uncore.tilelink2

import Chisel._

class ContentAddressableMemory [T <: Data] (
        t : T,
        size: Int,
        num_rd_ports : Int=0,
        num_wr_ports : Int=1) extends Module {
  val io = new Bundle {
    val init_val = t.asInput
    val cam_read = t.asInput
    val wr_data = Vec(num_wr_ports, t).asInput
    val wr_index = Vec(num_wr_ports, UInt(INPUT, width=log2Up(size)))
    val wen = Vec(num_wr_ports, Bool()).asInput
    val rd_data = Vec(num_rd_ports, t).asOutput
    val rd_index = Vec(num_rd_ports, UInt(INPUT, width=log2Up(size)))
    val matched = Bool(OUTPUT)
    val outindex = UInt(OUTPUT, width=log2Up(size))
  }
  
  val matched_vec = Wire(Vec(size, Bool()))
  val mem = Reg(init=Vec.fill(size) { io.init_val })
  
//Indexed Writes
  for (i <- 0 until num_wr_ports) {
    when (io.wen(i)) {
      mem(io.wr_index(i)) := io.wr_data(i)
    }
  }
  
// Indexed Reads
  for (i <- 0 until num_rd_ports) {
      io.rd_data(i) := mem(io.rd_index(i))
  }

// CAM Read
  for (i <- 0 until size) {
    matched_vec(i) := (io.cam_read.asUInt === mem(i).asUInt)
  }
  
  val outindex = OHToUInt(matched_vec)
  val matched = matched_vec.fold(Bool(false)) ( (i:Bool, j:Bool) => i || j )

  io.matched := matched
  io.outindex := outindex
}
