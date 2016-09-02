/// See LICENSE for license details.

package junctions

import Chisel._
import scala.math.max
import scala.collection.mutable.ArraySeq
import cde.{Parameters, Field}

class NarrowIO(val w: Int) extends Bundle {
  val chipToWorld = Decoupled(UInt(width = w))
  val worldToChip = Decoupled(UInt(width = w)).flip
  val host_clk = Bool(OUTPUT)
  val chipToWorld_sync = Bool(OUTPUT)
  val worldToChip_sync = Bool(OUTPUT)
  override def cloneType = new NarrowIO(w).asInstanceOf[this.type]
}

class NastiSerializer(w: Int, divide: Int, clockSignal: Clock = null, resetSignal: Bool = null)
    (implicit p: Parameters) extends NastiModule(clockSignal, resetSignal) {
  val io = new Bundle {
    val nasti = (new NastiIO).flip
    val narrow = new NarrowIO(w)
  }

  require(divide > 2) // The generated clock gets synchronized by the deserializer,
                      // so it must be slow enough that it is still well-aligned
                      // with these added 2 cycles of latency.

  val (div,div_done) = Counter(Bool(true),divide)
  val div_clk = Reg(init = Bool(false))
  when (div_done) { div_clk := ~div_clk }
  io.narrow.host_clk := div_clk

  val rise_edge = RegNext((div_done) & ~div_clk)
  val fall_edge = RegNext((div_done) & div_clk)
  val addr_update = RegNext(fall_edge) // Kind of a hack, but it gets the timing working

  val writePorts = Seq(io.nasti.aw, io.nasti.w, io.nasti.ar)
  val wBits = writePorts.map(_.bits.asUInt)
  val wWidth = wBits.map(_.getWidth)
  val wBeats = wWidth.map(x => (math.ceil(x.toFloat / w)).toInt)
  val wData = Wire(Vec(writePorts.length,Vec(wBeats.reduceLeft(max),UInt(width=w))))
  // [ben] wData widths are too wide, but a Chisel3 bug prevents us from doing this properly

  wData.zipWithIndex.map { case(data,i) => {
    for (j <- 0 until wBeats(i)-1)
      data(j) := wBits(i)(((j+1)*w)-1,j*w)
    data(wBeats(i)-1) := Cat(UInt(0,w - (wWidth(i) % w)), wBits(i)(wWidth(i)-1,(wBeats(i)-1)*w))
  }}

  val wValid = Wire(Vec(writePorts.length,Bool()))
  val wReady = Wire(Vec(writePorts.length,Bool()))
  for (i <- 0 until writePorts.length) {
    wValid(i) := writePorts(i).valid
    writePorts(i).ready := wReady(i)
    wReady(i) := Bool(false)
  }

  val readPorts = Seq(io.nasti.b, io.nasti.r)
  val rBits = readPorts.map(_.bits.asUInt)
  val rWidth = rBits.map(_.getWidth)
  val rBeats = rWidth.map(x => (math.ceil(x.toFloat / w)).toInt)
  val rData = Reg(Vec(readPorts.length,Vec(rBeats.reduceLeft(max),UInt(width=w))))

  rData.zipWithIndex.map { case(data,i) => {
    readPorts(i) := readPorts(i).cloneType.fromBits(data.reduceLeft[UInt] { (a,b) => Cat(b,a) })
  }}

  val rValid = Wire(Vec(readPorts.length,Bool()))
  val rReady = Wire(Vec(readPorts.length,Bool()))
  for (i <- 0 until readPorts.length) {
    readPorts(i).valid := rValid(i)
    rValid(i) := Bool(false)
    rReady(i) := readPorts(i).ready
  }

  val wadvance = Reg(init = Bool(false))
  val (waddr,waddr_done) = Counter(addr_update & wadvance,writePorts.length+1)
  val waddr_dec = waddr - UInt(1)
  val radvance = Reg(init = Bool(false))
  val (raddr,raddr_done) = Counter(addr_update & radvance,readPorts.length+1)
  val raddr_dec = raddr - UInt(1)

  val write_sync_reg = RegNext(next=(waddr===UInt(0)),init=Bool(false))
  val read_sync_reg = RegNext(next=(raddr===UInt(0)),init=Bool(false))
  io.narrow.chipToWorld_sync := write_sync_reg
  io.narrow.worldToChip_sync := read_sync_reg

  val wxfer = Reg(init=Bool(false))
  val wxferCount = Reg(init = Vec.fill(writePorts.length){UInt(0,width=log2Up(wBeats.reduceLeft(max)))})
  val wxferDone = Vec(wBeats.map(UInt(_)))
  val rxfer = Reg(init=Bool(false))
  val rxferCount = Reg(init = Vec.fill(readPorts.length){UInt(0,width=log2Up(rBeats.reduceLeft(max)))})
  val rxferDone = Vec(rBeats.map(UInt(_)))

  val out_valid = Reg(init=Bool(false))
  io.narrow.chipToWorld.valid := out_valid

  val in_ready = Reg(init=Bool(false))
  io.narrow.worldToChip.ready := in_ready

  val wdata = Reg(init=UInt(0,width=w))
  io.narrow.chipToWorld.bits := wdata

  when (rise_edge) { // Write on the rising edge
    wdata := wData(waddr_dec)(wxferCount(waddr_dec))
    when (waddr === UInt(0)) { // Write logic
      wadvance := Bool(true)
      out_valid := Bool(false)
    } .otherwise {
      out_valid := wValid(waddr_dec)
      when (wxfer) {
        wadvance := Bool(false)
        out_valid := Bool(false)
        when (wxferCount(waddr_dec) < wxferDone(waddr_dec)) {
          wxferCount(waddr_dec) := wxferCount(waddr_dec) + UInt(1)
        } .elsewhen (wxferCount(waddr_dec) === (wxferDone(waddr_dec))) {
          wxferCount(waddr_dec) := UInt(0)
          wxfer := Bool(false)
          wReady(waddr_dec) := Bool(true)
          wadvance := Bool(true)
        }
      } .otherwise { 
        wadvance := Bool(true)
      }
    }
    when (raddr === UInt(0)) { // Read logic
      radvance := Bool(true)
      in_ready := Bool(false)
    } .otherwise {
      in_ready := rReady(raddr_dec)
      when (rxfer) {
        radvance := Bool(false)
        in_ready := Bool(false)
      } .otherwise {
        radvance := Bool(true)
      }
    }
  }

  when (fall_edge) { // Read on the falling edge
    when ((waddr =/= UInt(0)) && ~wxfer && out_valid && io.narrow.chipToWorld.ready) { // Write logic
      wxfer := Bool(true)
      wadvance := Bool(false)
    }
    when ((raddr =/= UInt(0))) { // Read logic
      when (rxfer) {
        rData(raddr_dec)(rxferCount(raddr_dec)) := io.narrow.worldToChip.bits
        when (rxferCount(raddr_dec) < (rxferDone(raddr_dec))) {
          rxferCount(raddr_dec) := rxferCount(raddr_dec) + UInt(1)
        } .elsewhen (rxferCount(raddr_dec) === (rxferDone(raddr_dec))) {
          rxferCount(raddr_dec) := UInt(0)
          rxfer := Bool(false)
          rValid(raddr_dec) := Bool(true)
          radvance := Bool(true)
        }
      } .elsewhen (io.narrow.worldToChip.valid && in_ready) {
        rxfer := Bool(true)
        radvance := Bool(false)
      }
    }
  }
}

class NastiDeserializer(val w: Int)(implicit p: Parameters) extends NastiModule{
  val io = new Bundle {
    val mem_axi = new NastiIO
    val narrow = (new NarrowIO(w)).flip
  }

  val div_clk = ShiftRegister(io.narrow.host_clk,2) 
  val div_clk_reg = RegNext(div_clk,init=Bool(false))

  val rise_edge = ~div_clk_reg & div_clk
  val fall_edge = div_clk_reg & ~div_clk
  val addr_update = RegNext(fall_edge)

  val writePorts = Seq(io.mem_axi.b, io.mem_axi.r)
  val wBits = writePorts.map(_.bits.asUInt)
  val wWidth = wBits.map(_.getWidth)
  val wBeats = wWidth.map(x => (math.ceil(x.toFloat / w)).toInt)
  val wData = Wire(Vec(writePorts.length,Vec(wBeats.reduceLeft(max),UInt(width=w))))
  // [ben] wData widths are too wide, but a Chisel3 bug prevents us from doing this properly
  
  wData.zipWithIndex.map { case(data,i) => {
    for (j <- 0 until wBeats(i)-1) {
      data(j) := wBits(i)(((j+1)*w)-1,j*w)
    }
    data(wBeats(i)-1) := Cat(UInt(0,w - (wWidth(i) % w)), wBits(i)(wWidth(i)-1,(wBeats(i)-1)*w))
  }}

  val wValid = Wire(Vec(writePorts.length,Bool()))
  val wReady = Wire(Vec(writePorts.length,Bool()))
  for (i <- 0 until writePorts.length) {
    wValid(i) := writePorts(i).valid
    writePorts(i).ready := wReady(i)
    wReady(i) := Bool(false)
  }

  val readPorts = Seq(io.mem_axi.aw, io.mem_axi.w, io.mem_axi.ar)
  val rBits = readPorts.map(_.bits.asUInt)
  val rWidth = rBits.map(_.getWidth)
  val rBeats = rWidth.map(x => (math.ceil(x.toFloat / w)).toInt)
  val rData = Reg(Vec(readPorts.length,Vec(rBeats.reduceLeft(max),UInt(width=w))))

  rData.zipWithIndex.map { case(data,i) => {
    readPorts(i) := readPorts(i).cloneType.fromBits(data.reduceLeft[UInt] { (a,b) => Cat(b,a) })
  }}

  val rValid = Wire(Vec(readPorts.length,Bool()))
  val rReady = Wire(Vec(readPorts.length,Bool()))
  for (i <- 0 until readPorts.length) {
    readPorts(i).valid := rValid(i)
    rValid(i) := Bool(false)
    rReady(i) := readPorts(i).ready
  }

  val wadvance = Reg(init = Bool(false))
  val (waddr,waddr_done) = Counter(addr_update & wadvance,writePorts.length+1)
  val waddr_dec = waddr - UInt(1)
  val radvance = Reg(init = Bool(false))
  val (raddr,raddr_done) = Counter(addr_update & radvance,readPorts.length+1)
  val raddr_dec = raddr - UInt(1)

  val wxfer = Reg(init=Bool(false))
  val wxferCount = Reg(init = Vec.fill(writePorts.length){UInt(0,width=1+log2Up(wBeats.reduceLeft(max)))})
  val wxferDone = Vec(wBeats.map(UInt(_)))
  val rxfer = Reg(init=Bool(false))
  val rxferCount = Reg(init = Vec.fill(readPorts.length){UInt(0,width=1+log2Up(rBeats.reduceLeft(max)))})
  val rxferDone = Vec(rBeats.map(UInt(_)))

  val out_valid = Reg(init=Bool(false))
  io.narrow.worldToChip.valid := out_valid

  val in_ready = Reg(init=Bool(false))
  io.narrow.chipToWorld.ready := in_ready

  val wsync = Reg(init=Bool(false))
  val rsync = Reg(init=Bool(false))

  val wdata = Reg(init=UInt(0,width=w))
  io.narrow.worldToChip.bits := wdata

  when (rise_edge) { // Write on the rising edge
    wdata := wData(waddr_dec)(wxferCount(waddr_dec))
    when (~wsync) { // Write logic
      wadvance := Bool(false)
    } .elsewhen (waddr === UInt(0) && ~wxfer) {
      wadvance := Bool(true)
      out_valid := Bool(false)
    } .otherwise {
      out_valid := wValid(waddr_dec)
      when (wxfer) {
        wadvance := Bool(false)
        out_valid := Bool(false)
        when (wxferCount(waddr_dec) < wxferDone(waddr_dec)) {
          wxferCount(waddr_dec) := wxferCount(waddr_dec) + UInt(1)
        } .elsewhen (wxferCount(waddr_dec) === (wxferDone(waddr_dec))) {
          wxferCount(waddr_dec) := UInt(0)
          wxfer := Bool(false)
          wReady(waddr_dec) := Bool(true)
          wadvance := Bool(true)
        }
      } .otherwise { 
        wadvance := Bool(true)
      }
    }
    when (~rsync) { // Read logic
      radvance := Bool(false)
    } .elsewhen (raddr === UInt(0)) {
      radvance := Bool(true)
      in_ready := Bool(false)
    } .otherwise {
      in_ready := rReady(raddr_dec)
      when (rxfer) {
        radvance := Bool(false)
        in_ready := Bool(false)
      } .otherwise {
        radvance := Bool(true)
      }
    }
  }

  when (fall_edge) { // Read on the falling edge
    when (~wsync) { // Write logic
      when (io.narrow.worldToChip_sync) {
        wsync := Bool(true)
        wadvance := Bool(true)
      }
    } .elsewhen ((waddr =/= UInt(0)) && ~wxfer && out_valid && io.narrow.worldToChip.ready) {
      wxfer := Bool(true)
      wadvance := Bool(false)
    }
    when (~rsync) { // Read logic 
      when (io.narrow.chipToWorld_sync) {
        rsync := Bool(true)
        radvance := Bool(true)
      }
    } .elsewhen ((raddr =/= UInt(0))) {
      when (rxfer) {
        rData(raddr_dec)(rxferCount(raddr_dec)) := io.narrow.chipToWorld.bits
        when (rxferCount(raddr_dec) < (rxferDone(raddr_dec))) {
          rxferCount(raddr_dec) := rxferCount(raddr_dec) + UInt(1)
        } .elsewhen (rxferCount(raddr_dec) === (rxferDone(raddr_dec))) {
          rxferCount(raddr_dec) := UInt(0)
          rxfer := Bool(false)
          rValid(raddr_dec) := Bool(true)
          radvance := Bool(true)
        }
      } .elsewhen (io.narrow.chipToWorld.valid && in_ready) {
        rxfer := Bool(true)
        radvance := Bool(false)
      }
    }
  }
}
