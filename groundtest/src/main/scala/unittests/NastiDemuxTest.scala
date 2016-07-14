package groundtest.unittests

import Chisel._
import junctions._
import junctions.NastiConstants._
import cde.Parameters

class NastiDemuxDriver(n: Int)(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val start = Bool(INPUT)
    val finished = Bool(OUTPUT)
    val nasti = new NastiIO
    val select = UInt(OUTPUT, log2Up(n))
  }

  val (s_idle :: s_write_addr :: s_write_data :: s_write_resp ::
       s_read_addr :: s_read_resp :: s_done :: Nil) = Enum(Bits(), 7)
  val state = Reg(init = s_idle)

  val select = Reg(init = UInt(0, log2Up(n)))

  when (state === s_idle && io.start) { state := s_write_addr }
  when (io.nasti.aw.fire()) { state := s_write_data }
  when (io.nasti.w.fire()) { state := s_write_resp }
  when (io.nasti.b.fire()) { state := s_read_addr }
  when (io.nasti.ar.fire()) { state := s_read_resp }
  when (io.nasti.r.fire()) {
    when (select === UInt(n - 1)) {
      state := s_done
    } .otherwise {
      select := select + UInt(1)
      state := s_write_addr
    }
  }

  io.nasti.aw.valid := (state === s_write_addr)
  io.nasti.aw.bits := NastiWriteAddressChannel(
    id = UInt(0),
    addr = UInt(0),
    size = UInt("b011"))
  io.nasti.w.valid := (state === s_write_data)
  io.nasti.w.bits := NastiWriteDataChannel(data = select)
  io.nasti.b.ready := (state === s_write_resp)
  io.nasti.ar.valid := (state === s_read_addr)
  io.nasti.ar.bits := NastiReadAddressChannel(
    id = UInt(0),
    addr = UInt(0),
    size = UInt("b011"))
  io.nasti.r.ready := (state === s_read_resp)

  io.finished := (state === s_done)
  io.select := select

  assert(!io.nasti.r.valid || io.nasti.r.bits.data === select,
    "NASTI DeMux test: Read data did not match")
}

class NastiDemuxSlave(implicit p: Parameters) extends NastiModule()(p) {
  val io = (new NastiIO).flip

  val (s_write_wait :: s_write_data :: s_write_resp ::
       s_read_wait :: s_read_resp :: s_done :: Nil) = Enum(Bits(), 6)
  val state = Reg(init = s_write_wait)

  val value = Reg(UInt(width = 64))
  val id = Reg(UInt(width = nastiXIdBits))

  when (io.aw.fire()) {
    id := io.aw.bits.id
    state := s_write_data
  }

  when (io.w.fire()) {
    value := io.w.bits.data
    state := s_write_resp
  }

  when (io.b.fire()) { state := s_read_wait }

  when (io.ar.fire()) {
    id := io.ar.bits.id
    state := s_read_resp
  }

  when (io.r.fire()) { state := s_done }

  io.aw.ready := (state === s_write_wait)
  io.w.ready := (state === s_write_data)
  io.b.valid := (state === s_write_resp)
  io.b.bits := NastiWriteResponseChannel(id = id)
  io.ar.ready := (state === s_read_wait)
  io.r.valid := (state === s_read_resp)
  io.r.bits := NastiReadDataChannel(id = id, data = value)
}

class NastiMemoryDemuxTest(implicit p: Parameters) extends UnitTest {
  val nSlaves = 4

  val driver = Module(new NastiDemuxDriver(nSlaves))
  driver.io.start := io.start
  io.finished := driver.io.finished

  val demux = Module(new NastiMemoryDemux(nSlaves))
  demux.io.master <> driver.io.nasti
  demux.io.select := driver.io.select

  for (i <- 0 until nSlaves) {
    val slave = Module(new NastiDemuxSlave)
    slave.io <> demux.io.slaves(i)
  }
}
