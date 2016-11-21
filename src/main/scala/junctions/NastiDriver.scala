package junctions

import Chisel._
import config._

class NastiDriver(dataWidth: Int, burstLen: Int, nBursts: Int)
    (implicit p: Parameters) extends NastiModule {
  val io = new Bundle {
    val nasti = new NastiIO
    val finished = Bool(OUTPUT)
    val start = Bool(INPUT)
  }

  val dataBytes = dataWidth / 8
  val nastiDataBytes = nastiXDataBits / 8

  val (write_cnt, write_done) = Counter(io.nasti.w.fire(), burstLen)
  val (read_cnt, read_done) = Counter(io.nasti.r.fire(), burstLen)
  val (req_cnt, reqs_done) = Counter(read_done, nBursts)

  val req_addr = Cat(req_cnt, UInt(0, log2Up(burstLen * dataBytes)))

  val write_data    = UInt(0x10000000L, dataWidth) | Cat(req_cnt, write_cnt)
  val expected_data = UInt(0x10000000L, dataWidth) | Cat(req_cnt, read_cnt)

  val (s_idle :: s_write_addr :: s_write_data :: s_write_stall :: s_write_resp ::
       s_read_addr :: s_read_data :: s_read_stall :: s_done :: Nil) = Enum(Bits(), 9)
  val state = Reg(init = s_idle)

  val (stall_cnt, stall_done) = Counter(state === s_read_stall, 2)

  io.nasti.aw.valid := (state === s_write_addr)
  io.nasti.aw.bits := NastiWriteAddressChannel(
    id = UInt(0),
    addr = req_addr,
    size = UInt(log2Up(dataBytes)),
    len = UInt(burstLen - 1))

  io.nasti.w.valid := (state === s_write_data)
  io.nasti.w.bits := NastiWriteDataChannel(
    data = Cat(write_data, write_data),
    last = (write_cnt === UInt(burstLen - 1)))

  io.nasti.b.ready := (state === s_write_resp)

  io.nasti.ar.valid := (state === s_read_addr)
  io.nasti.ar.bits := NastiReadAddressChannel(
    id = UInt(0),
    addr = req_addr,
    size = UInt(log2Up(dataBytes)),
    len = UInt(burstLen - 1))

  io.nasti.r.ready := (state === s_read_data)

  io.finished := (state === s_done)

  when (state === s_idle && io.start) { state := s_write_addr }
  when (io.nasti.aw.fire()) { state := s_write_data }
  when (io.nasti.w.fire()) { state := s_write_stall }
  when (state === s_write_stall) { state := s_write_data }
  when (write_done) { state := s_write_resp }
  when (io.nasti.b.fire()) { state := s_read_addr }
  when (io.nasti.ar.fire()) { state := s_read_data }
  when (io.nasti.r.fire()) { state := s_read_stall }
  when (stall_done) { state := s_read_data }
  when (read_done) { state := s_write_addr }
  when (reqs_done) { state := s_done }

  val full_addr = req_addr + (read_cnt << UInt(log2Up(dataBytes)))
  val byteshift = full_addr(log2Up(nastiDataBytes) - 1, 0)
  val bitshift = Cat(byteshift, UInt(0, 3))
  val read_data = (io.nasti.r.bits.data >> bitshift) & Fill(dataWidth, UInt(1, 1))

  assert(!io.nasti.r.valid || read_data === expected_data,
    s"NastiDriver got wrong data")
}
