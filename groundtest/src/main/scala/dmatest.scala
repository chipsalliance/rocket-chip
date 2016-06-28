package groundtest

import Chisel._
import uncore.tilelink._
import uncore.devices._
import uncore.devices.DmaRequest._
import uncore.util._
import uncore.constants._
import rocket._
import junctions.{PAddrBits, HasAddrMapParameters}
import cde.{Parameters, Field}
import scala.math.max

case class DmaTestCase(source: Int, dest: Int, length: Int)

object DmaTestCases {
  def apply(cases: (Int, Int, Int) *): Seq[DmaTestCase] = {
    cases.toSeq.map {
      case (source, dest, length) => DmaTestCase(source, dest, length)
    }
  }
}

case class DmaStreamTestConfig(
  source: Int, dest: Int, len: Int, size: Int)

case object DmaTestSet extends Field[Seq[DmaTestCase]]
case object DmaTestDataStart extends Field[Int]
case object DmaTestDataStride extends Field[Int]

case object DmaStreamTestSettings extends Field[DmaStreamTestConfig]

class DmaStatusReg(implicit val p: Parameters) extends Module
    with HasDmaParameters with HasTileLinkParameters {

  val io = new Bundle {
    val csr = (new RoCCCSRs).flip
    val incr_outstanding = Bool(INPUT)
    val xact_outstanding = Bool(OUTPUT)
    val resp_status = UInt(OUTPUT, dmaStatusBits)
  }

  val status_reg = Reg(UInt(width = dmaStatusBits))

  val outstanding_cnt = TwoWayCounter(
    io.incr_outstanding, io.csr.wen, tlMaxClientXacts)

  when (io.csr.wen) { status_reg := io.csr.wdata }

  io.xact_outstanding := outstanding_cnt > UInt(0)
  io.resp_status := status_reg
  io.csr.rdata(0) := status_reg
}

class DmaStreamTest(implicit p: Parameters) extends GroundTest()(p)
    with HasDmaParameters with HasCoreParameters with HasAddrMapParameters {
  val (s_start :: s_setup_req :: s_setup_wait ::
       s_stream_out :: s_stream_in ::  s_stream_wait ::
       s_check_req :: s_check_wait :: s_done :: Nil) = Enum(Bits(), 9)
  val state = Reg(init = s_start)
  val lo_base = addrMap("devices:loopback").start
  val conf = p(DmaStreamTestSettings)

  val test_data = Vec.tabulate(conf.len) { i => UInt(i * 8, conf.size * 8) }

  val (req_index, req_done) = Counter(io.cache.head.req.fire(), conf.len)
  val (resp_index, resp_done) = Counter(io.cache.head.resp.fire(), conf.len)

  val out_req = ClientDmaRequest(
    cmd = DMA_CMD_SOUT,
    src_start = UInt(conf.source),
    dst_start = UInt(lo_base),
    segment_size = UInt(conf.len * conf.size),
    word_size = UInt(log2Up(conf.size)))

  val in_req = ClientDmaRequest(
    cmd = DMA_CMD_SIN,
    src_start = UInt(lo_base),
    dst_start = UInt(conf.dest),
    segment_size = UInt(conf.len * conf.size),
    word_size = UInt(log2Up(conf.size)))

  val frontend = Module(new DmaFrontend)
  frontend.io.cpu.req.valid := (state === s_stream_out) || (state === s_stream_in)
  frontend.io.cpu.req.bits := Mux(state === s_stream_out, out_req, in_req)

  io.ptw.head <> frontend.io.ptw
  io.mem.head <> frontend.io.mem

  val status_reg = Module(new DmaStatusReg)
  //status_reg.io.csr <> io.csr
  status_reg.io.incr_outstanding := frontend.io.incr_outstanding

  val cache_addr_base = Mux(state === s_setup_req, UInt(conf.source), UInt(conf.dest))
  val cache_req = io.cache.head.req

  cache_req.valid := (state === s_setup_req) || (state === s_check_req)
  cache_req.bits.addr := cache_addr_base + Cat(req_index, UInt(0, log2Up(conf.size)))
  cache_req.bits.data := test_data(req_index)
  cache_req.bits.typ  := UInt(log2Up(conf.size))
  cache_req.bits.cmd  := Mux(state === s_setup_req, M_XWR, M_XRD)

  when (state === s_start) { state := s_setup_req }
  when (state === s_setup_req && req_done) { state := s_setup_wait }
  when (state === s_check_req && req_done) { state := s_check_wait }
  when (state === s_setup_wait && resp_done) { state := s_stream_out }
  when (state === s_check_wait && resp_done) { state := s_done }

  when (frontend.io.cpu.req.fire()) {
    state := Mux(state === s_stream_out, s_stream_in, s_stream_wait)
  }

  val dma_done = (state === s_stream_wait) &&
    !frontend.io.busy && !status_reg.io.xact_outstanding

  when (dma_done) { state := s_check_req }

  val resp_data = io.cache.head.resp.bits.data(conf.size * 8 - 1, 0)
  assert(!io.cache.head.resp.valid || !io.cache.head.resp.bits.has_data ||
         resp_data === test_data(resp_index),
         "Result data streamed in does not match data streamed out")
  assert(!frontend.io.cpu.resp.valid || frontend.io.cpu.resp.bits.status === UInt(0),
          "Frontend error response")

  io.finished := (state === s_done)
}

class DmaTest(implicit p: Parameters) extends GroundTest()(p)
    with HasDmaParameters with HasCoreParameters {

  private val testSet = p(DmaTestSet)
  private val dataStart = p(DmaTestDataStart)
  private val dataStride = p(DmaTestDataStride)
  private val wordBits = 32
  private val wordBytes = wordBits / 8
  private val pAddrBits = p(PAddrBits)

  val sourceAddrs = Vec(testSet.map(test => UInt(test.source)))
  val destAddrs = Vec(testSet.map(test => UInt(test.dest)))
  val transferLengths = Vec(testSet.map(test => UInt(test.length)))
  val testIdx = Reg(init = UInt(0, log2Up(testSet.size)))

  val (s_start :: s_fill_req :: s_fill_resp :: s_copy_req :: s_copy_wait ::
       s_check_req :: s_check_resp :: s_finished :: Nil) = Enum(Bits(), 8)
  val state = Reg(init = s_start)

  val req_data = Reg(UInt(width = wordBits))
  val req_addr = Reg(UInt(width = pAddrBits))
  val bytes_left = Reg(UInt(width = pAddrBits))
  val prefetch = sourceAddrs(testIdx) === destAddrs(testIdx)

  val frontend = Module(new DmaFrontend)
  frontend.io.cpu.req.valid := (state === s_copy_req)
  frontend.io.cpu.req.bits := ClientDmaRequest(
    cmd = Mux(prefetch, DMA_CMD_PFR, DMA_CMD_COPY),
    src_start = sourceAddrs(testIdx),
    dst_start = destAddrs(testIdx),
    segment_size = transferLengths(testIdx))

  io.ptw.head <> frontend.io.ptw
  io.mem.head <> frontend.io.mem

  val status_reg = Module(new DmaStatusReg)
  //status_reg.io.csr <> io.csr
  status_reg.io.incr_outstanding := frontend.io.incr_outstanding

  val dma_done = !frontend.io.busy && !status_reg.io.xact_outstanding

  val cache_resp = io.cache.head.resp
  val cache_req = io.cache.head.req
  cache_req.valid := (state === s_fill_req) || (state === s_check_req)
  cache_req.bits.addr := req_addr
  cache_req.bits.data := req_data
  cache_req.bits.typ  := MT_W
  cache_req.bits.cmd  := Mux(state === s_fill_req, M_XWR, M_XRD)

  when (state === s_start) {
    req_addr := sourceAddrs(testIdx)
    req_data := UInt(dataStart)
    bytes_left := transferLengths(testIdx)
    state := s_fill_req
  }

  when (cache_req.fire()) {
    req_addr := req_addr + UInt(wordBytes)
    bytes_left := bytes_left - UInt(wordBytes)
    state := Mux(state === s_fill_req, s_fill_resp, s_check_resp)
  }

  when (state === s_fill_resp && cache_resp.valid) {
    req_data := req_data + UInt(dataStride)
    state := Mux(bytes_left === UInt(0), s_copy_req, s_fill_req)
  }

  when (frontend.io.cpu.req.fire()) { state := s_copy_wait }

  when (state === s_copy_wait && dma_done) {
    req_addr := destAddrs(testIdx)
    req_data := UInt(dataStart)
    bytes_left := transferLengths(testIdx)
    state := s_check_req
  }

  when (state === s_check_resp && cache_resp.valid) {
    req_data := req_data + UInt(dataStride)
    when (bytes_left > UInt(0)) {
      state := s_check_req
    } .elsewhen (testIdx === UInt(testSet.size - 1)) {
      state := s_finished
    } .otherwise {
      testIdx := testIdx + UInt(1)
      state := s_start
    }
  }

  io.finished := (state === s_finished)

  testSet.foreach { case DmaTestCase(source, dest, length) =>
    require(source % wordBytes == 0, "source address must be word-aligned")
    require(dest % wordBytes == 0, "destination address must be word-aligned")
    require(length % wordBytes == 0, "transfer length must be word-aligned")
  }

  assert(!cache_resp.valid || !cache_resp.bits.has_data ||
    cache_resp.bits.data === req_data, "Received data does not match")
  assert(!frontend.io.cpu.resp.valid || frontend.io.cpu.resp.bits.status === UInt(0),
          "Frontend error response")

  val cache_timeout = Timer(1000, cache_req.fire(), cache_resp.valid)
  assert(!cache_timeout, "Memory request timed out")
}
