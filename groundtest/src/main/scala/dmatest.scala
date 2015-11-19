package groundtest

import Chisel._
import uncore._
import rocket._
import junctions.PAddrBits
import cde.{Parameters, Field}

case class DmaTestCase(source: Int, dest: Int, length: Int)

object DmaTestCases {
  def apply(cases: (Int, Int, Int) *): Seq[DmaTestCase] = {
    cases.toSeq.map {
      case (source, dest, length) => DmaTestCase(source, dest, length)
    }
  }
}

case object DmaTestSet extends Field[Seq[DmaTestCase]]
case object DmaTestDataStart extends Field[Int]
case object DmaTestDataStride extends Field[Int]

class DmaTest(implicit p: Parameters) extends GroundTest()(p)
    with HasDmaParameters with HasCoreParameters {

  private val testSet = p(DmaTestSet)
  private val dataStart = p(DmaTestDataStart)
  private val dataStride = p(DmaTestDataStride)
  private val wordBits = 32
  private val wordBytes = wordBits / 8
  private val pAddrBits = p(PAddrBits)

  disablePorts(cache = false, dma = false, ptw = false)

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
    cmd = Mux(prefetch, DmaRequest.DMA_CMD_PFR, DmaRequest.DMA_CMD_COPY),
    src_start = sourceAddrs(testIdx),
    dst_start = destAddrs(testIdx),
    segment_size = transferLengths(testIdx))

  io.dma <> frontend.io.dma
  io.ptw <> frontend.io.ptw

  io.cache.req.valid := (state === s_fill_req) || (state === s_check_req)
  io.cache.req.bits.addr := req_addr
  io.cache.req.bits.data := req_data
  io.cache.req.bits.typ  := MT_W
  io.cache.req.bits.cmd  := Mux(state === s_fill_req, M_XWR, M_XRD)
  io.cache.req.bits.kill := Bool(false)
  io.cache.req.bits.phys := Bool(false)

  when (state === s_start) {
    req_addr := sourceAddrs(testIdx)
    req_data := UInt(dataStart)
    bytes_left := transferLengths(testIdx)
    state := s_fill_req
  }

  when (io.cache.req.fire()) {
    req_addr := req_addr + UInt(wordBytes)
    bytes_left := bytes_left - UInt(wordBytes)
    state := Mux(state === s_fill_req, s_fill_resp, s_check_resp)
  }

  when (state === s_fill_resp && io.cache.resp.valid) {
    req_data := req_data + UInt(dataStride)
    state := Mux(bytes_left === UInt(0), s_copy_req, s_fill_req)
  }

  when (frontend.io.cpu.req.fire()) { state := s_copy_wait }

  when (state === s_copy_wait && !frontend.io.busy) {
    req_addr := destAddrs(testIdx)
    req_data := UInt(dataStart)
    bytes_left := transferLengths(testIdx)
    state := s_check_req
  }

  when (state === s_check_resp && io.cache.resp.valid) {
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

  assert(!io.cache.resp.valid || !io.cache.resp.bits.has_data ||
    io.cache.resp.bits.data === req_data, "Received data does not match")

  val dma_timeout = Timer(1000, io.dma.req.fire(), io.dma.resp.fire())
  assert(!dma_timeout, "DMA request timed out")

  val cache_timeout = Timer(1000, io.cache.req.fire(), io.cache.resp.valid)
  assert(!cache_timeout, "Memory request timed out")
}
