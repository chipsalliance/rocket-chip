package uncore.test

import Chisel._
import java.io.File
import unittest._
import uncore.test._
import uncore.tilelink2._

case class TLFormalRAMModelParameters(
  maxOutstandingPuts : Int,
  maxOutstandingGets : Int
)

class TLFormalRAMModel(params: TLFormalRAMModelParameters) extends LazyModule
{
  val node = TLIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in = node.bundleIn
      val out = node.bundleOut
    }
    // !!! support multiple clients via clock division
    require (io.out.size == 1)

    val in = io.in(0)
    val out = io.out(0)
    
    val edge = node.edgesIn(0)
    val maxTransfer  = edge.manager.maxTransfer
    val beatBytes    = edge.manager.beatBytes
    val maxLgBeats   = log2Up(maxTransfer/beatBytes)
    val a_size = edge.size(in.a.bits)
    val a_base = edge.address(in.a.bits)
    val a_mask = edge.mask(a_base, a_size)
//    val a_fifo = edge.manager.hasFifoIdFast(a_base)
    val a_data_slice_vec = Vec.tabulate(beatBytes) (
                              (i:Int) => in.a.bits.data(8*(i+1)-1, 8*i).asUInt)

    val addr_hi_track = NoiseMaker(true, in.a.bits.addr_hi.getWidth, Bool(true), 1)
    val get_beat_track = NoiseMaker(true, edge.numBeats(in.a.bits).getWidth, Bool(true), 1)
    val addr_byte_track = NoiseMaker(true, log2Ceil(beatBytes), Bool(true), 1)
    val data_track = NoiseMaker(true, 8, Bool(true), 1)

    val numOutstandingPut = RegInit(UInt(0, width=log2Up(params.maxOutstandingPuts+1)))
    val numOutstandingGet = RegInit(UInt(0, width=log2Up(params.maxOutstandingGets+1)))
    val numBeats_a = edge.numBeats1(in.a.bits)
    val beat_counter_a = RegInit(UInt(0, width=maxLgBeats))
    
//    class sourceIDTracker extends Bundle {
//      val valid = Bool()
//      val data_match = Bool()
//    }
    
    val put_data = Reg(Bool(false), init=Bool(false))
    val a_data_slice = a_data_slice_vec(addr_byte_track)

    val source_put_track = Module(new ContentAddressableMemory(
                                          t=UInt(width=log2Up(edge.client.endSourceId)),
                                          size=params.maxOutstandingPuts
                                        ))
    val source_put_track_valid = Module(new ContentAddressableMemory(
                                          t=Bool(),
                                          size=params.maxOutstandingPuts,
                                          num_rd_ports=1,
                                          num_wr_ports=2
                                        ))
    val source_put_data_match = Reg(Vec(params.maxOutstandingPuts, Bool() ))
    val empty_source_put_idx = Wire(UInt(width=log2Up(params.maxOutstandingPuts)))
    source_put_track_valid.io.init_val := Bool(false)
    source_put_track_valid.io.cam_read := Bool(false)
    empty_source_put_idx := source_put_track_valid.io.outindex
    // TODO also include when writing term
    assert((source_put_track_valid.io.matched || !source_put_track_valid.io.wen(0)),
           "Always have empty slot when writing")

//    val source_put_track = Reg(Vec(params.maxOutstandingPuts, new sourceIDTracker))
//    val empty_source_put_idx = source_put_track.indexWhere( (p:sourceIDTracker) => p.valid==Bool(false) )

    val source_get_track = Module(new ContentAddressableMemory(
                                          t=UInt(width=log2Up(edge.client.endSourceId)),
                                          size=params.maxOutstandingGets
                                        ))
    val source_get_track_valid = Module(new ContentAddressableMemory(
                                          t=Bool(),
                                          size=params.maxOutstandingGets,
                                          num_rd_ports=1,
                                          num_wr_ports=2
                                        ))
    val source_get_match_put_data = Reg(Vec(params.maxOutstandingPuts, Bool() ))
    val empty_source_get_idx = Wire(UInt(width=log2Up(params.maxOutstandingGets)))
    source_get_track_valid.io.init_val := Bool(false)
    source_get_track_valid.io.cam_read := Bool(false)
    empty_source_get_idx := source_get_track_valid.io.outindex
    // TODO also include when writing term
    assert((source_get_track_valid.io.matched || !source_get_track_valid.io.wen(0)),
           "Always have empty slot when writing")

    // Passthrough
    out.a.valid := in.a.valid & source_put_track_valid.io.matched & source_get_track_valid.io.matched
    in.a.ready := out.a.ready & source_put_track_valid.io.matched & source_get_track_valid.io.matched
    out.a.bits := in.a.bits

    in.d.valid := out.d.valid
    out.d.ready := in.d.ready
    in.d.bits := out.d.bits
    
    // BCE unsupported
    in.b.valid := Bool(false)
    out.c.valid := Bool(false)
    out.e.valid := Bool(false)
    out.b.ready := Bool(true)
    in.c.ready := Bool(true)
    in.e.ready := Bool(true)

    //    val source_get_track = Reg(Vec(params.maxOutstandingGets, new sourceIDTracker))
//    val empty_source_get_idx = source_get_track.indexWhere( (p:sourceIDTracker) => p.valid==Bool(false) )

//    source_put_track.io.wen := Vec(1, Bool(false))
    source_put_track.io.wen(0) := Bool(false)
    source_put_track_valid.io.wen(0) := Bool(false)
    source_put_track_valid.io.wen(1) := Bool(false)
    source_get_track.io.wen(0) := Bool(false)
    source_get_track_valid.io.wen(0) := Bool(false)
    source_get_track_valid.io.wen(1) := Bool(false)
    
    when(in.a.fire()) {
      when(beat_counter_a===numBeats_a) {
        beat_counter_a := UInt(0)
      } .otherwise {
        beat_counter_a := beat_counter_a + UInt(1)
      }

      // When encountering put request of interest
      when (addr_hi_track===in.a.bits.addr_hi + beat_counter_a &&
            (in.a.bits.opcode === TLMessages.PutFullData ||
             in.a.bits.opcode === TLMessages.PutPartialData)) {
        source_put_track.io.wr_index(0) := empty_source_put_idx
        source_put_track.io.wen(0) := Bool(true)
        source_put_track.io.wr_data(0) := in.a.bits.source
        source_put_track_valid.io.wr_index(0) := empty_source_put_idx
        source_put_track_valid.io.wen(0) := Bool(true)
        source_put_track_valid.io.wr_data(0) := Bool(true)

        when (in.a.bits.mask(addr_byte_track)===UInt(1) &&
              a_data_slice === data_track) {
          source_put_data_match(empty_source_put_idx) := Bool(true)
        } .otherwise {
          source_put_data_match(empty_source_put_idx) := Bool(false)
        }
        
        numOutstandingPut := numOutstandingPut + UInt(1)
      }

      when (addr_hi_track===in.a.bits.addr_hi + beat_counter_a &&
            get_beat_track === beat_counter_a &&
            in.a.bits.mask(addr_byte_track)===UInt(1) &&
            (in.a.bits.opcode === TLMessages.Get)) {
        source_get_track.io.wr_index(0) := empty_source_get_idx
        source_get_track.io.wen(0) := Bool(true)
        source_get_track.io.wr_data(0) := in.a.bits.source
        source_get_track_valid.io.wr_index(0) := empty_source_get_idx
        source_get_track_valid.io.wen(0) := Bool(true)
        source_get_track_valid.io.wr_data(0) := Bool(true)
        source_get_match_put_data(empty_source_get_idx) := put_data
        
        numOutstandingGet := numOutstandingGet + UInt(1)
      }
    }
    
    val numBeats_d = edge.numBeats1(in.d.bits)
    val d_data_slice_vec = Vec.tabulate(beatBytes) (
                              (i:Int) => in.d.bits.data(8*(i+1)-1, 8*i).asUInt)
    val d_data_slice = d_data_slice_vec(addr_byte_track)

    val beat_counter_d = RegInit(UInt(0, width=maxLgBeats))
    
    source_put_track.io.cam_read := out.d.bits.source
    val source_recv_idx_valid = source_put_track.io.matched
    val source_recv_idx = source_put_track.io.outindex
    source_put_track_valid.io.rd_index(0) := source_recv_idx
    
    source_get_track.io.cam_read := out.d.bits.source
    val source_get_recv_idx_valid = source_get_track.io.matched
    val source_get_recv_idx = source_get_track.io.outindex
    source_get_track_valid.io.rd_index(0) := source_get_recv_idx
    // TODO assume not combinational path from a channel to d channel--not a correct assumption in general
    when (in.d.fire()) {
      when(beat_counter_d===numBeats_d) {
        beat_counter_d := UInt(0)
      } .otherwise {
        beat_counter_d := beat_counter_d + UInt(1)
      }

      when (source_put_track_valid.io.rd_data(0) === Bool(true) &
            source_recv_idx_valid === Bool(true)) {
        // TODO, only track when writing one put request per address for now
        when (numOutstandingPut === UInt(1) &&
              source_put_data_match(source_recv_idx)===Bool(true)) {
          put_data := Bool(true)
        } .otherwise {
          put_data := Bool(false)
        }
        source_put_track_valid.io.wen(1) := Bool(true)
        source_put_track_valid.io.wr_data(1) := Bool(false)
        source_put_track_valid.io.wr_index(1) := source_recv_idx
        numOutstandingPut := numOutstandingPut - UInt(1)
      }
      when (source_get_track_valid.io.rd_data(0) === Bool(true) &&
            get_beat_track === beat_counter_d &&
            source_get_recv_idx_valid === Bool(true)) {
        // TODO, only track when writing one put request per address for now
        when (put_data === Bool(true) &
              source_get_match_put_data(source_get_recv_idx) === Bool(true) ) {
          assert (d_data_slice === data_track, "data returned not match")
        }
        source_get_track_valid.io.wen(1) := Bool(true)
        source_get_track_valid.io.wr_data(1) := Bool(false)
        source_get_track_valid.io.wr_index(1) := source_get_recv_idx
        numOutstandingGet := numOutstandingGet - UInt(1)
      }
    }
  }
}


class TLFuzzNoRAM extends LazyModule
{
//  val model = LazyModule(new TLRAMModel)
  val ram  = LazyModule(new TLRAM(AddressSet(0, 0x3ff)))
//  val gpio = LazyModule(new RRTest1(0x400))
  val xbar = LazyModule(new TLXbar)
  val fuzz = LazyModule(new TLFuzzer(nOperations=5000,
      inFlight = 4,
      formalnoise = true))

//  model.node := fuzz.node
  xbar.node := TLWidthWidget(16)(TLHintHandler()(fuzz.node))
  ram.node := TLFragmenter(4,256)(TLBuffer()(xbar.node))
//  gpio.node := TLFragmenter(TLBuffer(xbar.node), 4, 32)

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := fuzz.module.io.finished
  }
}

class TLFuzzFormalTest2 extends LazyModule
{
//  val model = LazyModule(new TLRAMModel)
  val ram  = LazyModule(new TLRAM(AddressSet(0x800, 0x7ff)))
  val ram2 = LazyModule(new TLRAM(AddressSet(0, 0x3ff), beatBytes = 16))
//  val gpio = LazyModule(new RRTest1(0x400))
  val xbar = LazyModule(new TLXbar)
  val xbar2= LazyModule(new TLXbar)
  val fuzz = LazyModule(new TLFuzzer(nOperations=5000,
                     inFlight=4,
                     formalnoise = true
                        ))
//  val cross = LazyModule(new TLAsyncCrossing)

//  model.node := fuzz.node
  xbar2.node := fuzz.node
  ram2.node := TLFragmenter(16,256)(xbar2.node)
  xbar.node := TLWidthWidget(16)(TLHintHandler()(xbar2.node))
  ram.node := TLFragmenter(4,256)(TLBuffer()(xbar.node))
//  cross.node := TLFragmenter(TLBuffer(xbar.node), 4, 256)
//  ram.node := cross.node
//  gpio.node := TLFragmenter(TLBuffer(xbar.node), 4, 32)

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := fuzz.module.io.finished

//    // Shove the RAM into another clock domain
//    val clocks = Module(new ClockDivider)
//    ram.module.clock := clocks.io.clock_out
//    ram.module.reset := clocks.io.reset_out
//    clocks.io.clock_in := clock
//    clocks.io.reset_in := reset
//
//    // ... and safely cross TL2 into it
//    cross.module.io.in_clock := clock
//    cross.module.io.in_reset := reset
//    cross.module.io.out_clock := clocks.io.clock_out
//    cross.module.io.out_reset := clocks.io.reset_out
  }
}

class TLFuzzFormalTest3 extends LazyModule
{
  val model = LazyModule(new TLFormalRAMModel(new TLFormalRAMModelParameters(maxOutstandingPuts=2, maxOutstandingGets=2)))
  val ram  = LazyModule(new TLRAM(AddressSet(0x800, 0x7ff)))
  val ram2 = LazyModule(new TLRAM(AddressSet(0, 0x3ff), beatBytes = 16))
//  val gpio = LazyModule(new RRTest1(0x400))
  val xbar = LazyModule(new TLXbar)
  val xbar2= LazyModule(new TLXbar)
  val buff = LazyModule(new TLBuffer())
  val fuzz = LazyModule(new TLFuzzer(nOperations=5000,
                     inFlight=4,
                     formalnoise=true
                        ))
//  val cross = LazyModule(new TLAsyncCrossing)

  model.node := fuzz.node
  buff.node := model.node
  xbar2.node := buff.node
  ram2.node := TLFragmenter(16, 256)(xbar2.node)
  xbar.node := TLWidthWidget(16)(TLHintHandler()(xbar2.node))
  ram.node := TLFragmenter(4, 32)(TLBuffer()(xbar.node))
//  cross.node := TLFragmenter(4,256)(TLBuffer(xbar.node))
//  ram.node := cross.node
//  gpio.node := TLFragmenter(4,32)(TLBuffer()(xbar.node)))

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := fuzz.module.io.finished

//    // Shove the RAM into another clock domain
//    val clocks = Module(new ClockDivider)
//    ram.module.clock := clocks.io.clock_out
//    ram.module.reset := clocks.io.reset_out
//    clocks.io.clock_in := clock
//    clocks.io.reset_in := reset
//
//    // ... and safely cross TL2 into it
//    cross.module.io.in_clock := clock
//    cross.module.io.in_reset := reset
//    cross.module.io.out_clock := clocks.io.clock_out
//    cross.module.io.out_reset := clocks.io.reset_out
  }
}


object FormalTester extends App {
  import chisel3.internal.firrtl.Circuit
  
  val circuit = Driver.elaborate(()=>LazyModule(new TLFuzzFormalTest3).module)
//  val circuit = Driver.elaborate(()=>new ContentAddressableMemory(t=UInt(width=32), size=8))
  Driver.dumpFirrtl(circuit, Some(new File("FormalTester.fir")))
}