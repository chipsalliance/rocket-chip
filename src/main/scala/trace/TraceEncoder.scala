// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.trace

import chisel3._
import chisel3.util._
import scala.math.min

import org.chipsalliance.cde.config.Parameters

case class TraceEncoderParams(
  coreParams: TraceCoreParams,
  bufferDepth: Int,
  encoderBaseAddr: BigInt,
  useArbiterMonitor: Boolean,
  // a seq of functions that takes a parameter and returns a lazymodule and a target id
  buildSinks: Seq[Parameters => (LazyTraceSink, Int)] = Seq.empty[Parameters => (LazyTraceSink, Int)]
)

object FullHeaderType extends ChiselEnum {
  val FTakenBranch    = Value(0x0.U) // 000
  val FNotTakenBranch = Value(0x1.U) // 001
  val FUninfJump      = Value(0x2.U) // 010
  val FInfJump        = Value(0x3.U) // 011
  val FTrap           = Value(0x4.U) // 100
  val FSync           = Value(0x5.U) // 101
  val FValue          = Value(0x6.U) // 110
  val FReserved       = Value(0x7.U) // 111
}

object CompressedHeaderType extends ChiselEnum {
  val CTB = Value(0x0.U) // 00, taken branch
  val CNT = Value(0x1.U) // 01, not taken branch
  val CNA = Value(0x2.U) // 10, not a compressed packet
  val CIJ = Value(0x3.U) // 11, is a jump
}

object TraceSinkTarget extends ChiselEnum {
  val STPrint = Value(0x0.U) // 00, RTL print
  val STDMA = Value(0x1.U) // 01, DMA
}

object TrapType extends ChiselEnum {
  val TNone      = Value(0x0.U)
  val TException = Value(0x1.U)
  val TInterrupt = Value(0x2.U)
  val TReturn    = Value(0x4.U)
}

object HeaderByte {
  def apply(header_type: FullHeaderType.Type, trap_type: TrapType.Type): UInt = {
    Cat(
      trap_type.asUInt,
      header_type.asUInt,
      CompressedHeaderType.CNA.asUInt
    )
  }
}

// Variable-length encoding helper module
class VarLenEncoder(val maxWidth: Int) extends Module {
  val maxNumBytes = maxWidth/(8-1) + 1
  
  val io = IO(new Bundle {
    val input_value = Input(UInt(maxWidth.W))
    val input_valid = Input(Bool())
    val output_num_bytes = Output(UInt(log2Ceil(maxNumBytes).W))
    val output_bytes = Output(Vec(maxNumBytes, UInt(8.W)))
  })

  // 0-indexed MSB index 
  val msb_index = (maxWidth - 1).U - PriorityEncoder(Reverse(io.input_value))
  io.output_num_bytes := Mux(io.input_valid, (msb_index / 7.U) + 1.U, 0.U)

  for (i <- 0 until maxNumBytes) {
    val is_last_byte = (i.U === (io.output_num_bytes - 1.U))
    io.output_bytes(i) := Mux(i.U < io.output_num_bytes,
      io.input_value(min(i*7+6, maxWidth-1), i*7) | Mux(is_last_byte, 0x80.U, 0.U),
      0.U
    )
  }
}

// slice packets into bytes TODO: is this efficient?
class TracePacketizer(val params: TraceEncoderParams) extends Module {
  def getMaxNumBytes(width: Int): Int = { width/(8-1) + 1 }
  val addrMaxNumBytes = getMaxNumBytes(params.coreParams.iaddrWidth)
  val timeMaxNumBytes = getMaxNumBytes(params.coreParams.xlen)
  // println(s"addrMaxNumBytes: $addrMaxNumBytes, timeMaxNumBytes: $timeMaxNumBytes")
  val metaDataWidth = 2 * log2Ceil(addrMaxNumBytes) + log2Ceil(timeMaxNumBytes) + 1
  val io = IO(new Bundle {
    val target_addr = Flipped(Decoupled(Vec(addrMaxNumBytes, UInt(8.W))))
    val trap_addr = Flipped(Decoupled(Vec(addrMaxNumBytes, UInt(8.W))))
    val time = Flipped(Decoupled(Vec(timeMaxNumBytes, UInt(8.W))))
    val byte = Flipped(Decoupled(UInt(8.W)))
    val metadata = Flipped(Decoupled(UInt(metaDataWidth.W)))
    val out = Decoupled(UInt(8.W))
  })

  val pIdle :: pComp :: pFull :: Nil = Enum(3)
  val state = RegInit(pIdle)

  val trap_addr_num_bytes = Reg(UInt(log2Ceil(addrMaxNumBytes).W))
  val trap_addr_index = Reg(UInt(log2Ceil(addrMaxNumBytes).W))
  val target_addr_num_bytes = Reg(UInt(log2Ceil(addrMaxNumBytes).W))
  val target_addr_index = Reg(UInt(log2Ceil(addrMaxNumBytes).W))
  val time_num_bytes = Reg(UInt(log2Ceil(timeMaxNumBytes).W))
  val time_index = Reg(UInt(log2Ceil(timeMaxNumBytes).W))
  val header_num_bytes = Reg(UInt(1.W))
  val header_index = Reg(UInt(1.W))
  
  val is_compressed = io.metadata.bits(0)
  val trap_addr_metadata = io.metadata.bits(metaDataWidth-1, log2Ceil(timeMaxNumBytes)+log2Ceil(addrMaxNumBytes)+1)
  val target_addr_metadata = io.metadata.bits(log2Ceil(timeMaxNumBytes)+log2Ceil(addrMaxNumBytes), log2Ceil(timeMaxNumBytes)+1)
  val time_metadata = io.metadata.bits(log2Ceil(timeMaxNumBytes), 1)
  
  // default values
  io.out.valid := false.B
  io.metadata.ready := false.B
  io.target_addr.ready := false.B
  io.trap_addr.ready := false.B
  io.time.ready := false.B
  io.byte.ready := false.B
  io.out.bits := 0.U

  def prep_next_state(): Unit = {
    trap_addr_index := 0.U
    trap_addr_num_bytes := Mux(io.metadata.fire, trap_addr_metadata, 0.U)
    target_addr_index := 0.U
    target_addr_num_bytes := Mux(io.metadata.fire, target_addr_metadata, 0.U)
    time_index := 0.U
    time_num_bytes := Mux(io.metadata.fire, time_metadata, 0.U)
    header_index := 0.U
    header_num_bytes := Mux(io.metadata.fire, ~is_compressed, 0.U)
    state := Mux(io.metadata.fire, 
      Mux(is_compressed, pComp, pFull),
      pIdle
    )
  }
  
  switch (state) {
    is (pIdle) {
      io.metadata.ready := true.B
      when (io.metadata.fire) {
        trap_addr_num_bytes := trap_addr_metadata
        trap_addr_index := 0.U
        target_addr_num_bytes := target_addr_metadata
        target_addr_index := 0.U
        time_num_bytes := time_metadata
        time_index := 0.U
        header_num_bytes := ~is_compressed
        header_index := 0.U
        state := Mux(is_compressed, pComp, pFull)
      }
    }
    is (pComp) {
      // transmit a byte from byte buffer
      io.byte.ready := io.out.ready
      io.out.valid := io.byte.valid
      io.out.bits := io.byte.bits
      when (io.byte.fire) {
        // metadata runs ahead by 1 cycle for performance optimization
        io.metadata.ready := true.B
        prep_next_state()
      }
    }
    is (pFull) {
      // header, addr, time
      io.out.valid := true.B
      when (header_num_bytes > 0.U && header_index < header_num_bytes) {
        io.out.bits := io.byte.bits
        io.out.valid := io.byte.valid
        header_index := header_index + io.out.fire
      } .elsewhen (trap_addr_num_bytes > 0.U && trap_addr_index < trap_addr_num_bytes) {
        io.out.bits := io.trap_addr.bits(trap_addr_index)
        io.out.valid := io.trap_addr.valid
        trap_addr_index := trap_addr_index + io.out.fire
      } .elsewhen (target_addr_num_bytes > 0.U && target_addr_index < target_addr_num_bytes) {
        io.out.bits := io.target_addr.bits(target_addr_index)
        io.out.valid := io.target_addr.valid
        target_addr_index := target_addr_index + io.out.fire
      } .elsewhen (time_num_bytes > 0.U && time_index < time_num_bytes) {
        io.out.bits := io.time.bits(time_index)
        io.out.valid := io.time.valid
        time_index := time_index + io.out.fire
      } .otherwise {
        // FIXME: delay for 1 cycle
        io.out.valid := false.B
        // release buffers
        io.byte.ready := true.B
        // if we ever have a packet, we need to be ready to accept it
        io.target_addr.ready := target_addr_num_bytes =/= 0.U
        io.trap_addr.ready := trap_addr_num_bytes =/= 0.U
        io.time.ready := time_num_bytes =/= 0.U
        io.metadata.ready := true.B
        prep_next_state()
      }
    }
  }
}

class TraceEncoder(val params: TraceEncoderParams) extends Module {
  val io = IO(new Bundle {
    val control = Input(new TraceEncoderControlInterface())
    val in = Input(new TraceCoreInterface(params.coreParams))
    val stall = Output(Bool())
    val out = Decoupled(UInt(8.W))
  })

  val MAX_DELTA_TIME_COMP = 0xCF // 63, 6 bits

  when (io.stall) {
    printf("TraceEncoder stall detected\n")
  }

  // states
  val sIdle :: sSync :: sData :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val enabled = RegInit(false.B)
  val stall = Wire(Bool())
  val prev_time = Reg(UInt(params.coreParams.xlen.W))
  // pipeline of ingress data
  val ingress_0 = RegInit(0.U.asTypeOf(new TraceCoreInterface(params.coreParams)))
  val ingress_1 = RegInit(0.U.asTypeOf(new TraceCoreInterface(params.coreParams)))

  // shift every cycle, if not stalled
  val pipeline_advance = Wire(Bool())
  pipeline_advance := !stall && io.in.group(0).iretire === 1.U
  when (pipeline_advance) {
    ingress_0 := io.in
    ingress_1 := ingress_0
  }

  // encoders
  val trap_addr_encoder = Module(new VarLenEncoder(params.coreParams.iaddrWidth))
  val target_addr_encoder = Module(new VarLenEncoder(params.coreParams.iaddrWidth))
  val time_encoder = Module(new VarLenEncoder(params.coreParams.xlen))
  val metadataWidth = log2Ceil(trap_addr_encoder.maxNumBytes) + log2Ceil(target_addr_encoder.maxNumBytes) + log2Ceil(time_encoder.maxNumBytes) + 1

  // queue buffers
  val trap_addr_buffer = Module(new Queue(Vec(trap_addr_encoder.maxNumBytes, UInt(8.W)), params.bufferDepth))
  val target_addr_buffer = Module(new Queue(Vec(target_addr_encoder.maxNumBytes, UInt(8.W)), params.bufferDepth))
  val time_buffer = Module(new Queue(Vec(time_encoder.maxNumBytes, UInt(8.W)), params.bufferDepth))
  val byte_buffer = Module(new Queue(UInt(8.W), params.bufferDepth)) // buffer compressed packet or full header
  val metadata_buffer = Module(new Queue(UInt(metadataWidth.W), params.bufferDepth))
  // intermediate varlen encoder signals
  val full_trap_addr      = Wire(Vec(trap_addr_encoder.maxNumBytes, UInt(8.W)))
  val trap_addr_num_bytes = Wire(UInt(log2Ceil(trap_addr_encoder.maxNumBytes).W))
  val full_target_addr      = Wire(Vec(target_addr_encoder.maxNumBytes, UInt(8.W)))
  val target_addr_num_bytes = Wire(UInt(log2Ceil(target_addr_encoder.maxNumBytes).W))
  val full_time      = Wire(Vec(time_encoder.maxNumBytes, UInt(8.W)))
  val time_num_bytes = Wire(UInt(log2Ceil(time_encoder.maxNumBytes).W))
  full_trap_addr := trap_addr_encoder.io.output_bytes
  trap_addr_num_bytes := trap_addr_encoder.io.output_num_bytes
  full_target_addr := target_addr_encoder.io.output_bytes
  target_addr_num_bytes := target_addr_encoder.io.output_num_bytes
  full_time := time_encoder.io.output_bytes
  time_num_bytes := time_encoder.io.output_num_bytes

  // intermediate packet signals
  val is_compressed = Wire(Bool())
  val delta_time = ingress_1.time - prev_time
  val packet_valid = Wire(Bool())
  val header_byte   = Wire(UInt(8.W)) // full header
  val comp_packet   = Wire(UInt(8.W)) // compressed packet
  val comp_header   = Wire(UInt(CompressedHeaderType.getWidth.W)) // compressed header
  comp_packet := Cat(delta_time(5, 0), comp_header)

  // packetization of buffered message
  val trace_packetizer = Module(new TracePacketizer(params))
  trace_packetizer.io.target_addr <> target_addr_buffer.io.deq
  trace_packetizer.io.trap_addr <> trap_addr_buffer.io.deq
  trace_packetizer.io.time <> time_buffer.io.deq
  trace_packetizer.io.byte <> byte_buffer.io.deq
  trace_packetizer.io.metadata <> metadata_buffer.io.deq
  trace_packetizer.io.out <> io.out

  // intermediate encoder control signals
  val encode_trap_addr_valid = Wire(Bool())
  val encode_target_addr_valid = Wire(Bool())

  // metadata buffering
  val metadata = Cat(trap_addr_num_bytes, target_addr_num_bytes, time_num_bytes, is_compressed)
  metadata_buffer.io.enq.bits := metadata
  metadata_buffer.io.enq.valid := packet_valid
  // buffering compressed packet or full header depending on is_compressed
  byte_buffer.io.enq.bits := Mux(is_compressed, comp_packet, header_byte)
  byte_buffer.io.enq.valid := packet_valid
  // trap address buffering
  trap_addr_buffer.io.enq.bits := full_trap_addr
  trap_addr_buffer.io.enq.valid := !is_compressed && packet_valid && encode_trap_addr_valid
  // target address buffering
  target_addr_buffer.io.enq.bits := full_target_addr
  target_addr_buffer.io.enq.valid := !is_compressed && packet_valid && encode_target_addr_valid
  // time buffering
  time_buffer.io.enq.bits := full_time
  time_buffer.io.enq.valid := !is_compressed && packet_valid

  // stall if any buffer is full
  stall := !target_addr_buffer.io.enq.ready || !time_buffer.io.enq.ready || !byte_buffer.io.enq.ready
  io.stall := stall
  
  val sent = RegInit(false.B)
  // reset takes priority over enqueue
  when (pipeline_advance) {
    sent := false.B
  } .elsewhen (byte_buffer.io.enq.fire) {
    sent := true.B
  }

  trap_addr_encoder.io.input_valid := encode_trap_addr_valid && !is_compressed && packet_valid
  target_addr_encoder.io.input_valid := encode_target_addr_valid && !is_compressed && packet_valid
  time_encoder.io.input_valid := !is_compressed && packet_valid

  // default values
  trap_addr_encoder.io.input_value := 0.U
  target_addr_encoder.io.input_value := 0.U
  time_encoder.io.input_value := 0.U
  is_compressed := false.B
  packet_valid := false.B
  encode_target_addr_valid := false.B
  encode_trap_addr_valid := false.B
  comp_header := CompressedHeaderType.CNA.asUInt
  header_byte := HeaderByte(FullHeaderType.FReserved, TrapType.TNone)
  // state machine
  switch (state) {
    is (sIdle) {
      when (io.control.enable) { state := sSync }
    }
    is (sSync) {
      header_byte := HeaderByte(FullHeaderType.FSync, TrapType.TNone)
      time_encoder.io.input_value := ingress_0.time
      prev_time := ingress_0.time
      target_addr_encoder.io.input_value := ingress_0.group(0).iaddr >> 1.U // last bit is always 0
      encode_target_addr_valid := true.B
      is_compressed := false.B
      packet_valid := !sent
      // state transition: wait for message to go in
      state := Mux(pipeline_advance && (sent || byte_buffer.io.enq.fire), Mux(io.control.enable, sData, sIdle), sSync)
    }
    is (sData) {
      when (!io.control.enable) {
        state := sSync
      } .otherwise {
        switch (ingress_1.group(0).itype) {
          is (TraceItype.ITNothing) {
            packet_valid := false.B
          }
          is (TraceItype.ITBrTaken) {
            header_byte := HeaderByte(FullHeaderType.FTakenBranch, TrapType.TNone)
            comp_header := CompressedHeaderType.CTB.asUInt
            time_encoder.io.input_value := delta_time
            prev_time := ingress_1.time
            is_compressed := delta_time <= MAX_DELTA_TIME_COMP.U
            packet_valid := !sent
          }
          is (TraceItype.ITBrNTaken) {
            header_byte := HeaderByte(FullHeaderType.FNotTakenBranch, TrapType.TNone)
            comp_header := CompressedHeaderType.CNT.asUInt
            time_encoder.io.input_value := delta_time
            prev_time := ingress_1.time
            is_compressed := delta_time <= MAX_DELTA_TIME_COMP.U
            packet_valid := !sent
          }
          is (TraceItype.ITInJump) {
            header_byte := HeaderByte(FullHeaderType.FInfJump, TrapType.TNone)
            comp_header := CompressedHeaderType.CIJ.asUInt
            time_encoder.io.input_value := delta_time
            prev_time := ingress_1.time
            is_compressed := delta_time <= MAX_DELTA_TIME_COMP.U
            packet_valid := !sent
          }
          is (TraceItype.ITUnJump) {
            header_byte := HeaderByte(FullHeaderType.FUninfJump, TrapType.TNone)
            time_encoder.io.input_value := delta_time 
            prev_time := ingress_1.time
            target_addr_encoder.io.input_value := (ingress_1.group(0).iaddr ^ ingress_0.group(0).iaddr) >> 1.U
            encode_target_addr_valid := true.B
            is_compressed := false.B
            packet_valid := !sent
          }
          is (TraceItype.ITException) {
            header_byte := HeaderByte(FullHeaderType.FTrap, TrapType.TException)
            comp_header := CompressedHeaderType.CNA.asUInt
            time_encoder.io.input_value := delta_time
            prev_time := ingress_1.time
            target_addr_encoder.io.input_value := (ingress_1.group(0).iaddr ^ ingress_0.group(0).iaddr) >> 1.U
            encode_target_addr_valid := true.B
            trap_addr_encoder.io.input_value := ingress_1.group(0).iaddr
            encode_trap_addr_valid := true.B
            is_compressed := false.B
            packet_valid := !sent
          }
          is (TraceItype.ITInterrupt) {
            header_byte := HeaderByte(FullHeaderType.FTrap, TrapType.TInterrupt)
            comp_header := CompressedHeaderType.CNA.asUInt
            time_encoder.io.input_value := delta_time
            prev_time := ingress_1.time
            target_addr_encoder.io.input_value := (ingress_1.group(0).iaddr ^ ingress_0.group(0).iaddr) >> 1.U
            encode_target_addr_valid := true.B
            trap_addr_encoder.io.input_value := ingress_1.group(0).iaddr
            encode_trap_addr_valid := true.B
            is_compressed := false.B
            packet_valid := !sent
          }
          is (TraceItype.ITReturn) {
            header_byte := HeaderByte(FullHeaderType.FTrap, TrapType.TReturn)
            comp_header := CompressedHeaderType.CNA.asUInt
            time_encoder.io.input_value := delta_time
            prev_time := ingress_1.time
            target_addr_encoder.io.input_value := (ingress_1.group(0).iaddr ^ ingress_0.group(0).iaddr) >> 1.U
            encode_target_addr_valid := true.B
            trap_addr_encoder.io.input_value := ingress_1.group(0).iaddr
            encode_trap_addr_valid := true.B
            is_compressed := false.B
            packet_valid := !sent
          }
        }
      }
    }
  }
}
