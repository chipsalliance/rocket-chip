// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util.{DecoupledIO, Mux1H, OHToUInt, PriorityEncoder, PriorityEncoderOH}

class ReorderQueueWrite[T <: Data](dType: T, tagWidth: Int) extends Bundle {
  val data = dType.cloneType
  val tag = UInt(tagWidth.W)

}

class ReorderEnqueueIO[T <: Data](dType: T, tagWidth: Int)
  extends DecoupledIO(new ReorderQueueWrite(dType, tagWidth)) {

}

class ReorderDequeueIO[T <: Data](dType: T, tagWidth: Int) extends Bundle {
  val valid = Input(Bool())
  val tag = Input(UInt(tagWidth.W))
  val data = Output(dType.cloneType)
  val matches = Output(Bool())

}

class ReorderQueue[T <: Data](dType: T, tagWidth: Int, size: Option[Int] = None)
    extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(new ReorderEnqueueIO(dType, tagWidth))
    val deq = new ReorderDequeueIO(dType, tagWidth)
  })

  val tagSpaceSize = 1 << tagWidth
  val actualSize = size.getOrElse(tagSpaceSize)

  if (tagSpaceSize > actualSize) {
    val roq_data = Reg(Vec(actualSize, dType))
    val roq_tags = Reg(Vec(actualSize, UInt(tagWidth.W)))
    val roq_free = RegInit(VecInit(Seq.fill(actualSize)(true.B)))

    val roq_enq_addr = PriorityEncoder(roq_free)
    val roq_matches = roq_tags.zip(roq_free)
      .map { case (tag, free) => tag === io.deq.tag && !free }
    val roq_deq_onehot = PriorityEncoderOH(roq_matches)

    io.enq.ready := roq_free.reduce(_ || _)
    io.deq.data := Mux1H(roq_deq_onehot, roq_data)
    io.deq.matches := roq_matches.reduce(_ || _)

    when (io.enq.valid && io.enq.ready) {
      roq_data(roq_enq_addr) := io.enq.bits.data
      roq_tags(roq_enq_addr) := io.enq.bits.tag
      roq_free(roq_enq_addr) := false.B
    }

    when (io.deq.valid) {
      roq_free(OHToUInt(roq_deq_onehot)) := true.B
    }

    println(s"Warning - using a CAM for ReorderQueue, tagBits: ${tagWidth} size: ${actualSize}")
  } else {
    val roq_data = Mem(tagSpaceSize, dType)
    val roq_free = RegInit(VecInit(Seq.fill(tagSpaceSize)(true.B)))

    io.enq.ready := roq_free(io.enq.bits.tag)
    io.deq.data := roq_data(io.deq.tag)
    io.deq.matches := !roq_free(io.deq.tag)

    when (io.enq.valid && io.enq.ready) {
      roq_data(io.enq.bits.tag) := io.enq.bits.data
      roq_free(io.enq.bits.tag) := false.B
    }

    when (io.deq.valid) {
      roq_free(io.deq.tag) := true.B
    }
  }
}


