// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.util

import Chisel._

class ReorderQueueWrite[T <: Data](dType: T, tagWidth: Int) extends Bundle {
  val data = dType.cloneType
  val tag = UInt(width = tagWidth)

  override def cloneType =
    new ReorderQueueWrite(dType, tagWidth).asInstanceOf[this.type]
}

class ReorderEnqueueIO[T <: Data](dType: T, tagWidth: Int)
  extends DecoupledIO(new ReorderQueueWrite(dType, tagWidth)) {

  override def cloneType =
    new ReorderEnqueueIO(dType, tagWidth).asInstanceOf[this.type]
}

class ReorderDequeueIO[T <: Data](dType: T, tagWidth: Int) extends Bundle {
  val valid = Bool(INPUT)
  val tag = UInt(INPUT, tagWidth)
  val data = dType.cloneType.asOutput
  val matches = Bool(OUTPUT)

  override def cloneType =
    new ReorderDequeueIO(dType, tagWidth).asInstanceOf[this.type]
}

class ReorderQueue[T <: Data](dType: T, tagWidth: Int, size: Option[Int] = None)
    extends Module {
  val io = new Bundle {
    val enq = new ReorderEnqueueIO(dType, tagWidth).flip
    val deq = new ReorderDequeueIO(dType, tagWidth)
  }

  val tagSpaceSize = 1 << tagWidth
  val actualSize = size.getOrElse(tagSpaceSize)

  if (tagSpaceSize > actualSize) {
    val roq_data = Reg(Vec(actualSize, dType))
    val roq_tags = Reg(Vec(actualSize, UInt(width = tagWidth)))
    val roq_free = Reg(init = Vec.fill(actualSize)(Bool(true)))

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
      roq_free(roq_enq_addr) := Bool(false)
    }

    when (io.deq.valid) {
      roq_free(OHToUInt(roq_deq_onehot)) := Bool(true)
    }

    println(s"Warning - using a CAM for ReorderQueue, tagBits: ${tagWidth} size: ${actualSize}")
  } else {
    val roq_data = Mem(tagSpaceSize, dType)
    val roq_free = Reg(init = Vec.fill(tagSpaceSize)(Bool(true)))

    io.enq.ready := roq_free(io.enq.bits.tag)
    io.deq.data := roq_data(io.deq.tag)
    io.deq.matches := !roq_free(io.deq.tag)

    when (io.enq.valid && io.enq.ready) {
      roq_data(io.enq.bits.tag) := io.enq.bits.data
      roq_free(io.enq.bits.tag) := Bool(false)
    }

    when (io.deq.valid) {
      roq_free(io.deq.tag) := Bool(true)
    }
  }
}


