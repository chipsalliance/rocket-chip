package util

import Chisel._
import cde.Parameters

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

class ReorderQueue[T <: Data](dType: T, tagWidth: Int,
    size: Option[Int] = None, nDeq: Int = 1)
    extends Module {
  val io = new Bundle {
    val enq = new ReorderEnqueueIO(dType, tagWidth).flip
    val deq = Vec(nDeq, new ReorderDequeueIO(dType, tagWidth))
  }

  val tagSpaceSize = 1 << tagWidth
  val actualSize = size.getOrElse(tagSpaceSize)

  if (tagSpaceSize > actualSize) {
    val roq_data = Reg(Vec(actualSize, dType))
    val roq_tags = Reg(Vec(actualSize, UInt(width = tagWidth)))
    val roq_free = Reg(init = Vec.fill(actualSize)(Bool(true)))

    val roq_enq_addr = PriorityEncoder(roq_free)

    io.enq.ready := roq_free.reduce(_ || _)

    when (io.enq.valid && io.enq.ready) {
      roq_data(roq_enq_addr) := io.enq.bits.data
      roq_tags(roq_enq_addr) := io.enq.bits.tag
      roq_free(roq_enq_addr) := Bool(false)
    }

    io.deq.foreach { deq =>
      val roq_matches = roq_tags.zip(roq_free)
        .map { case (tag, free) => tag === deq.tag && !free }
      val roq_deq_onehot = PriorityEncoderOH(roq_matches)

      deq.data := Mux1H(roq_deq_onehot, roq_data)
      deq.matches := roq_matches.reduce(_ || _)

      when (deq.valid) {
        roq_free(OHToUInt(roq_deq_onehot)) := Bool(true)
      }
    }

    println(s"Warning - using a CAM for ReorderQueue, tagBits: ${tagWidth} size: ${actualSize}")
  } else if (tagSpaceSize == actualSize) {
    val roq_data = Mem(tagSpaceSize, dType)
    val roq_free = Reg(init = Vec.fill(tagSpaceSize)(Bool(true)))

    io.enq.ready := roq_free(io.enq.bits.tag)

    when (io.enq.valid && io.enq.ready) {
      roq_data(io.enq.bits.tag) := io.enq.bits.data
      roq_free(io.enq.bits.tag) := Bool(false)
    }

    io.deq.foreach { deq =>
      deq.data := roq_data(deq.tag)
      deq.matches := !roq_free(deq.tag)

      when (deq.valid) {
        roq_free(deq.tag) := Bool(true)
      }
    }
  } else {
    require(actualSize % tagSpaceSize == 0)

    val qDepth = actualSize / tagSpaceSize
    val queues = Seq.fill(tagSpaceSize) {
      Module(new Queue(dType, qDepth))
    }

    io.enq.ready := Bool(false)
    io.deq.foreach(_.matches := Bool(false))
    io.deq.foreach(_.data := dType.fromBits(UInt(0)))

    for ((q, i) <- queues.zipWithIndex) {
      when (io.enq.bits.tag === UInt(i)) { io.enq.ready := q.io.enq.ready }
      q.io.enq.valid := io.enq.valid && io.enq.bits.tag === UInt(i)
      q.io.enq.bits  := io.enq.bits.data

      val deqReadys = Wire(Vec(nDeq, Bool()))
      io.deq.zip(deqReadys).foreach { case (deq, rdy) =>
        when (deq.tag === UInt(i)) {
          deq.matches := q.io.deq.valid
          deq.data := q.io.deq.bits
        }
        rdy := deq.valid && deq.tag === UInt(i)
      }
      q.io.deq.ready := deqReadys.reduce(_ || _)
    }
  }
}
