package rocket

import Chisel._
import Node._
import Constants._

class SkidBuffer[T <: Data](resetSignal: Bool = null)(data: => T) extends Component(resetSignal)
{
  val io = new Bundle {
    val enq = new FIFOIO()(data).flip
    val deq = new FIFOIO()(data)
  }

  val fq = new Queue(1, flow = true)(data)
  val pq = new Queue(1, pipe = true)(data)

  fq.io.enq <> io.enq
  pq.io.enq <> fq.io.deq
  io.deq <> pq.io.deq
}

object SkidBuffer
{
  def apply[T <: Data](enq: FIFOIO[T]): FIFOIO[T] = {
    val s = new SkidBuffer()(enq.bits.clone)
    s.io.enq <> enq
    s.io.deq
  }
}
