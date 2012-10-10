package rocket

import Chisel._
import Node._;

class SkidBuffer[T <: Data](entries: Int, lateEnq: Boolean = false)(data: => T) extends Component
{
  val io = new Bundle {
    val enq = new FIFOIO()(data).flip
    val deq = new FIFOIO()(data)
  }

  require(entries >= 2)
  val fq = new Queue(1, flow = true)(data)
  val pq = new Queue(entries-1, pipe = true)(data)
  val (iq, oq) = if (lateEnq) (pq, fq) else (fq, pq)

  iq.io.enq <> io.enq
  oq.io.enq <> iq.io.deq
  io.deq <> oq.io.deq
}

object SkidBuffer
{
  def apply[T <: Data](enq: FIFOIO[T], entries: Int = 2): FIFOIO[T] = {
    val s = new SkidBuffer(entries)(enq.bits.clone)
    s.io.enq <> enq
    s.io.deq
  }
}
