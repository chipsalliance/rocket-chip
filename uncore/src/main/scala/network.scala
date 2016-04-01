// See LICENSE for license details.

package uncore
import Chisel._
import cde.{Parameters, Field}

case object LNEndpoints extends Field[Int]
case object LNHeaderBits extends Field[Int]

class PhysicalHeader(n: Int) extends Bundle {
  val src = UInt(width = log2Up(n))
  val dst = UInt(width = log2Up(n))
}

class PhysicalNetworkIO[T <: Data](n: Int, dType: T) extends Bundle {
  val header = new PhysicalHeader(n)
  val payload = dType.cloneType
  override def cloneType = new PhysicalNetworkIO(n,dType).asInstanceOf[this.type]
}

class BasicCrossbarIO[T <: Data](n: Int, dType: T) extends Bundle {
    val in  = Vec(n, Decoupled(new PhysicalNetworkIO(n,dType))).flip 
    val out = Vec(n, Decoupled(new PhysicalNetworkIO(n,dType)))
}

abstract class PhysicalNetwork extends Module

class BasicCrossbar[T <: Data](n: Int, dType: T, count: Int = 1, needsLock: Option[PhysicalNetworkIO[T] => Bool] = None) extends PhysicalNetwork {
  val io = new BasicCrossbarIO(n, dType)

  io.in.foreach { _.ready := Bool(false) }

  io.out.zipWithIndex.map{ case (out, i) => {
    val rrarb = Module(new LockingRRArbiter(io.in(0).bits, n, count, needsLock))
    (rrarb.io.in, io.in).zipped.map{ case (arb, in) => {
      val destined = in.bits.header.dst === UInt(i)
      arb.valid := in.valid && destined
      arb.bits := in.bits
      when (arb.ready && destined) { in.ready := Bool(true) }
    }}
    out <> rrarb.io.out
  }}
}

abstract class LogicalNetwork extends Module

class LogicalHeader(implicit p: Parameters) extends junctions.ParameterizedBundle()(p) {
  val src = UInt(width = p(LNHeaderBits))
  val dst = UInt(width = p(LNHeaderBits))
}

class LogicalNetworkIO[T <: Data](dType: T)(implicit p: Parameters) extends Bundle {
  val header = new LogicalHeader
  val payload = dType.cloneType
  override def cloneType = new LogicalNetworkIO(dType)(p).asInstanceOf[this.type]
}

object DecoupledLogicalNetworkIOWrapper {
  def apply[T <: Data](
        in: DecoupledIO[T],
        src: UInt = UInt(0),
        dst: UInt = UInt(0))
      (implicit p: Parameters): DecoupledIO[LogicalNetworkIO[T]] = {
    val out = Wire(Decoupled(new LogicalNetworkIO(in.bits)))
    out.valid := in.valid
    out.bits.payload := in.bits
    out.bits.header.dst := dst
    out.bits.header.src := src
    in.ready := out.ready
    out
  }
}

object DecoupledLogicalNetworkIOUnwrapper {
  def apply[T <: Data](in: DecoupledIO[LogicalNetworkIO[T]])
                      (implicit p: Parameters): DecoupledIO[T] = {
    val out = Wire(Decoupled(in.bits.payload))
    out.valid := in.valid
    out.bits := in.bits.payload
    in.ready := out.ready
    out
  }
}

object DefaultFromPhysicalShim {
  def apply[T <: Data](in: DecoupledIO[PhysicalNetworkIO[T]])
                      (implicit p: Parameters): DecoupledIO[LogicalNetworkIO[T]] = {
    val out = Wire(Decoupled(new LogicalNetworkIO(in.bits.payload)))
    out.bits.header := in.bits.header
    out.bits.payload := in.bits.payload
    out.valid := in.valid
    in.ready := out.ready
    out
  }
}

object DefaultToPhysicalShim {
  def apply[T <: Data](n: Int, in: DecoupledIO[LogicalNetworkIO[T]])
                      (implicit p: Parameters): DecoupledIO[PhysicalNetworkIO[T]] = {
    val out = Wire(Decoupled(new PhysicalNetworkIO(n, in.bits.payload)))
    out.bits.header := in.bits.header
    out.bits.payload := in.bits.payload
    out.valid := in.valid
    in.ready := out.ready
    out
  }
}
