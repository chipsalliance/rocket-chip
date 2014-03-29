package uncore
import Chisel._

case class PhysicalNetworkConfiguration(nEndpoints: Int, idBits: Int)

class PhysicalHeader(implicit conf: PhysicalNetworkConfiguration) extends Bundle {
  val src = UInt(width = conf.idBits)
  val dst = UInt(width = conf.idBits)
}

class PhysicalNetworkIO[T <: Data](dType: T)(implicit conf: PhysicalNetworkConfiguration) extends Bundle {
  val header = new PhysicalHeader
  val payload = dType.clone
  override def clone = { new PhysicalNetworkIO(dType).asInstanceOf[this.type] }
}

abstract class PhysicalNetwork(conf: PhysicalNetworkConfiguration) extends Module

class BasicCrossbarIO[T <: Data](dType: T)(implicit conf: PhysicalNetworkConfiguration) extends Bundle {
    val in  = Vec.fill(conf.nEndpoints){Decoupled(new PhysicalNetworkIO(dType))}.flip 
    val out = Vec.fill(conf.nEndpoints){Decoupled(new PhysicalNetworkIO(dType))}
}
class BasicCrossbar[T <: Data](dType: T, count: Int = 1)(implicit conf: PhysicalNetworkConfiguration) extends PhysicalNetwork(conf) {
  val io = new BasicCrossbarIO(dType)

  val rdyVecs = List.fill(conf.nEndpoints)(Vec.fill(conf.nEndpoints)(Bool()))

  io.out.zip(rdyVecs).zipWithIndex.map{ case ((out, rdys), i) => {
    val rrarb = Module(new LockingRRArbiter(io.in(0).bits, conf.nEndpoints, count))
    (rrarb.io.in, io.in, rdys).zipped.map{ case (arb, in, rdy) => {
      arb.valid := in.valid && (in.bits.header.dst === UInt(i)) 
      arb.bits := in.bits
      rdy := arb.ready && (in.bits.header.dst === UInt(i))
    }}
    out <> rrarb.io.out
  }}
  for(i <- 0 until conf.nEndpoints) {
    io.in(i).ready := rdyVecs.map(r => r(i)).reduceLeft(_||_)
  }
}

case class LogicalNetworkConfiguration(idBits: Int, nMasters: Int, nClients: Int) {
  val nEndpoints = nMasters + nClients
}

abstract class LogicalNetwork[TileLinkType <: Bundle](implicit conf: LogicalNetworkConfiguration) extends Module

class LogicalHeader(implicit conf: LogicalNetworkConfiguration) extends Bundle {
  val src = UInt(width = conf.idBits)
  val dst = UInt(width = conf.idBits)
}

class LogicalNetworkIO[T <: Data](dType: T)(implicit conf: LogicalNetworkConfiguration) extends Bundle {
  val header = new LogicalHeader
  val payload = dType.clone
  override def clone = { new LogicalNetworkIO(dType).asInstanceOf[this.type] }
}

object DecoupledLogicalNetworkIOWrapper {
  def apply[T <: Data](in: DecoupledIO[T], src: UInt = UInt(0), dst: UInt = UInt(0))(implicit conf: LogicalNetworkConfiguration) = {
    val out = Decoupled(new LogicalNetworkIO(in.bits.clone)).asDirectionless
    out.valid := in.valid
    out.bits.payload := in.bits
    out.bits.header.dst := dst
    out.bits.header.src := src
    in.ready := out.ready
    out
  }
}

object DecoupledLogicalNetworkIOUnwrapper {
  def apply[T <: Data](in: DecoupledIO[LogicalNetworkIO[T]])(implicit conf: LogicalNetworkConfiguration) = {
    val out = Decoupled(in.bits.payload.clone).asDirectionless
    out.valid := in.valid
    out.bits := in.bits.payload
    in.ready := out.ready
    out
  }
}
