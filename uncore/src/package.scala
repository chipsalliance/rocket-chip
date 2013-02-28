package object uncore {
import Chisel._
import Node._
import scala.collection.mutable.Stack

//TODO: Remove these Networking classes from the package object once Scala bug
//SI-3439 is resolved.

case class PhysicalNetworkConfiguration(nEndpoints: Int, idBits: Int)

class PhysicalHeader(implicit conf: PhysicalNetworkConfiguration) extends Bundle {
  val src = UFix(width = conf.idBits)
  val dst = UFix(width = conf.idBits)
}

abstract class PhysicalNetworkIO[T <: Data]()(data: => T)(implicit conf: PhysicalNetworkConfiguration) extends Bundle {
  val header = (new PhysicalHeader)
  val payload = data
}

class BasicCrossbarIO[T <: Data]()(data: => T)(implicit conf: PhysicalNetworkConfiguration) extends PhysicalNetworkIO()(data)(conf) {
  override def clone = { new BasicCrossbarIO()(data).asInstanceOf[this.type] }
}

abstract class PhysicalNetwork(conf: PhysicalNetworkConfiguration) extends Component

class BasicCrossbar[T <: Data]()(data: => T)(implicit conf: PhysicalNetworkConfiguration) extends PhysicalNetwork(conf) {
  val io = new Bundle {
    val in  = Vec(conf.nEndpoints){(new FIFOIO){(new BasicCrossbarIO){data}}}.flip 
    val out = Vec(conf.nEndpoints){(new FIFOIO){(new BasicCrossbarIO){data}}}
  }

  val rdyVecs = List.fill(conf.nEndpoints)(Vec(conf.nEndpoints){Bool()})

  io.out.zip(rdyVecs).zipWithIndex.map{ case ((out, rdys), i) => {
    val rrarb = (new RRArbiter(conf.nEndpoints)){io.in(0).bits.clone}
    (rrarb.io.in, io.in, rdys).zipped.map{ case (arb, in, rdy) => {
      arb.valid := in.valid && (in.bits.header.dst === UFix(i)) 
      arb.bits := in.bits
      rdy := arb.ready && (in.bits.header.dst === UFix(i))
    }}
    out <> rrarb.io.out
    //out.bits.header.src := rrarb.io.chosen.toUFix
    //out.bits.header.dst := UFix(i)
  }}
  for(i <- 0 until conf.nEndpoints) {
    io.in(i).ready := rdyVecs.map(r => r(i)).reduceLeft(_||_)
  }
}

case class LogicalNetworkConfiguration(nEndpoints: Int, idBits: Int, nHubs: Int, nTiles: Int)

abstract class LogicalNetwork[TileLinkType <: Bundle](endpoints: Seq[CoherenceAgentRole])(implicit conf: LogicalNetworkConfiguration) extends Component {
  val io: Vec[TileLinkType]
  val physicalNetworks: Seq[PhysicalNetwork]
  require(endpoints.length == conf.nEndpoints)
}

class LogicalHeader(implicit conf: LogicalNetworkConfiguration) extends Bundle {
  val src = UFix(width = conf.idBits)
  val dst = UFix(width = conf.idBits)
}

object FIFOedLogicalNetworkIOWrapper {
  def apply[T <: Data](in: FIFOIO[T], src: UFix = UFix(0), dst: UFix = UFix(0))(implicit conf: LogicalNetworkConfiguration) = {
    val shim = (new FIFOedLogicalNetworkIOWrapper(src, dst)){ in.bits.clone }
    shim.io.in.valid := in.valid
    shim.io.in.bits := in.bits
    in.ready := shim.io.in.ready
    shim.io.out
  }
}
class FIFOedLogicalNetworkIOWrapper[T <: Data](src: UFix, dst: UFix)(data: => T)(implicit lconf: LogicalNetworkConfiguration) extends Component {
  val io = new Bundle {
    val in = (new FIFOIO){ data }.flip
    val out = (new FIFOIO){(new LogicalNetworkIO){ data }} 
  }
  io.out.valid := io.in.valid
  io.out.bits.payload := io.in.bits
  io.out.bits.header.dst := dst
  io.out.bits.header.src := src
  io.in.ready := io.out.ready
}

object FIFOedLogicalNetworkIOUnwrapper {
  def apply[T <: Data](in: FIFOIO[LogicalNetworkIO[T]])(implicit conf: LogicalNetworkConfiguration) = {
    val shim = (new FIFOedLogicalNetworkIOUnwrapper){ in.bits.payload.clone }
    shim.io.in.valid := in.valid
    shim.io.in.bits := in.bits
    in.ready := shim.io.in.ready
    shim.io.out
  }
}
class FIFOedLogicalNetworkIOUnwrapper[T <: Data]()(data: => T)(implicit lconf: LogicalNetworkConfiguration) extends Component {
  val io = new Bundle {
    val in = (new FIFOIO){(new LogicalNetworkIO){ data }}.flip
    val out = (new FIFOIO){ data }
  }
  io.out.valid := io.in.valid
  io.out.bits := io.in.bits.payload
  io.in.ready := io.out.ready
}

class LogicalNetworkIO[T <: Data]()(data: => T)(implicit conf: LogicalNetworkConfiguration) extends Bundle {
  val header = new LogicalHeader
  val payload = data
  override def clone = { new LogicalNetworkIO()(data).asInstanceOf[this.type] }
}
}
