package object uncore {
import Chisel._

//TODO: Remove these Networking classes from the package object once Scala bug
//SI-3439 is resolved.

case class PhysicalNetworkConfiguration(nEndpoints: Int, idBits: Int)

class PhysicalHeader(implicit conf: PhysicalNetworkConfiguration) extends Bundle {
  val src = UFix(width = conf.idBits)
  val dst = UFix(width = conf.idBits)
}

abstract class PhysicalNetworkIO[T <: Data]()(data: => T)(implicit conf: PhysicalNetworkConfiguration) extends FIFOIO()(data) {
  val header = (new PhysicalHeader).asOutput
}

class BasicCrossbarIO[T <: Data]()(data: => T)(implicit conf: PhysicalNetworkConfiguration) extends PhysicalNetworkIO()(data)(conf)

abstract class PhysicalNetwork(conf: PhysicalNetworkConfiguration) extends Component

class BasicCrossbar[T <: Data]()(data: => T)(implicit conf: PhysicalNetworkConfiguration) extends PhysicalNetwork(conf) {
  val io = new Bundle {
    val in  = Vec(conf.nEndpoints) { (new BasicCrossbarIO) { data } }.flip 
    val out = Vec(conf.nEndpoints) { (new BasicCrossbarIO) { data } }
  }

  val rdyVecs = List.fill(conf.nEndpoints)(Vec(conf.nEndpoints){Bool()})

  io.out.zip(rdyVecs).zipWithIndex.map{ case ((out, rdys), i) => {
    val rrarb = new RRArbiter(conf.nEndpoints)(data)
    (rrarb.io.in, io.in, rdys).zipped.map{ case (arb, in, rdy) => {
      arb.valid := in.valid && (in.header.dst === UFix(i)) 
      arb.bits := in.bits
      rdy := arb.ready && (in.header.dst === UFix(i))
    }}
    out <> rrarb.io.out
    out.header.src := rrarb.io.chosen.toUFix
    out.header.dst := UFix(i)
  }}
  for(i <- 0 until conf.nEndpoints) {
    io.in(i).ready := rdyVecs.map(r => r(i)).reduceLeft(_||_)
  }
}

case class LogicalNetworkConfiguration(nEndpoints: Int, idBits: Int, nHubs: Int, nTiles: Int)

abstract class LogicalNetwork[TileLinkType <: Bundle](endpoints: Seq[CoherenceAgent])(implicit conf: LogicalNetworkConfiguration) extends Component {
  val io: Vec[TileLinkType]
  val physicalNetworks: Seq[PhysicalNetwork]
  require(endpoints.length == conf.nEndpoints)
}

class LogicalHeader(implicit conf: LogicalNetworkConfiguration) extends Bundle {
  val src = UFix(width = conf.idBits)
  val dst = UFix(width = conf.idBits)
}

abstract class LogicalNetworkIO[T <: Data]()(data: => T)(implicit conf: LogicalNetworkConfiguration) extends FIFOIO()(data) {
  val header = (new LogicalHeader).asOutput
}

class TileIO[T <: Data]()(data: => T)(implicit conf: LogicalNetworkConfiguration) extends LogicalNetworkIO()(data)(conf)
class HubIO[T <: Data]()(data: => T)(implicit conf: LogicalNetworkConfiguration) extends LogicalNetworkIO()(data)(conf){flip()}

class TileLinkIO(implicit conf: LogicalNetworkConfiguration) extends Bundle { 
  val xact_init      = (new TileIO) { new TransactionInit }
  val xact_init_data = (new TileIO) { new TransactionInitData }
  val xact_abort     = (new HubIO)  { new TransactionAbort }
  val probe_req      = (new HubIO)  { new ProbeRequest }
  val probe_rep      = (new TileIO) { new ProbeReply }
  val probe_rep_data = (new TileIO) { new ProbeReplyData }
  val xact_rep       = (new HubIO)  { new TransactionReply }
  val xact_finish    = (new TileIO) { new TransactionFinish }
  override def clone = { new TileLinkIO().asInstanceOf[this.type] }
}
}
