package object uncore {
import Chisel._

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

  for(i <- 0 until conf.nEndpoints) {
    val rrarb = new RRArbiter(conf.nEndpoints)(data)
    (rrarb.io.in, io.in).zipped.map( (arb, io) => {
      arb.valid := io.valid && (io.header.dst === UFix(i)) 
      arb.bits := io.bits
      io.ready := arb.ready
    })
    io.out(i) <> rrarb.io.out
  }
}

case class LogicalNetworkConfiguration(nEndpoints: Int, idBits: Int, nHubs: Int, nTiles: Int)

abstract class LogicalNetwork[TileLinkType <: Bundle](endpoints: Seq[Component])(implicit conf: LogicalNetworkConfiguration) extends Component {
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

class TileLink(implicit conf: LogicalNetworkConfiguration) extends Bundle { 
  val xact_init      = (new TileIO) { new TransactionInit }
  val xact_init_data = (new TileIO) { new TransactionInitData }
  val xact_abort     = (new HubIO)  { new TransactionAbort }
  val probe_req      = (new HubIO)  { new ProbeRequest }
  val probe_rep      = (new TileIO) { new ProbeReply }
  val probe_rep_data = (new TileIO) { new ProbeReplyData }
  val xact_rep       = (new HubIO)  { new TransactionReply }
  val xact_finish    = (new TileIO) { new TransactionFinish }
  val incoherent     = Bool(OUTPUT)
}
}
