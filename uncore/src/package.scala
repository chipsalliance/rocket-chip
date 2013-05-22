package object uncore {
import Chisel._
import Node._
import scala.collection.mutable.Stack

//TODO: Remove these Networking classes from the package object once Scala bug
//SI-3439 is resolved.

implicit def toOption[A](a: A) = Option(a)

class PairedDataIO[M <: Data, D <: Data]()(m: => M, d: => D) extends Bundle {
  val meta = new FIFOIO()(m)
  val data = new FIFOIO()(d)
  override def clone = { new PairedDataIO()(m,d).asInstanceOf[this.type] }
}

class PairedArbiterIO[M <: Data, D <: Data](n: Int)(m: => M, d: => D) extends Bundle {
  val in  = Vec(n) { new PairedDataIO()(m,d) }.flip
  val out = new PairedDataIO()(m,d)  
  val meta_chosen = Bits(OUTPUT, log2Up(n)) 
  val data_chosen = Bits(OUTPUT, log2Up(n)) 
  override def clone = { new PairedArbiterIO(n)(m,d).asInstanceOf[this.type] }
}

class PairedLockingRRArbiter[M <: Data, D <: Data](n: Int, count: Int, needsLock: Option[M => Bool] = None)(meta: => M, data: => D) extends Component {
  require(isPow2(count))
  val io = new PairedArbiterIO(n)(meta,data)
  val locked  = if(count > 1) Reg(resetVal = Bool(false)) else Bool(false)
  val lockIdx = if(count > 1) Reg(resetVal = UFix(n-1)) else UFix(n-1)
  val grant = List.fill(n)(Bool())
  val meta_chosen = Bits(width = log2Up(n))

  val chosen_meta_has_data = needsLock.map(_(io.in(meta_chosen).meta.bits)).getOrElse(Bool(true))
  val valid_meta_has_data = io.in(meta_chosen).meta.valid && chosen_meta_has_data
  val grant_chosen_meta = !(locked && chosen_meta_has_data)
  (0 until n).map(i => io.in(i).meta.ready := grant(i) && grant_chosen_meta && io.out.meta.ready)
  (0 until n).map(i => io.in(i).data.ready := Mux(locked, lockIdx === UFix(i), grant(i) && valid_meta_has_data) && io.out.data.ready)
  io.out.meta.valid := io.in(meta_chosen).meta.valid && grant_chosen_meta
  io.out.data.valid := Mux(locked, io.in(lockIdx).data.valid, io.in(meta_chosen).data.valid && valid_meta_has_data)
  io.out.meta.bits := io.in(meta_chosen).meta.bits
  io.out.data.bits := Mux(locked, io.in(lockIdx).data.bits, io.in(meta_chosen).data.bits)
  io.meta_chosen := meta_chosen
  io.data_chosen := Mux(locked, lockIdx, meta_chosen)

  if(count > 1){
    val cnt = Reg(resetVal = UFix(0, width = log2Up(count)))
    val cnt_next = cnt + UFix(1)
    when(io.out.data.fire()){
      cnt := cnt_next
      when(cnt_next === UFix(0)) {
        locked := Bool(false)
      }
    }
    when(io.out.meta.fire()) {
      when(needsLock.map(_(io.out.meta.bits)).getOrElse(Bool(true))) {
        when(!locked) {
          locked := Bool(true)
          lockIdx := Vec(io.in.map{in => in.meta.fire()}){Bool()}.indexWhere{i: Bool => i} 
        }
      }
    }
  }
  val last_grant = Reg(resetVal = Bits(0, log2Up(n)))
  val ctrl = ArbiterCtrl((0 until n).map(i => io.in(i).meta.valid && UFix(i) > last_grant) ++ io.in.map(_.meta.valid))
  (0 until n).map(i => grant(i) := ctrl(i) && UFix(i) > last_grant || ctrl(i + n))

  var choose = Bits(n-1)
  for (i <- n-2 to 0 by -1)
    choose = Mux(io.in(i).meta.valid, Bits(i), choose)
  for (i <- n-1 to 1 by -1)
    choose = Mux(io.in(i).meta.valid && UFix(i) > last_grant, Bits(i), choose)
  meta_chosen := choose

  when (io.out.meta.fire()) { last_grant := meta_chosen }
}

class PairedCrossbar[M <: Data, D <: Data](count: Int, needsLock: Option[BasicCrossbarIO[M] => Bool] = None)(meta: => M, data: => D)(implicit conf: PhysicalNetworkConfiguration) extends PhysicalNetwork(conf) {
  val io = new Bundle {
    val in  = Vec(conf.nEndpoints){new PairedDataIO()(new BasicCrossbarIO()(meta),new BasicCrossbarIO()(data))}.flip 
    val out = Vec(conf.nEndpoints){new PairedDataIO()(new BasicCrossbarIO()(meta),new BasicCrossbarIO()(data))}
  }

  val metaRdyVecs = List.fill(conf.nEndpoints)(Vec(conf.nEndpoints){Bool()})
  val dataRdyVecs = List.fill(conf.nEndpoints)(Vec(conf.nEndpoints){Bool()})
  val rdyVecs = metaRdyVecs zip dataRdyVecs

  io.out.zip(rdyVecs).zipWithIndex.map{ case ((out, rdys), i) => {
    val rrarb = new PairedLockingRRArbiter(conf.nEndpoints, count, needsLock)(io.in(0).meta.bits.clone, io.in(0).data.bits.clone)
    rrarb.io.in zip io.in zip rdys._1 zip rdys._2 map { case (((arb, in), meta_rdy), data_rdy) => {
      arb.meta.valid := in.meta.valid && (in.meta.bits.header.dst === UFix(i)) 
      arb.meta.bits := in.meta.bits
      meta_rdy := arb.meta.ready && (in.meta.bits.header.dst === UFix(i))
      arb.data.valid := in.data.valid && (in.data.bits.header.dst === UFix(i)) 
      arb.data.bits := in.data.bits
      data_rdy := arb.data.ready && (in.data.bits.header.dst === UFix(i))
    }}
    out <> rrarb.io.out
  }}
  for(i <- 0 until conf.nEndpoints) {
    io.in(i).meta.ready := rdyVecs.map(r => r._1(i)).reduceLeft(_||_)
    io.in(i).data.ready := rdyVecs.map(r => r._2(i)).reduceLeft(_||_)
  }
}

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

class BasicCrossbar[T <: Data](count: Int = 1)(data: => T)(implicit conf: PhysicalNetworkConfiguration) extends PhysicalNetwork(conf) {
  val io = new Bundle {
    val in  = Vec(conf.nEndpoints){(new FIFOIO){(new BasicCrossbarIO){data}}}.flip 
    val out = Vec(conf.nEndpoints){(new FIFOIO){(new BasicCrossbarIO){data}}}
  }

  val rdyVecs = List.fill(conf.nEndpoints)(Vec(conf.nEndpoints){Bool()})

  io.out.zip(rdyVecs).zipWithIndex.map{ case ((out, rdys), i) => {
    val rrarb = (new LockingRRArbiter(conf.nEndpoints, count)){io.in(0).bits.clone}
    (rrarb.io.in, io.in, rdys).zipped.map{ case (arb, in, rdy) => {
      arb.valid := in.valid && (in.bits.header.dst === UFix(i)) 
      arb.bits := in.bits
      rdy := arb.ready && (in.bits.header.dst === UFix(i))
    }}
    out <> rrarb.io.out
  }}
  for(i <- 0 until conf.nEndpoints) {
    io.in(i).ready := rdyVecs.map(r => r(i)).reduceLeft(_||_)
  }
}

case class LogicalNetworkConfiguration(nEndpoints: Int, idBits: Int, nMasters: Int, nClients: Int)

abstract class LogicalNetwork[TileLinkType <: Bundle](endpoints: Seq[CoherenceAgentRole])(implicit conf: LogicalNetworkConfiguration) extends Component {
  override val io: Vec[TileLinkType]
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
