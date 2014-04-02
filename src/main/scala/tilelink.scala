package uncore
import Chisel._

case class TileLinkConfiguration(co: CoherencePolicyWithUncached, ln: LogicalNetworkConfiguration, addrBits: Int, masterXactIdBits: Int, clientXactIdBits: Int, dataBits: Int, writeMaskBits: Int, wordAddrBits: Int, atomicOpBits: Int) 

abstract trait TileLinkSubBundle extends Bundle {
  implicit val conf: TileLinkConfiguration
  override def clone = this.getClass.getConstructors.head.newInstance(conf).asInstanceOf[this.type]
}

trait HasPhysicalAddress extends TileLinkSubBundle {
  val addr = UInt(width = conf.addrBits)
}

trait HasClientTransactionId extends TileLinkSubBundle {
  val client_xact_id = Bits(width = conf.clientXactIdBits)
}

trait HasMasterTransactionId extends TileLinkSubBundle {
  val master_xact_id = Bits(width = conf.masterXactIdBits)
}

trait HasTileLinkData extends TileLinkSubBundle {
  val data = Bits(width = conf.dataBits)
}

trait SourcedMessage extends Bundle
trait ClientSourcedMessage extends SourcedMessage
trait MasterSourcedMessage extends SourcedMessage

object Acquire 
{
  def apply(a_type: Bits, addr: UInt, client_xact_id: UInt)(implicit conf: TileLinkConfiguration): Acquire = {
    val acq = new Acquire
    acq.a_type := a_type
    acq.addr := addr
    acq.client_xact_id := client_xact_id
    acq.data := Bits(0)
    acq.write_mask := Bits(0)
    acq.subword_addr := Bits(0)
    acq.atomic_opcode := Bits(0)
    acq
  }
  def apply(a_type: Bits, addr: UInt, client_xact_id: UInt, data: UInt)(implicit conf: TileLinkConfiguration): Acquire =  {
    val acq = apply(a_type, addr, client_xact_id)
    acq.data := data
    acq
  }
  def apply(a_type: UInt, addr: UInt, client_xact_id: UInt, write_mask: Bits, data: UInt)(implicit conf: TileLinkConfiguration): Acquire = {
    val acq = apply(a_type, addr, client_xact_id, data)
    acq.write_mask := write_mask
    acq
  }
  def apply(a_type: UInt, addr: UInt, client_xact_id: UInt, subword_addr: UInt, atomic_opcode: UInt, data: UInt)(implicit conf: TileLinkConfiguration): Acquire = {
    val acq = apply(a_type, addr, client_xact_id, data)
    acq.subword_addr := subword_addr
    acq.atomic_opcode := atomic_opcode
    acq
  }
  def apply(a: Acquire)(implicit conf: TileLinkConfiguration): Acquire = {
    val acq = new Acquire
    acq := a
    acq
  }
}

class Acquire(implicit val conf: TileLinkConfiguration) extends ClientSourcedMessage 
    with HasPhysicalAddress 
    with HasClientTransactionId 
    with HasTileLinkData {
  val a_type = UInt(width = conf.co.acquireTypeWidth)
  val write_mask = Bits(width = conf.writeMaskBits)
  val subword_addr = Bits(width = conf.wordAddrBits)
  val atomic_opcode = Bits(width = conf.atomicOpBits)
}

object Probe
{
  def apply(p_type: UInt, addr: UInt, master_xact_id: UInt)(implicit conf: TileLinkConfiguration) = {
    val prb = new Probe
    prb.p_type := p_type
    prb.addr := addr
    prb.master_xact_id := master_xact_id
    prb
  }
}

class Probe(implicit val conf: TileLinkConfiguration) extends MasterSourcedMessage 
    with HasPhysicalAddress 
    with HasMasterTransactionId {
  val p_type = UInt(width = conf.co.probeTypeWidth)
}

object Release
{
  def apply(r_type: UInt, addr: UInt, data: UInt)(implicit conf: TileLinkConfiguration) = {
    val rel = new Release
    rel.r_type := r_type
    rel.addr := addr
    rel.data := data
    rel
  }
  def apply(r_type: UInt, addr: UInt, client_xact_id: UInt, master_xact_id: UInt)(implicit conf: TileLinkConfiguration) = {
    val rel = new Release
    rel.r_type := r_type
    rel.addr := addr
    rel.client_xact_id := client_xact_id
    rel.master_xact_id := master_xact_id
    rel.data := UInt(0)
    rel
  }
  def apply(r_type: UInt, addr: UInt, client_xact_id: UInt, master_xact_id: UInt, data: UInt)(implicit conf: TileLinkConfiguration): Release = {
    val rel = apply(r_type, addr, client_xact_id, master_xact_id)
    rel.data := data
    rel
  }
}

class Release(implicit val conf: TileLinkConfiguration) extends ClientSourcedMessage 
    with HasPhysicalAddress 
    with HasClientTransactionId 
    with HasMasterTransactionId 
    with HasTileLinkData {
  val r_type = UInt(width = conf.co.releaseTypeWidth)
}

object Grant
{
  def apply(g_type: UInt, client_xact_id: UInt, master_xact_id: UInt)(implicit conf: TileLinkConfiguration) = {
    val gnt = new Grant
    gnt.g_type := g_type
    gnt.client_xact_id := client_xact_id
    gnt.master_xact_id := master_xact_id
    gnt.data := UInt(0)
    gnt
  }
  def apply(g_type: UInt, client_xact_id: UInt, master_xact_id: UInt, data: UInt)(implicit conf: TileLinkConfiguration): Grant = {
    val gnt = apply(g_type, client_xact_id, master_xact_id)
    gnt.data := data
    gnt
  }
}

class Grant(implicit val conf: TileLinkConfiguration) extends MasterSourcedMessage 
    with HasTileLinkData 
    with HasClientTransactionId 
    with HasMasterTransactionId {
  val g_type = UInt(width = conf.co.grantTypeWidth)
}

class GrantAck(implicit val conf: TileLinkConfiguration) extends ClientSourcedMessage with HasMasterTransactionId


class UncachedTileLinkIO(implicit conf: TileLinkConfiguration) extends Bundle {
  implicit val ln = conf.ln
  val acquire   = new DecoupledIO(new LogicalNetworkIO(new Acquire))
  val grant     = new DecoupledIO(new LogicalNetworkIO(new Grant)).flip
  val grant_ack = new DecoupledIO(new LogicalNetworkIO(new GrantAck))
  override def clone = { new UncachedTileLinkIO().asInstanceOf[this.type] }
}

class TileLinkIO(implicit conf: TileLinkConfiguration) extends UncachedTileLinkIO()(conf) { 
  val probe     = new DecoupledIO(new LogicalNetworkIO(new Probe)).flip
  val release   = new DecoupledIO(new LogicalNetworkIO(new Release))
  override def clone = { new TileLinkIO().asInstanceOf[this.type] }
}

abstract class TileLinkArbiterLike(val arbN: Int)(implicit conf: TileLinkConfiguration) extends Module {
  implicit val (ln, co) = (conf.ln, conf.co)

  type MasterSourcedWithId = MasterSourcedMessage with HasClientTransactionId
  type ClientSourcedWithId = ClientSourcedMessage with HasClientTransactionId 

  def clientSourcedClientXactId(in: ClientSourcedWithId, id: Int): Bits
  def masterSourcedClientXactId(in: MasterSourcedWithId): Bits
  def arbIdx(in: MasterSourcedWithId): UInt

  def hookupClientSource[M <: ClientSourcedWithId]
                        (ins: Seq[DecoupledIO[LogicalNetworkIO[M]]], 
                         out: DecoupledIO[LogicalNetworkIO[M]]) {
    def hasData(m: LogicalNetworkIO[M]) = co.messageHasData(m.payload)
    val arb = Module(new RRArbiter(out.bits.clone, arbN))
    out <> arb.io.out
    ins.zipWithIndex.zip(arb.io.in).map{ case ((req,id), arb) => {
      arb.valid := req.valid
      arb.bits := req.bits
      arb.bits.payload.client_xact_id := clientSourcedClientXactId(req.bits.payload, id)
      req.ready := arb.ready
    }}
  }

  def hookupMasterSource[M <: MasterSourcedWithId]
                        (ins: Seq[DecoupledIO[LogicalNetworkIO[M]]], 
                         out: DecoupledIO[LogicalNetworkIO[M]]) {
    out.ready := Bool(false)
    for (i <- 0 until arbN) {
      ins(i).valid := Bool(false)
      when (arbIdx(out.bits.payload) === UInt(i)) {
        ins(i).valid := out.valid
        out.ready := ins(i).ready
      }
      ins(i).bits := out.bits
      ins(i).bits.payload.client_xact_id := masterSourcedClientXactId(out.bits.payload) 
    }
  }
}

abstract class UncachedTileLinkIOArbiter(n: Int)(implicit conf: TileLinkConfiguration) extends TileLinkArbiterLike(n)(conf) {
  val io = new Bundle {
    val in = Vec.fill(n){new UncachedTileLinkIO}.flip
    val out = new UncachedTileLinkIO
  }

  hookupClientSource(io.in.map(_.acquire), io.out.acquire)
  hookupMasterSource(io.in.map(_.grant), io.out.grant)

  val grant_ack_arb = Module(new RRArbiter(new LogicalNetworkIO(new GrantAck), n))
  io.out.grant_ack <> grant_ack_arb.io.out
  grant_ack_arb.io.in zip io.in map { case (arb, req) => arb <> req.grant_ack }
}

abstract class TileLinkIOArbiter(n: Int)(implicit conf: TileLinkConfiguration) extends TileLinkArbiterLike(n)(conf) {
  val io = new Bundle {
    val in = Vec.fill(n){new TileLinkIO}.flip
    val out = new TileLinkIO
  }

  hookupClientSource(io.in.map(_.acquire), io.out.acquire)
  hookupClientSource(io.in.map(_.release), io.out.release)
  hookupMasterSource(io.in.map(_.grant), io.out.grant)

  io.in.map{ _.probe.valid := io.out.probe.valid }
  io.in.map{ _.probe.bits := io.out.probe.bits }
  io.out.probe.ready := io.in.map(_.probe.ready).reduce(_||_)

  val grant_ack_arb = Module(new RRArbiter(new LogicalNetworkIO(new GrantAck), n))
  io.out.grant_ack <> grant_ack_arb.io.out
  grant_ack_arb.io.in zip io.in map { case (arb, req) => arb <> req.grant_ack }
}

abstract trait AppendsArbiterId {
  val arbN: Int
  def clientSourcedClientXactId(in: ClientSourcedMessage with HasClientTransactionId, id: Int) =
    Cat(in.client_xact_id, UInt(id, log2Up(arbN)))
  def masterSourcedClientXactId(in: MasterSourcedMessage with HasClientTransactionId) = 
    in.client_xact_id >> UInt(log2Up(arbN))
  def arbIdx(in: MasterSourcedMessage with HasClientTransactionId) = 
    in.client_xact_id(log2Up(arbN)-1,0).toUInt
}

abstract trait PassesId {
  def clientSourcedClientXactId(in: ClientSourcedMessage with HasClientTransactionId, id: Int) = 
    in.client_xact_id
  def masterSourcedClientXactId(in: MasterSourcedMessage with HasClientTransactionId) = 
    in.client_xact_id
  def arbIdx(in: MasterSourcedMessage with HasClientTransactionId) = 
    in.client_xact_id
}

abstract trait UsesNewId {
  val arbN: Int
  def clientSourcedClientXactId(in: ClientSourcedMessage with HasClientTransactionId, id: Int) = 
    UInt(id, log2Up(arbN))
  def masterSourcedClientXactId(in: MasterSourcedMessage with HasClientTransactionId) = 
    UInt(0)
  def arbIdx(in: MasterSourcedMessage with HasClientTransactionId) = 
    in.client_xact_id
}

class UncachedTileLinkIOArbiterThatAppendsArbiterId(val n: Int)(implicit conf: TileLinkConfiguration) extends UncachedTileLinkIOArbiter(n)(conf) with AppendsArbiterId
class UncachedTileLinkIOArbiterThatPassesId(val n: Int)(implicit conf: TileLinkConfiguration) extends UncachedTileLinkIOArbiter(n)(conf) with PassesId
class UncachedTileLinkIOArbiterThatUsesNewId(val n: Int)(implicit conf: TileLinkConfiguration) extends UncachedTileLinkIOArbiter(n)(conf) with UsesNewId
class TileLinkIOArbiterThatAppendsArbiterId(val n: Int)(implicit conf: TileLinkConfiguration) extends TileLinkIOArbiter(n)(conf) with AppendsArbiterId
class TileLinkIOArbiterThatPassesId(val n: Int)(implicit conf: TileLinkConfiguration) extends TileLinkIOArbiter(n)(conf) with PassesId
class TileLinkIOArbiterThatUsesNewId(val n: Int)(implicit conf: TileLinkConfiguration) extends TileLinkIOArbiter(n)(conf) with UsesNewId
