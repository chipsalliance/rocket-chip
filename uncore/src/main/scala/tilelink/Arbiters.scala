package uncore.tilelink
import Chisel._
import junctions._
import cde.{Parameters, Field}

/** Utility functions for constructing TileLinkIO arbiters */
trait TileLinkArbiterLike extends HasTileLinkParameters {
  // Some shorthand type variables
  type ManagerSourcedWithId = ManagerToClientChannel with HasClientTransactionId
  type ClientSourcedWithId = ClientToManagerChannel with HasClientTransactionId
  type ClientSourcedWithIdAndData = ClientToManagerChannel with HasClientTransactionId with HasTileLinkData

  val arbN: Int // The number of ports on the client side

  // These abstract funcs are filled in depending on whether the arbiter mucks with the 
  // outgoing client ids to track sourcing and then needs to revert them on the way back
  def clientSourcedClientXactId(in: ClientSourcedWithId, id: Int): Bits
  def managerSourcedClientXactId(in: ManagerSourcedWithId): Bits
  def arbIdx(in: ManagerSourcedWithId): UInt

  // The following functions are all wiring helpers for each of the different types of TileLink channels

  def hookupClientSource[M <: ClientSourcedWithIdAndData](
      clts: Seq[DecoupledIO[LogicalNetworkIO[M]]],
      mngr: DecoupledIO[LogicalNetworkIO[M]]) {
    def hasData(m: LogicalNetworkIO[M]) = m.payload.hasMultibeatData()
    val arb = Module(new LockingRRArbiter(mngr.bits, arbN, tlDataBeats, Some(hasData _)))
    clts.zipWithIndex.zip(arb.io.in).map{ case ((req, id), arb) => {
      arb.valid := req.valid
      arb.bits := req.bits
      arb.bits.payload.client_xact_id := clientSourcedClientXactId(req.bits.payload, id)
      req.ready := arb.ready
    }}
    mngr <> arb.io.out
  }

  def hookupClientSourceHeaderless[M <: ClientSourcedWithIdAndData](
      clts: Seq[DecoupledIO[M]],
      mngr: DecoupledIO[M]) {
    def hasData(m: M) = m.hasMultibeatData()
    val arb = Module(new LockingRRArbiter(mngr.bits, arbN, tlDataBeats, Some(hasData _)))
    clts.zipWithIndex.zip(arb.io.in).map{ case ((req, id), arb) => {
      arb.valid := req.valid
      arb.bits := req.bits
      arb.bits.client_xact_id := clientSourcedClientXactId(req.bits, id)
      req.ready := arb.ready
    }}
    mngr <> arb.io.out
  }

  def hookupManagerSourceWithHeader[M <: ManagerToClientChannel](
      clts: Seq[DecoupledIO[LogicalNetworkIO[M]]], 
      mngr: DecoupledIO[LogicalNetworkIO[M]]) {
    mngr.ready := Bool(false)
    for (i <- 0 until arbN) {
      clts(i).valid := Bool(false)
      when (mngr.bits.header.dst === UInt(i)) {
        clts(i).valid := mngr.valid
        mngr.ready := clts(i).ready
      }
      clts(i).bits := mngr.bits
    }
  }

  def hookupManagerSourceWithId[M <: ManagerSourcedWithId](
      clts: Seq[DecoupledIO[LogicalNetworkIO[M]]], 
      mngr: DecoupledIO[LogicalNetworkIO[M]]) {
    mngr.ready := Bool(false)
    for (i <- 0 until arbN) {
      clts(i).valid := Bool(false)
      when (arbIdx(mngr.bits.payload) === UInt(i)) {
        clts(i).valid := mngr.valid
        mngr.ready := clts(i).ready
      }
      clts(i).bits := mngr.bits
      clts(i).bits.payload.client_xact_id := managerSourcedClientXactId(mngr.bits.payload)
    }
  }

  def hookupManagerSourceHeaderlessWithId[M <: ManagerSourcedWithId](
      clts: Seq[DecoupledIO[M]], 
      mngr: DecoupledIO[M]) {
    mngr.ready := Bool(false)
    for (i <- 0 until arbN) {
      clts(i).valid := Bool(false)
      when (arbIdx(mngr.bits) === UInt(i)) {
        clts(i).valid := mngr.valid
        mngr.ready := clts(i).ready
      }
      clts(i).bits := mngr.bits
      clts(i).bits.client_xact_id := managerSourcedClientXactId(mngr.bits)
    }
  }

  def hookupManagerSourceBroadcast[M <: Data](clts: Seq[DecoupledIO[M]], mngr: DecoupledIO[M]) {
    clts.map{ _.valid := mngr.valid }
    clts.map{ _.bits := mngr.bits }
    mngr.ready := clts.map(_.ready).reduce(_&&_)
  }

  def hookupFinish[M <: LogicalNetworkIO[Finish]]( clts: Seq[DecoupledIO[M]], mngr: DecoupledIO[M]) {
    val arb = Module(new RRArbiter(mngr.bits, arbN))
    arb.io.in <> clts
    mngr <> arb.io.out
  }
}

/** Abstract base case for any Arbiters that have UncachedTileLinkIOs */
abstract class UncachedTileLinkIOArbiter(val arbN: Int)(implicit val p: Parameters) extends Module
    with TileLinkArbiterLike {
  val io = new Bundle {
    val in = Vec(arbN, new UncachedTileLinkIO).flip
    val out = new UncachedTileLinkIO
  }
  hookupClientSource(io.in.map(_.acquire), io.out.acquire)
  hookupFinish(io.in.map(_.finish), io.out.finish)
  hookupManagerSourceWithId(io.in.map(_.grant), io.out.grant)
}

/** Abstract base case for any Arbiters that have cached TileLinkIOs */
abstract class TileLinkIOArbiter(val arbN: Int)(implicit val p: Parameters) extends Module
    with TileLinkArbiterLike {
  val io = new Bundle {
    val in = Vec(arbN, new TileLinkIO).flip
    val out = new TileLinkIO
  }
  hookupClientSource(io.in.map(_.acquire), io.out.acquire)
  hookupClientSource(io.in.map(_.release), io.out.release)
  hookupFinish(io.in.map(_.finish), io.out.finish)
  hookupManagerSourceBroadcast(io.in.map(_.probe), io.out.probe)
  hookupManagerSourceWithId(io.in.map(_.grant), io.out.grant)
}

/** Appends the port index of the arbiter to the client_xact_id */
trait AppendsArbiterId extends TileLinkArbiterLike {
  def clientSourcedClientXactId(in: ClientSourcedWithId, id: Int) =
    Cat(in.client_xact_id, UInt(id, log2Up(arbN)))
  def managerSourcedClientXactId(in: ManagerSourcedWithId) = {
    /* This shouldn't be necessary, but Chisel3 doesn't emit correct Verilog
     * when right shifting by too many bits.  See
     * https://github.com/ucb-bar/firrtl/issues/69 */
    if (in.client_xact_id.getWidth > log2Up(arbN))
      in.client_xact_id >> log2Up(arbN)
    else
      UInt(0)
  }
  def arbIdx(in: ManagerSourcedWithId) = in.client_xact_id(log2Up(arbN)-1,0)
}

/** Uses the client_xact_id as is (assumes it has been set to port index) */
trait PassesId extends TileLinkArbiterLike {
  def clientSourcedClientXactId(in: ClientSourcedWithId, id: Int) = in.client_xact_id
  def managerSourcedClientXactId(in: ManagerSourcedWithId) = in.client_xact_id
  def arbIdx(in: ManagerSourcedWithId) = in.client_xact_id
}

/** Overwrites some default client_xact_id with the port idx */
trait UsesNewId extends TileLinkArbiterLike {
  def clientSourcedClientXactId(in: ClientSourcedWithId, id: Int) = UInt(id, log2Up(arbN))
  def managerSourcedClientXactId(in: ManagerSourcedWithId) = UInt(0)
  def arbIdx(in: ManagerSourcedWithId) = in.client_xact_id
}

// Now we can mix-in thevarious id-generation traits to make concrete arbiter classes
class UncachedTileLinkIOArbiterThatAppendsArbiterId(val n: Int)(implicit p: Parameters) extends UncachedTileLinkIOArbiter(n)(p) with AppendsArbiterId
class UncachedTileLinkIOArbiterThatPassesId(val n: Int)(implicit p: Parameters) extends UncachedTileLinkIOArbiter(n)(p) with PassesId
class UncachedTileLinkIOArbiterThatUsesNewId(val n: Int)(implicit p: Parameters) extends UncachedTileLinkIOArbiter(n)(p) with UsesNewId
class TileLinkIOArbiterThatAppendsArbiterId(val n: Int)(implicit p: Parameters) extends TileLinkIOArbiter(n)(p) with AppendsArbiterId
class TileLinkIOArbiterThatPassesId(val n: Int)(implicit p: Parameters) extends TileLinkIOArbiter(n)(p) with PassesId
class TileLinkIOArbiterThatUsesNewId(val n: Int)(implicit p: Parameters) extends TileLinkIOArbiter(n)(p) with UsesNewId

/** Concrete uncached client-side arbiter that appends the arbiter's port id to client_xact_id */
class ClientUncachedTileLinkIOArbiter(val arbN: Int)(implicit val p: Parameters) extends Module with TileLinkArbiterLike with AppendsArbiterId {
  val io = new Bundle {
    val in = Vec(arbN, new ClientUncachedTileLinkIO).flip
    val out = new ClientUncachedTileLinkIO
  }
  if (arbN > 1) {
    hookupClientSourceHeaderless(io.in.map(_.acquire), io.out.acquire)
    hookupManagerSourceHeaderlessWithId(io.in.map(_.grant), io.out.grant)
  } else { io.out <> io.in.head }
}

/** Concrete client-side arbiter that appends the arbiter's port id to client_xact_id */
class ClientTileLinkIOArbiter(val arbN: Int)(implicit val p: Parameters) extends Module with TileLinkArbiterLike with AppendsArbiterId {
  val io = new Bundle {
    val in = Vec(arbN, new ClientTileLinkIO).flip
    val out = new ClientTileLinkIO
  }
  if (arbN > 1) {
    hookupClientSourceHeaderless(io.in.map(_.acquire), io.out.acquire)
    hookupClientSourceHeaderless(io.in.map(_.release), io.out.release)
    hookupManagerSourceBroadcast(io.in.map(_.probe), io.out.probe)
    hookupManagerSourceHeaderlessWithId(io.in.map(_.grant), io.out.grant)
  } else { io.out <> io.in.head }
}
