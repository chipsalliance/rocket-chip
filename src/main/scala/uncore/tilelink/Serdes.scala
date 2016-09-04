package uncore.tilelink

import Chisel._
import junctions._
import scala.math.max
import cde.{Parameters, Field}

trait HasTileLinkSerializerParams extends HasTileLinkParameters {
  val nClientToManagerChannels = 3
  val nManagerToClientChannels = 2
  val clientToManagerChannelBits = log2Up(nClientToManagerChannels)
  val managerToClientChannelBits = log2Up(nManagerToClientChannels)

  def clientToManagerDataBits = {
    Seq(Wire(new AcquireFromSrc), Wire(new ReleaseFromSrc), Wire(new FinishToDst))
      .map(ch => ch.asUInt.getWidth)
      .reduce(max(_, _))
  }

  def managerToClientDataBits = {
    Seq(Wire(new GrantFromSrc), Wire(new GrantToDst), Wire(new ProbeToDst))
      .map(ch => ch.asUInt.getWidth)
      .reduce(max(_, _))
  }
}

abstract class TLSerBundle(implicit val p: Parameters)
    extends ParameterizedBundle()(p)
    with HasTileLinkSerializerParams

abstract class TLSerModule(implicit val p: Parameters)
    extends Module with HasTileLinkSerializerParams

class TLSerClientToManagerChannel(implicit p: Parameters)
    extends TLSerBundle()(p) {
  val chan = UInt(width = clientToManagerChannelBits)
  val data = UInt(width = clientToManagerDataBits)
  val last = Bool()
}

class TLSerManagerToClientChannel(implicit p: Parameters)
    extends TLSerBundle()(p) {
  val chan = UInt(width = managerToClientChannelBits)
  val data = UInt(width = managerToClientDataBits)
  val last = Bool()
}

class TLSerializedIO(implicit p: Parameters) extends TLSerBundle()(p) {
  val ctom = Decoupled(new TLSerClientToManagerChannel)
  val mtoc = Decoupled(new TLSerManagerToClientChannel).flip
}

trait HasTileLinkSerializers {
  def serialize(in: Acquire)(implicit p: Parameters): TLSerClientToManagerChannel = {
    val out = Wire(new TLSerClientToManagerChannel)
    out.chan := UInt(2)
    out.data := in.asUInt
    out.last := in.last()
    out
  }

  def serialize(in: Release)(implicit p: Parameters): TLSerClientToManagerChannel = {
    val out = Wire(new TLSerClientToManagerChannel)
    out.chan := UInt(1)
    out.data := in.asUInt
    out.last := in.last()
    out
  }

  def serialize(in: Finish)(implicit p: Parameters): TLSerClientToManagerChannel = {
    val out = Wire(new TLSerClientToManagerChannel)
    out.chan := UInt(0)
    out.data := in.asUInt
    out.last := Bool(true)
    out
  }

  def serialize(in: Probe)(implicit p: Parameters): TLSerManagerToClientChannel = {
    val out = Wire(new TLSerManagerToClientChannel)
    out.chan := UInt(1)
    out.data := in.asUInt
    out.last := Bool(true)
    out
  }

  def serialize(in: Grant)(implicit p: Parameters): TLSerManagerToClientChannel = {
    val out = Wire(new TLSerManagerToClientChannel)
    out.chan := UInt(0)
    out.data := in.asUInt
    out.last := in.last()
    out
  }
}

class ClientTileLinkIOSerdes(w: Int)(implicit p: Parameters)
    extends TLSerModule()(p) with HasTileLinkSerializers {
  val io = new Bundle {
    val tl = (new ClientTileLinkIO).flip
    val serial = new SerialIO(w)
  }

  val ctomArb = Module(new JunctionsPeekingArbiter(
    new TLSerClientToManagerChannel,
    nClientToManagerChannels,
    (b: TLSerClientToManagerChannel) => b.last))
  ctomArb.io.in(0).valid := io.tl.finish.valid
  io.tl.finish.ready := ctomArb.io.in(0).ready
  ctomArb.io.in(0).bits := serialize(io.tl.finish.bits)
  ctomArb.io.in(1).valid := io.tl.release.valid
  io.tl.release.ready := ctomArb.io.in(1).ready
  ctomArb.io.in(1).bits := serialize(io.tl.release.bits)
  ctomArb.io.in(2).valid := io.tl.acquire.valid
  io.tl.acquire.ready := ctomArb.io.in(2).ready
  ctomArb.io.in(2).bits := serialize(io.tl.acquire.bits)

  val ser = Module(new Serializer(w, new TLSerClientToManagerChannel))
  ser.io.in <> ctomArb.io.out
  io.serial.out <> ser.io.out

  val des = Module(new Deserializer(w, new TLSerManagerToClientChannel))
  des.io.in <> io.serial.in
  io.tl.grant.valid := des.io.out.valid && des.io.out.bits.chan === UInt(0)
  io.tl.grant.bits := io.tl.grant.bits.fromBits(des.io.out.bits.data)
  io.tl.probe.valid := des.io.out.valid && des.io.out.bits.chan === UInt(1)
  io.tl.probe.bits := io.tl.probe.bits.fromBits(des.io.out.bits.data)
  des.io.out.ready := MuxLookup(des.io.out.bits.chan, Bool(false), Seq(
    UInt(0) -> io.tl.grant.ready,
    UInt(1) -> io.tl.probe.ready))
}

class ClientTileLinkIODesser(w: Int)(implicit p: Parameters)
    extends TLSerModule()(p) with HasTileLinkSerializers {
  val io = new Bundle {
    val serial = new SerialIO(w)
    val tl = new ClientTileLinkIO
  }

  val mtocArb = Module(new JunctionsPeekingArbiter(
    new TLSerManagerToClientChannel,
    nManagerToClientChannels,
    (b: TLSerManagerToClientChannel) => b.last))
  mtocArb.io.in(0).valid := io.tl.grant.valid
  io.tl.grant.ready := mtocArb.io.in(0).ready
  mtocArb.io.in(0).bits := serialize(io.tl.grant.bits)
  mtocArb.io.in(1).valid := io.tl.probe.valid
  io.tl.probe.ready := mtocArb.io.in(1).ready
  mtocArb.io.in(1).bits := serialize(io.tl.probe.bits)

  val ser = Module(new Serializer(w, new TLSerManagerToClientChannel))
  ser.io.in <> mtocArb.io.out
  io.serial.out <> ser.io.out

  val des = Module(new Deserializer(w, new TLSerClientToManagerChannel))
  des.io.in <> io.serial.in
  io.tl.finish.valid := des.io.out.valid && des.io.out.bits.chan === UInt(0)
  io.tl.finish.bits := io.tl.finish.bits.fromBits(des.io.out.bits.data)
  io.tl.release.valid := des.io.out.valid && des.io.out.bits.chan === UInt(1)
  io.tl.release.bits := io.tl.release.bits.fromBits(des.io.out.bits.data)
  io.tl.acquire.valid := des.io.out.valid && des.io.out.bits.chan === UInt(2)
  io.tl.acquire.bits := io.tl.acquire.bits.fromBits(des.io.out.bits.data)
  des.io.out.ready := MuxLookup(des.io.out.bits.chan, Bool(false), Seq(
    UInt(0) -> io.tl.finish.ready,
    UInt(1) -> io.tl.release.ready,
    UInt(2) -> io.tl.acquire.ready))
}
