package referencechip

import Chisel._
import uncore._
import scala.reflect._
import scala.reflect.runtime.universe._

object TileLinkHeaderOverwriter {
  def apply[T <: ClientSourcedMessage](in: DecoupledIO[LogicalNetworkIO[T]], clientId: Int)(implicit conf: TileLinkConfiguration): DecoupledIO[LogicalNetworkIO[T]] = {
    val out = in.clone.asDirectionless
    out.bits.payload := in.bits.payload
    out.bits.header.src := UInt(clientId)
    out.bits.header.dst := in.bits.header.dst
    out.valid := in.valid
    in.ready := out.ready
    out
  }
  def apply[T <: ClientSourcedMessage with HasPhysicalAddress](in: DecoupledIO[LogicalNetworkIO[T]], clientId: Int, nBanks: Int, addrConvert: UInt => UInt)(implicit conf: TileLinkConfiguration): DecoupledIO[LogicalNetworkIO[T]] = {
    val out: DecoupledIO[LogicalNetworkIO[T]] = apply(in, clientId)
    out.bits.header.dst := (if(nBanks > 1) addrConvert(in.bits.payload.addr) else UInt(0))
    out
  }
}

class ReferenceChipCrossbarNetwork(implicit conf: UncoreConfiguration) extends LogicalNetwork[TileLinkIO]()(conf.tl.ln) {
  implicit val (tl, ln, co) = (conf.tl, conf.tl.ln, conf.tl.co)
  val io = new Bundle {
    val clients = Vec.fill(ln.nClients){(new TileLinkIO).flip}
    val masters = Vec.fill(ln.nMasters){new TileLinkIO}
  }

  implicit val pconf = new PhysicalNetworkConfiguration(ln.nEndpoints, ln.idBits) // Same config for all networks

  // Actually instantiate the particular networks required for TileLink
  val acqNet = Module(new BasicCrossbar(new Acquire))
  val relNet = Module(new BasicCrossbar(new Release))
  val prbNet = Module(new BasicCrossbar(new Probe))
  val gntNet = Module(new BasicCrossbar(new Grant))
  val ackNet = Module(new BasicCrossbar(new GrantAck))

  // Aliases for the various network IO bundle types
  type FBCIO[T <: Data] = DecoupledIO[PhysicalNetworkIO[T]]
  type FLNIO[T <: Data] = DecoupledIO[LogicalNetworkIO[T]]
  type FromCrossbar[T <: Data] = FBCIO[T] => FLNIO[T]
  type ToCrossbar[T <: Data] = FLNIO[T] => FBCIO[T]

  // Shims for converting between logical network IOs and physical network IOs
  //TODO: Could be less verbose if you could override subbundles after a <>
  def DefaultFromCrossbarShim[T <: Data](in: FBCIO[T]): FLNIO[T] = {
    val out = Decoupled(new LogicalNetworkIO(in.bits.payload)).asDirectionless
    out.bits.header := in.bits.header
    out.bits.payload := in.bits.payload
    out.valid := in.valid
    in.ready := out.ready
    out
  }
  def CrossbarToMasterShim[T <: Data](in: FBCIO[T]): FLNIO[T] = {
    val out = DefaultFromCrossbarShim(in)
    out.bits.header.src := in.bits.header.src - UInt(ln.nMasters)
    out
  }
  def CrossbarToClientShim[T <: Data](in: FBCIO[T]): FLNIO[T] = {
    val out = DefaultFromCrossbarShim(in)
    out.bits.header.dst := in.bits.header.dst - UInt(ln.nMasters)
    out
  }
  def DefaultToCrossbarShim[T <: Data](in: FLNIO[T]): FBCIO[T] = {
    val out = Decoupled(new PhysicalNetworkIO(in.bits.payload)).asDirectionless
    out.bits.header := in.bits.header
    out.bits.payload := in.bits.payload
    out.valid := in.valid
    in.ready := out.ready
    out
  }
  def MasterToCrossbarShim[T <: Data](in: FLNIO[T]): FBCIO[T] = {
    val out = DefaultToCrossbarShim(in)
    out.bits.header.dst := in.bits.header.dst + UInt(ln.nMasters)
    out
  }
  def ClientToCrossbarShim[T <: Data](in: FLNIO[T]): FBCIO[T] = {
    val out = DefaultToCrossbarShim(in)
    out.bits.header.src := in.bits.header.src + UInt(ln.nMasters)
    out
  }

  // Make an individual connection between virtual and physical ports using
  // a particular shim. Also seal the unused FIFO control signal.
  def doFIFOInputHookup[T <: Data](phys_in: FBCIO[T], phys_out: FBCIO[T], log_io: FLNIO[T], shim: ToCrossbar[T]) = {
    val s = shim(log_io)
    phys_in.valid := s.valid
    phys_in.bits := s.bits
    s.ready := phys_in.ready
    phys_out.ready := Bool(false)
  }

  def doFIFOOutputHookup[T <: Data](phys_in: FBCIO[T], phys_out: FBCIO[T], log_io: FLNIO[T], shim: FromCrossbar[T]) = {
    val s = shim(phys_out)
    log_io.valid := s.valid
    log_io.bits := s.bits
    s.ready := log_io.ready
    phys_in.valid := Bool(false)
  }

  def doFIFOHookup[T <: Data](isEndpointSourceOfMessage: Boolean, physIn: FBCIO[T], physOut: FBCIO[T], logIO: FLNIO[T], inShim: ToCrossbar[T], outShim: FromCrossbar[T]) = {
    if(isEndpointSourceOfMessage) doFIFOInputHookup(physIn, physOut, logIO, inShim)
    else doFIFOOutputHookup(physIn, physOut, logIO, outShim)
  }
    
  //Hookup all instances of a particular subbundle of TileLink
  def doFIFOHookups[T <: Data: TypeTag](physIO: BasicCrossbarIO[T], getLogIO: TileLinkIO => FLNIO[T]) = {
    typeTag[T].tpe match{ 
      case t if t <:< typeTag[ClientSourcedMessage].tpe => {
        io.masters.zipWithIndex.map{ case (i, id) => doFIFOHookup[T](false, physIO.in(id), physIO.out(id), getLogIO(i), ClientToCrossbarShim, CrossbarToMasterShim) }
        io.clients.zipWithIndex.map{ case (i, id) => doFIFOHookup[T](true, physIO.in(id+ln.nMasters), physIO.out(id+ln.nMasters), getLogIO(i), ClientToCrossbarShim, CrossbarToMasterShim) }
      }
      case t if t <:< typeTag[MasterSourcedMessage].tpe => {
        io.masters.zipWithIndex.map{ case (i, id) => doFIFOHookup[T](true, physIO.in(id), physIO.out(id), getLogIO(i), MasterToCrossbarShim, CrossbarToClientShim) }
        io.clients.zipWithIndex.map{ case (i, id) => doFIFOHookup[T](false, physIO.in(id+ln.nMasters), physIO.out(id+ln.nMasters), getLogIO(i), MasterToCrossbarShim, CrossbarToClientShim) }
      }
      case _ => require(false, "Unknown message sourcing.")
    }
  }

  doFIFOHookups(acqNet.io, (tl: TileLinkIO) => tl.acquire)
  doFIFOHookups(relNet.io, (tl: TileLinkIO) => tl.release)
  doFIFOHookups(prbNet.io, (tl: TileLinkIO) => tl.probe)
  doFIFOHookups(gntNet.io, (tl: TileLinkIO) => tl.grant)
  doFIFOHookups(ackNet.io, (tl: TileLinkIO) => tl.grant_ack)
}
