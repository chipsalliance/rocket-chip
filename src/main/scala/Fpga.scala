package rocketchip

import Chisel._
import uncore._
import junctions._
import cde.Parameters

class ZynqAdapter(implicit val p: Parameters) extends Module
    with HasTopLevelParameters {

  val adapterParams = p.alterPartial({
    case NastiKey => NastiParameters(
      dataBits = 32,
      addrBits = 32,
      idBits = 12)
    case TLId => "L1toL2"
  })

  val io = new Bundle {
    val nasti = new NastiIO()(adapterParams).flip
    val host = new HostIO(htifW).flip
    val reset = Bool(OUTPUT)
  }

  val conv = Module(new NastiIOHostIOConverter(htifW)(adapterParams))
  io <> conv.io
}
