package rocket

import Chisel._
import Node._
import Constants._
import uncore._
import collection.mutable.ArrayBuffer

object DummyTopLevelConstants extends uncore.constants.CoherenceConfigConstants {
  val NTILES = 1
  val ENABLE_SHARING = true
  val ENABLE_CLEAN_EXCLUSIVE = true
}
import DummyTopLevelConstants._

case class RocketConfiguration(ntiles: Int, co: CoherencePolicyWithUncached)

class Top extends Component 
{
  val co =  if(ENABLE_SHARING) {
              if(ENABLE_CLEAN_EXCLUSIVE) new MESICoherence
              else new MSICoherence
            } else {
              if(ENABLE_CLEAN_EXCLUSIVE) new MEICoherence
              else new MICoherence
            }
  implicit val rconf = RocketConfiguration(NTILES, co)
  implicit val uconf = UncoreConfiguration(NTILES+1, log2Up(NTILES)+1, co)

  val io = new Bundle  {
    val debug   = new ioDebug
    val host    = new ioHost(16)
    val mem     = new ioMemPipe
  }

  val htif = new rocketHTIF(io.host.w)
  val hub = new CoherenceHubBroadcast
  hub.io.tiles(NTILES) <> htif.io.mem
  io.host <> htif.io.host

  io.mem.req_cmd <> Queue(hub.io.mem.req_cmd)
  io.mem.req_data <> Queue(hub.io.mem.req_data, REFILL_CYCLES)
  hub.io.mem.resp <> Pipe(io.mem.resp)
  Assert(hub.io.mem.resp.ready, "hub.io.mem.resp.ready")

  var error_mode = Bool(false)
  for (i <- 0 until NTILES) {
    val hl = htif.io.cpu(i)
    val tl = hub.io.tiles(i)
    val tile = new Tile(resetSignal = hl.reset)

    tile.io.host.reset := Reg(Reg(hl.reset))
    tile.io.host.pcr_req <> Queue(hl.pcr_req)
    hl.pcr_rep <> Queue(tile.io.host.pcr_rep)
    hl.ipi_req <> Queue(tile.io.host.ipi_req)
    tile.io.host.ipi_rep <> Queue(hl.ipi_rep)
    error_mode = error_mode || Reg(tile.io.host.debug.error_mode)

    tl.xact_init <> Queue(tile.io.tilelink.xact_init)
    tl.xact_init_data <> Queue(tile.io.tilelink.xact_init_data)
    tile.io.tilelink.xact_abort <> Queue(tl.xact_abort)
    tile.io.tilelink.xact_rep <> Queue(tl.xact_rep, 1, pipe = true)
    tl.xact_finish <> Queue(tile.io.tilelink.xact_finish)
    tile.io.tilelink.probe_req <> Queue(tl.probe_req)
    tl.probe_rep <> Queue(tile.io.tilelink.probe_rep, 1)
    tl.probe_rep_data <> Queue(tile.io.tilelink.probe_rep_data)
    tl.incoherent := hl.reset
  }
  io.debug.error_mode := error_mode
}

object top_main {
  def main(args: Array[String]): Unit = { 
    val top = args(0)
    val chiselArgs = ArrayBuffer[String]()

    var i = 1
    while (i < args.length) {
      val arg = args(i)
      arg match {
        case "--NUM_PVFB" => {
          hwacha.Constants.NUM_PVFB = args(i+1).toInt
          i += 1
        }
        case "--WIDTH_PVFB" => {
          hwacha.Constants.WIDTH_PVFB = args(i+1).toInt
          hwacha.Constants.DEPTH_PVFB = args(i+1).toInt
          i += 1
        }
        case "--CG" => {
          hwacha.Constants.coarseGrained = true
        }
        case any => chiselArgs += arg
      }
      i += 1
    }

    chiselMain(chiselArgs.toArray, () => Class.forName(top).newInstance.asInstanceOf[Component])
  }
}
