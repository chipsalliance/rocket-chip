package rocket

import Chisel._
import Node._;
import Constants._;

class ioTop(htif_width: Int) extends Bundle  {
  val debug   = new ioDebug();
  val host    = new ioHost(htif_width);
  val host_clk = Bool(OUTPUT)
  val mem_backup = new ioMemSerialized
  val mem_backup_en = Bool(INPUT)
  val mem_backup_clk = Bool(OUTPUT)
  val mem     = new ioMem
}

class Top extends Component
{
  val clkdiv = 32
  val htif_width = 8
  val io = new ioTop(htif_width)

  val co =  if(ENABLE_SHARING) {
              if(ENABLE_CLEAN_EXCLUSIVE) new MESICoherence
              else new MSICoherence
            } else {
              if(ENABLE_CLEAN_EXCLUSIVE) new MEICoherence
              else new MICoherence
            }
  val htif = new rocketHTIF(htif_width, NTILES, co)
  val hub = new CoherenceHubBroadcast(NTILES+1, co)
  hub.io.tiles(NTILES) <> htif.io.mem

  // mux between main and backup memory ports
  val mem_serdes = new MemSerdes
  val mem_cmdq = (new queue(1)) { new MemReqCmd }
  mem_cmdq.io.enq <> hub.io.mem.req_cmd
  mem_cmdq.io.deq.ready := Mux(io.mem_backup_en, mem_serdes.io.wide.req_cmd.ready, io.mem.req_cmd.ready)
  io.mem.req_cmd.valid := mem_cmdq.io.deq.valid && !io.mem_backup_en
  io.mem.req_cmd.bits := mem_cmdq.io.deq.bits
  mem_serdes.io.wide.req_cmd.valid := mem_cmdq.io.deq.valid && io.mem_backup_en
  mem_serdes.io.wide.req_cmd.bits := mem_cmdq.io.deq.bits

  val mem_dataq = (new queue(2)) { new MemData }
  mem_dataq.io.enq <> hub.io.mem.req_data
  mem_dataq.io.deq.ready := Mux(io.mem_backup_en, mem_serdes.io.wide.req_data.ready, io.mem.req_data.ready)
  io.mem.req_data.valid := mem_dataq.io.deq.valid && !io.mem_backup_en
  io.mem.req_data.bits := mem_dataq.io.deq.bits
  mem_serdes.io.wide.req_data.valid := mem_dataq.io.deq.valid && io.mem_backup_en
  mem_serdes.io.wide.req_data.bits := mem_dataq.io.deq.bits

  hub.io.mem.resp.valid := Mux(io.mem_backup_en, mem_serdes.io.wide.resp.valid, io.mem.resp.valid)
  hub.io.mem.resp.bits := Mux(io.mem_backup_en, mem_serdes.io.wide.resp.bits, io.mem.resp.bits)

  // pad out the HTIF using a divided clock
  val hio = (new slowIO(clkdiv, 4)) { Bits(width = htif_width) }
  htif.io.host.out <> hio.io.out_fast
  io.host.out <> hio.io.out_slow
  htif.io.host.in <> hio.io.in_fast
  io.host.in <> hio.io.in_slow
  io.host_clk := hio.io.clk_slow

  // pad out the backup memory link with the HTIF divided clk
  val mio = (new slowIO(clkdiv, 4)) { Bits(width = MEM_BACKUP_WIDTH) }
  mem_serdes.io.narrow.req <> mio.io.out_fast
  io.mem_backup.req <> mio.io.out_slow
  mem_serdes.io.narrow.resp.valid := mio.io.in_fast.valid
  mio.io.in_fast.ready := Bool(true)
  mem_serdes.io.narrow.resp.bits := mio.io.in_fast.bits
  io.mem_backup.resp <> mio.io.in_slow
  io.mem_backup_clk := mio.io.clk_slow

  var error_mode = Bool(false)
  for (i <- 0 until NTILES) {
    val tile = new Tile(co)
    tile.io.host <> htif.io.cpu(i)
    hub.io.tiles(i) <> tile.io.tilelink
    error_mode = error_mode || tile.io.host.debug.error_mode
  }
  io.debug.error_mode := error_mode
}

object top_main {
  def main(args: Array[String]) = { 
     chiselMain(args.drop(1), () => Class.forName(args(0)).newInstance.asInstanceOf[Component])
  }
}
