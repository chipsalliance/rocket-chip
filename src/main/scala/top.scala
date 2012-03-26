package rocket

import Chisel._
import Node._;
import Constants._;

class ioTop(htif_width: Int, mem_backup_width: Int) extends Bundle  {
  val debug   = new ioDebug();
  val host    = new ioHost(htif_width);
  val host_clk = Bool(OUTPUT)
  val mem_backup = new ioMemSerialized(mem_backup_width)
  val mem_backup_en = Bool(INPUT)
  val mem     = new ioMem
}

class Top() extends Component
{
  val clkdiv = 32
  val htif_width = 16
  val mem_backup_width = 16
  val io = new ioTop(htif_width, mem_backup_width);

  val tile = new Tile
  val htif = new rocketHTIF(htif_width, 1)
  
  val hub = new CoherenceHubBroadcast(2)
  hub.io.tiles(0) <> tile.io.tilelink
  hub.io.tiles(1) <> htif.io.mem

  // mux between main and backup memory ports
  val mem_serdes = new MemSerdes(mem_backup_width)
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
  val mio = (new slowIO(clkdiv, 4)) { Bits(width = mem_backup_width) }
  mem_serdes.io.narrow.req <> mio.io.out_fast
  io.mem_backup.req <> mio.io.out_slow
  mem_serdes.io.narrow.resp.valid := mio.io.in_fast.valid
  mio.io.in_fast.ready := Bool(true)
  mem_serdes.io.narrow.resp.bits := mio.io.in_fast.bits
  io.mem_backup.resp <> mio.io.in_slow

  tile.io.host <> htif.io.cpu(0)
  io.debug <> tile.io.host.debug
}

object top_main {
  def main(args: Array[String]) = { 
     chiselMain(args, () => new Top());
  }
}
