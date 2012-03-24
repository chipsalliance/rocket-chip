package rocket

import Chisel._
import Node._;
import Constants._;

class ioTop(htif_width: Int) extends Bundle  {
  val debug   = new ioDebug();
  val host    = new ioHost(htif_width);
  val host_clk = Bool(OUTPUT)
  val mem     = new ioMem
}

class Top() extends Component {

  val htif_width = 16
  val io = new ioTop(htif_width);

  val tile = new Tile
  val htif = new rocketHTIF(htif_width, 1)
  
  val hub = new CoherenceHubBroadcast(2)
  hub.io.tiles(0) <> tile.io.tilelink
  hub.io.tiles(1) <> htif.io.mem

  io.mem.req_cmd <> Queue(hub.io.mem.req_cmd)
  io.mem.req_data <> Queue(hub.io.mem.req_data)
  hub.io.mem.resp <> Pipe(io.mem.resp)

  // pad out the HTIF using a divided clock
  val slow_io = (new slowIO(64, 16)) { Bits(width = htif_width) }
  htif.io.host.out <> slow_io.io.out_fast
  io.host.out <> slow_io.io.out_slow
  htif.io.host.in <> slow_io.io.in_fast
  io.host.in <> slow_io.io.in_slow
  io.host_clk := slow_io.io.clk_slow

  tile.io.host <> htif.io.cpu(0)
  io.debug <> tile.io.host.debug
}

object top_main {
  def main(args: Array[String]) = { 
     chiselMain(args, () => new Top());
  }
}
