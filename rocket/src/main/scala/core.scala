package rocket

import Chisel._
import Util._
import uncore.HTIFIO

class RocketIO(implicit conf: RocketConfiguration) extends Bundle
{
  val host = new HTIFIO(conf.tl.ln.nClients)
  val imem = new CPUFrontendIO()(conf.icache)
  val dmem = new HellaCacheIO()(conf.dcache)
  val ptw = new DatapathPTWIO()(conf.as).flip
  val rocc = new RoCCInterface().flip
}

class Core(implicit conf: RocketConfiguration) extends Module
{
  val io    = new RocketIO
   
  val ctrl  = Module(new Control)
  val dpath = Module(new Datapath)

  if (!conf.fpu.isEmpty) {
    val fpu = Module(new FPU(conf.fpu.get))
    dpath.io.fpu <> fpu.io.dpath
    ctrl.io.fpu <> fpu.io.ctrl
  }

  ctrl.io.dpath <> dpath.io.ctrl
  dpath.io.host <> io.host

  ctrl.io.imem <> io.imem
  dpath.io.imem <> io.imem

  ctrl.io.dmem <> io.dmem
  dpath.io.dmem <> io.dmem

  dpath.io.ptw <> io.ptw

  ctrl.io.rocc <> io.rocc
  dpath.io.rocc <> io.rocc
}
