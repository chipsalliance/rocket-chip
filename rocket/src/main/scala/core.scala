package rocket

import Chisel._
import Util._
import uncore.HTIFIO

case object FPUParams extends Field[PF]
case object HasFPU extends Field[Boolean]

class RocketIO(implicit conf: RocketConfiguration) extends Bundle
{
  val host =  new HTIFIO(params[Int]("nClients"))
  val imem = new CPUFrontendIO()(conf.icache)
  val dmem = new HellaCacheIO()(conf.dcache)
  val ptw = new DatapathPTWIO()(conf.as).flip
  val rocc = new RoCCInterface().flip
}

class Core(implicit conf: RocketConfiguration) extends Module
{
  //xprlen
  //hasfpu
  //hasrocc
  //fastloadword
  //fastloadbyte
  //as <- unfolded
  
  //fpuparams

  val io    = new RocketIO
  //nClients

  //icache
  //dcache
   
  val ctrl  = Module(new Control)
  val dpath = Module(new Datapath)

  if (!params(HasFPU)) {
    val fpu = Module(new FPU,params(FPUParams))
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
