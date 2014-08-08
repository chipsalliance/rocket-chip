package rocket

import Chisel._
import Util._
import uncore.HTIFIO

case object FPUParams extends Field[PF]
case object BuildFPU extends Field[Option[() => FPU]]

class RocketIO extends Bundle
{
  val host =  new HTIFIO
  val imem = new CPUFrontendIO
  val dmem = new HellaCacheIO
  val ptw = new DatapathPTWIO().flip
  val rocc = new RoCCInterface().flip
}

class Core extends Module
{
  val io = new RocketIO
   
  val ctrl = Module(new Control)
  val dpath = Module(new Datapath)

  if (!params(BuildFPU).isEmpty) {
    val fpu = Module(params(BuildFPU).get(),params(FPUParams))
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
