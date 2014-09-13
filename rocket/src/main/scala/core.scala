// See LICENSE for license details.

package rocket

import Chisel._
import Util._
import uncore._

case object BuildFPU extends Field[Option[() => FPU]]
case object XprLen extends Field[Int]
case object NMultXpr extends Field[Int]
case object RetireWidth extends Field[Int]
case object UseVM extends Field[Boolean]
case object FastLoadWord extends Field[Boolean]
case object FastLoadByte extends Field[Boolean]
case object FastMulDiv extends Field[Boolean]
case object CoreInstBits extends Field[Int]
case object CoreDataBits extends Field[Int]
case object CoreDCacheReqTagBits extends Field[Int]

abstract trait CoreParameters extends UsesParameters {
  val xprLen = params(XprLen)
  val coreInstBits = params(CoreInstBits)
  val coreInstBytes = coreInstBits/8
  val coreDataBits = xprLen
  val coreDataBytes = coreDataBits/8
  val coreDCacheReqTagBits = params(CoreDCacheReqTagBits)
  val coreMaxAddrBits = math.max(params(PPNBits),params(VPNBits)+1) + params(PgIdxBits)

  if(params(FastLoadByte)) require(params(FastLoadWord))
  require(params(RetireWidth) == 1) // for now...
}
abstract class CoreBundle extends Bundle with CoreParameters
abstract class CoreModule extends Module with CoreParameters

class RocketIO extends Bundle
{
  val host =  new HTIFIO
  val imem = new CPUFrontendIO
  val dmem = new HellaCacheIO
  val ptw = new DatapathPTWIO().flip
  val rocc = new RoCCInterface().flip
}

class Core extends Module with CoreParameters
{
  val io = new RocketIO
   
  val ctrl = Module(new Control)
  val dpath = Module(new Datapath)

  //If so specified, build an FPU module and wire it in
  params(BuildFPU) 
    .map { bf => bf() } 
    .foreach { fpu => 
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
