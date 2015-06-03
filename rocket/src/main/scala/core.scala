// See LICENSE for license details.

package rocket

import Chisel._
import Util._
import uncore._

case object BuildFPU extends Field[Option[() => FPU]]
case object FDivSqrt extends Field[Boolean]
case object XLen extends Field[Int]
case object NMultXpr extends Field[Int]
case object FetchWidth extends Field[Int]
case object RetireWidth extends Field[Int]
case object UseVM extends Field[Boolean]
case object FastLoadWord extends Field[Boolean]
case object FastLoadByte extends Field[Boolean]
case object FastMulDiv extends Field[Boolean]
case object CoreInstBits extends Field[Int]
case object CoreDataBits extends Field[Int]
case object CoreDCacheReqTagBits extends Field[Int]
case object NCustomMRWCSRs extends Field[Int]

abstract trait CoreParameters extends UsesParameters {
  val xLen = params(XLen)
  val paddrBits = params(PAddrBits)
  val vaddrBits = params(VAddrBits)
  val pgIdxBits = params(PgIdxBits)
  val ppnBits = params(PPNBits)
  val vpnBits = params(VPNBits)
  val pgLevels = params(PgLevels)
  val pgLevelBits = params(PgLevelBits)
  val asIdBits = params(ASIdBits)

  val retireWidth = params(RetireWidth)
  val coreFetchWidth = params(FetchWidth)
  val coreInstBits = params(CoreInstBits)
  val coreInstBytes = coreInstBits/8
  val coreDataBits = xLen
  val coreDataBytes = coreDataBits/8
  val coreDCacheReqTagBits = params(CoreDCacheReqTagBits)
  val coreMaxAddrBits = math.max(ppnBits,vpnBits+1) + pgIdxBits
  val vaddrBitsExtended = vaddrBits + (vaddrBits < xLen).toInt

  if(params(FastLoadByte)) require(params(FastLoadWord))
}

abstract trait RocketCoreParameters extends CoreParameters
{
  require(params(FetchWidth) == 1)  // for now...
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
      if(!params(BuildRoCC).isEmpty) {
        io.rocc.fpu_req <> fpu.io.cp_req
        io.rocc.fpu_resp <> fpu.io.cp_resp
      } else {
        fpu.io.cp_req.valid := Bool(false)  
      }
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
