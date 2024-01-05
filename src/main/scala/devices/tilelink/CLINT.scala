// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import chisel3._
import chisel3.util.ShiftRegister
import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

/** If isACLINT is false, the code will generate a clint
 *  If isACLINT is true, here is the template:
 *  CLINTParams(
 *    isACLINT: Boolean = true,
 *    mtimer: Option[MTIMERParams]  = Some(MTIMERParams(mtimecmpBaseAddress = yyy, mtimeBaseAddress = zzz)),
 *    mswi: Option[MSWIParams]      = Some(MSWIParams(baseAddress = xxx))
 *  )
 */
case class CLINTParams(
  isACLINT: Boolean = false,
  mtimer: Option[MTIMERParams]  = None,
  mswi: Option[MSWIParams]      = Some(MSWIParams())
){
  require(mtimer.isDefined || mswi.isDefined, "If both mtimer and mswi are empty, please directly set CLINTKey to empty")
  require(!(!isACLINT && mtimer.isDefined), "The mtimer should not be specified when isACLINT = false")
}

case object CLINTKey extends Field[Option[CLINTParams]](None)

// If isACLINT is false, a clint will be generated according to base address of mswi device
// The Chisel MSWI device will implement the entirety of the CLINT in this case
trait CanHavePeripheryCLINT { this: BaseSubsystem =>
  val mswiOpt = p(CLINTKey) match { 
    case Some(clintParams) => 
      {
        val mswiOpt = clintParams.mswi.map { params =>
          val tlbus = locateTLBusWrapper(p(MSWIAttachKey).slaveWhere)
          val beatBytes = tlbus.beatBytes
          val mswi = LazyModule(new MSWI(params, MTIMERParams(mtimeBaseAddress = params.baseAddress + SWIConsts.size), clintParams.isACLINT, beatBytes))
          mswi.node := tlbus.coupleTo("mswi") { TLFragmenter(tlbus) := _ }

          InModuleBody {
            mswi.module.clock := tlbus.module.clock
            mswi.module.reset := tlbus.module.reset
          }

          mswi
        }

        mswiOpt
      }
    case _ => None
  }

  val mtimerOpt = p(CLINTKey) match { 
    case Some(clintParams) if clintParams.isACLINT =>
      {
        val mtimerOpt = clintParams.mtimer.map { params =>
          val tlbus = locateTLBusWrapper(p(MTIMERAttachKey).slaveWhere)
          val beatBytes = tlbus.beatBytes
          val mtimer = LazyModule(new MTIMER(params, beatBytes))
          mtimer.mtimecmpNode := tlbus.coupleTo("mtimecmp") { TLFragmenter(tlbus) := _ }
          mtimer.mtimeNode := tlbus.coupleTo("mtime") { TLFragmenter(tlbus) := _ }

          InModuleBody {
            mtimer.module.clock := tlbus.module.clock
            mtimer.module.reset := tlbus.module.reset
          }

          mtimer
        }

        mtimerOpt
      }
    case _ => None 
  }
}