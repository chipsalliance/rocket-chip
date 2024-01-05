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

object SWIConsts
{
  def sipOffset(hart: Int) = hart * sipBytes
  def sipBytes = 4
  def size = 0x4000
  def ipiWidth = 32
  def ints = 1
  def clintSize = 0x10000
}

object SWI {
  def apply(swiType: String, intnode: IntNexusNode, intStages: Int) = {
    import SWIConsts._ 

    val nTiles  = intnode.out.size
    val ipi     = Seq.fill(nTiles) { RegInit(0.U(1.W)) }
    
    val (intnode_out, _) = intnode.out.unzip
    intnode_out.zipWithIndex.foreach { case (int, i) =>
      int(0) := ShiftRegister(ipi(i)(0), intStages)
    }

    val swiRegGroup = RegFieldGroup(swiType + "sip", Some(swiType.toUpperCase + "SIP Bits"), ipi.zipWithIndex.flatMap { case (r, i) => 
      RegField(1, r, RegFieldDesc(swiType + s"sip_$i", swiType.toUpperCase + s"SIP bit for Hart $i", reset=Some(0))) :: RegField(SWIConsts.ipiWidth - 1) :: Nil })
  
    swiRegGroup
  }
}