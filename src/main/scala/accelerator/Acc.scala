
package acc_pkg

import  chisel3._
import  chisel3.util.{Enum,Queue,log2Ceil}

import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem.{BaseSubsystem, RocketSubsystem}

import freechips.rocketchip.system._

// Core settings
trait Acc_params {

  val source_min  = 0x10
  val outstanding = 0x20 // allow up to 16 outstanding transactions, 64 bytes each
  val source_max  = source_min + outstanding 
  
}

class GenericRocc(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes, nPTWPorts = 1) with Acc_params { 

  override lazy val module = new GenericRoccImp(this)

  override val tlNode =  TLClientNode(Seq(TLClientPortParameters(Seq(TLClientParameters(
    sourceId = IdRange (source_min, source_max), name = s"RoccCore0")))))
}

class GenericRoccImp(outer: GenericRocc)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) with HasCoreParameters with Acc_params {
  val (tl_out, edge) = outer.tlNode.out(0)
}
