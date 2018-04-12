// See LICENSE for license details.
package freechips.rocketchip.regmapper

import Chisel._
import chisel3.experimental._
import chisel3.{Input, Output}
import freechips.rocketchip.util.{AsyncResetRegVec, SimpleRegIO}

object DescribedReg {
  import freechips.rocketchip.regmapper.RegFieldAccessType._
  import freechips.rocketchip.regmapper.RegFieldWrType._
  import freechips.rocketchip.regmapper.RegFieldRdAction._

  def apply[T <: Data](
    gen: => T,
    name: String,
    desc: String,
    reset: Option[T],
    access: RegFieldAccessType = RW,
    wrType: Option[RegFieldWrType] = None,
    rdAction: Option[RegFieldRdAction] = None,
    volatile: Boolean = false,
    enumerations: Map[BigInt, (String, String)] = Map()): (T, RegFieldDesc) = {
    val rdesc = RegFieldDesc(name, desc, None, None,
      access, wrType, rdAction, volatile, reset.map{_.litValue}, enumerations)
    val reg = reset.map{i => RegInit(i)}.getOrElse(Reg(gen))
    reg.suggestName(name + "_reg")
    (reg, rdesc)
  }

  def async(
    width: Int,
    name: String,
    desc: String,
    reset: Int,
    access: RegFieldAccessType = RW,
    wrType: Option[RegFieldWrType] = None,
    rdAction: Option[RegFieldRdAction] = None,
    volatile: Boolean = false,
    enumerations: Map[BigInt, (String, String)] = Map()): (SimpleRegIO, RegFieldDesc) = {
    val rdesc = RegFieldDesc(name, desc, None, None,
      access, wrType, rdAction, volatile, Some(reset), enumerations)
    val reg = Module(new AsyncResetRegVec(w = width, init = reset))
    reg.suggestName(name + "_reg")
    (reg.io, rdesc)
  }
}
