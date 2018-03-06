// See LICENSE for license details.
package freechips.rocketchip.regmapper

import Chisel._
import chisel3.experimental._
import chisel3.{Input, Output}
import freechips.rocketchip.util.{AsyncResetRegVec, SimpleRegIO}

object DescribedReg {
  import freechips.rocketchip.regmapper.RegFieldAccessType._

  def apply[T <: Data](
    gen: => T,
    name: String,
    desc: String,
    reset: Option[T],
    access: RegFieldAccessType = RW,
    enumerations: Map[BigInt, (String, String)] = Map()): (T, RegFieldDesc) = {
    val rdesc = RegFieldDesc(name, desc, None, None,
      access, reset.map{_.litValue}, enumerations)
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
    enumerations: Map[BigInt, (String, String)] = Map()): (SimpleRegIO, RegFieldDesc) = {
    val rdesc = RegFieldDesc(name, desc, None, None,
      access, Some(reset), enumerations)
    val reg = Module(new AsyncResetRegVec(w = width, init = reset))
    reg.suggestName(name + "_reg")
    (reg.io, rdesc)
  }
}
