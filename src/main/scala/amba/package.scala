// See LICENSE.SiFive for license details.

package freechips.rocketchip

import chisel3._
import freechips.rocketchip.util._

package object amba {
  class AMBAProtBundle extends Bundle {
    val bufferable = Bool() // writeback caching ok?
    val modifiable = Bool() // legal to read/write-combine/expand this request?
    val readalloc  = Bool()
    val writealloc = Bool()
    val privileged = Bool() // machine_mode=true,   user_mode=false
    val secure     = Bool() // secure_master=true,  normal=false
    val fetch      = Bool() // instruct_fetch=true, load/store=false
  }

  case object AMBAProt extends ControlKey[AMBAProtBundle]("amba_prot")

  case class AMBAProtField() extends BundleField[AMBAProtBundle](AMBAProt, Output(new AMBAProtBundle), x => {
    x.bufferable := false.B
    x.modifiable := false.B
    x.readalloc  := false.B
    x.writealloc := false.B
    x.privileged := true.B
    x.secure     := true.B
    x.fetch      := false.B
  })

  // Used to convert a TileLink corrupt signal into an AMBA user bit
  case object AMBACorrupt extends DataKey[Bool]("corrupt")
  case class AMBACorruptField() extends BundleField[Bool](AMBACorrupt, Output(Bool()), x => x := false.B)
}