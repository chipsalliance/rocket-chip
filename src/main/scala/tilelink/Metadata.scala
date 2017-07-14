// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.tilelink

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.util._

object ClientStates {
  val width = 2

  def Nothing = UInt(0, width)
  def Branch  = UInt(1, width)
  def Trunk   = UInt(2, width)
  def Dirty   = UInt(3, width)

  def hasReadPermission(state: UInt): Bool = state > Nothing
  def hasWritePermission(state: UInt): Bool = state > Branch
}

object MemoryOpCategories extends MemoryOpConstants {
  def wr = Cat(Bool(true), Bool(true))   // Op actually writes
  def wi = Cat(Bool(false), Bool(true))  // Future op will write
  def rd = Cat(Bool(false), Bool(false)) // Op only reads

  def categorize(cmd: UInt): UInt = {
    val cat = Cat(isWrite(cmd), isWriteIntent(cmd))
    //assert(cat.isOneOf(wr,wi,rd), "Could not categorize command.")
    cat
  }
}

/** Stores the client-side coherence information,
  * such as permissions on the data and whether the data is dirty.
  * Its API can be used to make TileLink messages in response to
  * memory operations, cache control oeprations, or Probe messages.
  */
class ClientMetadata extends Bundle {
  /** Actual state information stored in this bundle */
  val state = UInt(width = ClientStates.width)

  /** Metadata equality */
  def ===(rhs: UInt): Bool = state === rhs
  def ===(rhs: ClientMetadata): Bool = state === rhs.state
  def =/=(rhs: ClientMetadata): Bool = !this.===(rhs)

  /** Is the block's data present in this cache */
  def isValid(dummy: Int = 0): Bool = state > ClientStates.Nothing

  /** Determine whether this cmd misses, and the new state (on hit) or param to be sent (on miss) */
  private def growStarter(cmd: UInt): (Bool, UInt) = {
    import MemoryOpCategories._
    import TLPermissions._
    import ClientStates._
    val c = categorize(cmd)
    MuxTLookup(Cat(c, state), (Bool(false), UInt(0)), Seq(
    //(effect, am now) -> (was a hit,   next)
      Cat(rd, Dirty)   -> (Bool(true),  Dirty),
      Cat(rd, Trunk)   -> (Bool(true),  Trunk),
      Cat(rd, Branch)  -> (Bool(true),  Branch),
      Cat(wi, Dirty)   -> (Bool(true),  Dirty),
      Cat(wi, Trunk)   -> (Bool(true),  Trunk),
      Cat(wr, Dirty)   -> (Bool(true),  Dirty),
      Cat(wr, Trunk)   -> (Bool(true),  Dirty),
    //(effect, am now) -> (was a miss,  param)
      Cat(rd, Nothing) -> (Bool(false), NtoB),
      Cat(wi, Branch)  -> (Bool(false), BtoT),
      Cat(wi, Nothing) -> (Bool(false), NtoT),
      Cat(wr, Branch)  -> (Bool(false), BtoT),
      Cat(wr, Nothing) -> (Bool(false), NtoT)))
  }

  /** Determine what state to go to after miss based on Grant param
    * For now, doesn't depend on state (which may have been Probed).
    */
  private def growFinisher(cmd: UInt, param: UInt): UInt = {
    import MemoryOpCategories._
    import TLPermissions._
    import ClientStates._
    val c = categorize(cmd)
    //assert(c === rd || param === toT, "Client was expecting trunk permissions.")
    MuxLookup(Cat(c, param), Nothing, Seq(
    //(effect param) -> (next)
      Cat(rd, toB)   -> Branch,
      Cat(rd, toT)   -> Trunk,
      Cat(wi, toT)   -> Trunk,
      Cat(wr, toT)   -> Dirty))
  }

  /** Does this cache have permissions on this block sufficient to perform op,
    * and what to do next (Acquire message param or updated metadata). */
  def onAccess(cmd: UInt): (Bool, UInt, ClientMetadata) = {
    val r = growStarter(cmd)
    (r._1, r._2, ClientMetadata(r._2))
  }

  /** Does a secondary miss on the block require another Acquire message */
  def onSecondaryAccess(first_cmd: UInt, second_cmd: UInt): (Bool, Bool, UInt, ClientMetadata, UInt) = {
    import MemoryOpCategories._
    val r1 = growStarter(first_cmd)
    val r2 = growStarter(second_cmd)
    val needs_second_acq = isWriteIntent(second_cmd) && !isWriteIntent(first_cmd)
    val hit_again = r1._1 && r2._1
    val dirties = categorize(second_cmd) === wr
    val biggest_grow_param = Mux(dirties, r2._2, r1._2)
    val dirtiest_state = ClientMetadata(biggest_grow_param)
    val dirtiest_cmd = Mux(dirties, second_cmd, first_cmd)
    (needs_second_acq, hit_again, biggest_grow_param, dirtiest_state, dirtiest_cmd)
  }

  /** Metadata change on a returned Grant */
  def onGrant(cmd: UInt, param: UInt): ClientMetadata = ClientMetadata(growFinisher(cmd, param))

  /** Determine what state to go to based on Probe param */
  private def shrinkHelper(param: UInt): (Bool, UInt, UInt) = {
    import ClientStates._
    import TLPermissions._
    MuxTLookup(Cat(param, state), (Bool(false), UInt(0), UInt(0)), Seq(
    //(wanted, am now)  -> (hasDirtyData resp, next)
      Cat(toT, Dirty)   -> (Bool(true),  TtoT, Trunk),
      Cat(toT, Trunk)   -> (Bool(false), TtoT, Trunk),
      Cat(toT, Branch)  -> (Bool(false), BtoB, Branch),
      Cat(toT, Nothing) -> (Bool(false), NtoN, Nothing),
      Cat(toB, Dirty)   -> (Bool(true),  TtoB, Branch),
      Cat(toB, Trunk)   -> (Bool(false), TtoB, Branch),  // Policy: Don't notify on clean downgrade
      Cat(toB, Branch)  -> (Bool(false), BtoB, Branch),
      Cat(toB, Nothing) -> (Bool(false), BtoN, Nothing),
      Cat(toN, Dirty)   -> (Bool(true),  TtoN, Nothing),
      Cat(toN, Trunk)   -> (Bool(false), TtoN, Nothing), // Policy: Don't notify on clean downgrade
      Cat(toN, Branch)  -> (Bool(false), BtoN, Nothing), // Policy: Don't notify on clean downgrade
      Cat(toN, Nothing) -> (Bool(false), NtoN, Nothing)))
  }

  /** Translate cache control cmds into Probe param */
  private def cmdToPermCap(cmd: UInt): UInt = {
    import MemoryOpCategories._
    import TLPermissions._
    MuxLookup(cmd, toN, Seq(
      M_FLUSH   -> toN,
      M_PRODUCE -> toB,
      M_CLEAN   -> toT))
  }

  def onCacheControl(cmd: UInt): (Bool, UInt, ClientMetadata) = {
    val r = shrinkHelper(cmdToPermCap(cmd))
    (r._1, r._2, ClientMetadata(r._3))
  }

  def onProbe(param: UInt): (Bool, UInt, ClientMetadata) = { 
    val r = shrinkHelper(param)
    (r._1, r._2, ClientMetadata(r._3))
  }
}

/** Factories for ClientMetadata, including on reset */
object ClientMetadata {
  def apply(perm: UInt) = {
    val meta = Wire(new ClientMetadata)
    meta.state := perm
    meta
  }
  def onReset = ClientMetadata(ClientStates.Nothing)
  def maximum = ClientMetadata(ClientStates.Dirty)
}
