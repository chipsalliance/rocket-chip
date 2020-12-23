// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.util._

/** State of a TileLink L1 Cache.
  * Comparing to system cache, which will never have a branch node above it.
  */
object ClientStates {
  val width = 2

  /** A node that does not currently cache a copy of the data. Has neither read nor write permissions. */
  def Nothing = UInt(0, width)
  /** A node with a cached copy that is above the Tip. Has read-only permissions on its copy. */
  def Branch  = UInt(1, width)
  /** Tip (with no Branches)
    * A node with a cached copy that is serving as the point of memory access serialization.
    * Has Read/Write permissions on its copy.
    *
    * Has no dirty data.
    *
    * @todo refactor this to Tip.
    */
  def Trunk   = UInt(2, width)
  /** Tip (with no Branches)
    * A node with a cached copy that is serving as the point of memory access serialization.
    * Has Read/Write permissions on its copy.
    *
    * Has dirty data.
    */
  def Dirty   = UInt(3, width)

  def hasReadPermission(state: UInt): Bool = state > Nothing
  def hasWritePermission(state: UInt): Bool = state > Branch
}

/** @todo Why not use decoder here? */
object MemoryOpCategories extends MemoryOpConstants {
  /** Operator actually writes. */
  def wr = Cat(Bool(true), Bool(true))   // Op actually writes
  /** Operator intent to write in the future. */
  def wi = Cat(Bool(false), Bool(true))  // Future op will write
  /** Operator read. */
  def rd = Cat(Bool(false), Bool(false)) // Op only reads

  /** covert [[MemoryOpConstants]] to [[MemoryOpCategories]]. */
  def categorize(cmd: UInt): UInt = {
    val cat = Cat(isWrite(cmd), isWriteIntent(cmd))
    //assert(cat.isOneOf(wr,wi,rd), "Could not categorize command.")
    cat
  }
}

/** Stores the client-side coherence information,
  * such as permissions on the data and whether the data is dirty.
  * Its API can be used to make TileLink messages in response to
  * memory operations, cache control operations, or Probe messages.
  */
class ClientMetadata extends Bundle {
  /** Actual state information stored in this bundle */
  val state = UInt(width = ClientStates.width)

  // Metadata equality
  def ===(rhs: UInt): Bool = state === rhs
  def ===(rhs: ClientMetadata): Bool = state === rhs.state
  def =/=(rhs: ClientMetadata): Bool = !this.===(rhs)

  /** Is the block's data present in this cache.
    * @todo why dummy here?
    */
  def isValid(dummy: Int = 0): Bool = state > ClientStates.Nothing

  /** Determine whether this cmd misses, and the new state (on hit) or param to be sent (on miss).
    * @todo I think mux should be refactor to the qmc decoder.
    *
    * @param cmd command encoded in [[MemoryOpConstants]].
    * @return result of decoder:
    *          miss, state
    *         (true, next state encoded in [[ClientStates]])
    *         (false, next TileLink permission encoded in [[TLPermissions]] to use)
    */
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
    *
    * @param cmd cmd command encoded in [[MemoryOpConstants]].
    * @param param TileLink parameter encoded in [[TLPermissions]].
    * @return next state encoded in [[ClientStates]] to transform
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
    * and what to do next (Acquire message param or updated metadata).
    *
    * @param cmd cmd command encoded in [[MemoryOpConstants]].
    * @return result of decoder:
    *          miss, param, state
    *         (true, _, next state encoded in [[ClientStates]])
    *         (false, next TileLink permission encoded in [[TLPermissions]] to use, _)
    */
  def onAccess(cmd: UInt): (Bool, UInt, ClientMetadata) = {
    val r = growStarter(cmd)
    (r._1, r._2, ClientMetadata(r._2))
  }

  /** Does a secondary miss on the block require another Acquire message.
    *
    * @param first_cmd first command encoded in [[MemoryOpConstants]] request sent by CPU.
    * @param second_cmd second command encoded in [[MemoryOpConstants]] request sent by CPU.
    *
    * @return (
    *         second access acquire block under first access needn't acquire block,
    *         both of access hit a block,
    *         the biggest permission to grow between two accesses.
    *         the dirtiest state encoded in [[ClientStates]] to transform between two accesses.
    *         the dirtiest command between first access and second access.
    *         )
    *
    */
  def onSecondaryAccess(first_cmd: UInt, second_cmd: UInt): (Bool, Bool, UInt, ClientMetadata, UInt) = {
    import MemoryOpCategories._
    val r1 = growStarter(first_cmd)
    val r2 = growStarter(second_cmd)
    /** [[second_cmd]] acquire block under [[first_cmd]] needn't acquire block. */
    val needs_second_acq = isWriteIntent(second_cmd) && !isWriteIntent(first_cmd)
    /** both of access hit a block. */
    val hit_again = r1._1 && r2._1
    /** after second access, state will become dirty. */
    val dirties = categorize(second_cmd) === wr
    /** the biggest permission encoded in [[TLPermissions]] to grow between two accesses. */
    val biggest_grow_param = Mux(dirties, r2._2, r1._2)
    /** the dirtiest state encoded in [[ClientStates]] to transform between two accesses. */
    val dirtiest_state = ClientMetadata(biggest_grow_param)
    /** the dirtiest command between first access and second access. */
    val dirtiest_cmd = Mux(dirties, second_cmd, first_cmd)
    (needs_second_acq, hit_again, biggest_grow_param, dirtiest_state, dirtiest_cmd)
  }

  /** Metadata change on a returned Grant */
  def onGrant(cmd: UInt, param: UInt): ClientMetadata = ClientMetadata(growFinisher(cmd, param))

  /** Determine what state to go to based on Probe param.
    *
    * @todo rewrite with decoder
    *
    * @param param TileLink parameter encoded in [[TLPermissions]] received for Probe.
    *
    * @return (
    *         hasDirtyData,
    *         TileLink respond parameter encoded in [[TLPermissions]],
    *         next state encoded in [[ClientStates]]
    *         )
    */
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
      Cat(toB, Nothing) -> (Bool(false), NtoN, Nothing),
      Cat(toN, Dirty)   -> (Bool(true),  TtoN, Nothing),
      Cat(toN, Trunk)   -> (Bool(false), TtoN, Nothing), // Policy: Don't notify on clean downgrade
      Cat(toN, Branch)  -> (Bool(false), BtoN, Nothing), // Policy: Don't notify on clean downgrade
      Cat(toN, Nothing) -> (Bool(false), NtoN, Nothing)))
  }

  /** Translate cache control cmds into Probe param
    *
    * @param cmd command encoded in [[MemoryOpConstants]] request sent by CPU.
    *
    * @return TileLink parameter encoded in [[TLPermissions]] to send.
    */
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
  /** Convert [[UInt]] encoded in [[ClientStates]] to [[ClientMetadata]] type. */
  def apply(perm: UInt) = {
    val meta = Wire(new ClientMetadata)
    meta.state := perm
    meta
  }
  def onReset = ClientMetadata(ClientStates.Nothing)
  def maximum = ClientMetadata(ClientStates.Dirty)
}
