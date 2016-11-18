package uncore.util

import Chisel._
import uncore.tilelink._
import util.TwoWayCounter
import config._

class BeatCounterStatus extends Bundle {
  val idx = UInt()
  val done = Bool()
}

class TwoWayBeatCounterStatus extends Bundle {
  val pending = Bool()
  val up = new BeatCounterStatus()
  val down = new BeatCounterStatus()
}

/** Utility trait containing wiring functions to keep track of how many data beats have 
  * been sent or recieved over a particular [[uncore.TileLinkChannel]] or pair of channels. 
  *
  * Won't count message types that don't have data. 
  * Used in [[uncore.XactTracker]] and [[uncore.FinishUnit]].
  */
trait HasDataBeatCounters {
  type HasBeat = TileLinkChannel with HasTileLinkBeatId
  type HasId = TileLinkChannel with HasClientId

  /** Returns the current count on this channel and when a message is done
    * @param inc increment the counter (usually .valid or .fire())
    * @param data the actual channel data
    * @param beat count to return for single-beat messages
    */
  def connectDataBeatCounter[S <: TileLinkChannel](inc: Bool, data: S, beat: UInt) = {
    val multi = data.hasMultibeatData()
    val (multi_cnt, multi_done) = Counter(inc && multi, data.tlDataBeats)
    val cnt = Mux(multi, multi_cnt, beat)
    val done = Mux(multi, multi_done, inc)
    (cnt, done)
  }

  /** Counter for beats on outgoing [[chisel.DecoupledIO]] */
  def connectOutgoingDataBeatCounter[T <: TileLinkChannel](
      out: DecoupledIO[T],
      beat: UInt = UInt(0)): (UInt, Bool) =
    connectDataBeatCounter(out.fire(), out.bits, beat)

  /** Returns done but not cnt. Use the addr_beat subbundle instead of cnt for beats on 
    * incoming channels in case of network reordering.
    */
  def connectIncomingDataBeatCounter[T <: TileLinkChannel](in: DecoupledIO[T]): Bool =
    connectDataBeatCounter(in.fire(), in.bits, UInt(0))._2

  /** Counter for beats on incoming DecoupledIO[LogicalNetworkIO[]]s returns done */
  def connectIncomingDataBeatCounterWithHeader[T <: TileLinkChannel](in: DecoupledIO[LogicalNetworkIO[T]]): Bool =
    connectDataBeatCounter(in.fire(), in.bits.payload, UInt(0))._2

  /** If the network might interleave beats from different messages, we need a Vec of counters,
    * one for every outstanding message id that might be interleaved.
    *
    * @param getId mapping from Message to counter id
    */
  def connectIncomingDataBeatCountersWithHeader[T <: TileLinkChannel with HasClientTransactionId](
      in: DecoupledIO[LogicalNetworkIO[T]],
      entries: Int,
      getId: LogicalNetworkIO[T] => UInt): Vec[Bool] = {
    Vec((0 until entries).map { i =>
      connectDataBeatCounter(in.fire() && getId(in.bits) === UInt(i), in.bits.payload, UInt(0))._2 
    })
  }

  /** Provides counters on two channels, as well a meta-counter that tracks how many
    * messages have been sent over the up channel but not yet responded to over the down channel
    *
    * @param status bundle of status of the counters
    * @param up outgoing channel
    * @param down incoming channel
    * @param max max number of outstanding ups with no down
    * @param beat overrides cnts on single-beat messages
    * @param track whether up's message should be tracked
    * @return a tuple containing whether their are outstanding messages, up's count,
    *         up's done, down's count, down's done
    */
  def connectTwoWayBeatCounters[T <: TileLinkChannel, S <: TileLinkChannel](
      status: TwoWayBeatCounterStatus,
      up: DecoupledIO[T],
      down: DecoupledIO[S],
      max: Int = 1,
      beat: UInt = UInt(0),
      trackUp: T => Bool = (t: T) => Bool(true),
      trackDown: S => Bool = (s: S) => Bool(true)) {
    val (up_idx, up_done) = connectDataBeatCounter(up.fire() && trackUp(up.bits), up.bits, beat)
    val (dn_idx, dn_done) = connectDataBeatCounter(down.fire() && trackDown(down.bits), down.bits, beat)
    val cnt = TwoWayCounter(up_done, dn_done, max)
    status.pending := cnt > UInt(0)
    status.up.idx := up_idx
    status.up.done := up_done
    status.down.idx := dn_idx
    status.down.done := dn_done
  }
}


