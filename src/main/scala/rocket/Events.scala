// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package rocket

import util._
import Chisel._

class EventSet(gate: (UInt, UInt) => Bool, events: Seq[(String, () => Bool)]) {
  def size = events.size
  def hits = events.map(_._2()).asUInt
  def check(mask: UInt) = gate(mask, hits)
}

class EventSets(eventSets: Seq[EventSet]) {
  def maskEventSelector(eventSel: UInt): UInt = {
    // allow full associativity between counters and event sets (for now?)
    val setMask = (BigInt(1) << log2Ceil(eventSets.size)) - 1
    val maskMask = ((BigInt(1) << eventSets.map(_.size).max) - 1) << eventSetIdBits
    eventSel & (setMask | maskMask).U
  }

  private def decode(counter: UInt): (UInt, UInt) = {
    require(eventSets.size <= (1 << eventSetIdBits))
    (counter(log2Ceil(eventSets.size)-1, 0), counter >> eventSetIdBits)
  }

  def evaluate(eventSel: UInt): Bool = {
    val (set, mask) = decode(eventSel)
    val sets = eventSets map (_ check mask)
    sets(set)
  }

  private def eventSetIdBits = 8
}
