// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy
import freechips.rocketchip.util.{RationalDirection, FastToSlow, AsyncQueueParams, CreditedDelay}

// TODO this should all be moved to package freechips.rocketchip.prci now that it exists

trait CrossingType

trait HasDomainCrossing extends LazyScope { this: LazyModule =>
  type DomainCrossingType <: CrossingType
}

trait HasClockDomainCrossing extends HasDomainCrossing { this: LazyModule =>
  type DomainCrossingType = ClockCrossingType
}

/** Enumerates the types of clock crossings generally supported by Diplomatic bus protocols  */
sealed trait ClockCrossingType extends CrossingType
{
  def sameClock = this match {
    case _: SynchronousCrossing | _: CreditedCrossing => true
    case _ => false
  }
}

case object NoCrossing // converts to SynchronousCrossing(BufferParams.none) via implicit def in package
case class SynchronousCrossing(params: BufferParams = BufferParams.default) extends ClockCrossingType
case class RationalCrossing(direction: RationalDirection = FastToSlow) extends ClockCrossingType
case class AsynchronousCrossing(depth: Int = 8, sourceSync: Int = 3, sinkSync: Int = 3, safe: Boolean = true, narrow: Boolean = false) extends ClockCrossingType
{
  def asSinkParams = AsyncQueueParams(depth, sinkSync, safe, narrow)
}
case class CreditedCrossing(sourceDelay: CreditedDelay, sinkDelay: CreditedDelay) extends ClockCrossingType

object CreditedCrossing {
  def apply(delay: CreditedDelay): CreditedCrossing = CreditedCrossing(delay, delay.flip)
  def apply(): CreditedCrossing = CreditedCrossing(CreditedDelay(1, 1))
}
