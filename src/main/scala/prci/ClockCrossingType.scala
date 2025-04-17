// See LICENSE.SiFive for license details.

package freechips.rocketchip.prci
import org.chipsalliance.diplomacy.lazymodule.{LazyScope, LazyModule}
import freechips.rocketchip.diplomacy.{BufferParams}
import freechips.rocketchip.util.{RationalDirection, FastToSlow, AsyncQueueParams, CreditedDelay}

trait CrossingType

trait HasDomainCrossing extends LazyScope { this: LazyModule =>
  type DomainCrossingType <: CrossingType
}

trait HasClockDomainCrossing extends HasDomainCrossing { this: LazyModule =>
  type DomainCrossingType = ClockCrossingType
}

/** Enumerates the types of clock crossings generally supported by Diplomatic bus protocols  */
trait ClockCrossingType extends CrossingType
{
  def sameClock: Boolean
}

case object NoCrossing // converts to SynchronousCrossing(BufferParams.none) via implicit def in package
case class SynchronousCrossing(params: BufferParams = BufferParams.default) extends ClockCrossingType
{
  def sameClock = true
}
case class RationalCrossing(direction: RationalDirection = FastToSlow) extends ClockCrossingType
{
  def sameClock = false
}
case class AsynchronousCrossing(depth: Int = 8, sourceSync: Int = 3, sinkSync: Int = 3, safe: Boolean = true, narrow: Boolean = false) extends ClockCrossingType
{
  def sameClock = false
  def asSinkParams = AsyncQueueParams(depth, sinkSync, safe, narrow)
}
case class CreditedCrossing(sourceDelay: CreditedDelay, sinkDelay: CreditedDelay) extends ClockCrossingType
{
  def sameClock = true
}

object CreditedCrossing {
  def apply(delay: CreditedDelay): CreditedCrossing = CreditedCrossing(delay, delay.flip)
  def apply(): CreditedCrossing = CreditedCrossing(CreditedDelay(1, 1))
}
