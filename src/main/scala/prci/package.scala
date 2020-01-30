// See LICENSE.SiFive for license details.

package freechips.rocketchip

import freechips.rocketchip.diplomacy.{AsynchronousCrossing, ClockCrossingType}

package object prci
{
  def asyncMux[T](xType: ClockCrossingType, async: T, notasync: T): T = xType match {
    case _: AsynchronousCrossing => async
    case _ => notasync
  }
}
