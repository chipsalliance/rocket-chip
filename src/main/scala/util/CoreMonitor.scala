// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._

// this class is used to expose some internal core signals
// to verification monitors
case class CoreSignalMonitor(
  clock: Clock,
  reset: Reset,
  xLen: Int,
  cease: Bool,
  reg_mscratch: UInt,
  hartid: UInt)
  extends HasCoreSignalMonitorParameters

trait HasCoreSignalMonitorParameters {
  def clock: Clock
  def reset: Reset
  def xLen: Int
  def cease: Bool
  def reg_mscratch: UInt
  def hartid: UInt
}

// mark a module that has cores with a CoreSignalMonitor
trait HasCoreSignalMonitors {
  def coreSignalMonitors: List[CoreSignalMonitor]
}
