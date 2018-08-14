// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import freechips.rocketchip.config._
import Chisel._

// This key allows to pass a bundle monitor object through parameters
// It does not define acutal implementation

case object BundleMonitorKey extends Field[Option[(String, Bundle) => Unit]] (None)
