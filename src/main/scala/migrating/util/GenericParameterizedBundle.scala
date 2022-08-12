// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._

@deprecated("GenericParameterizedBundle is useless anymore after autoclonetype2 is on.", "Rocket Chip 2021.04")
abstract class GenericParameterizedBundle[+T <: Object](val params: T) extends Bundle