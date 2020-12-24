// See LICENSE.Berkeley for license details.

package freechips.rocketchip

package object rocket extends rocket.constants.ScalarOpConstants with rocket.constants.MemoryOpConstants {
  val DecodeLogic = _root_.org.chipsalliance.utils.decoder.qmc.DecodeLogic
  val Simplify = _root_.org.chipsalliance.utils.decoder.qmc.Simplify
  val SimplifyDC = _root_.org.chipsalliance.utils.decoder.qmc.SimplifyDC
  type Term = _root_.org.chipsalliance.utils.decoder.qmc.Term
}
