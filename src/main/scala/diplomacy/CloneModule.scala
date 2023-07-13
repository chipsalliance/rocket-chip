// See LICENSE.SiFive for license details.

// !!! HACK TO WORK-AROUND MISSING CHISEL FEATURE
// !!! We need to be inside the chisel3 package to access Builder

package chisel3.shim

import chisel3._

import scala.collection.immutable.SeqMap

class ClonePorts protected[shim](elts: Data*) extends Record
{
  val elements = SeqMap(elts.map(d => d.instanceName -> chiselTypeOf(d)): _*)
  def apply(field: String) = elements(field)
}
