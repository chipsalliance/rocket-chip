// See LICENSE.SiFive for license details.

// !!! HACK TO WORK-AROUND MISSING CHISEL FEATURE
// !!! We need to be inside the chisel3 package to access Builder

package chisel3.shim

import chisel3._
import chisel3.experimental.BaseModule
import chisel3.{RawModule, Module}
import chisel3.internal.Builder
import chisel3.internal.firrtl.{Command, DefInstance}
import scala.collection.immutable.ListMap
import scala.collection.mutable.ArrayBuffer

class ClonePorts protected[shim](elts: Data*) extends Record
{
  val elements = ListMap(elts.map(d => d.instanceName -> chiselTypeOf(d)): _*)
  def apply(field: String) = elements(field)
}
