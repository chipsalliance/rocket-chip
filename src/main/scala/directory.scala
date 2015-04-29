// See LICENSE for license details.

package uncore
import Chisel._

// This class encapsulates transformations on different directory information
// storage formats
abstract class DirectoryRepresentation(val width: Int) {
  def pop(prev: UInt, id: UInt): UInt
  def push(prev: UInt, id: UInt): UInt
  def flush: UInt
  def none(s: UInt): Bool
  def one(s: UInt): Bool
  def count(s: UInt): UInt
  def next(s: UInt): UInt
  def full(s: UInt): UInt
}

abstract trait HasDirectoryRepresentation {
  val dir: DirectoryRepresentation
}

class NullRepresentation(nClients: Int) extends DirectoryRepresentation(1) {
  def pop(prev: UInt, id: UInt) = UInt(0)
  def push(prev: UInt, id: UInt) = UInt(0)
  def flush  = UInt(0)
  def none(s: UInt) = Bool(false)
  def one(s: UInt) = Bool(false)
  def count(s: UInt) = UInt(nClients)
  def next(s: UInt) = UInt(0)
  def full(s: UInt) = SInt(-1, width = nClients).toUInt
}

class FullRepresentation(nClients: Int) extends DirectoryRepresentation(nClients) {
  def pop(prev: UInt, id: UInt) =  prev &  ~UIntToOH(id)
  def push(prev: UInt, id: UInt) = prev | UIntToOH(id)
  def flush = UInt(0, width = width)
  def none(s: UInt) = s === UInt(0)
  def one(s: UInt) = PopCount(s) === UInt(1)
  def count(s: UInt) = PopCount(s)
  def next(s: UInt) = PriorityEncoder(s)
  def full(s: UInt) = s
}
