// See LICENSE for license details.

package uncore
import Chisel._

case object NASTIDataBits extends Field[Int]
case object NASTIAddrBits extends Field[Int]
case object NASTIReadIdBits extends Field[Int]
case object NASTIWriteIdBits extends Field[Int]

trait NASTIParameters extends UsesParameters {
  val dataBits = params(NASTIDataBits)
  val strobeBits = dataBits / 8
  val addrBits = params(NASTIAddrBits)
  val widBits = params(NASTIWriteIdBits)
  val ridBits = params(NASTIReadIdBits)
  val userBits = 32
  val awUserBits = userBits
  val wUserBits = userBits
  val bUserBits = userBits
  val arUserBits = userBits
  val rUserBits = userBits
}

abstract class NASTIBundle extends Bundle with NASTIParameters
abstract class NASTIModule extends Module with NASTIParameters

trait NASTIChannel extends NASTIBundle
trait NASTIMasterToSlaveChannel extends NASTIChannel
trait NASTISlaveToMasterChannel extends NASTIChannel

class NASTIMasterIO extends Bundle {
  val writeAddress  = Decoupled(new NASTIaw)
  val writeData     = Decoupled(new NASTIw)
  val writeResponse = Decoupled(new NASTIb).flip
  val readAddress   = Decoupled(new NASTIar)
  val readData      = Decoupled(new NASTIr).flip
}

class NASTISlaveIO extends NASTIMasterIO { flip() }

class NASTIaw extends NASTIMasterToSlaveChannel {
  val awID      = UInt(width = widBits)
  val awAddr    = UInt(width = addrBits)
  val awLen     = UInt(width = 8)
  val awSize    = UInt(width = 3)
  val awBurst   = UInt(width = 2)
  val awCache   = UInt(width = 4)
  val awProt    = UInt(width = 3)
  val awQOS     = UInt(width = 4)
  val awRegion  = UInt(width = 4)
  val awLock    = Bool()
  val awUser    = UInt(width = awUserBits)
}

class NASTIw extends NASTIMasterToSlaveChannel {
  val wData   = UInt(width = dataBits)
  val wStrb   = UInt(width = strobeBits)
  val wUser   = UInt(width = wUserBits)
  val wLast   = Bool()
}

class NASTIb extends NASTISlaveToMasterChannel {
  val bID     = UInt(width = widBits)
  val bResp   = UInt(width = 2)
  val bUser   = UInt(width = bUserBits)
}

class NASTIar extends NASTIMasterToSlaveChannel {
  val arID      = UInt(width = ridBits)
  val arAddr    = UInt(width = addrBits)
  val arLen     = UInt(width = 8)
  val arSize    = UInt(width = 3)
  val arBurst   = UInt(width = 2)
  val arCache   = UInt(width = 4)
  val arQOS     = UInt(width = 3)
  val arRegion  = UInt(width = 4)
  val arProt    = UInt(width = 4)
  val arLock    = Bool()
  val arUser    = UInt(width = arUserBits)
}

class NASTIr extends NASTISlaveToMasterChannel {
  val rID     = UInt(width = ridBits)
  val rData   = UInt(width = dataBits)
  val rResp   = UInt(width = 2)
  val rUser   = UInt(width = rUserBits)
  val rLast   = Bool()
}
