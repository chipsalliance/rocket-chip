// See LICENSE for license details.

package uncore.tilelink2

import Chisel._

class TLEdgeOut(
  client:  TLClientPortParameters,
  manager: TLManagerPortParameters)
  extends TLEdgeParameters(client, manager)
{
  // Transfers
  def Acquire(fromSource: UInt, toAddress: UInt, lgSize: UInt, growPermissions: UInt) = {
    require (manager.anySupportAcquire)
    val legal = manager.supportsAcquire(toAddress, lgSize)
    val a = new TLBundleA(bundle)
    a.opcode  := TLMessages.Acquire
    a.param   := growPermissions
    a.size    := lgSize
    a.source  := fromSource
    a.address := toAddress
    a.wmask   := SInt(-1).asUInt
    a.data    := UInt(0)
    (legal, a)
  }

  def Release(fromSource: UInt, toAddress: UInt, lgSize: UInt, shrinkPermissions: UInt) = {
    require (manager.anySupportAcquire)
    val legal = manager.supportsAcquire(toAddress, lgSize)
    val c = new TLBundleC(bundle)
    c.opcode  := TLMessages.Release
    c.param   := shrinkPermissions
    c.size    := lgSize
    c.source  := fromSource
    c.address := toAddress
    c.data    := UInt(0)
    c.error   := Bool(false)
    (legal, c)
  }

  def Release(fromSource: UInt, toAddress: UInt, lgSize: UInt, shrinkPermissions: UInt, data: UInt) = {
    require (manager.anySupportAcquire)
    val legal = manager.supportsAcquire(toAddress, lgSize)
    val c = new TLBundleC(bundle)
    c.opcode  := TLMessages.ReleaseData
    c.param   := shrinkPermissions
    c.size    := lgSize
    c.source  := fromSource
    c.address := toAddress
    c.data    := data
    c.error   := Bool(false)
    (legal, c)
  }

  def ProbeAck(toAddress: UInt, lgSize: UInt, reportPermissions: UInt) = {
    val c = new TLBundleC(bundle)
    c.opcode  := TLMessages.ProbeAck
    c.param   := reportPermissions
    c.size    := lgSize
    c.source  := UInt(0)
    c.address := toAddress
    c.data    := UInt(0)
    c.error   := Bool(false)
    c
  }

  def ProbeAck(toAddress: UInt, lgSize: UInt, reportPermissions: UInt, data: UInt) = {
    val c = new TLBundleC(bundle)
    c.opcode  := TLMessages.ProbeAckData
    c.param   := reportPermissions
    c.size    := lgSize
    c.source  := UInt(0)
    c.address := toAddress
    c.data    := data
    c.error   := Bool(false)
    c
  }

  def GrantAck(toSink: UInt) = {
    val e = new TLBundleE(bundle)
    e.sink := toSink
    e
  }

  // Accesses
  def Get(fromSource: UInt, toAddress: UInt, lgSize: UInt) = {
    require (manager.anySupportGet)
    val legal = manager.supportsGet(toAddress, lgSize)
    val a = new TLBundleA(bundle)
    a.opcode  := TLMessages.Get
    a.param   := UInt(0)
    a.size    := lgSize
    a.source  := fromSource
    a.address := toAddress
    a.wmask   := fullMask(toAddress, lgSize)
    a.data    := UInt(0)
    (legal, a)
  }

  def Put(fromSource: UInt, toAddress: UInt, lgSize: UInt, data: UInt) = {
    require (manager.anySupportPutFull)
    val legal = manager.supportsPutFull(toAddress, lgSize)
    val a = new TLBundleA(bundle)
    a.opcode  := TLMessages.PutFullData
    a.param   := UInt(0)
    a.size    := lgSize
    a.source  := fromSource
    a.address := toAddress
    a.wmask   := fullMask(toAddress, lgSize)
    a.data    := data
    (legal, a)
  }

  def Put(fromSource: UInt, toAddress: UInt, lgSize: UInt, data: UInt, wmask: UInt) = {
    require (manager.anySupportPutPartial)
    val legal = manager.supportsPutPartial(toAddress, lgSize)
    val a = new TLBundleA(bundle)
    a.opcode  := TLMessages.PutPartialData
    a.param   := UInt(0)
    a.size    := lgSize
    a.source  := fromSource
    a.address := toAddress
    a.wmask   := wmask
    a.data    := data
    (legal, a)
  }

  def Arithmetic(fromSource: UInt, toAddress: UInt, lgSize: UInt, data: UInt, atomic: UInt) = {
    require (manager.anySupportArithmetic)
    val legal = manager.supportsArithmetic(toAddress, lgSize)
    val a = new TLBundleA(bundle)
    a.opcode  := TLMessages.ArithmeticData
    a.param   := atomic
    a.size    := lgSize
    a.source  := fromSource
    a.address := toAddress
    a.wmask   := fullMask(toAddress, lgSize)
    a.data    := data
    (legal, a)
  }

  def Logical(fromSource: UInt, toAddress: UInt, lgSize: UInt, data: UInt, atomic: UInt) = {
    require (manager.anySupportLogical)
    val legal = manager.supportsLogical(toAddress, lgSize)
    val a = new TLBundleA(bundle)
    a.opcode  := TLMessages.LogicalData
    a.param   := atomic
    a.size    := lgSize
    a.source  := fromSource
    a.address := toAddress
    a.wmask   := fullMask(toAddress, lgSize)
    a.data    := data
    (legal, a)
  }

  def Hint(fromSource: UInt, toAddress: UInt, lgSize: UInt, param: UInt) = {
    require (manager.anySupportHint)
    val legal = manager.supportsHint(toAddress)
    val a = new TLBundleA(bundle)
    a.opcode  := TLMessages.Hint
    a.param   := param
    a.size    := lgSize
    a.source  := fromSource
    a.address := toAddress
    a.wmask   := fullMask(toAddress, lgSize)
    a.data    := UInt(0)
    (legal, a)
  }

  def AccessAck(toAddress: UInt, lgSize: UInt, error: Bool = Bool(false)) = {
    val c = new TLBundleC(bundle)
    c.opcode  := TLMessages.AccessAck
    c.param   := UInt(0)
    c.size    := lgSize
    c.source  := UInt(0)
    c.address := toAddress
    c.data    := UInt(0)
    c.error   := error
    c
  }

  def AccessAck(toAddress: UInt, lgSize: UInt, data: UInt) = {
    val c = new TLBundleC(bundle)
    c.opcode  := TLMessages.AccessAckData
    c.param   := UInt(0)
    c.size    := lgSize
    c.source  := UInt(0)
    c.address := toAddress
    c.data    := data
    c.error   := Bool(false)
    c
  }
}

class TLEdgeIn(
  client:  TLClientPortParameters,
  manager: TLManagerPortParameters)
  extends TLEdgeParameters(client, manager)
{
  // Transfers
  def Probe(fromAddress: UInt, toSource: UInt, lgSize: UInt, capPermissions: UInt) = {
    require (client.anySupportProbe)
    val legal = client.supportsProbe(fromAddress, lgSize)
    val b = new TLBundleB(bundle)
    b.opcode  := TLMessages.Probe
    b.param   := capPermissions
    b.size    := lgSize
    b.source  := toSource
    b.address := fromAddress
    b.wmask   := fullMask(fromAddress, lgSize)
    b.data    := UInt(0)
    (legal, b)
  }

  def Grant(fromSink: UInt, toSource: UInt, lgSize: UInt, capPermissions: UInt) = {
    val d = new TLBundleD(bundle)
    d.opcode := TLMessages.Grant
    d.param  := capPermissions
    d.size   := lgSize
    d.source := toSource
    d.sink   := fromSink
    d.data   := UInt(0)
    d.error  := Bool(false)
    d
  }

  def GrantData(fromSink: UInt, toSource: UInt, lgSize: UInt, capPermissions: UInt, data: UInt) = {
    val d = new TLBundleD(bundle)
    d.opcode := TLMessages.GrantData
    d.param  := capPermissions
    d.size   := lgSize
    d.source := toSource
    d.sink   := fromSink
    d.data   := data
    d.error  := Bool(false)
    d
  }

  def ReleaseAck(toSource: UInt, lgSize: UInt) = {
    val d = new TLBundleD(bundle)
    d.opcode := TLMessages.ReleaseAck
    d.param  := UInt(0)
    d.size   := lgSize
    d.source := toSource
    d.sink   := UInt(0)
    d.data   := UInt(0)
    d.error  := Bool(false)
    d
  }

  // Accesses
  def Get(fromAddress: UInt, toSource: UInt, lgSize: UInt) = {
    require (client.anySupportGet)
    val legal = client.supportsGet(toSource, lgSize)
    val b = new TLBundleB(bundle)
    b.opcode  := TLMessages.Get
    b.param   := UInt(0)
    b.size    := lgSize
    b.source  := toSource
    b.address := fromAddress
    b.wmask   := fullMask(fromAddress, lgSize)
    b.data    := UInt(0)
    (legal, b)
  }

  def Put(fromAddress: UInt, toSource: UInt, lgSize: UInt, data: UInt) = {
    require (client.anySupportPutFull)
    val legal = client.supportsPutFull(toSource, lgSize)
    val b = new TLBundleB(bundle)
    b.opcode  := TLMessages.PutFullData
    b.param   := UInt(0)
    b.size    := lgSize
    b.source  := toSource
    b.address := fromAddress
    b.wmask   := fullMask(fromAddress, lgSize)
    b.data    := data
    (legal, b)
  }

  def Put(fromAddress: UInt, toSource: UInt, lgSize: UInt, data: UInt, wmask: UInt) = {
    require (client.anySupportPutPartial)
    val legal = client.supportsPutPartial(toSource, lgSize)
    val b = new TLBundleB(bundle)
    b.opcode  := TLMessages.PutPartialData
    b.param   := UInt(0)
    b.size    := lgSize
    b.source  := toSource
    b.address := fromAddress
    b.wmask   := wmask
    b.data    := data
    (legal, b)
  }

  def Arithmetic(fromAddress: UInt, toSource: UInt, lgSize: UInt, data: UInt, atomic: UInt) = {
    require (client.anySupportArithmetic)
    val legal = client.supportsArithmetic(toSource, lgSize)
    val b = new TLBundleB(bundle)
    b.opcode  := TLMessages.ArithmeticData
    b.param   := atomic
    b.size    := lgSize
    b.source  := toSource
    b.address := fromAddress
    b.wmask   := fullMask(fromAddress, lgSize)
    b.data    := data
    (legal, b)
  }

  def Logical(fromAddress: UInt, toSource: UInt, lgSize: UInt, data: UInt, atomic: UInt) = {
    require (client.anySupportLogical)
    val legal = client.supportsLogical(toSource, lgSize)
    val b = new TLBundleB(bundle)
    b.opcode  := TLMessages.LogicalData
    b.param   := atomic
    b.size    := lgSize
    b.source  := toSource
    b.address := fromAddress
    b.wmask   := fullMask(fromAddress, lgSize)
    b.data    := data
    (legal, b)
  }

  def Hint(fromAddress: UInt, toSource: UInt, lgSize: UInt, param: UInt) = {
    require (client.anySupportHint)
    val legal = client.supportsHint(toSource)
    val b = new TLBundleB(bundle)
    b.opcode  := TLMessages.Hint
    b.param   := param
    b.size    := lgSize
    b.source  := toSource
    b.address := fromAddress
    b.wmask   := fullMask(fromAddress, lgSize)
    b.data    := UInt(0)
    (legal, b)
  }

  def AccessAck(toSource: UInt, lgSize: UInt, error: Bool = Bool(false)) = {
    val d = new TLBundleD(bundle)
    d.opcode := TLMessages.AccessAck
    d.param  := UInt(0)
    d.size   := lgSize
    d.source := toSource
    d.sink   := UInt(0)
    d.data   := UInt(0)
    d.error  := error
    d
  }

  def AccessAck(toSource: UInt, lgSize: UInt, data: UInt) = {
    val d = new TLBundleD(bundle)
    d.opcode := TLMessages.AccessAckData
    d.param  := UInt(0)
    d.size   := lgSize
    d.source := toSource
    d.sink   := UInt(0)
    d.data   := data
    d.error  := Bool(false)
    d
  }
}
