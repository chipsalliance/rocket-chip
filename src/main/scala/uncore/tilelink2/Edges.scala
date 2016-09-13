// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo

class TLEdge(
  client:  TLClientPortParameters,
  manager: TLManagerPortParameters)
  extends TLEdgeParameters(client, manager)
{
  def isHiAligned(addr_hi: UInt, lgSize: UInt): Bool = {
    if (maxLgSize == 0) Bool(true) else {
      val mask = UIntToOH1(lgSize, maxLgSize) >> log2Ceil(manager.beatBytes)
      (addr_hi & mask) === UInt(0)
    }
  }

  def isLoAligned(addr_lo: UInt, lgSize: UInt): Bool = {
    if (maxLgSize == 0) Bool(true) else {
      val mask = UIntToOH1(lgSize, maxLgSize)
      (addr_lo & mask) === UInt(0)
    }
  }

  // This gets used everywhere, so make the smallest circuit possible ...
  def mask(addr_lo: UInt, lgSize: UInt): UInt = {
    val lgBytes = log2Ceil(manager.beatBytes)
    val sizeOH = UIntToOH(lgSize, log2Up(manager.beatBytes))
    def helper(i: Int): Seq[(Bool, Bool)] = {
      if (i == 0) {
        Seq((lgSize >= UInt(lgBytes), Bool(true)))
      } else {
        val sub = helper(i-1)
        val size = sizeOH(lgBytes - i)
        val bit = addr_lo(lgBytes - i)
        val nbit = !bit
        Seq.tabulate (1 << i) { j =>
          val (sub_acc, sub_eq) = sub(j/2)
          val eq = sub_eq && (if (j % 2 == 1) bit else nbit)
          val acc = sub_acc || (size && eq)
          (acc, eq)
        }
      }
    }
    Cat(helper(lgBytes).map(_._1).reverse)
  }

  // !!! make sure to align addr_lo for PutPartials with 0 masks
  def addr_lo(mask: UInt, lgSize: UInt): UInt = {
    val sizeOH1 = UIntToOH1(lgSize, log2Up(manager.beatBytes))
    // Almost OHToUInt, but bits set => bits not set
    def helper(mask: UInt, width: Int): UInt = {
      if (width <= 1) {
        UInt(0)
      } else if (width == 2) {
        ~mask(0, 0)
      } else {
        val mid = 1 << (log2Up(width)-1)
        val hi = mask(width-1, mid)
        val lo = mask(mid-1, 0)
        Cat(!lo.orR, helper(hi | lo, mid))
      }
    }
    helper(mask, bundle.dataBits/8) & ~sizeOH1
  }

  def full_mask(imask: UInt, lgSize: UInt): UInt = {
    mask(addr_lo(imask, lgSize), lgSize)
  }

  def staticHasData(bundle: TLChannel): Option[Boolean] = {
    bundle match {
      case _:TLBundleA => {
        // Do there exist A messages with Data?
        val aDataYes = manager.anySupportArithmetic || manager.anySupportLogical || manager.anySupportPutFull || manager.anySupportPutPartial
        // Do there exist A messages without Data?
        val aDataNo  = manager.anySupportAcquire || manager.anySupportGet || manager.anySupportHint
        // Statically optimize the case where hasData is a constant
        if (!aDataYes) Some(false) else if (!aDataNo) Some(true) else None
      }
      case _:TLBundleB => {
        // Do there exist B messages with Data?
        val bDataYes = client.anySupportArithmetic || client.anySupportLogical || client.anySupportPutFull || client.anySupportPutPartial
        // Do there exist B messages without Data?
        val bDataNo  = client.anySupportProbe || client.anySupportGet || client.anySupportHint
        // Statically optimize the case where hasData is a constant
        if (!bDataYes) Some(false) else if (!bDataNo) Some(true) else None
      }
      case _:TLBundleC => {
        // Do there eixst C messages with Data?
        val cDataYes = client.anySupportGet || client.anySupportArithmetic || client.anySupportLogical || client.anySupportProbe
        // Do there exist C messages without Data?
        val cDataNo  = client.anySupportPutFull || client.anySupportPutPartial || client.anySupportHint || client.anySupportProbe
        if (!cDataYes) Some(false) else if (!cDataNo) Some(true) else None
      }
      case _:TLBundleD => {
        // Do there eixst D messages with Data?
        val dDataYes = manager.anySupportGet || manager.anySupportArithmetic || manager.anySupportLogical || manager.anySupportAcquire
        // Do there exist D messages without Data?
        val dDataNo  = manager.anySupportPutFull || manager.anySupportPutPartial || manager.anySupportHint || manager.anySupportAcquire
        if (!dDataYes) Some(false) else if (!dDataNo) Some(true) else None
      }
      case _:TLBundleE => Some(false)
    }
  }

  def hasFollowUp(x: TLChannel): Bool = {
    x match {
      case a: TLBundleA => Bool(true)
      case b: TLBundleB => Bool(true)
      case c: TLBundleC => c.opcode(2) && c.opcode(1)
        //    opcode === TLMessages.Release ||
        //    opcode === TLMessages.ReleaseData
      case d: TLBundleD => d.opcode(2) && !d.opcode(1)
        //    opcode === TLMessages.Grant     ||
        //    opcode === TLMessages.GrantData
      case e: TLBundleE => Bool(false)
    }
  }

  def hasData(x: TLChannel): Bool = {
    val opdata = x match {
      case a: TLBundleA => !a.opcode(2)
        //    opcode === TLMessages.PutFullData    ||
        //    opcode === TLMessages.PutPartialData ||
        //    opcode === TLMessages.ArithmeticData ||
        //    opcode === TLMessages.LogicalData
      case b: TLBundleB => !b.opcode(2)
        //    opcode === TLMessages.PutFullData    ||
        //    opcode === TLMessages.PutPartialData ||
        //    opcode === TLMessages.ArithmeticData ||
        //    opcode === TLMessages.LogicalData
      case c: TLBundleC => c.opcode(0)
        //    opcode === TLMessages.AccessAckData ||
        //    opcode === TLMessages.ProbeAckData  ||
        //    opcode === TLMessages.ReleaseData
      case d: TLBundleD => d.opcode(0)
        //    opcode === TLMessages.AccessAckData ||
        //    opcode === TLMessages.GrantData
      case e: TLBundleE => Bool(false)
    }
    staticHasData(x).map(Bool(_)).getOrElse(opdata)
  }

  def size(x: TLDataChannel): UInt = {
    x match {
      case a: TLBundleA => a.size
      case b: TLBundleB => b.size
      case c: TLBundleC => c.size
      case d: TLBundleD => d.size
    }
  }

  def data(x: TLDataChannel): UInt = {
    x match {
      case a: TLBundleA => a.data
      case b: TLBundleB => b.data
      case c: TLBundleC => c.data
      case d: TLBundleD => d.data
    }
  }

  def mask(x: TLDataChannel): UInt = {
    x match {
      case a: TLBundleA => a.mask
      case b: TLBundleB => b.mask
      case c: TLBundleC => mask(c.addr_lo, c.size)
      case d: TLBundleD => mask(d.addr_lo, d.size)
    }
  }

  def addr_lo(x: TLDataChannel): UInt = {
    x match {
      case a: TLBundleA => addr_lo(a.mask, a.size)
      case b: TLBundleB => addr_lo(b.mask, b.size)
      case c: TLBundleC => c.addr_lo
      case d: TLBundleD => d.addr_lo
    }
  }

  def full_mask(x: TLDataChannel): UInt = {
    x match {
      case a: TLBundleA => full_mask(a.mask, a.size)
      case b: TLBundleB => full_mask(b.mask, b.size)
      case c: TLBundleC => mask(c.addr_lo, c.size)
      case d: TLBundleD => mask(d.addr_lo, d.size)
    }
  }

  def address(x: TLAddrChannel): UInt = {
    val hi = x match {
      case a: TLBundleA => a.addr_hi
      case b: TLBundleB => b.addr_hi
      case c: TLBundleC => c.addr_hi
    }
    if (manager.beatBytes == 1) hi else Cat(hi, addr_lo(x))
  }

  def numBeats(x: TLChannel): UInt = {
    x match {
      case _: TLBundleE => UInt(1)
      case bundle: TLDataChannel => {
        val hasData = this.hasData(bundle)
        val size = this.size(bundle)
        val cutoff = log2Ceil(manager.beatBytes)
        val small = if (manager.maxTransfer <= manager.beatBytes) Bool(true) else size <= UInt(cutoff)
        val decode = UIntToOH(size, maxLgSize+1) >> cutoff
        Mux(hasData, decode | small.asUInt, UInt(1))
      }
    }
  }

  def numBeats1(x: TLChannel): UInt = {
    x match {
      case _: TLBundleE => UInt(0)
      case bundle: TLDataChannel => {
        val decode = UIntToOH1(size(bundle), maxLgSize) >> log2Ceil(manager.beatBytes)
        Mux(hasData(bundle), decode, UInt(0))
      }
    }
  }
}

class TLEdgeOut(
  client:  TLClientPortParameters,
  manager: TLManagerPortParameters)
  extends TLEdge(client, manager)
{
  // Transfers
  def Acquire(fromSource: UInt, toAddress: UInt, lgSize: UInt, growPermissions: UInt) = {
    require (manager.anySupportAcquire)
    val legal = manager.supportsAcquire(toAddress, lgSize)
    val a = Wire(new TLBundleA(bundle))
    a.opcode  := TLMessages.Acquire
    a.param   := growPermissions
    a.size    := lgSize
    a.source  := fromSource
    a.addr_hi := toAddress >> log2Ceil(manager.beatBytes)
    a.mask    := SInt(-1).asUInt
    a.data    := UInt(0)
    (legal, a)
  }

  def Release(fromSource: UInt, toAddress: UInt, lgSize: UInt, shrinkPermissions: UInt) = {
    require (manager.anySupportAcquire)
    val legal = manager.supportsAcquire(toAddress, lgSize)
    val c = Wire(new TLBundleC(bundle))
    c.opcode  := TLMessages.Release
    c.param   := shrinkPermissions
    c.size    := lgSize
    c.source  := fromSource
    c.addr_hi := toAddress >> log2Ceil(manager.beatBytes)
    c.addr_lo := toAddress
    c.data    := UInt(0)
    c.error   := Bool(false)
    (legal, c)
  }

  def Release(fromSource: UInt, toAddress: UInt, lgSize: UInt, shrinkPermissions: UInt, data: UInt) = {
    require (manager.anySupportAcquire)
    val legal = manager.supportsAcquire(toAddress, lgSize)
    val c = Wire(new TLBundleC(bundle))
    c.opcode  := TLMessages.ReleaseData
    c.param   := shrinkPermissions
    c.size    := lgSize
    c.source  := fromSource
    c.addr_hi := toAddress >> log2Ceil(manager.beatBytes)
    c.addr_lo := toAddress
    c.data    := data
    c.error   := Bool(false)
    (legal, c)
  }

  def ProbeAck(fromSource: UInt, toAddress: UInt, lgSize: UInt, reportPermissions: UInt) = {
    val c = Wire(new TLBundleC(bundle))
    c.opcode  := TLMessages.ProbeAck
    c.param   := reportPermissions
    c.size    := lgSize
    c.source  := fromSource
    c.addr_hi := toAddress >> log2Ceil(manager.beatBytes)
    c.addr_lo := toAddress
    c.data    := UInt(0)
    c.error   := Bool(false)
    c
  }

  def ProbeAck(fromSource: UInt, toAddress: UInt, lgSize: UInt, reportPermissions: UInt, data: UInt) = {
    val c = Wire(new TLBundleC(bundle))
    c.opcode  := TLMessages.ProbeAckData
    c.param   := reportPermissions
    c.size    := lgSize
    c.source  := fromSource
    c.addr_hi := toAddress >> log2Ceil(manager.beatBytes)
    c.addr_lo := toAddress
    c.data    := data
    c.error   := Bool(false)
    c
  }

  def GrantAck(toSink: UInt) = {
    val e = Wire(new TLBundleE(bundle))
    e.sink := toSink
    e
  }

  // Accesses
  def Get(fromSource: UInt, toAddress: UInt, lgSize: UInt) = {
    require (manager.anySupportGet)
    val legal = manager.supportsGet(toAddress, lgSize)
    val a = Wire(new TLBundleA(bundle))
    a.opcode  := TLMessages.Get
    a.param   := UInt(0)
    a.size    := lgSize
    a.source  := fromSource
    a.addr_hi := toAddress >> log2Ceil(manager.beatBytes)
    a.mask    := mask(toAddress, lgSize)
    a.data    := UInt(0)
    (legal, a)
  }

  def Put(fromSource: UInt, toAddress: UInt, lgSize: UInt, data: UInt) = {
    require (manager.anySupportPutFull)
    val legal = manager.supportsPutFull(toAddress, lgSize)
    val a = Wire(new TLBundleA(bundle))
    a.opcode  := TLMessages.PutFullData
    a.param   := UInt(0)
    a.size    := lgSize
    a.source  := fromSource
    a.addr_hi := toAddress >> log2Ceil(manager.beatBytes)
    a.mask    := mask(toAddress, lgSize)
    a.data    := data
    (legal, a)
  }

  def Put(fromSource: UInt, toAddress: UInt, lgSize: UInt, data: UInt, mask : UInt) = {
    require (manager.anySupportPutPartial)
    val legal = manager.supportsPutPartial(toAddress, lgSize)
    val a = Wire(new TLBundleA(bundle))
    a.opcode  := TLMessages.PutPartialData
    a.param   := UInt(0)
    a.size    := lgSize
    a.source  := fromSource
    a.addr_hi := toAddress >> log2Ceil(manager.beatBytes)
    a.mask    := mask
    a.data    := data
    (legal, a)
  }

  def Arithmetic(fromSource: UInt, toAddress: UInt, lgSize: UInt, data: UInt, atomic: UInt) = {
    require (manager.anySupportArithmetic)
    val legal = manager.supportsArithmetic(toAddress, lgSize)
    val a = Wire(new TLBundleA(bundle))
    a.opcode  := TLMessages.ArithmeticData
    a.param   := atomic
    a.size    := lgSize
    a.source  := fromSource
    a.addr_hi := toAddress >> log2Ceil(manager.beatBytes)
    a.mask    := mask(toAddress, lgSize)
    a.data    := data
    (legal, a)
  }

  def Logical(fromSource: UInt, toAddress: UInt, lgSize: UInt, data: UInt, atomic: UInt) = {
    require (manager.anySupportLogical)
    val legal = manager.supportsLogical(toAddress, lgSize)
    val a = Wire(new TLBundleA(bundle))
    a.opcode  := TLMessages.LogicalData
    a.param   := atomic
    a.size    := lgSize
    a.source  := fromSource
    a.addr_hi := toAddress >> log2Ceil(manager.beatBytes)
    a.mask    := mask(toAddress, lgSize)
    a.data    := data
    (legal, a)
  }

  def Hint(fromSource: UInt, toAddress: UInt, lgSize: UInt, param: UInt) = {
    require (manager.anySupportHint)
    val legal = manager.supportsHint(toAddress, lgSize)
    val a = Wire(new TLBundleA(bundle))
    a.opcode  := TLMessages.Hint
    a.param   := param
    a.size    := lgSize
    a.source  := fromSource
    a.addr_hi := toAddress >> log2Ceil(manager.beatBytes)
    a.mask    := mask(toAddress, lgSize)
    a.data    := UInt(0)
    (legal, a)
  }

  def AccessAck(b: TLBundleB): TLBundleC = AccessAck(b.source, address(b), b.size)
  def AccessAck(b: TLBundleB, error: Bool): TLBundleC = AccessAck(b.source, address(b), b.size, error)
  def AccessAck(fromSource: UInt, toAddress: UInt, lgSize: UInt): TLBundleC = AccessAck(fromSource, toAddress, lgSize, Bool(false))
  def AccessAck(fromSource: UInt, toAddress: UInt, lgSize: UInt, error: Bool) = {
    val c = Wire(new TLBundleC(bundle))
    c.opcode  := TLMessages.AccessAck
    c.param   := UInt(0)
    c.size    := lgSize
    c.source  := fromSource
    c.addr_hi := toAddress >> log2Ceil(manager.beatBytes)
    c.addr_lo := toAddress
    c.data    := UInt(0)
    c.error   := error
    c
  }

  def AccessAck(b: TLBundleB, data: UInt): TLBundleC = AccessAck(b.source, address(b), b.size, data)
  def AccessAck(b: TLBundleB, data: UInt, error: Bool): TLBundleC = AccessAck(b.source, address(b), b.size, data, error)
  def AccessAck(fromSource: UInt, toAddress: UInt, lgSize: UInt, data: UInt): TLBundleC = AccessAck(fromSource, toAddress, lgSize, data, Bool(false))
  def AccessAck(fromSource: UInt, toAddress: UInt, lgSize: UInt, data: UInt, error: Bool) = {
    val c = Wire(new TLBundleC(bundle))
    c.opcode  := TLMessages.AccessAckData
    c.param   := UInt(0)
    c.size    := lgSize
    c.source  := fromSource
    c.addr_hi := toAddress >> log2Ceil(manager.beatBytes)
    c.addr_lo := toAddress
    c.data    := data
    c.error   := error
    c
  }

  def HintAck(b: TLBundleB): TLBundleC = HintAck(b.source, address(b), b.size)
  def HintAck(fromSource: UInt, toAddress: UInt, lgSize: UInt) = {
    val c = Wire(new TLBundleC(bundle))
    c.opcode  := TLMessages.HintAck
    c.param   := UInt(0)
    c.size    := lgSize
    c.source  := fromSource
    c.addr_hi := toAddress >> log2Ceil(manager.beatBytes)
    c.addr_lo := toAddress
    c.data    := UInt(0)
    c.error   := Bool(false)
    c
  }
}

class TLEdgeIn(
  client:  TLClientPortParameters,
  manager: TLManagerPortParameters)
  extends TLEdge(client, manager)
{
  // Transfers
  def Probe(fromAddress: UInt, toSource: UInt, lgSize: UInt, capPermissions: UInt) = {
    require (client.anySupportProbe)
    val legal = client.supportsProbe(fromAddress, lgSize)
    val b = Wire(new TLBundleB(bundle))
    b.opcode  := TLMessages.Probe
    b.param   := capPermissions
    b.size    := lgSize
    b.source  := toSource
    b.addr_hi := fromAddress >> log2Ceil(manager.beatBytes)
    b.mask    := SInt(-1).asUInt
    b.data    := UInt(0)
    (legal, b)
  }

  def Grant(fromAddress: UInt, fromSink: UInt, toSource: UInt, lgSize: UInt, capPermissions: UInt): TLBundleD = Grant(fromAddress, fromSink, toSource, lgSize, capPermissions, Bool(false))
  def Grant(fromAddress: UInt, fromSink: UInt, toSource: UInt, lgSize: UInt, capPermissions: UInt, error: Bool) = {
    val d = Wire(new TLBundleD(bundle))
    d.opcode  := TLMessages.Grant
    d.param   := capPermissions
    d.size    := lgSize
    d.source  := toSource
    d.sink    := fromSink
    d.addr_lo := fromAddress
    d.data    := UInt(0)
    d.error   := error
    d
  }

  def Grant(fromAddress: UInt, fromSink: UInt, toSource: UInt, lgSize: UInt, capPermissions: UInt, data: UInt): TLBundleD = Grant(fromAddress, fromSink, toSource, lgSize, capPermissions, data, Bool(false))
  def Grant(fromAddress: UInt, fromSink: UInt, toSource: UInt, lgSize: UInt, capPermissions: UInt, data: UInt, error: Bool) = {
    val d = Wire(new TLBundleD(bundle))
    d.opcode  := TLMessages.GrantData
    d.param   := capPermissions
    d.size    := lgSize
    d.source  := toSource
    d.sink    := fromSink
    d.addr_lo := fromAddress
    d.data    := data
    d.error   := error
    d
  }

  def ReleaseAck(fromAddress: UInt, fromSink: UInt, toSource: UInt, lgSize: UInt) = {
    val d = Wire(new TLBundleD(bundle))
    d.opcode  := TLMessages.ReleaseAck
    d.param   := UInt(0)
    d.size    := lgSize
    d.source  := toSource
    d.sink    := fromSink
    d.addr_lo := fromAddress
    d.data    := UInt(0)
    d.error   := Bool(false)
    d
  }

  // Accesses
  def Get(fromAddress: UInt, toSource: UInt, lgSize: UInt) = {
    require (client.anySupportGet)
    val legal = client.supportsGet(toSource, lgSize)
    val b = Wire(new TLBundleB(bundle))
    b.opcode  := TLMessages.Get
    b.param   := UInt(0)
    b.size    := lgSize
    b.source  := toSource
    b.addr_hi := fromAddress >> log2Ceil(manager.beatBytes)
    b.mask    := mask(fromAddress, lgSize)
    b.data    := UInt(0)
    (legal, b)
  }

  def Put(fromAddress: UInt, toSource: UInt, lgSize: UInt, data: UInt) = {
    require (client.anySupportPutFull)
    val legal = client.supportsPutFull(toSource, lgSize)
    val b = Wire(new TLBundleB(bundle))
    b.opcode  := TLMessages.PutFullData
    b.param   := UInt(0)
    b.size    := lgSize
    b.source  := toSource
    b.addr_hi := fromAddress >> log2Ceil(manager.beatBytes)
    b.mask    := mask(fromAddress, lgSize)
    b.data    := data
    (legal, b)
  }

  def Put(fromAddress: UInt, toSource: UInt, lgSize: UInt, data: UInt, mask : UInt) = {
    require (client.anySupportPutPartial)
    val legal = client.supportsPutPartial(toSource, lgSize)
    val b = Wire(new TLBundleB(bundle))
    b.opcode  := TLMessages.PutPartialData
    b.param   := UInt(0)
    b.size    := lgSize
    b.source  := toSource
    b.addr_hi := fromAddress >> log2Ceil(manager.beatBytes)
    b.mask    := mask
    b.data    := data
    (legal, b)
  }

  def Arithmetic(fromAddress: UInt, toSource: UInt, lgSize: UInt, data: UInt, atomic: UInt) = {
    require (client.anySupportArithmetic)
    val legal = client.supportsArithmetic(toSource, lgSize)
    val b = Wire(new TLBundleB(bundle))
    b.opcode  := TLMessages.ArithmeticData
    b.param   := atomic
    b.size    := lgSize
    b.source  := toSource
    b.addr_hi := fromAddress >> log2Ceil(manager.beatBytes)
    b.mask    := mask(fromAddress, lgSize)
    b.data    := data
    (legal, b)
  }

  def Logical(fromAddress: UInt, toSource: UInt, lgSize: UInt, data: UInt, atomic: UInt) = {
    require (client.anySupportLogical)
    val legal = client.supportsLogical(toSource, lgSize)
    val b = Wire(new TLBundleB(bundle))
    b.opcode  := TLMessages.LogicalData
    b.param   := atomic
    b.size    := lgSize
    b.source  := toSource
    b.addr_hi := fromAddress >> log2Ceil(manager.beatBytes)
    b.mask    := mask(fromAddress, lgSize)
    b.data    := data
    (legal, b)
  }

  def Hint(fromAddress: UInt, toSource: UInt, lgSize: UInt, param: UInt) = {
    require (client.anySupportHint)
    val legal = client.supportsHint(toSource, lgSize)
    val b = Wire(new TLBundleB(bundle))
    b.opcode  := TLMessages.Hint
    b.param   := param
    b.size    := lgSize
    b.source  := toSource
    b.addr_hi := fromAddress >> log2Ceil(manager.beatBytes)
    b.mask    := mask(fromAddress, lgSize)
    b.data    := UInt(0)
    (legal, b)
  }

  def AccessAck(a: TLBundleA, fromSink: UInt): TLBundleD = AccessAck(address(a), fromSink, a.source, a.size)
  def AccessAck(a: TLBundleA, fromSink: UInt, error: Bool): TLBundleD = AccessAck(address(a), fromSink, a.source, a.size, error)
  def AccessAck(fromAddress: UInt, fromSink: UInt, toSource: UInt, lgSize: UInt): TLBundleD = AccessAck(fromAddress, fromSink, toSource, lgSize, Bool(false))
  def AccessAck(fromAddress: UInt, fromSink: UInt, toSource: UInt, lgSize: UInt, error: Bool) = {
    val d = Wire(new TLBundleD(bundle))
    d.opcode  := TLMessages.AccessAck
    d.param   := UInt(0)
    d.size    := lgSize
    d.source  := toSource
    d.sink    := fromSink
    d.addr_lo := fromAddress
    d.data    := UInt(0)
    d.error   := error
    d
  }

  def AccessAck(a: TLBundleA, fromSink: UInt, data: UInt): TLBundleD = AccessAck(address(a), fromSink, a.source, a.size, data)
  def AccessAck(a: TLBundleA, fromSink: UInt, data: UInt, error: Bool): TLBundleD = AccessAck(address(a), fromSink, a.source, a.size, data, error)
  def AccessAck(fromAddress: UInt, fromSink: UInt, toSource: UInt, lgSize: UInt, data: UInt): TLBundleD = AccessAck(fromAddress, fromSink, toSource, lgSize, data, Bool(false))
  def AccessAck(fromAddress: UInt, fromSink: UInt, toSource: UInt, lgSize: UInt, data: UInt, error: Bool) = {
    val d = Wire(new TLBundleD(bundle))
    d.opcode  := TLMessages.AccessAckData
    d.param   := UInt(0)
    d.size    := lgSize
    d.source  := toSource
    d.sink    := fromSink
    d.addr_lo := fromAddress
    d.data    := data
    d.error   := error
    d
  }

  def HintAck(a: TLBundleA, fromSink: UInt): TLBundleD = HintAck(address(a), fromSink, a.source, a.size)
  def HintAck(fromAddress: UInt, fromSink: UInt, toSource: UInt, lgSize: UInt) = {
    val d = Wire(new TLBundleD(bundle))
    d.opcode  := TLMessages.HintAck
    d.param   := UInt(0)
    d.size    := lgSize
    d.source  := toSource
    d.sink    := fromSink
    d.addr_lo := fromAddress
    d.data    := UInt(0)
    d.error   := Bool(false)
    d
  }
}
