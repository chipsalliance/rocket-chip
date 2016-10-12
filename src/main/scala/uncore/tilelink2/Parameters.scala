// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import diplomacy._
import scala.math.max

case class TLManagerParameters(
  address:            Seq[AddressSet],
  sinkId:             IdRange       = IdRange(0, 1),
  regionType:         RegionType.T  = RegionType.GET_EFFECTS,
  executable:         Boolean       = false, // processor can execute from this memory
  nodePath:           Seq[BaseNode] = Seq(),
  // Supports both Acquire+Release+Finish of these sizes
  supportsAcquire:    TransferSizes = TransferSizes.none,
  supportsArithmetic: TransferSizes = TransferSizes.none,
  supportsLogical:    TransferSizes = TransferSizes.none,
  supportsGet:        TransferSizes = TransferSizes.none,
  supportsPutFull:    TransferSizes = TransferSizes.none,
  supportsPutPartial: TransferSizes = TransferSizes.none,
  supportsHint:       TransferSizes = TransferSizes.none,
  // If fifoId=Some, all accesses sent to the same fifoId are executed and ACK'd in FIFO order
  fifoId:             Option[Int]   = None,
  customDTS:          Option[String]= None)
{
  address.foreach { a => require (a.finite) }
  address.combinations(2).foreach { case Seq(x,y) => require (!x.overlaps(y)) }
  require (supportsPutFull.contains(supportsPutPartial))

  // Largest support transfer of all types
  val maxTransfer = List(
    supportsAcquire.max,
    supportsArithmetic.max,
    supportsLogical.max,
    supportsGet.max,
    supportsPutFull.max,
    supportsPutPartial.max).max
  val maxAddress = address.map(_.max).max

  val name = nodePath.lastOption.map(_.lazyModule.name).getOrElse("disconnected")

  // Generate the config string (in future device tree)
  lazy val dts = customDTS.getOrElse {
    val header = s"${name} {\n"
    val middle = address.map { a =>
      require (a.contiguous) // Config String is not so flexible
      "  addr 0x%x;\n  size 0x%x;\n".format(a.base, a.mask+1)
    }
    val footer = "}\n"
    header + middle.reduce(_ + _) + footer
  }

  // The device had better not support a transfer larger than it's alignment
  val minAlignment = address.map(_.alignment).min
  require (minAlignment >= maxTransfer)
}

case class TLManagerPortParameters(
  managers:   Seq[TLManagerParameters],
  beatBytes:  Int,
  minLatency: Int = 0)
{
  require (!managers.isEmpty)
  require (isPow2(beatBytes))
  require (minLatency >= 0)

  // Require disjoint ranges for Ids and addresses
  managers.combinations(2).foreach({ case Seq(x,y) =>
    require (!x.sinkId.overlaps(y.sinkId))
    x.address.foreach({ a => y.address.foreach({ b =>
      require (!a.overlaps(b))
    })})
  })

  // Bounds on required sizes
  def endSinkId   = managers.map(_.sinkId.end).max
  def maxAddress  = managers.map(_.maxAddress).max
  def maxTransfer = managers.map(_.maxTransfer).max
  
  // Operation sizes supported by all outward Managers
  val allSupportAcquire    = managers.map(_.supportsAcquire)   .reduce(_ intersect _)
  val allSupportArithmetic = managers.map(_.supportsArithmetic).reduce(_ intersect _)
  val allSupportLogical    = managers.map(_.supportsLogical)   .reduce(_ intersect _)
  val allSupportGet        = managers.map(_.supportsGet)       .reduce(_ intersect _)
  val allSupportPutFull    = managers.map(_.supportsPutFull)   .reduce(_ intersect _)
  val allSupportPutPartial = managers.map(_.supportsPutPartial).reduce(_ intersect _)
  val allSupportHint       = managers.map(_.supportsHint)      .reduce(_ intersect _)

  // Operation supported by at least one outward Managers
  val anySupportAcquire    = managers.map(!_.supportsAcquire.none)   .reduce(_ || _)
  val anySupportArithmetic = managers.map(!_.supportsArithmetic.none).reduce(_ || _)
  val anySupportLogical    = managers.map(!_.supportsLogical.none)   .reduce(_ || _)
  val anySupportGet        = managers.map(!_.supportsGet.none)       .reduce(_ || _)
  val anySupportPutFull    = managers.map(!_.supportsPutFull.none)   .reduce(_ || _)
  val anySupportPutPartial = managers.map(!_.supportsPutPartial.none).reduce(_ || _)
  val anySupportHint       = managers.map(!_.supportsHint.none)      .reduce(_ || _)

  // Which bits suffice to distinguish between all managers
  lazy val routingMask = AddressDecoder(managers.map(_.address))

  // These return Option[TLManagerParameters] for your convenience
  def find(address: BigInt) = managers.find(_.address.exists(_.contains(address)))
  def findById(id: Int) = managers.find(_.sinkId.contains(id))

  // Synthesizable lookup methods
  def findById(id: UInt) = Vec(managers.map(_.sinkId.contains(id)))
  def findIdStartSafe(address: UInt) = Mux1H(findSafe(address), managers.map(m => UInt(m.sinkId.start)))
  def findIdStartFast(address: UInt) = Mux1H(findFast(address), managers.map(m => UInt(m.sinkId.start)))
  def findIdEndSafe(address: UInt) = Mux1H(findSafe(address), managers.map(m => UInt(m.sinkId.end)))
  def findIdEndFast(address: UInt) = Mux1H(findFast(address), managers.map(m => UInt(m.sinkId.end)))

  // The safe version will check the entire address
  def findSafe(address: UInt) = Vec(managers.map(_.address.map(_.contains(address)).reduce(_ || _)))
  // The fast version assumes the address is valid
  def findFast(address: UInt) = Vec(managers.map(_.address.map(_.widen(~routingMask)).distinct.map(_.contains(address)).reduce(_ || _)))

  // Note: returns the actual fifoId + 1 or 0 if None
  def findFifoIdSafe(address: UInt) = Mux1H(findSafe(address), managers.map(m => UInt(m.fifoId.map(_+1).getOrElse(0))))
  def findFifoIdFast(address: UInt) = Mux1H(findFast(address), managers.map(m => UInt(m.fifoId.map(_+1).getOrElse(0))))
  def hasFifoIdSafe(address: UInt) = Mux1H(findSafe(address), managers.map(m => Bool(m.fifoId.isDefined)))
  def hasFifoIdFast(address: UInt) = Mux1H(findFast(address), managers.map(m => Bool(m.fifoId.isDefined)))

  // Does this Port manage this ID/address?
  def containsSafe(address: UInt) = findSafe(address).reduce(_ || _)
  // containsFast would be useless; it could always be true
  def containsById(id: UInt) = findById(id).reduce(_ || _)

  private def safety_helper(member: TLManagerParameters => TransferSizes, select: UInt => Vec[Bool])(address: UInt, lgSize: UInt) = {
    val allSame = managers.map(member(_) == member(managers(0))).reduce(_ && _)
    if (allSame) member(managers(0)).containsLg(lgSize) else {
      Mux1H(select(address), managers.map(member(_).containsLg(lgSize)))
    }
  }

  // Check for support of a given operation at a specific address
  val supportsAcquireSafe    = safety_helper(_.supportsAcquire,    findSafe) _
  val supportsArithmeticSafe = safety_helper(_.supportsArithmetic, findSafe) _
  val supportsLogicalSafe    = safety_helper(_.supportsLogical,    findSafe) _
  val supportsGetSafe        = safety_helper(_.supportsGet,        findSafe) _
  val supportsPutFullSafe    = safety_helper(_.supportsPutFull,    findSafe) _
  val supportsPutPartialSafe = safety_helper(_.supportsPutPartial, findSafe) _
  val supportsHintSafe       = safety_helper(_.supportsHint,       findSafe) _

  val supportsAcquireFast    = safety_helper(_.supportsAcquire,    findFast) _
  val supportsArithmeticFast = safety_helper(_.supportsArithmetic, findFast) _
  val supportsLogicalFast    = safety_helper(_.supportsLogical,    findFast) _
  val supportsGetFast        = safety_helper(_.supportsGet,        findFast) _
  val supportsPutFullFast    = safety_helper(_.supportsPutFull,    findFast) _
  val supportsPutPartialFast = safety_helper(_.supportsPutPartial, findFast) _
  val supportsHintFast       = safety_helper(_.supportsHint,       findFast) _
}

case class TLClientParameters(
  sourceId:            IdRange       = IdRange(0,1),
  nodePath:            Seq[BaseNode] = Seq(),
  // Supports both Probe+Grant of these sizes
  supportsProbe:       TransferSizes = TransferSizes.none,
  supportsArithmetic:  TransferSizes = TransferSizes.none,
  supportsLogical:     TransferSizes = TransferSizes.none,
  supportsGet:         TransferSizes = TransferSizes.none,
  supportsPutFull:     TransferSizes = TransferSizes.none,
  supportsPutPartial:  TransferSizes = TransferSizes.none,
  supportsHint:        TransferSizes = TransferSizes.none)
{
  require (supportsPutFull.contains(supportsPutPartial))
  // We only support these operations if we support Probe (ie: we're a cache)
  require (supportsProbe.contains(supportsArithmetic))
  require (supportsProbe.contains(supportsLogical))
  require (supportsProbe.contains(supportsGet))
  require (supportsProbe.contains(supportsPutFull))
  require (supportsProbe.contains(supportsPutPartial))
  require (supportsProbe.contains(supportsHint))

  val maxTransfer = List(
    supportsProbe.max,
    supportsArithmetic.max,
    supportsLogical.max,
    supportsGet.max,
    supportsPutFull.max,
    supportsPutPartial.max).max

  val name = nodePath.lastOption.map(_.lazyModule.name).getOrElse("disconnected")
}

case class TLClientPortParameters(
  clients:       Seq[TLClientParameters],
  unsafeAtomics: Boolean = false,
  minLatency:    Int = 0) // Atomics are executed as get+put
{
  require (!clients.isEmpty)
  require (minLatency >= 0)

  // Require disjoint ranges for Ids
  clients.combinations(2).foreach({ case Seq(x,y) =>
    require (!x.sourceId.overlaps(y.sourceId))
  })

  // Bounds on required sizes
  def endSourceId = clients.map(_.sourceId.end).max
  def maxTransfer = clients.map(_.maxTransfer).max

  // Operation sizes supported by all inward Clients
  val allSupportProbe      = clients.map(_.supportsProbe)     .reduce(_ intersect _)
  val allSupportArithmetic = clients.map(_.supportsArithmetic).reduce(_ intersect _)
  val allSupportLogical    = clients.map(_.supportsLogical)   .reduce(_ intersect _)
  val allSupportGet        = clients.map(_.supportsGet)       .reduce(_ intersect _)
  val allSupportPutFull    = clients.map(_.supportsPutFull)   .reduce(_ intersect _)
  val allSupportPutPartial = clients.map(_.supportsPutPartial).reduce(_ intersect _)
  val allSupportHint       = clients.map(_.supportsHint)      .reduce(_ intersect _)

  // Operation is supported by at least one client
  val anySupportProbe      = clients.map(!_.supportsProbe.none)     .reduce(_ || _)
  val anySupportArithmetic = clients.map(!_.supportsArithmetic.none).reduce(_ || _)
  val anySupportLogical    = clients.map(!_.supportsLogical.none)   .reduce(_ || _)
  val anySupportGet        = clients.map(!_.supportsGet.none)       .reduce(_ || _)
  val anySupportPutFull    = clients.map(!_.supportsPutFull.none)   .reduce(_ || _)
  val anySupportPutPartial = clients.map(!_.supportsPutPartial.none).reduce(_ || _)
  val anySupportHint       = clients.map(!_.supportsHint.none)      .reduce(_ || _)

  // These return Option[TLClientParameters] for your convenience
  def find(id: Int) = clients.find(_.sourceId.contains(id))

  // Synthesizable lookup methods
  def find(id: UInt) = Vec(clients.map(_.sourceId.contains(id)))
  def contains(id: UInt) = find(id).reduce(_ || _)

  private def safety_helper(member: TLClientParameters => TransferSizes)(id: UInt, lgSize: UInt) = {
    val allSame = clients.map(member(_) == member(clients(0))).reduce(_ && _)
    if (allSame) member(clients(0)).containsLg(lgSize) else {
      Mux1H(find(id), clients.map(member(_).containsLg(lgSize)))
    }
  }

  // Check for support of a given operation at a specific id
  val supportsProbe      = safety_helper(_.supportsProbe)      _
  val supportsArithmetic = safety_helper(_.supportsArithmetic) _
  val supportsLogical    = safety_helper(_.supportsLogical)    _
  val supportsGet        = safety_helper(_.supportsGet)        _
  val supportsPutFull    = safety_helper(_.supportsPutFull)    _
  val supportsPutPartial = safety_helper(_.supportsPutPartial) _
  val supportsHint       = safety_helper(_.supportsHint)       _
}

case class TLBundleParameters(
  addrHiBits: Int,
  dataBits:   Int,
  sourceBits: Int,
  sinkBits:   Int,
  sizeBits:   Int)
{
  // Chisel has issues with 0-width wires
  require (addrHiBits  >= 1)
  require (dataBits    >= 8)
  require (sourceBits  >= 1)
  require (sinkBits    >= 1)
  require (sizeBits    >= 1)
  require (isPow2(dataBits))

  val addrLoBits = log2Up(dataBits/8)
  val addressBits = addrHiBits + log2Ceil(dataBits/8)

  def union(x: TLBundleParameters) =
    TLBundleParameters(
      max(addrHiBits,  x.addrHiBits),
      max(dataBits,    x.dataBits),
      max(sourceBits,  x.sourceBits),
      max(sinkBits,    x.sinkBits),
      max(sizeBits,    x.sizeBits))
}

object TLBundleParameters
{
  def apply(client: TLClientPortParameters, manager: TLManagerPortParameters) =
    new TLBundleParameters(
      addrHiBits  = log2Up(manager.maxAddress + 1) - log2Ceil(manager.beatBytes),
      dataBits    = manager.beatBytes * 8,
      sourceBits  = log2Up(client.endSourceId),
      sinkBits    = log2Up(manager.endSinkId),
      sizeBits    = log2Up(log2Ceil(max(client.maxTransfer, manager.maxTransfer))+1))
}

case class TLEdgeParameters(
  client:  TLClientPortParameters,
  manager: TLManagerPortParameters)
{
  val maxTransfer = max(client.maxTransfer, manager.maxTransfer)
  val maxLgSize = log2Ceil(maxTransfer)

  // Sanity check the link...
  require (maxTransfer >= manager.beatBytes)

  val bundle = TLBundleParameters(client, manager)
}

case class TLAsyncManagerPortParameters(depth: Int, base: TLManagerPortParameters) { require (isPow2(depth)) }
case class TLAsyncClientPortParameters(base: TLClientPortParameters)
case class TLAsyncBundleParameters(depth: Int, base: TLBundleParameters) { require (isPow2(depth)) }
case class TLAsyncEdgeParameters(client: TLAsyncClientPortParameters, manager: TLAsyncManagerPortParameters)
{
  val bundle = TLAsyncBundleParameters(manager.depth, TLBundleParameters(client.base, manager.base))
}
