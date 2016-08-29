// See LICENSE for license details.

package uncore.tilelink2

import Chisel._

class TLRegisterNode(address: AddressSet, beatBytes: Int = 4)
  extends TLManagerNode(beatBytes, TLManagerParameters(
    address            = Seq(address),
    supportsGet        = TransferSizes(1, beatBytes),
    supportsPutPartial = TransferSizes(1, beatBytes),
    supportsPutFull    = TransferSizes(1, beatBytes),
    fifoId             = Some(0))) // requests are handled in order
{
  require (!address.strided)

  // Calling this method causes the matching TL2 bundle to be
  // configured to route all requests to the listed RegFields.
  def regmap(mapping: RegField.Map*) = {
    val regmap = mapping.toList
    require (!regmap.isEmpty)

    // Flatten the regmap into (Reg:Int, Offset:Int, field:RegField)
    val flat = regmap.map { case (reg, fields) => 
      val offsets = fields.scanLeft(0)(_ + _.width).init
      (offsets zip fields) map { case (o, f) => (reg, o, f) }
    }.flatten

    // Confirm that no register is too big
    require (flat.map(_._2).max <= beatBytes*8)

    // All registers must fit inside the device address space
    val maxIndex = regmap.map(_._1).max
    require (address.mask >= maxIndex*beatBytes)

    // Which register is touched?
    val alignBits = log2Ceil(beatBytes)
    val addressBits = log2Up(maxIndex+1)
    val a = bundleIn(0).a // Must apply Queue !!! (so no change once started)
    val d = bundleIn(0).d
    val regIdx = a.bits.address(addressBits+alignBits-1, alignBits)
    val regSel = UIntToOH(regIdx)

    // What is the access?
    val opcode = a.bits.opcode
    val read  = a.valid && opcode === TLMessages.Get
    val write = a.valid && (opcode === TLMessages.PutFullData || opcode === TLMessages.PutPartialData)
    val wmaskWide = Vec.tabulate(beatBytes*8) { i => a.bits.wmask(i/8) } .toBits.asUInt
    val dataIn = a.bits.data & wmaskWide // zero undefined bits

    // The output values for each register
    val dataOutAcc = Array.tabulate(maxIndex+1) { _ => UInt(0) }
    // The ready state for read and write
    val rReadyAcc = Array.tabulate(maxIndex+1) { _ => Bool(true) }
    val wReadyAcc = Array.tabulate(maxIndex+1) { _ => Bool(true) }

    // Apply all the field methods
    flat.foreach { case (reg, low, field) =>
      val high = low + field.width - 1
      val rfire = wmaskWide(high, low).orR()
      val wfire = wmaskWide(high, low).andR()
      val sel = regSel(reg)
      val ren = read  && sel && rfire
      val wen = write && sel && wfire
      val (rReady, rResult) = field.read(ren)
      val wReady = field.write(wen, dataIn(high, low))
      dataOutAcc(reg) = dataOutAcc(reg) | (rResult << low)
      rReadyAcc(reg) = rReadyAcc(reg) && (!rfire || rReady)
      wReadyAcc(reg) = wReadyAcc(reg) && (!wfire || wReady)
    }

    // Create the output data signal
    val dataOut = Vec(dataOutAcc)(regIdx)
    val rReady = Vec(rReadyAcc)(regIdx)
    val wReady = Vec(wReadyAcc)(regIdx)

    val ready = (read && rReady) || (write && wReady)
    a.ready := ready && d.ready
    d.valid := a.valid && ready

    val edge = edgesIn(0)
    d.bits := edge.AccessAck(a.bits.source, a.bits.size)
    // avoid a Mux on the data bus by manually overriding two fields
    d.bits.data := dataOut
    d.bits.opcode := Mux(opcode === TLMessages.Get, TLMessages.AccessAck, TLMessages.AccessAckData)
  }
}

object TLRegisterNode
{
  def apply(address: AddressSet, beatBytes: Int = 4) = new TLRegisterNode(address, beatBytes)
}

// These convenience methods below combine to make it possible to create a TL2 
// register mapped device from a totally abstract register mapped device.
// See GPIO.scala in this directory for an example

abstract class TLRegFactory(address: AddressSet, beatBytes: Int) extends TLFactory
{
  val node = TLRegisterNode(address, beatBytes)
}

class TLRegBundle[P](val params: P, val tl_in: Vec[TLBundle]) extends Bundle

class TLRegModule[P, B <: Bundle](val params: P, bundleBuilder: => B, factory: TLRegFactory)
  extends TLModule(factory) with HasRegMap
{
  val io = bundleBuilder
  def regmap(mapping: RegField.Map*) = factory.node.regmap(mapping:_*)
}

class TLRegisterRouter[B <: Bundle, M <: TLModule]
   (address: Option[BigInt] = None, size: BigInt = 4096, beatBytes: Int = 4)
   (bundleBuilder: Vec[TLBundle] => B)
   (moduleBuilder: (=> B, TLRegFactory) => M)
  extends TLRegFactory(AddressSet(size-1, address), beatBytes)
{
  require (size % 4096 == 0) // devices should be 4K aligned
  require (isPow2(size))
  require (size >= 4096)

  lazy val module = Module(moduleBuilder(bundleBuilder(node.bundleIn), this))
}
