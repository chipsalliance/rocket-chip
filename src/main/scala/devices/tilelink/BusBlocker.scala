// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class BusBlockerParams(
  controlAddress:   BigInt,
  controlBeatBytes: Int,
  deviceBeatBytes:  Int,
  pmpRegisters:     Int)
{
  val page = 4096
  val pageBits = log2Ceil(page)
  val size = (((pmpRegisters * 8) + page - 1) / page) * page

  require (pmpRegisters > 0)
  require (controlAddress > 0)
  require (controlAddress % size == 0)
  require (controlBeatBytes > 0 && isPow2(controlBeatBytes))
  require (deviceBeatBytes  > 0 && isPow2(deviceBeatBytes))
}

case class DevicePMPParams(addressBits: Int, pageBits: Int)
class DevicePMP(params: DevicePMPParams) extends GenericParameterizedBundle(params)
{
  require (params.addressBits > params.pageBits)

  val l = UInt(width = 1) // locked
  val a = UInt(width = 1) // LSB of A (0=disabled, 1=TOR)
  val r = UInt(width = 1)
  val w = UInt(width = 1)

  val addr_hi = UInt(width = params.addressBits-params.pageBits)
  def address = Cat(addr_hi, UInt(0, width=params.pageBits))
  def blockPriorAddress = l(0) && a(0)

  def fields(blockAddress: Bool): Seq[RegField] = {
    def field(bits: Int, reg: UInt, lock: Bool = l(0)) =
      RegField(bits, RegReadFn(reg), RegWriteFn((wen, data) => {
        when (wen && !lock) { reg := data }
        Bool(true)
      }))
    Seq(
      RegField(10),
      field(params.addressBits-params.pageBits, addr_hi, l(0) || blockAddress),
      RegField(56 - (params.addressBits-2)),
      field(1, r),
      field(1, w),
      RegField(1), // x
      field(1, a),
      RegField(3), // a high + 2 reserved
      field(1, l))
  }
}

object DevicePMP
{
  def apply(addressBits: Int, pageBits: Int) = {
    val out = Wire(new DevicePMP(DevicePMPParams(addressBits, pageBits)))
    out.l := UInt(0)
    out.a := UInt(0)
    out.r := UInt(0)
    out.w := UInt(0)
    out
  }
}

class BusBlocker(params: BusBlockerParams)(implicit p: Parameters) extends TLBusBypassBase(params.deviceBeatBytes)
{
  val device = new SimpleDevice("bus-blocker", Seq("sifive,bus-blocker0"))

  val controlNode = TLRegisterNode(
    address   = Seq(AddressSet(params.controlAddress, params.size-1)),
    device    = device,
    beatBytes = params.controlBeatBytes)

  lazy val module = new LazyModuleImp(this) {
    // We need to be able to represent +1 larger than the largest populated address
    val addressBits = log2Ceil(nodeOut.edges.out(0).manager.maxAddress+1+1)
    val pmps = RegInit(Vec.fill(params.pmpRegisters) { DevicePMP(addressBits, params.pageBits) })
    val blocks = pmps.tail.map(_.blockPriorAddress) :+ Bool(false)
    controlNode.regmap(0 -> (pmps zip blocks).map { case (p, b) => p.fields(b) }.toList.flatten)

    val (in, edge) = nodeIn.in(0)

    // Determine if a request is allowed
    val needW = in.a.bits.opcode =/= TLMessages.Get
    val needR = in.a.bits.opcode =/= TLMessages.PutFullData && in.a.bits.opcode =/= TLMessages.PutPartialData
    val lt = Bool(false) +: pmps.map(in.a.bits.address < _.address)
    val sel = (pmps.map(_.a) zip (lt.init zip lt.tail)) map { case (a, (l, r)) => a(0) && !l && r }
    val ok = pmps.map(p => (p.r(0) || !needR) && (p.w(0) || !needW))
    val allow = PriorityMux(sel :+ Bool(true), ok :+ Bool(false)) // no match => deny

    bar.module.io.bypass := !allow
  }
}
