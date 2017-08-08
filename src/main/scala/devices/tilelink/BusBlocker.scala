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
  val size = (((pmpRegisters * 8) + page - 1) / page) * page

  require (pmpRegisters > 0)
  require (controlAddress > 0)
  require (controlAddress % size == 0)
  require (controlBeatBytes > 0 && isPow2(controlBeatBytes))
  require (deviceBeatBytes  > 0 && isPow2(deviceBeatBytes))
}

case class DevicePMPParams(addressBits: Int)
class DevicePMP(params: DevicePMPParams) extends GenericParameterizedBundle(params)
{
  require (params.addressBits > 12)

  val l = UInt(width = 1) // locked
  val a = UInt(width = 1) // LSB of A (0=disabled, 1=TOR)
  val r = UInt(width = 1)
  val w = UInt(width = 1)

  val addr_hi = UInt(width = params.addressBits-12)
  def address = Cat(addr_hi, UInt(0, width=12))

  def fields(locked: Bool): Seq[RegField] = {
    def field(bits: Int, reg: UInt) =
      RegField(bits, RegReadFn(reg), RegWriteFn((wen, data) => {
        when (wen && !locked) { reg := data }
        Bool(true)
      }))
    Seq(
      RegField(10),
      field(params.addressBits-12, addr_hi),
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
  def apply(addressBits: Int) = {
    val out = Wire(new DevicePMP(DevicePMPParams(addressBits)))
    out.l := UInt(0)
    out.a := UInt(1) // TOR
    out.r := UInt(0)
    out.w := UInt(0)
    out.addr_hi := ~UInt(0, width=addressBits-12)
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
    val io = new Bundle {
      val ctl = controlNode.bundleIn
      val in = nodeIn.bundleIn
      val out = nodeOut.bundleOut
    }

    // We need to be able to represent +1 larger than the largest populated address
    val addressBits = log2Ceil(nodeOut.edgesOut(0).manager.maxAddress+1+1)
    val pmps = RegInit(Vec.fill(params.pmpRegisters) { DevicePMP(addressBits) })
    val locks = (pmps.map(_.l) zip (UInt(0) +: pmps.map(_.l))) map { case (x, n) => x | n }
    controlNode.regmap(0 -> (pmps zip locks).map { case (p, l) => p.fields(l(0)) }.toList.flatten)

    val in = io.in(0)
    val edge = nodeIn.edgesIn(0)

    // Determine if a request is allowed
    val needW = in.a.bits.opcode =/= TLMessages.Get
    val needR = in.a.bits.opcode =/= TLMessages.PutFullData && in.a.bits.opcode =/= TLMessages.PutPartialData
    val lte = Bool(false) +: pmps.map(in.a.bits.address < _.address)
    val sel = (pmps.map(_.a) zip (lte.init zip lte.tail)) map { case (a, (l, r)) => a(0) && !l && r }
    val ok = pmps.map(p => (p.r(0) || !needR) && (p.w(0) || !needW))
    val allow = PriorityMux(sel :+ Bool(true), ok :+ Bool(true)) // no match => allow

    bar.module.io.bypass := !allow
  }
}
