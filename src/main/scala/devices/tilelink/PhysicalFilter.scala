// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

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
      RegField(params.pageBits-2),
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

case class PMPInitialValue(address: BigInt = 0, l: Boolean = false, a: Boolean = false, r: Boolean = false, w: Boolean = false)

object DevicePMP
{
  def apply(addressBits: Int, pageBits: Int, initial: Option[PMPInitialValue] = None) = {
    val out = Wire(new DevicePMP(DevicePMPParams(addressBits, pageBits)))

    initial.foreach { i =>
      require ((i.address >> addressBits) == 0)
      require ((i.address >> pageBits) << pageBits == i.address)
      out.addr_hi := UInt(i.address >> pageBits)
    }

    def get(f: PMPInitialValue => Boolean) = initial.map(x => Bool(f(x)).asUInt).getOrElse(UInt(0))
    out.l := get(_.l)
    out.a := get(_.a)
    out.r := get(_.r)
    out.w := get(_.w)
    out
  }
}

/** PhysicalFilter uses a set of DevicePMP registers to control whether
  * accesses of certain types are allowed to proceed or denied. The Filter
  * will only prevent acquisition of NEW permissions; it will not shoot
  * down permissions Acquired previously.
  */


case class PhysicalFilterParams(
  controlAddress:   BigInt,
  controlBeatBytes: Int,
  pmpRegisters:     Seq[PMPInitialValue] = Seq.fill(4) { PMPInitialValue() } )
{
  val page = 4096
  val pageBits = log2Ceil(page)
  val size = (((pmpRegisters.size * 8) + page - 1) / page) * page

  require (!pmpRegisters.isEmpty)
  require (controlAddress > 0)
  require (controlAddress % size == 0)
  require (controlBeatBytes > 0 && isPow2(controlBeatBytes))
}

class PhysicalFilter(params: PhysicalFilterParams)(implicit p: Parameters) extends LazyModule
{
  val node = TLAdapterNode(managerFn = { mp => mp.copy(
    managers      = mp.managers.map(_.copy(alwaysGrantsT = false)),
    endSinkId     = if (mp.endSinkId == 0) { 0 } else { mp.endSinkId+1 },
    minLatency    = 1 min mp.minLatency)})

  val device = new SimpleDevice("physical-filter", Seq("sifive,physical-filter-v0"))
  val controlNode = TLRegisterNode(
    address   = Seq(AddressSet(params.controlAddress, params.size-1)),
    device    = device,
    beatBytes = params.controlBeatBytes)

  lazy val module = new LazyModuleImp(this) {
    // We need to be able to represent +1 larger than the largest populated address
    val addressBits = log2Ceil(node.edges.out.map(_.manager.maxAddress).max+1+1)
    val pmps = RegInit(Vec(params.pmpRegisters.map { ival => DevicePMP(addressBits, params.pageBits, Some(ival)) }))
    val blocks = pmps.tail.map(_.blockPriorAddress) :+ Bool(false)
    controlNode.regmap(0 -> (pmps zip blocks).map { case (p, b) => p.fields(b) }.toList.flatten)

    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in

      // Generally useful wires needed below
      val mySinkId = UInt(edgeOut.manager.endSinkId)
      val a_first = edgeIn.first(in.a)
      val (d_first, d_last, _) = edgeIn.firstlast(in.d)

      // Determine if a request is allowed
      val needW = in.a.bits.opcode =/= TLMessages.Get &&
                  (in.a.bits.opcode =/= TLMessages.AcquireBlock ||
                   in.a.bits.param  =/= TLPermissions.NtoB ||
                   Bool(!edgeIn.manager.anySupportAcquireB))
      val needR = in.a.bits.opcode =/= TLMessages.PutFullData &&
                  in.a.bits.opcode =/= TLMessages.PutPartialData
      val lt = Bool(false) +: pmps.map(in.a.bits.address < _.address)
      val sel = (pmps.map(_.a) zip (lt.init zip lt.tail)) map { case (a, (l, r)) => a(0) && !l && r }
      val ok = pmps.map(p => (p.r(0) || !needR) && (p.w(0) || !needW))
      val allowFirst = PriorityMux(sel :+ Bool(true), ok :+ Bool(false)) // no match => deny
      val allow = allowFirst holdUnless a_first // don't change our mind mid-transaction

      // Track the progress of transactions from A => D
      val d_rack  = Bool(edgeIn.manager.anySupportAcquireB) && in.d.bits.opcode === TLMessages.ReleaseAck
      val flight = RegInit(UInt(0, width = log2Ceil(edgeIn.client.endSourceId+1+1))) // +1 for inclusive range +1 for a_first vs. d_last
      val denyWait = RegInit(Bool(false)) // deny already inflight?
      flight := flight + (a_first && in.a.fire()) - (d_last && !d_rack && in.d.fire())

      // Discard denied A traffic, but first block it until there is nothing in-flight
      val deny_ready = !denyWait && flight === UInt(0)
      in.a.ready := Mux(allow, out.a.ready, !a_first || deny_ready)
      out.a.valid := in.a.valid && allow

      // Frame an appropriate deny message
      val denyValid = RegInit(Bool(false))
      val deny = Reg(in.d.bits)
      val d_opcode = TLMessages.adResponse(in.a.bits.opcode)
      val d_grant = Bool(edgeIn.manager.anySupportAcquireB) && deny.opcode === TLMessages.Grant
      when (in.a.valid && !allow && deny_ready && a_first) {
        denyValid    := Bool(true)
        denyWait     := Bool(true)
        deny.opcode  := d_opcode
        deny.param   := UInt(0) // toT, but error grants must be handled transiently (ie: you don't keep permissions)
        deny.size    := in.a.bits.size
        deny.source  := in.a.bits.source
        deny.sink    := mySinkId
        deny.denied  := Bool(true)
        deny.data    := UInt(0)
        deny.corrupt := d_opcode(0)
      }
      when (denyValid && in.d.ready && d_last) {
        denyValid := Bool(false)
        when (!d_grant) {
          denyWait := Bool(false)
        }
      }

      val out_d = Wire(in.d.bits)
      out_d := out.d.bits

      // Deny can have unconditional priority, because the only out.d message possible is
      // ReleaseAck, because we waited for all A=>D traffic to complete. ReleaseAck is
      // single-beat, so it's safe to just arbitrate without counting the responses.
      in.d.valid := out.d.valid || denyValid
      out.d.ready := !denyValid && in.d.ready
      in.d.bits := Mux(denyValid, deny, out_d)

      // Track when a request is not allowed to be promoted toT
      if (edgeIn.manager.anySupportAcquireB) {
        val wSourceVec = Reg(Vec(edgeIn.client.endSourceId, Bool()))
        val aWOk = PriorityMux(sel, pmps.map(_.w(0)))
        val dWOk = wSourceVec(in.d.bits.source)
        val bypass = Bool(edgeIn.manager.minLatency == 0) && in.a.valid && in.a.bits.source === in.d.bits.source
        val d_grant = in.d.bits.opcode === TLMessages.Grant || in.d.bits.opcode === TLMessages.GrantData
        val dWHeld = Mux(bypass, aWOk, dWOk) holdUnless d_first

        when (d_grant && !dWHeld) {
          in.d.bits.param := TLPermissions.toB
        }

        when (in.a.fire() && a_first) {
          wSourceVec(in.a.bits.source) := aWOk
        }

        edgeIn.client.unusedSources.foreach { id =>
          wSourceVec(id) := Bool(true)
        }

        // Swallow GrantAcks
        val isMyId = mySinkId === in.e.bits.sink
        out.e.valid := in.e.valid && !isMyId
        in.e.ready := out.e.ready || isMyId

        when (in.e.fire() && isMyId) {
          denyWait := Bool(false)
        }
      }
    }
  }
}
