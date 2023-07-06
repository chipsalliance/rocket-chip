// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class DevicePMPParams(addressBits: Int, pageBits: Int)

/** Defines the fields of the Device PMP registers */
class DevicePMP(val params: DevicePMPParams) extends Bundle
{
  require (params.addressBits > params.pageBits)

  val l = UInt(1.W) // locked
  val a = UInt(1.W) // LSB of A (0=disabled, 1=TOR)
  val r = UInt(1.W)
  val w = UInt(1.W)

  val addr_hi = UInt((params.addressBits-params.pageBits).W)
  def address = Cat(addr_hi, 0.U(params.pageBits.W))
  def blockPriorAddress = l(0) && a(0)

  def fields(blockAddress: Bool, initial: PMPInitialValue): Seq[RegField] = {
    val initialInts = DevicePMP.getInitialValueInts(params.addressBits, params.pageBits, Some(initial))

    val lDesc = RegFieldDesc("l",
      "Lock bit. When set, prevents modification to other fields in the register. Cannot be modified if l bit is set.",
      reset = Some(initialInts.l),
      wrType = Some(RegFieldWrType.MODIFY))

    val aDesc = RegFieldDesc("a", "Address match mode. When clear, this PMP does not match any address. When set, Top-of-Range (TOR) matching is applied by this PMP. Cannot be modified if lock bit is set.",
      reset = Some(initialInts.a),
      wrType = Some(RegFieldWrType.MODIFY))

    val rDesc = RegFieldDesc("r", "Read bit. When set grants read access to the matching address range. Cannot be modified if lock bit is set.",
      reset = Some(initialInts.r),
      wrType = Some(RegFieldWrType.MODIFY))

    val wDesc = RegFieldDesc("w", "Write bit. When set grants write access to the matching address range. Cannot be modified if lock bit is set.",
      reset = Some(initialInts.w),
      wrType = Some(RegFieldWrType.MODIFY))

    val addrHiDesc = RegFieldDesc("addr_hi", "Page address. Specifies top-of-range page address for this PMP and bottom-of-range address for following PMP. Cannot be modified if lock bit is set, or if address match mode is TOR and lock bit is set on the subsequent PMP.",
      reset = Some(initialInts.addr_hi),
      wrType = Some(RegFieldWrType.MODIFY))

    def field(bits: Int, reg: UInt, desc: RegFieldDesc, lock: Bool = l(0)) =
      RegField(bits, RegReadFn(reg), RegWriteFn((wen, data) => {
        when (wen && !lock) { reg := data }
        true.B
      }), Some(desc))

    Seq(
      RegField(params.pageBits-2),
      field(params.addressBits-params.pageBits, addr_hi, addrHiDesc,
        l(0) || blockAddress),
      RegField(56 - (params.addressBits-2)),
      field(1, r, rDesc),
      field(1, w, wDesc),
      RegField(1), // x
      field(1, a, aDesc),
      RegField(3), // a high + 2 reserved
      field(1, l, lDesc))
  }
}


/* Initial value of the PMP registers
 *  
 *  @param address - Initial value of the address field. Will be shifted down by pageBits before being stored in the PMP register addr_hi register.
 *  @param l Initial value of the l (locked) bit. True means no modifications of other fields are allowed.
 *  @param a Initial value of the a (address match mode) bit. False means disabled, True means use TOR address matching. Also disables modification of previous PMP address.
 *  @param r Initial value of the r (read access match) bit. True means allow read accesses.
 *  @param w Initial value of the w (write access match) bit. True means allow write accesses.
 *  
 */
case class PMPInitialValue(address: BigInt = 0, l: Boolean = false, a: Boolean = false, r: Boolean = false, w: Boolean = false)
case class PMPInitialValueInt(addr_hi: BigInt = 0, l: Int, a: Int, r: Int, w: Int)

object DevicePMP
{
  /** Create DevicePMP bundle with appropriate initial value intended for use in RegInit
    *  
    *  @param addressBits width of addresses
    *  @param pageBits number of address bits per page
    *  @param initial optional initial value. 0 is used for all fields if not specified.
    *  
    */

  def apply(addressBits: Int, pageBits: Int, initial: Option[PMPInitialValue] = None): DevicePMP = {

    val out = Wire(new DevicePMP(DevicePMPParams(addressBits, pageBits)))
    val outInts = getInitialValueInts(addressBits, pageBits, initial)

    out.addr_hi := outInts.addr_hi.U
    out.l := outInts.l.U
    out.a := outInts.a.U
    out.r := outInts.r.U
    out.w := outInts.w.U

    out
  }

  /** Helper to convert from Booleans in the optional PMPInitialValue to Int/BigInt values needed to create the UInts in a DevicePMP Bundle */
  def getInitialValueInts(addressBits: Int, pageBits: Int, initial: Option[PMPInitialValue]): PMPInitialValueInt = {

    val addr_hi = initial.map { i =>
      require ((i.address >> addressBits) == 0,
        s"Device PMP Initial value address must be 0 for bits past ${addressBits}, not ${i.address}")
      require ((i.address >> pageBits) << pageBits == i.address,
        s"Device PMP Initial value address must be 0 for bits less than ${pageBits}, not ${i.address}")
      i.address >> pageBits
    }.getOrElse (BigInt(0))

    // Convert from optional Boolean to Int with a default of 0
    def get(f: PMPInitialValue => Boolean): Int = initial.map { i => if (f(i)) 1 else 0}.getOrElse(0)
    PMPInitialValueInt(addr_hi = addr_hi, l = get(_.l), a = get(_.a), r = get(_.r), w = get(_.w))
  }
}

case class PhysicalFilterParams(
  controlAddress:   BigInt,
  controlBeatBytes: Int,
  pmpRegisters:     Seq[PMPInitialValue] = Seq.fill(4) { PMPInitialValue() } )
{
  val page = 4096
  val pageBits = log2Ceil(page)
  val size = (((pmpRegisters.size * 8) + page - 1) / page) * page

  require (!pmpRegisters.isEmpty,
    "Must specify at least one Device PMP register")
  require (controlAddress > 0,
    s"Must specify a positive, non-zero control address for PhysicalFilter, not ${controlAddress}")
  require (controlAddress % size == 0,
    s"PhysicalFilter Control address must be aligned to its size, not size ${size} at ${controlAddress}")
  require (controlBeatBytes > 0 && isPow2(controlBeatBytes),
    s"PhyiscalFilter control beat bytes must be a positive power of two, not ${controlBeatBytes}")
}

/** The PhyisicalFilter provides physical memory protection for Tile Link bus traffic, granting or denying accesses based on address and access type.
  *  
  *  The PhysicalFilter uses a set of DevicePMP registers to control whether
  *  accesses of certain types are allowed or denied.
  *  For transactions in flight, the PhysicalFilter will only prevent acquisition of NEW permissions;
  *  it will not shoot down permissions acquired previously.
  *  
  *  The blocking behavior is controlled by a series of PMP registers which are accessible via memory mapped reads and writes.
  *  The list is a priority allow list. If no PMP matches the transaction will be denied. Otherwise the first PMP which is active
  *  and address matches is compared against the requested read and/or write permissions. 
  *  When an access is denied the Phyiscal Filter crafts and responds with a Tile Link Denied response message.
  *  
  *  When a device PMP register's `a` bit is set, it is enabled and Top of Range (TOR) matching is applied.
  *  For a given PMP register, the associated address register forms the top of the
  *  address range, and the preceding PMP address register forms the bottom of the
  *  address range.  If PMP[i]'s `a` field is set to TOR, the entry matches
  *  any address `y` such that `PMP[i-1].address <= y < PMP[i].address` .
  *  If PMP[0].a is set (TOR is applied), zero is used for the lower bound, and so
  *  it matches any address `y < PMP[0].address`.
  *  Note that the addresses bits stored in the PMP registers are addr_hi, or page address.
  * 
  *  Accesses which need write access are anything but Get, AcquireBlock, or NtoB.
  *  Accesses which need read access are anything but PutFull data or PutPartial Data.
  * 
  *  Setting a PMP's r or w bit set grants read or write access respectively.
  *
  *  PMP registers are protected by a lock bit. Once lock bit is set the PMP register can no
  *  longer be modified. In addition, if the following PMP access is set to TOR, the PMP's address
  *  cannot be modified, even if its own lock bit is not set.
  * 
  *  @param params sets the base address and width of the control registers, the number of PMP registers and their initial values.
  */

class PhysicalFilter(params: PhysicalFilterParams)(implicit p: Parameters) extends LazyModule
{
  val node = TLAdapterNode(managerFn = { mp => mp.v1copy(
    managers      = mp.managers.map(_.v1copy(alwaysGrantsT = false)),
    endSinkId     = if (mp.endSinkId == 0) { 0 } else { mp.endSinkId+1 },
    minLatency    = 1 min mp.minLatency)})

  val device = new SimpleDevice("physical-filter", Seq("sifive,physical-filter-v0"))
  val controlNode = TLRegisterNode(
    address   = Seq(AddressSet(params.controlAddress, params.size-1)),
    device    = device,
    beatBytes = params.controlBeatBytes)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    // We need to be able to represent +1 larger than the largest populated address
    val addressBits = log2Ceil(node.edges.out.map(_.manager.maxAddress).max+1+1)
    val pmps = RegInit(VecInit(params.pmpRegisters.map { ival => DevicePMP(addressBits, params.pageBits, Some(ival)) }))
    val blocks = pmps.tail.map(_.blockPriorAddress) :+ false.B
    controlNode.regmap(0 -> ((pmps zip blocks) zip params.pmpRegisters).zipWithIndex.map{ case (((p, b), init), i) =>
      RegFieldGroup(s"devicepmp${i}", Some(s"Physical Filter Device PMP Register ${i}"), p.fields(b, init))
    }.toList.flatten)

    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in

      // Generally useful wires needed below
      val mySinkId = edgeOut.manager.endSinkId.U
      val a_first = edgeIn.first(in.a)
      val (d_first, d_last, _) = edgeIn.firstlast(in.d)

      // Determine if a request is allowed
      val needW = in.a.bits.opcode =/= TLMessages.Get &&
                  (in.a.bits.opcode =/= TLMessages.AcquireBlock ||
                   in.a.bits.param  =/= TLPermissions.NtoB ||
                   (!edgeIn.manager.anySupportAcquireB).B)
      val needR = in.a.bits.opcode =/= TLMessages.PutFullData &&
                  in.a.bits.opcode =/= TLMessages.PutPartialData
      val lt = false.B +: pmps.map(in.a.bits.address < _.address)
      // sel[i] is true if PMP[i].a is set and PMP[i-1].address <= address < PMP[i].address
      val sel = (pmps.map(_.a) zip (lt.init zip lt.tail)) map { case (a, (l, r)) => a(0) && !l && r }
      val ok = pmps.map(p => (p.r(0) || !needR) && (p.w(0) || !needW))
      // If PMP[i] matches the address and is active, apply PMP[i].r/w permissions.
      val allowFirst = PriorityMux(sel :+ true.B, ok :+ false.B) // deny if no match
      val allow = allowFirst holdUnless a_first // don't change our mind mid-transaction

      // Track the progress of transactions from A => D
      val d_rack  = edgeIn.manager.anySupportAcquireB.B && in.d.bits.opcode === TLMessages.ReleaseAck
      val flight = RegInit(0.U(log2Ceil(edgeIn.client.endSourceId+1+1).W)) // +1 for inclusive range +1 for a_first vs. d_last
      val denyWait = RegInit(false.B) // deny already inflight?
      flight := flight + (a_first && in.a.fire) - (d_last && !d_rack && in.d.fire)

      // Discard denied A traffic, but first block it until there is nothing in-flight
      val deny_ready = !denyWait && flight === 0.U
      in.a.ready := Mux(allow, out.a.ready, !a_first || deny_ready)
      out.a.valid := in.a.valid && allow

      // Frame an appropriate deny message
      val denyValid = RegInit(false.B)
      val deny = Reg(in.d.bits)
      val d_opcode = TLMessages.adResponse(in.a.bits.opcode)
      val d_grant = edgeIn.manager.anySupportAcquireB.B && deny.opcode === TLMessages.Grant
      when (in.a.valid && !allow && deny_ready && a_first) {
        denyValid    := true.B
        denyWait     := true.B
        deny.opcode  := d_opcode
        deny.param   := 0.U // toT, but error grants must be handled transiently (ie: you don't keep permissions)
        deny.size    := in.a.bits.size
        deny.source  := in.a.bits.source
        deny.sink    := mySinkId
        deny.denied  := true.B
        deny.data    := 0.U
        deny.corrupt := d_opcode(0)
      }
      when (denyValid && in.d.ready && d_last) {
        denyValid := false.B
        when (!d_grant) {
          denyWait := false.B
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
        val bypass = (edgeIn.manager.minLatency == 0).B && in.a.valid && in.a.bits.source === in.d.bits.source
        val d_grant = in.d.bits.opcode === TLMessages.Grant || in.d.bits.opcode === TLMessages.GrantData
        val dWHeld = Mux(bypass, aWOk, dWOk) holdUnless d_first

        when (d_grant && !dWHeld) {
          in.d.bits.param := TLPermissions.toB
        }

        when (in.a.fire && a_first) {
          wSourceVec(in.a.bits.source) := aWOk
        }

        edgeIn.client.unusedSources.foreach { id =>
          wSourceVec(id) := true.B
        }

        // Swallow GrantAcks
        val isMyId = mySinkId === in.e.bits.sink
        out.e.valid := in.e.valid && !isMyId
        in.e.ready := out.e.ready || isMyId

        when (in.e.fire && isMyId) {
          denyWait := false.B
        }
      }
    }
  }
}
