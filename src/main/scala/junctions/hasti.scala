// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package junctions

import Chisel._
import config._
import unittest.UnitTest
import util._

object HastiConstants
{
  // Values for htrans
  val SZ_HTRANS     = 2
  def HTRANS_IDLE   = UInt(0, SZ_HTRANS) // No transfer requested, not in a burst
  def HTRANS_BUSY   = UInt(1, SZ_HTRANS) // No transfer requested, in a burst
  def HTRANS_NONSEQ = UInt(2, SZ_HTRANS) // First (potentially only) request in a burst
  def HTRANS_SEQ    = UInt(3, SZ_HTRANS) // Following requests in a burst

  // Values for hburst
  val SZ_HBURST     = 3
  def HBURST_SINGLE = UInt(0, SZ_HBURST) // Single access (no burst)
  def HBURST_INCR   = UInt(1, SZ_HBURST) // Incrementing burst of arbitrary length, not crossing 1KB
  def HBURST_WRAP4  = UInt(2, SZ_HBURST) // 4-beat wrapping burst
  def HBURST_INCR4  = UInt(3, SZ_HBURST) // 4-beat incrementing burst
  def HBURST_WRAP8  = UInt(4, SZ_HBURST) // 8-beat wrapping burst
  def HBURST_INCR8  = UInt(5, SZ_HBURST) // 8-beat incrementing burst
  def HBURST_WRAP16 = UInt(6, SZ_HBURST) // 16-beat wrapping burst
  def HBURST_INCR16 = UInt(7, SZ_HBURST) // 16-beat incrementing burst

  // Values for hresp
  val SZ_HRESP      = 1
  def HRESP_OKAY    = UInt(0, SZ_HRESP)
  def HRESP_ERROR   = UInt(1, SZ_HRESP)

  // Values for hsize are identical to TileLink MT_SZ
  // ie: 8*2^SZ_HSIZE bit transfers
  val SZ_HSIZE = 3
  
  // Values for hprot (a bitmask)
  val SZ_HPROT = 4
  def HPROT_DATA       = UInt("b0001") // Data access or Opcode fetch
  def HPROT_PRIVILEGED = UInt("b0010") // Privileged or User access
  def HPROT_BUFFERABLE = UInt("b0100") // Bufferable or non-bufferable
  def HPROT_CACHEABLE  = UInt("b1000") // Cacheable or non-cacheable

  def dgate(valid: Bool, b: UInt) = Fill(b.getWidth, valid) & b
}

import HastiConstants._

case class HastiParameters(dataBits: Int, addrBits: Int)
case object HastiId extends Field[String]
case class HastiKey(id: String) extends Field[HastiParameters]

trait HasHastiParameters {
  implicit val p: Parameters
  val hastiParams = p(HastiKey(p(HastiId)))
  val hastiAddrBits = hastiParams.addrBits
  val hastiDataBits = hastiParams.dataBits
  val hastiDataBytes = hastiDataBits/8
  val hastiAlignment = log2Ceil(hastiDataBytes)
}

abstract class HastiModule(implicit val p: Parameters) extends Module
  with HasHastiParameters
abstract class HastiBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasHastiParameters

class HastiMasterIO(implicit p: Parameters) extends HastiBundle()(p) {
  val htrans    = UInt(OUTPUT, SZ_HTRANS)
  val hmastlock = Bool(OUTPUT)
  val haddr     = UInt(OUTPUT, hastiAddrBits)
  val hwrite    = Bool(OUTPUT)
  val hburst    = UInt(OUTPUT, SZ_HBURST)
  val hsize     = UInt(OUTPUT, SZ_HSIZE)
  val hprot     = UInt(OUTPUT, SZ_HPROT)

  val hwdata = Bits(OUTPUT, hastiDataBits)
  val hrdata = Bits(INPUT,  hastiDataBits)

  val hready = Bool(INPUT)
  val hresp  = UInt(INPUT, SZ_HRESP)

  def isNSeq(dummy:Int=0) = htrans === HTRANS_NONSEQ // SEQ does not start a NEW request
  def isHold(dummy:Int=0) = htrans === HTRANS_BUSY || htrans === HTRANS_SEQ
  def isIdle(dummy:Int=0) = htrans === HTRANS_IDLE || htrans === HTRANS_BUSY
}

class HastiSlaveIO(implicit p: Parameters) extends HastiBundle()(p) {
  val htrans    = UInt(INPUT, SZ_HTRANS)
  val hmastlock = Bool(INPUT)
  val haddr     = UInt(INPUT, hastiAddrBits)
  val hwrite    = Bool(INPUT)
  val hburst    = UInt(INPUT, SZ_HBURST)
  val hsize     = UInt(INPUT, SZ_HSIZE)
  val hprot     = UInt(INPUT, SZ_HPROT)

  val hwdata = Bits(INPUT,  hastiDataBits)
  val hrdata = Bits(OUTPUT, hastiDataBits)

  val hsel   = Bool(INPUT)
  val hready = Bool(OUTPUT)
  val hresp  = UInt(OUTPUT, SZ_HRESP)
}

/* A diverted master is told hready when his address phase goes nowhere.
 * In this case, we buffer his address phase request and replay it later.
 * NOTE: this must optimize to nothing when divert is constantly false.
 */
class MasterDiversion(implicit p: Parameters) extends HastiModule()(p) {
  val io = new Bundle {
    val in     = (new HastiMasterIO).flip
    val out    = (new HastiMasterIO)
    val divert = Bool(INPUT)
  }
  
  val full   = Reg(init = Bool(false))
  val buffer = Reg(new HastiMasterIO)
  
  when (io.out.hready) {
    full := Bool(false)
  }
  when (io.divert) {
    full := Bool(true)
    buffer := io.in
  }
  
  // If the master is diverted, he must also have been told hready
  assert (!io.divert || io.in.hready,
    "Diverted but not ready");
  
  // Replay the request we diverted
  io.out.htrans    := Mux(full, buffer.htrans,    io.in.htrans)
  io.out.hmastlock := Mux(full, buffer.hmastlock, io.in.hmastlock)
  io.out.haddr     := Mux(full, buffer.haddr,     io.in.haddr)
  io.out.hwrite    := Mux(full, buffer.hwrite,    io.in.hwrite)
  io.out.hburst    := Mux(full, buffer.hburst,    io.in.hburst)
  io.out.hsize     := Mux(full, buffer.hsize,     io.in.hsize)
  io.out.hprot     := Mux(full, buffer.hprot,     io.in.hprot)
  io.out.hwdata    := Mux(full, buffer.hwdata,    io.in.hwdata)
  
  // Pass slave response back
  io.in.hrdata := io.out.hrdata
  io.in.hresp  := io.out.hresp
  io.in.hready := io.out.hready && !full // Block master while we steal his address phase
}

/* Masters with lower index have priority over higher index masters.
 * However, a lower priority master will retain control of a slave when EITHER:
 *   1. a burst is in progress (switching slaves mid-burst violates AHB-lite at slave)
 *   2. a transfer was waited (the standard forbids changing requests in this case)
 *
 * If a master raises hmastlock, it will be waited until no other master has inflight
 * requests; then, it acquires exclusive control of the crossbar until hmastlock is low.
 *
 * To implement an AHB-lite crossbar, it is important to realize that requests and
 * responses are coupled. Unlike modern bus protocols where the response data has flow
 * control independent of the request data, in AHB-lite, both flow at the same time at
 * the sole discretion of the slave via the hready signal. The address and data are
 * delivered on two back-to-back cycles, the so-called address and data phases.
 *
 * Masters can only be connected to a single slave at a time. If a master had two different
 * slave connections on the address and data phases, there would be two independent hready
 * signals. An AHB-lite slave can assume that data flows when it asserts hready. If the data
 * slave deasserts hready while the address slave asserts hready, the master is put in the
 * impossible position of being in data phase on two slaves at once. For this reason, when
 * a master issues back-to-back accesses to distinct slaves, we inject a pipeline bubble
 * between the two requests to limit the master to just a single slave at a time.
 *
 * Conversely, a slave CAN have two masters attached to it. This is unproblematic, because
 * the only signal which governs data flow is hready. Thus, both masters can be stalled
 * safely by the single slave.
 */
class HastiXbar(nMasters: Int, addressMap: Seq[UInt=>Bool])(implicit p: Parameters) extends HastiModule()(p) {
  val io = new Bundle {
    val masters = Vec(nMasters,        new HastiMasterIO).flip
    val slaves  = Vec(addressMap.size, new HastiSlaveIO).flip
  }
  
  val nSlaves = addressMap.size
  
  // Setup diversions infront of each master
  val diversions = Seq.tabulate(nMasters) { m => Module(new MasterDiversion) }
  (io.masters zip diversions) foreach { case (m, d) => d.io.in <> m }
  
  // Handy short-hand
  val masters = diversions map (_.io.out)
  val slaves  = io.slaves
  
  // Lock status of the crossbar
  val lockedM = Reg(init = Vec.fill(nMasters)(Bool(false)))
  val isLocked = lockedM.reduce(_ || _)
  
  // This matrix governs the master-slave connections in the address phase
  // It is indexed by addressPhaseGrantSM(slave)(master)
  // It is guaranteed to have at most one 'true' per column and per row
  val addressPhaseGrantSM = Wire(Vec(nSlaves, Vec(nMasters, Bool())))
  // This matrix governs the master-slave connections in the data phase
  // It is guaranteed to have at most one 'true' per column and per row
  val dataPhaseGrantSM    = Reg (init = Vec.fill(nSlaves)(Vec.fill(nMasters)(Bool(false))))
  // This matrix is the union of the address and data phases.
  // It is transposed with respect to the two previous matrices.
  // It is guaranteed to contain at most one 'true' per master row.
  // However, two 'true's per slave column are permitted.
  val unionGrantMS = Vec.tabulate(nMasters) { m => Vec.tabulate(nSlaves) { s => 
                       addressPhaseGrantSM(s)(m) || dataPhaseGrantSM(s)(m) } }
  
  // Confirm the guarantees made above
  def justOnce(v: Vec[Bool]) = v.fold(Bool(false)) { case (p, v) =>
    assert (!p || !v)
    p || v
  }
  addressPhaseGrantSM foreach { s => justOnce(s) }
  unionGrantMS        foreach { s => justOnce(s) }
  
  // Data phase follows address phase whenever the slave is ready
  (slaves zip (dataPhaseGrantSM zip addressPhaseGrantSM)) foreach { case (s, (d, a)) =>
    when (s.hready) { d := a }
  }
  
  // Record the grant state from the previous cycle; needed in case we hold access
  val priorAddressPhaseGrantSM = RegNext(addressPhaseGrantSM)
  
  // If a master says BUSY or SEQ, it is in the middle of a burst.
  // In this case, it MUST stay attached to the same slave as before.
  // Otherwise, it would violate the AHB-lite specification as seen by
  // the slave, which is guaranteed a complete burst of the promised length.
  // One case where this matters is preventing preemption of low-prio masters.
  // NOTE: this exposes a slave to bad addresses when a master is buggy
  val holdBurstM = Vec(masters map { _.isHold() })
  
  // Transform the burst hold requirement from master indexing to slave indexing
  // We use the previous cycle's binding because the master continues the prior burst
  val holdBurstS = Vec(priorAddressPhaseGrantSM map { m => Mux1H(m, holdBurstM) })
  
  // If a slave says !hready to a request, it must retain the same master next cycle.
  // The AHB-lite specification requires that a waited transfer remain unchanged.
  // If we preempted a waited master, the new master's request could potentially differ.
  val holdBusyS = RegNext(Vec(slaves map { s => !s.hready && s.hsel }))
  
  // Combine the above two grounds to determine if the slave retains its prior master
  val holdS = Vec((holdBurstS zip holdBusyS) map ({ case (a,b) => a||b }))
  
  // Determine which master addresses match which slaves
  val matchMS = Vec(masters map { m => Vec(addressMap map { afn => afn(m.haddr) }) })
  // Detect requests to nowhere; we need to allow progress in this case
  val nowhereM = Vec(matchMS map { s => !s.reduce(_ || _) })
  
  // Detect if we need to inject a pipeline bubble between the master requests.
  // Divert masters already granted a data phase different from next request.
  // NOTE: if only one slave, matchMS is always true => bubble always false
  //       => the diversion registers are optimized away as they are unread
  // NOTE: bubble => dataPhase => have an hready signal
  val bubbleM =
    Vec.tabulate(nMasters) { m =>
      Vec.tabulate(nSlaves) { s => dataPhaseGrantSM(s)(m) && !matchMS(m)(s) }
      .reduce(_ || _) }
  
  // Block any request that requires bus ownership or conflicts with isLocked
  val blockedM = 
    Vec((lockedM zip masters) map { case(l, m) => !l && (isLocked || m.hmastlock) })
  
  // Requested access to slaves from masters (pre-arbitration)
  // NOTE: isNSeq does NOT include SEQ; thus, masters who are midburst do not
  // request access to a new slave. They stay tied to the old and do not get two.
  // NOTE: if a master was waited, it must repeat the same request as last cycle;
  // thus, it will request the same slave and not end up with two (unless buggy).
  val NSeq = masters.map(_.isNSeq())
  val requestSM = Vec.tabulate(nSlaves) { s => Vec.tabulate(nMasters) { m => 
    matchMS(m)(s) && NSeq(m) && !bubbleM(m) && !blockedM(m) } }
  
  // Select at most one master request per slave (lowest index = highest priority)
  val selectedRequestSM = Vec(requestSM map { m => Vec(PriorityEncoderOH(m)) })
  
  // Calculate new crossbar interconnect state
  addressPhaseGrantSM := Vec((holdS zip (priorAddressPhaseGrantSM zip selectedRequestSM))
                             map { case (h, (p, r)) => Mux(h, p, r) })

  for (m <- 0 until nMasters) {
    // If the master is connected to a slave, the slave determines hready.
    // However, if no slave is connected, for progress report ready anyway, if:
    //   bad address (swallow request) OR idle (permit stupid masters to move FSM)
    val autoready = nowhereM(m) || masters(m).isIdle()
    val hready    = if (nSlaves == 1)
                      Mux(unionGrantMS(m)(0), slaves(0).hready ^ autoready, Bool(false)) ^ autoready
                    else
                      Mux1H(unionGrantMS(m), slaves.map(_.hready ^ autoready)) ^ autoready
    masters(m).hready := hready
    // If we diverted a master, we need to absorb his address phase to replay later
    diversions(m).io.divert := (bubbleM(m) || blockedM(m)) && NSeq(m) && hready
  }
  
  // Master muxes (address and data phase are the same)
  (masters zip unionGrantMS) foreach { case (m, g) => {
    m.hrdata := Mux1H(g, slaves.map(_.hrdata))
    m.hresp  := Mux1H(g, slaves.map(_.hresp))
  } }
  
  // Slave address phase muxes
  (slaves zip addressPhaseGrantSM) foreach { case (s, g) => {
    s.htrans    := Mux1H(g, masters.map(_.htrans))
    s.haddr     := Mux1H(g, masters.map(_.haddr))
    s.hmastlock := isLocked
    s.hwrite    := Mux1H(g, masters.map(_.hwrite))
    s.hsize     := Mux1H(g, masters.map(_.hsize))
    s.hburst    := Mux1H(g, masters.map(_.hburst))
    s.hprot     := Mux1H(g, masters.map(_.hprot))
    s.hsel      := g.reduce(_ || _)
  } }
  
  // Slave data phase muxes
  (slaves zip dataPhaseGrantSM) foreach { case (s, g) => {
    s.hwdata := Mux1H(g, masters.map(_.hwdata))
  } }
  
  // When no master-slave connections are active, a master can take-over the bus
  val canLock = !addressPhaseGrantSM.map({ v => v.reduce(_ || _) }).reduce(_ || _)
  
  // Lowest index highest priority for lock arbitration
  val reqLock = masters.map(_.hmastlock)
  val winLock = PriorityEncoderOH(reqLock)
  
  // Lock arbitration
  when (isLocked) {
    lockedM := (lockedM zip reqLock) map { case (a,b) => a && b }
  } .elsewhen (canLock) {
    lockedM := winLock
  }
}

class HastiBus(amap: Seq[UInt=>Bool])(implicit p: Parameters) extends HastiModule()(p) {
  val io = new Bundle {
    val master = new HastiMasterIO().flip
    val slaves = Vec(amap.size, new HastiSlaveIO).flip
  }

  val bar = Module(new HastiXbar(1, amap))
  bar.io.masters(0) <> io.master
  bar.io.slaves <> io.slaves
}

class HastiSlaveMux(n: Int)(implicit p: Parameters) extends HastiModule()(p) {
  val io = new Bundle {
    val ins = Vec(n, new HastiSlaveIO)
    val out = new HastiSlaveIO().flip
  }
  
  val amap = Seq({ (_:UInt) => Bool(true)})
  val bar = Module(new HastiXbar(n, amap))
  io.ins <> bar.io.masters
  io.out <> bar.io.slaves(0)
}

class HastiSlaveToMaster(implicit p: Parameters) extends HastiModule()(p) {
  val io = new Bundle {
    val in  = new HastiSlaveIO
    val out = new HastiMasterIO
  }

  io.out.htrans    := Mux(io.in.hsel, io.in.htrans, HTRANS_IDLE)
  io.out.hmastlock := io.in.hmastlock
  io.out.haddr     := io.in.haddr
  io.out.hwrite    := io.in.hwrite
  io.out.hburst    := io.in.hburst
  io.out.hsize     := io.in.hsize
  io.out.hprot     := io.in.hprot
  io.out.hwdata    := io.in.hwdata
  io.in.hrdata := io.out.hrdata
  io.in.hready := io.out.hready
  io.in.hresp  := io.out.hresp
}

class HastiMasterIONastiIOConverter(implicit p: Parameters) extends HastiModule()(p)
    with HasNastiParameters {
  val io = new Bundle {
    val nasti = new NastiIO().flip
    val hasti = new HastiMasterIO
  }

  require(hastiAddrBits == nastiXAddrBits)
  require(hastiDataBits == nastiXDataBits)

  val r_queue = Module(new Queue(new NastiReadDataChannel, 2, pipe = true))

  val s_idle :: s_read :: s_write :: s_write_resp :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  val addr = Reg(UInt(width = hastiAddrBits))
  val id = Reg(UInt(width = nastiXIdBits))
  val size = Reg(UInt(width = nastiXSizeBits))
  val len = Reg(UInt(width = nastiXLenBits))
  val data = Reg(UInt(width = nastiXDataBits))
  val first = Reg(init = Bool(false))
  val is_rtrans = (state === s_read) &&
                  (io.hasti.htrans === HTRANS_SEQ ||
                   io.hasti.htrans === HTRANS_NONSEQ)
  val rvalid = RegEnable(is_rtrans, Bool(false), io.hasti.hready)

  io.nasti.aw.ready := (state === s_idle)
  io.nasti.ar.ready := (state === s_idle) && !io.nasti.aw.valid
  io.nasti.w.ready := (state === s_write) && io.hasti.hready
  io.nasti.b.valid := (state === s_write_resp)
  io.nasti.b.bits := NastiWriteResponseChannel(id = id)
  io.nasti.r <> r_queue.io.deq

  r_queue.io.enq.valid := io.hasti.hready && rvalid
  r_queue.io.enq.bits := NastiReadDataChannel(
    id = id,
    data = io.hasti.hrdata,
    last = (len === UInt(0)))

  assert(!r_queue.io.enq.valid || r_queue.io.enq.ready,
    "NASTI -> HASTI converter queue overflow")

  // How many read requests have we not delivered a response for yet?
  val pending_count = r_queue.io.count + rvalid

  io.hasti.haddr := addr
  io.hasti.hsize := size
  io.hasti.hwrite := (state === s_write)
  io.hasti.hburst := HBURST_INCR
  io.hasti.hprot := UInt(0)
  io.hasti.hwdata := data
  io.hasti.hmastlock := Bool(false)
  io.hasti.htrans := MuxLookup(state, HTRANS_IDLE, Seq(
    s_write -> Mux(io.nasti.w.valid,
      Mux(first, HTRANS_NONSEQ, HTRANS_SEQ),
      Mux(first, HTRANS_IDLE, HTRANS_BUSY)),
    s_read -> MuxCase(HTRANS_BUSY, Seq(
      first -> HTRANS_NONSEQ,
      (pending_count <= UInt(1)) -> HTRANS_SEQ))))

  when (io.nasti.aw.fire()) {
    first := Bool(true)
    addr := io.nasti.aw.bits.addr
    id := io.nasti.aw.bits.id
    size := io.nasti.aw.bits.size
    state := s_write
  }

  when (io.nasti.ar.fire()) {
    first := Bool(true)
    addr := io.nasti.ar.bits.addr
    id := io.nasti.ar.bits.id
    size := io.nasti.ar.bits.size
    len := io.nasti.ar.bits.len
    state := s_read
  }

  when (io.nasti.w.fire()) {
    first := Bool(false)
    addr := addr + (UInt(1) << size)
    data := io.nasti.w.bits.data
    when (io.nasti.w.bits.last) { state := s_write_resp }
  }

  when (io.nasti.b.fire()) { state := s_idle }

  when (is_rtrans && io.hasti.hready) {
    first := Bool(false)
    addr := addr + (UInt(1) << size)
    len := len - UInt(1)
    when (len === UInt(0)) { state := s_idle }
  }
}

class HastiTestSRAM(depth: Int)(implicit p: Parameters) extends HastiModule()(p) {
  val io = new HastiSlaveIO
  
  // This is a test SRAM with random delays
  val ready = LFSR16(Bool(true))(0) // Bool(true)
  
  // Calculate the bitmask of which bytes are being accessed
  val mask_decode = Vec.tabulate(hastiAlignment+1) (UInt(_) <= io.hsize)
  val mask_wide   = Vec.tabulate(hastiDataBytes) { i => mask_decode(log2Ceil(i+1)) }
  val mask_shift  = if (hastiAlignment == 0) UInt(1) else
                    mask_wide.asUInt() << io.haddr(hastiAlignment-1,0)
  
  // The request had better have been aligned! (AHB-lite requires this)
  if (hastiAlignment >= 1) {
    assert (io.htrans === HTRANS_IDLE || io.htrans === HTRANS_BUSY ||
      (io.haddr & mask_decode.asUInt()(hastiAlignment,1)) === UInt(0),
      "HASTI request not aligned")
  }
  
  // The mask and address during the address phase
  val a_request   = io.hsel && (io.htrans === HTRANS_NONSEQ || io.htrans === HTRANS_SEQ)
  val a_mask      = Wire(UInt(width = hastiDataBytes))
  val a_address   = io.haddr(depth-1, hastiAlignment)
  val a_write     = io.hwrite

  // for backwards compatibility with chisel2, we needed a static width in definition
  a_mask := mask_shift(hastiDataBytes-1, 0)
  
  // The data phase signals
  val d_read  = RegEnable(a_request && !a_write, Bool(false), ready)
  val d_mask  = RegEnable(a_mask, ready && a_request)
  val d_wdata = Vec.tabulate(hastiDataBytes) { i => io.hwdata(8*(i+1)-1, 8*i) }
  
  // AHB writes must occur during the data phase; this poses a structural
  // hazard with reads which must occur during the address phase. To solve
  // this problem, we delay the writes until there is a free cycle.
  //
  // The idea is to record the address information from address phase and
  // then as soon as possible flush the pending write. This cannot be done
  // on a cycle when there is an address phase read, but on any other cycle
  // the write will execute. In the case of reads following a write, the
  // result must bypass data from the pending write into the read if they
  // happen to have matching address.
  
  // Pending write?
  val p_valid     = RegInit(Bool(false))
  val p_address   = Reg(a_address)
  val p_mask      = Reg(a_mask)
  val p_latch_d   = RegNext(ready && a_request && a_write, Bool(false))
  val p_wdata     = d_wdata holdUnless p_latch_d
  
  // Use single-ported memory with byte-write enable
  val mem = SeqMem(1 << (depth-hastiAlignment), Vec(hastiDataBytes, Bits(width = 8)))
  
  // Decide is the SRAM port is used for reading or (potentially) writing
  val read = ready && a_request && !a_write
  // In case we are stalled, we need to hold the read data
  val d_rdata = mem.readAndHold(a_address, read)
  // Whenever the port is not needed for reading, execute pending writes
  when (!read && p_valid) { mem.write(p_address, p_wdata, p_mask.toBools) }
  when (!read) { p_valid := Bool(false) }
  
  // Record the request for later?
  when (ready && a_request && a_write) {
    p_valid   := Bool(true)
    p_address := a_address
    p_mask    := a_mask
  }
  
  // Does the read need to be muxed with the previous write?
  val a_bypass = a_address === p_address && p_valid
  val d_bypass = RegEnable(a_bypass, ready && a_request)
  
  // Mux in data from the pending write
  val muxdata = Vec((p_mask.toBools zip (p_wdata zip d_rdata))
                    map { case (m, (p, r)) => Mux(d_bypass && m, p, r) })
  // Wipe out any data the master should not see (for testing)
  val outdata = Vec((d_mask.toBools zip muxdata)
                    map { case (m, p) => Mux(d_read && ready && m, p, Bits(0)) })

  // Finally, the outputs
  io.hrdata := outdata.asUInt
  io.hready := ready
  io.hresp  := HRESP_OKAY
}

class HastiTest(implicit p: Parameters) extends UnitTest {
  val sram = Module(new HastiTestSRAM(8))
  val bus = Module(new HastiBus(Seq(a => Bool(true))))
  val conv = Module(new HastiMasterIONastiIOConverter)
  val driver = Module(new NastiDriver(32, 8, 2))

  bus.io.slaves(0) <> sram.io
  bus.io.master <> conv.io.hasti
  conv.io.nasti <> driver.io.nasti
  io.finished := driver.io.finished
  driver.io.start := io.start
}
