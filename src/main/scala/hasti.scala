package junctions

import Chisel._
import cde.{Parameters, Field}

trait HastiConstants
{
  // Values for htrans
  val SZ_HTRANS     = 2
  val HTRANS_IDLE   = UInt(0, SZ_HTRANS) // No transfer requested, not in a burst
  val HTRANS_BUSY   = UInt(1, SZ_HTRANS) // No transfer requested, in a burst
  val HTRANS_NONSEQ = UInt(2, SZ_HTRANS) // First (potentially only) request in a burst
  val HTRANS_SEQ    = UInt(3, SZ_HTRANS) // Following requests in a burst

  // Values for hburst
  val SZ_HBURST     = 3
  val HBURST_SINGLE = UInt(0, SZ_HBURST) // Single access (no burst)
  val HBURST_INCR   = UInt(1, SZ_HBURST) // Incrementing burst of arbitrary length, not crossing 1KB
  val HBURST_WRAP4  = UInt(2, SZ_HBURST) // 4-beat wrapping burst
  val HBURST_INCR4  = UInt(3, SZ_HBURST) // 4-beat incrementing burst
  val HBURST_WRAP8  = UInt(4, SZ_HBURST) // 8-beat wrapping burst
  val HBURST_INCR8  = UInt(5, SZ_HBURST) // 8-beat incrementing burst
  val HBURST_WRAP16 = UInt(6, SZ_HBURST) // 16-beat wrapping burst
  val HBURST_INCR16 = UInt(7, SZ_HBURST) // 16-beat incrementing burst

  // Values for hresp
  val SZ_HRESP      = 1
  val HRESP_OKAY    = UInt(0, SZ_HRESP)
  val HRESP_ERROR   = UInt(1, SZ_HRESP)

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

case class HastiParameters(dataBits: Int, addrBits: Int)
case object HastiId extends Field[String]
case class HastiKey(id: String) extends Field[HastiParameters]

trait HasHastiParameters {
  implicit val p: Parameters
  val hastiParams = p(HastiKey(p(HastiId)))
  val hastiAddrBits = hastiParams.addrBits
  val hastiDataBits = hastiParams.dataBits
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
  assert (!io.divert || io.in.hready);
  
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
  // !!! handle hmastlock
  
  // Setup diversions infront of each master
  val diversions = Seq.tabulate(nMasters) { m => Module(new MasterDiversion) }
  (io.masters zip diversions) foreach { case (m, d) => d.io.in <> m }
  
  // Handy short-hand
  val masters = diversions map (_.io.out)
  val slaves  = io.slaves
  
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
  
  // Requested access to slaves from masters (pre-arbitration)
  // NOTE: isNSeq does NOT include SEQ; thus, masters who are midburst do not
  // request access to a new slave. They stay tied to the old and do not get two.
  // NOTE: if a master was waited, it must repeat the same request as last cycle;
  // thus, it will request the same slave and not end up with two (unless buggy).
  val NSeq = Vec(masters.map(_.isNSeq()))
  val requestSM = Vec.tabulate(nSlaves) { s => Vec.tabulate(nMasters) { m => matchMS(m)(s) && NSeq(m) && !bubbleM(m) } }
  
  // Select at most one master request per slave (lowest index = highest priority)
  val selectedRequestSM = Vec(requestSM map { m => Vec(PriorityEncoderOH(m)) })
  
  // Calculate new crossbar interconnect state
  addressPhaseGrantSM := Vec((holdS zip (priorAddressPhaseGrantSM zip selectedRequestSM))
                             map { case (h, (p, r)) => Mux(h, p, r) })

  // If we diverted a master, we need to absorb his address phase to replay later
  for (m <- 0 until nMasters) {
    diversions(m).io.divert := bubbleM(m) && NSeq(m) && masters(m).hready
  }
  
  // Master muxes (address and data phase are the same)
  (masters zip (unionGrantMS zip nowhereM)) foreach { case (m, (g, n)) => {
    // If the master is connected to a slave, the slave determines hready.
    // However, if no slave is connected, for progress report ready anyway, if:
    //   bad address (swallow request) OR idle (permit stupid slaves to move FSM)
    val autoready = n || m.isIdle()
    m.hready := Mux1H(g, slaves.map(_.hready ^ autoready)) ^ autoready
    m.hrdata := Mux1H(g, slaves.map(_.hrdata))
    m.hresp  := Mux1H(g, slaves.map(_.hresp))
  } }
  
  // Slave address phase muxes
  (slaves zip addressPhaseGrantSM) foreach { case (s, g) => {
    s.htrans    := Mux1H(g, masters.map(_.htrans)) // defaults to HTRANS_IDLE (0)
    s.haddr     := Mux1H(g, masters.map(_.haddr))
    s.hmastlock := Mux1H(g, masters.map(_.hmastlock)) // !!! use global crossbar lock state
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
}

class HastiBus(amap: Seq[UInt=>Bool])(implicit p: Parameters) extends HastiModule()(p) {
  val io = new Bundle {
    val master = new HastiMasterIO().flip
    val slaves = Vec(amap.size, new HastiSlaveIO).flip
  }

  val bar = Module(new HastiXbar(1, amap))
  io.master <> bar.io.masters(0)
  io.slaves <> bar.io.slaves
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

  val s_idle :: s_read :: s_write :: s_write_resp :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  val addr = Reg(UInt(width = hastiAddrBits))
  val id = Reg(UInt(width = nastiXIdBits))
  val size = Reg(UInt(width = nastiXSizeBits))
  val len = Reg(UInt(width = nastiXLenBits))
  val data = Reg(UInt(width = nastiXDataBits))
  val first = Reg(init = Bool(false))
  val rvalid = Reg(init = Bool(false))

  io.nasti.aw.ready := (state === s_idle)
  io.nasti.ar.ready := (state === s_idle) && !io.nasti.aw.valid
  io.nasti.w.ready := (state === s_write) && io.hasti.hready
  io.nasti.b.valid := (state === s_write_resp)
  io.nasti.b.bits := NastiWriteResponseChannel(id = id)
  io.nasti.r.valid := (state === s_read) && io.hasti.hready && !first
  io.nasti.r.bits := NastiReadDataChannel(
    id = id,
    data = io.hasti.hrdata,
    last = (len === UInt(0)))


  io.hasti.haddr := addr
  io.hasti.hsize := size
  io.hasti.hwrite := (state === s_write)
  io.hasti.hburst := HBURST_INCR
  io.hasti.hprot := UInt(0)
  io.hasti.hwdata := data
  io.hasti.htrans := MuxLookup(state, HTRANS_IDLE, Seq(
    s_write -> Mux(io.nasti.w.valid,
      Mux(first, HTRANS_NONSEQ, HTRANS_SEQ),
      Mux(first, HTRANS_IDLE, HTRANS_BUSY)),
    s_read -> MuxCase(HTRANS_BUSY, Seq(
      first -> HTRANS_NONSEQ,
      (len === UInt(0)) -> HTRANS_IDLE,
      io.nasti.r.ready -> HTRANS_SEQ))))

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

  when (state === s_read && first) {
    first := Bool(false)
    addr := addr + (UInt(1) << size)
  }

  when (io.nasti.r.fire()) {
    addr := addr + (UInt(1) << size)
    len := len - UInt(1)
    when (len === UInt(0)) { state := s_idle }
  }
}
