// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.amba.axi4._
import scala.math.{min, max}

class TLtoAXI4IdMap(tl: TLClientPortParameters, axi4: AXI4MasterPortParameters) {
  private val axiDigits = String.valueOf(axi4.endId-1).length()
  private val tlDigits = String.valueOf(tl.endSourceId-1).length()
  private val fmt = s"\t[%${axiDigits}d, %${axiDigits}d) <= [%${tlDigits}d, %${tlDigits}d) %s%s%s"
  private val sorted = tl.clients.sortWith(TLToAXI4.sortByType)

  val mapping: Seq[TLToAXI4IdMapEntry] = (sorted zip axi4.masters) map { case (c, m) =>
    TLToAXI4IdMapEntry(m.id, c.sourceId, c.name, c.supportsProbe, c.requestFifo)
  }

  def pretty: String = mapping.map(_.pretty(fmt)).mkString(",\n")
}

case class TLToAXI4IdMapEntry(axi4Id: IdRange, tlId: IdRange, name: String, isCache: Boolean, requestFifo: Boolean) {
  def pretty(fmt: String) = fmt.format(
    axi4Id.start,
    axi4Id.end,
    tlId.start,
    tlId.end,
    s""""$name"""",
    if (isCache) " [CACHE]" else "",
    if (requestFifo) " [FIFO]" else "")
}

case class TLToAXI4Node(stripBits: Int = 0)(implicit valName: ValName) extends MixedAdapterNode(TLImp, AXI4Imp)(
  dFn = { p =>
    p.clients.foreach { c =>
      require (c.sourceId.start % (1 << stripBits) == 0 &&
               c.sourceId.end   % (1 << stripBits) == 0,
               s"Cannot strip bits of aligned client ${c.name}: ${c.sourceId}")
    }
    val clients = p.clients.sortWith(TLToAXI4.sortByType _)
    val idSize = clients.map { c => if (c.requestFifo) 1 else (c.sourceId.size >> stripBits) }
    val idStart = idSize.scanLeft(0)(_+_).init
    val masters = ((idStart zip idSize) zip clients) map { case ((start, size), c) =>
      AXI4MasterParameters(
        name      = c.name,
        id        = IdRange(start, start+size),
        aligned   = true,
        maxFlight = Some(if (c.requestFifo) c.sourceId.size else (1 << stripBits)),
        nodePath  = c.nodePath)
    }
    AXI4MasterPortParameters(
      masters  = masters,
      userBits = log2Ceil(p.endSourceId) + 4)
  },
  uFn = { p => TLManagerPortParameters(
    managers = p.slaves.map { case s =>
      TLManagerParameters(
        address            = s.address,
        resources          = s.resources,
        regionType         = s.regionType,
        executable         = s.executable,
        nodePath           = s.nodePath,
        supportsGet        = s.supportsRead,
        supportsPutFull    = s.supportsWrite,
        supportsPutPartial = s.supportsWrite,
        fifoId             = Some(0),
        mayDenyPut         = true,
        mayDenyGet         = true)},
      beatBytes = p.beatBytes,
      minLatency = p.minLatency)
  })

class TLToAXI4(val combinational: Boolean = true, val adapterName: Option[String] = None, val stripBits: Int = 0)(implicit p: Parameters) extends LazyModule
{
  val node = TLToAXI4Node(stripBits)

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val slaves  = edgeOut.slave.slaves

      // All pairs of slaves must promise that they will never interleave data
      require (slaves(0).interleavedId.isDefined)
      slaves.foreach { s => require (s.interleavedId == slaves(0).interleavedId) }

      // Construct the source=>ID mapping table
      val map = new TLtoAXI4IdMap(edgeIn.client, edgeOut.master)
      val sourceStall = Wire(Vec(edgeIn.client.endSourceId, Bool()))
      val sourceTable = Wire(Vec(edgeIn.client.endSourceId, out.aw.bits.id))
      val idStall = Wire(init = Vec.fill(edgeOut.master.endId) { Bool(false) })
      var idCount = Array.fill(edgeOut.master.endId) { None:Option[Int] }

      Annotated.idMapping(this, map.mapping).foreach { case TLToAXI4IdMapEntry(axi4Id, tlId, _, _, fifo) =>
        for (i <- 0 until tlId.size) {
          val id = axi4Id.start + (if (fifo) 0 else (i >> stripBits))
          sourceStall(tlId.start + i) := idStall(id)
          sourceTable(tlId.start + i) := UInt(id)
        }
        if (fifo) { idCount(axi4Id.start) = Some(tlId.size) }
      }

      adapterName.foreach { n =>
        println(s"$n AXI4-ID <= TL-Source mapping:\n${map.pretty}\n")
        ElaborationArtefacts.add(s"$n.axi4.json", s"""{"mapping":[${map.mapping.mkString(",")}]}""")
      }

      // We need to keep the following state from A => D: (size, source)
      // All of those fields could potentially require 0 bits (argh. Chisel.)
      // We will pack all of that extra information into the user bits.

      val sourceBits = log2Ceil(edgeIn.client.endSourceId)
      val sizeBits = log2Ceil(edgeIn.maxLgSize+1)
      val stateBits = sizeBits + sourceBits // could be 0
      require (stateBits <= out.aw.bits.params.userBits)

      val a_address = edgeIn.address(in.a.bits)
      val a_source  = in.a.bits.source
      val a_size    = edgeIn.size(in.a.bits)
      val a_isPut   = edgeIn.hasData(in.a.bits)
      val (a_first, a_last, _) = edgeIn.firstlast(in.a)

      // Make sure the fields are within the bounds we assumed
      assert (a_source  < UInt(BigInt(1) << sourceBits))
      assert (a_size    < UInt(BigInt(1) << sizeBits))

      // Carefully pack/unpack fields into the state we send
      val baseEnd = 0
      val (sourceEnd, sourceOff) = (sourceBits + baseEnd,   baseEnd)
      val (sizeEnd,   sizeOff)   = (sizeBits   + sourceEnd, sourceEnd)
      require (sizeEnd == stateBits)

      val a_state = (a_source << sourceOff) | (a_size << sizeOff)

      val r_state = out.r.bits.user.getOrElse(UInt(0))
      val r_source  = if (sourceBits > 0) r_state(sourceEnd-1, sourceOff) else UInt(0)
      val r_size    = if (sizeBits   > 0) r_state(sizeEnd  -1, sizeOff)   else UInt(0)

      val b_state = out.b.bits.user.getOrElse(UInt(0))
      val b_source  = if (sourceBits > 0) b_state(sourceEnd-1, sourceOff) else UInt(0)
      val b_size    = if (sizeBits   > 0) b_state(sizeEnd  -1, sizeOff)   else UInt(0)

      // We need these Queues because AXI4 queues are irrevocable
      val depth = if (combinational) 1 else 2
      val out_arw = Wire(Decoupled(new AXI4BundleARW(out.params)))
      val out_w = Wire(out.w)
      out.w <> Queue.irrevocable(out_w, entries=depth, flow=combinational)
      val queue_arw = Queue.irrevocable(out_arw, entries=depth, flow=combinational)

      // Fan out the ARW channel to AR and AW
      out.ar.bits := queue_arw.bits
      out.aw.bits := queue_arw.bits
      out.ar.valid := queue_arw.valid && !queue_arw.bits.wen
      out.aw.valid := queue_arw.valid &&  queue_arw.bits.wen
      queue_arw.ready := Mux(queue_arw.bits.wen, out.aw.ready, out.ar.ready)

      val beatBytes = edgeIn.manager.beatBytes
      val maxSize   = UInt(log2Ceil(beatBytes))
      val doneAW    = RegInit(Bool(false))
      when (in.a.fire()) { doneAW := !a_last }

      val arw = out_arw.bits
      arw.wen   := a_isPut
      arw.id    := sourceTable(a_source)
      arw.addr  := a_address
      arw.len   := UIntToOH1(a_size, AXI4Parameters.lenBits + log2Ceil(beatBytes)) >> log2Ceil(beatBytes)
      arw.size  := Mux(a_size >= maxSize, maxSize, a_size)
      arw.burst := AXI4Parameters.BURST_INCR
      arw.lock  := UInt(0) // not exclusive (LR/SC unsupported b/c no forward progress guarantee)
      arw.cache := UInt(0) // do not allow AXI to modify our transactions
      arw.prot  := AXI4Parameters.PROT_PRIVILEDGED
      arw.qos   := UInt(0) // no QoS
      arw.user.foreach { _ := a_state }

      val stall = sourceStall(in.a.bits.source) && a_first
      in.a.ready := !stall && Mux(a_isPut, (doneAW || out_arw.ready) && out_w.ready, out_arw.ready)
      out_arw.valid := !stall && in.a.valid && Mux(a_isPut, !doneAW && out_w.ready, Bool(true))

      out_w.valid := !stall && in.a.valid && a_isPut && (doneAW || out_arw.ready)
      out_w.bits.data := in.a.bits.data
      out_w.bits.strb := in.a.bits.mask
      out_w.bits.last := a_last
      out_w.bits.corrupt.foreach { _ := in.a.bits.corrupt }

      // R and B => D arbitration
      val r_holds_d = RegInit(Bool(false))
      when (out.r.fire()) { r_holds_d := !out.r.bits.last }
      // Give R higher priority than B
      val r_wins = out.r.valid || r_holds_d

      out.r.ready := in.d.ready
      out.b.ready := in.d.ready && !r_wins
      in.d.valid := Mux(r_wins, out.r.valid, out.b.valid)

      // If the first beat of the AXI RRESP is RESP_DECERR, treat this as a denied
      // request. We must pulse extend this value as AXI is allowed to change the
      // value of RRESP on every beat, and ChipLink may not.
      val r_first = RegInit(Bool(true))
      when (out.r.fire()) { r_first := out.r.bits.last }
      val r_denied  = out.r.bits.resp === AXI4Parameters.RESP_DECERR holdUnless r_first
      val r_corrupt = out.r.bits.resp =/= AXI4Parameters.RESP_OKAY
      val b_denied  = out.b.bits.resp =/= AXI4Parameters.RESP_OKAY

      val r_d = edgeIn.AccessAck(r_source, r_size, UInt(0), denied = r_denied, corrupt = r_corrupt || r_denied)
      val b_d = edgeIn.AccessAck(b_source, b_size, denied = b_denied)

      in.d.bits := Mux(r_wins, r_d, b_d)
      in.d.bits.data := out.r.bits.data // avoid a costly Mux

      // We need to track if any reads or writes are inflight for a given ID.
      // If the opposite type arrives, we must stall until it completes.
      val a_sel = UIntToOH(arw.id, edgeOut.master.endId).asBools
      val d_sel = UIntToOH(Mux(r_wins, out.r.bits.id, out.b.bits.id), edgeOut.master.endId).asBools
      val d_last = Mux(r_wins, out.r.bits.last, Bool(true))
      // If FIFO was requested, ensure that R+W ordering is preserved
      (a_sel zip d_sel zip idStall zip idCount) foreach { case (((as, ds), s), n) =>
        // AXI does not guarantee read vs. write ordering. In particular, if we
        // are in the middle of receiving a read burst and then issue a write,
        // the write might affect the read burst. This violates FIFO behaviour.
        // To solve this, we must wait until the last beat of a burst, but this
        // means that a TileLink master which performs early source reuse can
        // have one more transaction inflight than we promised AXI; stall it too.
        val maxCount = n.getOrElse(1)
        val count = RegInit(UInt(0, width = log2Ceil(maxCount + 1)))
        val write = Reg(Bool())
        val idle = count === UInt(0)

        val inc = as && out_arw.fire()
        val dec = ds && d_last && in.d.fire()
        count := count + inc.asUInt - dec.asUInt

        assert (!dec || count =/= UInt(0))        // underflow
        assert (!inc || count =/= UInt(maxCount)) // overflow

        when (inc) { write := arw.wen }
        // If only one transaction can be inflight, it can't mismatch
        val mismatch = if (maxCount > 1) { write =/= arw.wen } else { Bool(false) }
        s := (!idle && mismatch) || (count === UInt(maxCount))
      }

      // Tie off unused channels
      in.b.valid := Bool(false)
      in.c.ready := Bool(true)
      in.e.ready := Bool(true)
    }
  }
}

object TLToAXI4
{
  def apply(combinational: Boolean = true, adapterName: Option[String] = None, stripBits: Int = 0)(implicit p: Parameters) =
  {
    val tl2axi4 = LazyModule(new TLToAXI4(combinational, adapterName, stripBits))
    tl2axi4.node
  }

  def sortByType(a: TLClientParameters, b: TLClientParameters): Boolean = {
    if ( a.supportsProbe && !b.supportsProbe) return false
    if (!a.supportsProbe &&  b.supportsProbe) return true
    if ( a.requestFifo   && !b.requestFifo  ) return false
    if (!a.requestFifo   &&  b.requestFifo  ) return true
    return false
  }
}
