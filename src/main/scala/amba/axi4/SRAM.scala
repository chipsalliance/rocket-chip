// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import Chisel._
import freechips.rocketchip.DRAMModel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class AXI4RAM(
    address: AddressSet,
    executable: Boolean = true,
    beatBytes: Int = 4,
    devName: Option[String] = None,
    errors: Seq[AddressSet] = Nil,
    wcorrupt: Boolean = false)
  (implicit p: Parameters) extends DiplomaticSRAM(address, beatBytes, devName)
{
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address       = List(address) ++ errors,
      resources     = resources,
      regionType    = RegionType.UNCACHED,
      executable    = executable,
      supportsRead  = TransferSizes(1, beatBytes),
      supportsWrite = TransferSizes(1, beatBytes),
      interleavedId = Some(0))),
    beatBytes  = beatBytes,
    wcorrupt   = wcorrupt,
    minLatency = 1)))

  lazy val module = new LazyModuleImp(this) {
    val (in, _) = node.in(0)
    val mem = makeSinglePortedByteWriteSeqMem(1 << mask.filter(b=>b).size)
    val corrupt = if (wcorrupt) Some(SeqMem(1 << mask.filter(b=>b).size, UInt(width=2))) else None

    val r_addr = Cat((mask zip (in.ar.bits.addr >> log2Ceil(beatBytes)).toBools).filter(_._1).map(_._2).reverse)
    val w_addr = Cat((mask zip (in.aw.bits.addr >> log2Ceil(beatBytes)).toBools).filter(_._1).map(_._2).reverse)
    val r_sel0 = address.contains(in.ar.bits.addr)
    val w_sel0 = address.contains(in.aw.bits.addr)

    val w_full = RegInit(Bool(false))
    val w_id   = Reg(UInt())
    val w_user = Reg(UInt(width = 1 max in.params.userBits))
    val r_sel1 = Reg(r_sel0)
    val w_sel1 = Reg(w_sel0)

    when (in. b.fire()) { w_full := Bool(false) }
    when (in.aw.fire()) { w_full := Bool(true) }

    when (in.aw.fire()) {
      w_id := in.aw.bits.id
      w_sel1 := w_sel0
      in.aw.bits.user.foreach { w_user := _ }
    }

    val wdata = Vec.tabulate(beatBytes) { i => in.w.bits.data(8*(i+1)-1, 8*i) }
    when (in.aw.fire() && w_sel0) {
      mem.write(w_addr, wdata, in.w.bits.strb.toBools)
      corrupt.foreach { _.write(w_addr, in.w.bits.corrupt.get.asUInt) }
    }

    in. b.valid := w_full
    in.aw.ready := in. w.valid && (in.b.ready || !w_full)
    in. w.ready := in.aw.valid && (in.b.ready || !w_full)

    in.b.bits.id   := w_id
    in.b.bits.resp := Mux(w_sel1, AXI4Parameters.RESP_OKAY, AXI4Parameters.RESP_DECERR)
    in.b.bits.user.foreach { _ := w_user }

    val r_full = RegInit(Bool(false))
    val r_id   = Reg(UInt())
    val r_user = Reg(UInt(width = 1 max in.params.userBits))

    when (in. r.fire()) { r_full := Bool(false) }
    when (in.ar.fire()) { r_full := Bool(true) }

    when (in.ar.fire()) {
      r_id := in.ar.bits.id
      r_sel1 := r_sel0
      in.ar.bits.user.foreach { r_user := _ }
    }

    val ren = in.ar.fire()
    val rdata = mem.readAndHold(r_addr, ren)
    val rcorrupt = corrupt.map(_.readAndHold(r_addr, ren)(0)).getOrElse(Bool(false))

    in. r.valid := r_full
    in.ar.ready := in.r.ready || !r_full

    in.r.bits.id   := r_id
    in.r.bits.resp := Mux(r_sel1, Mux(rcorrupt, AXI4Parameters.RESP_SLVERR, AXI4Parameters.RESP_OKAY), AXI4Parameters.RESP_DECERR)
    in.r.bits.data := Cat(rdata.reverse)
    in.r.bits.user.foreach { _ := r_user }
    in.r.bits.last := Bool(true)
  }
}

object AXI4RAM
{
  def apply(
    address: AddressSet,
    executable: Boolean = true,
    beatBytes: Int = 4,
    devName: Option[String] = None,
    errors: Seq[AddressSet] = Nil)
  (implicit p: Parameters) =
  {
    val axi4ram = LazyModule(new AXI4RAM(address, executable, beatBytes, devName, errors))
    axi4ram.node
  }
}


class BusData(val dataBits : Int, val tagBits : Int) extends Bundle{
  val data = UInt(width = dataBits)
  val tag = UInt(width = tagBits)
}

class AXI4DDR(
    address: AddressSet,
    executable: Boolean = true,
    beatBytes: Int = 4,
    devName: Option[String] = None,
    errors: Seq[AddressSet] = Nil,
    wcorrupt: Boolean = false)
  (implicit p: Parameters) extends DiplomaticDRAM(address, beatBytes, devName) {
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address       = List(address) ++ errors,
      resources     = resources,
      regionType    = RegionType.UNCACHED,
      executable    = executable,
      supportsRead  = TransferSizes(1, beatBytes),
      supportsWrite = TransferSizes(1, beatBytes),
      interleavedId = Some(0))),
    beatBytes  = beatBytes,
    wcorrupt   = wcorrupt,
    minLatency = 1)))

  //val mif = MemoryParameters(memIF = MemoryIFConfiguration(log2Up(node.portParams(0).maxAddress), node.portParams(0).beatBytes * 8 / 4, log2Up(node.portParams(0).maxTransfer) + 2 , 4))
  val mif = MemoryParameters(memIF = MemoryIFConfiguration(32, node.portParams(0).beatBytes * 8 / 4, log2Up(node.portParams(0).maxTransfer) + 3 , 4))
  println(s"#######datawidth = "+mif.memIF.dataBits)
  println(s"node maxAddress = "+node.portParams(0).maxAddress)
  println(s"node beatBytes = "+node.portParams(0).beatBytes)
  val mem = LazyModule(new DRAMSystemWrapper(mif))

  lazy val module = new LazyModuleImp(this) {
    val (in, _) = node.in(0)

    when(in.ar.fire()){
      printf("ar fire!\n")
    }

    println(s"instantiate dram controller\n")

    val r_addr = Cat((mask zip (in.ar.bits.addr >> log2Ceil(beatBytes)).toBools).filter(_._1).map(_._2).reverse)
    val w_addr = Cat((mask zip (in.aw.bits.addr >> log2Ceil(beatBytes)).toBools).filter(_._1).map(_._2).reverse)
    val r_sel0 = address.contains(in.ar.bits.addr)
    val w_sel0 = address.contains(in.aw.bits.addr)

    val w_full = RegInit(Bool(false))
    val w_id = in.aw.bits.id
    val w_user = Reg(UInt(width = 1 max in.params.userBits))
    val r_sel1 = Reg(r_sel0)
    val w_sel1 = Reg(w_sel0)
    implicit val conMif = mif
    val cmdSplit = Wire(Decoupled(new MemReqCmd))
    val cmd = Queue(cmdSplit, 16)

    val rdataSplit = Wire(Decoupled(new BusData(in.params.dataBits, in.params.idBits + (1 max in.params.userBits))))
    val rdata = Queue(rdataSplit, 8)
    val wdataSplit = Wire(Decoupled(new BusData(in.params.dataBits / 4, in.params.idBits + (1 max in.params.userBits))))
    val wdata = Queue(wdataSplit, 8)
    val ar_user = Wire(UInt(width = 1 max in.params.userBits))
    val aw_user = Wire(UInt(width = 1 max in.params.userBits))

    in.ar.bits.user.foreach{ar_user := _}
    in.aw.bits.user.foreach{aw_user := _}

    cmdSplit.valid := in.ar.valid || in.aw.valid
    cmdSplit.bits.addr := in.ar.bits.addr
    cmdSplit.bits.rw := in.aw.valid
    cmdSplit.bits.tag := Mux(in.ar.valid, ar_user ## in.ar.bits.id, aw_user ## in.aw.bits.id)

    mem.module.io.memReqCmd <> cmd
    mem.module.io.memReqData <> wdata
    //rdataSplit <> mem.module.io.memResp

    val wdataReg = Reg(init = UInt(0, width = in.params.dataBits))
    val idle :: enq1 :: enq2 :: enq3 :: Nil = Enum(UInt(), 4)
    val currStateWr = Reg(init = idle)
    val nextStateWr = Wire(UInt())
    nextStateWr := currStateWr
    currStateWr := nextStateWr

    wdataSplit.valid := Bool(false)
    when(in.w.fire() && currStateWr === idle && wdataSplit.ready){
      wdataReg := in.w.bits.data
      wdataSplit.bits.data := in.w.bits.data(in.params.dataBits/4 - 1, 0)
      wdataSplit.valid := Bool(true)
      nextStateWr := enq1
    }.elsewhen(currStateWr === enq1 && wdataSplit.ready){
      wdataSplit.bits.data := wdataReg(in.params.dataBits/2 -1 , in.params.dataBits/4)
      wdataSplit.valid := Bool(true)
      nextStateWr := enq2
    }.elsewhen(currStateWr === enq2 && wdataSplit.ready){
      wdataSplit.bits.data := wdataReg(in.params.dataBits * 3 /4 -1 , in.params.dataBits/2)
      wdataSplit.valid := Bool(true)
      nextStateWr := enq3
    }.elsewhen(currStateWr === enq3 && wdataSplit.ready){
      wdataSplit.bits.data := wdataReg(in.params.dataBits -1 , in.params.dataBits * 3 /4)
      wdataSplit.valid := Bool(true)
      nextStateWr := idle
    }

    val rdata0 = Reg(init = UInt(0, width = in.params.dataBits / 4))
    val rdata1 = Reg(init = UInt(0, width = in.params.dataBits / 4))
    val rdata2 = Reg(init = UInt(0, width = in.params.dataBits / 4))
    val currStateRd = Reg(init = idle)
    val nextStateRd = Wire(UInt())
    nextStateRd := currStateRd
    currStateRd := nextStateRd

    rdataSplit.valid := Bool(false)

    when(mem.module.io.memResp.fire() && currStateRd === idle){
      rdata0 := mem.module.io.memResp.bits.data
      nextStateRd := enq1
    }.elsewhen(mem.module.io.memResp.fire() && currStateRd === enq1){
      rdata1 := mem.module.io.memResp.bits.data
      nextStateRd := enq2
    }.elsewhen(mem.module.io.memResp.fire() && currStateRd === enq2){
      rdata2 := mem.module.io.memResp.bits.data
      nextStateRd := enq3
    }.elsewhen(mem.module.io.memResp.fire() && currStateRd === enq3){
      rdataSplit.bits.data := Cat(mem.module.io.memResp.bits.data, rdata2, rdata1, rdata0)
      rdataSplit.valid := Bool(true)
      rdataSplit.bits.tag := mem.module.io.memResp.bits.tag
      nextStateRd := idle
    }

    mem.module.io.params.tRAS := UInt(4)
    mem.module.io.params.tRP := UInt(4)
    mem.module.io.params.tRCD := UInt(4)
    mem.module.io.params.tRP := UInt(4)
    mem.module.io.params.tCCD := UInt(4)
    mem.module.io.params.tRTP := UInt(4)
    mem.module.io.params.tWTR := UInt(4)
    mem.module.io.params.tWR := UInt(4)
    mem.module.io.params.tRRD := UInt(4)

    def quickMult( x : UInt, y : UInt) : UInt = {
      (x <<  Mux(y(4), UInt(4), UInt(0) ) )+ (x << Mux(y(3), UInt(3), UInt(0))) + (x << Mux(y(2), UInt(2), UInt(0))) + (x << Mux(y(1), UInt(1), UInt(0))) + Mux(y(0), x, UInt(0))
    }

    mem.module.io.memResp.ready := in.r.ready
    in.ar.ready := cmdSplit.ready
    in.aw.ready := cmdSplit.ready
    in.w.ready := wdataSplit.ready && currStateWr === idle

    in.b.valid := cmd.fire() & cmd.bits.rw
    in.b.bits.id := cmd.bits.tag(in.params.idBits -1 ,0)
    in.b.bits.resp := UInt(0)
    in.b.bits.user.foreach { _ := cmd.bits.tag(in.params.idBits) }
    rdata.ready := in.r.ready
    in.r.valid := rdata.valid
    in.r.bits.id   := rdata.bits.tag(in.params.idBits-1 ,0 )
    in.r.bits.resp := UInt(0)
    in.r.bits.data := rdata.bits.data
    in.r.bits.last := rdata.valid
    in.r.bits.user.foreach { _ := rdata.bits.tag(in.params.idBits)}
  }
}

object AXI4DDR
{
  def apply(
    address: AddressSet,
    executable: Boolean = true,
    beatBytes: Int = 4,
    devName: Option[String] = None,
    errors: Seq[AddressSet] = Nil)
  (implicit p: Parameters) =
  {
    val axi4ram = LazyModule(new AXI4DDR(address, executable, beatBytes, devName, errors))
    axi4ram.node
  }
}
