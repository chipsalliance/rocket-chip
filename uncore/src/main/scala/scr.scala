package uncore

import Chisel._
import junctions.{SmiIO, MMIOBase}
import cde.Parameters
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

/** Stores a map between SCR file names and address in the SCR file, which can
  * later be dumped to a header file for the test bench. */
class SCRFileMap(prefix: String, maxAddress: Int, baseAddress: BigInt) {
  private val addr2name = HashMap.empty[Int, String]
  private val name2addr = HashMap.empty[String, Int]

  def allocate(address: Int, name: String): Int = {
    Predef.assert(!addr2name.contains(address), "address already allocated")
    Predef.assert(!name2addr.contains(name), "name already allocated")
    Predef.assert(address < maxAddress, "address too large")
    addr2name += (address -> name)
    name2addr += (name -> address)
    println(prefix + ": %x -> ".format(baseAddress + address) + name)
    address
  }

  def allocate(name: String): Int = {
    val addr = (0 until maxAddress).filter{ addr => !addr2name.contains(addr) }(0)
    allocate(addr, name)
  }

  def as_c_header(): String = {
    addr2name.map{ case(address, name) =>
      "#define " + prefix + "__" + name + "__PADDR  0x%x".format(baseAddress + address)
      "#define " + prefix + "__" + name + "__OFFSET 0x%x".format(address)
    }.mkString("\n") + "\n"
  }
}

class SCRIO(map: SCRFileMap)(implicit p: Parameters) extends HtifBundle()(p) {
  val rdata = Vec(nSCR, Bits(INPUT, scrDataBits))
  val wen = Bool(OUTPUT)
  val waddr = UInt(OUTPUT, log2Up(nSCR))
  val wdata = Bits(OUTPUT, scrDataBits)

  def attach(regs: Seq[Data], name_base: String): Seq[Data] = {
    regs.zipWithIndex.map{ case(reg, i) => attach(reg, name_base + "__" + i) }
  }

  def attach(reg: Data, name: String): Data = {
    val addr = map.allocate(name)
    when (wen && (waddr === UInt(addr))) {
      reg := wdata
    }
    rdata(addr) := reg
    reg
  }

  def allocate(address: Int, name: String): Unit = {
    map.allocate(address, name)
  }
}

class SCRFile(prefix: String, baseAddress: BigInt)(implicit p: Parameters) extends HtifModule()(p) {
  val map = new SCRFileMap(prefix, 64, baseAddress)
  AllSCRFiles += map

  val io = new Bundle {
    val smi = new SmiIO(scrDataBits, scrAddrBits).flip
    val scr = new SCRIO(map)
  }

  val scr_rdata = Wire(Vec(io.scr.rdata.size, Bits(width=scrDataBits)))
  for (i <- 0 until scr_rdata.size)
    scr_rdata(i) := io.scr.rdata(i)

  val read_addr = Reg(init = UInt(0, scrAddrBits))
  val resp_valid = Reg(init = Bool(false))

  io.smi.req.ready := !resp_valid
  io.smi.resp.valid := resp_valid
  io.smi.resp.bits := scr_rdata(read_addr)

  io.scr.wen := io.smi.req.fire() && io.smi.req.bits.rw
  io.scr.wdata := io.smi.req.bits.data
  io.scr.waddr := io.smi.req.bits.addr

  when (io.smi.req.fire()) {
    read_addr := io.smi.req.bits.addr
    resp_valid := Bool(true)
  }
  when (io.smi.resp.fire()) { resp_valid := Bool(false) }
}

/** Every elaborated SCR file ends up in this global arry so it can be printed
  * out later. */
object AllSCRFiles {
  private var maps = ArrayBuffer.empty[SCRFileMap]

  def +=(map: SCRFileMap): Unit = { maps += map }
  def foreach( f: (SCRFileMap => Unit) ): Unit = { maps.foreach{ m => f(m) } }
}
