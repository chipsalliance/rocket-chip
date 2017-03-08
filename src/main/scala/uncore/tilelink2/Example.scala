// See LICENSE.SiFive for license details.

package uncore.tilelink2

import Chisel._
import config._
import diplomacy._
import regmapper._

/** This is an example of how to create and test a simple
  * TL slave device that uses the regmapper library to
  * manage access to its registers.
  */

case class ExampleParams(num: Int, address: BigInt)

trait ExampleBundle
{
  val params: ExampleParams
  val gpio = UInt(width = params.num)
}

trait ExampleModule extends HasRegMap
{
  val params: ExampleParams
  val io: ExampleBundle
  val interrupts: Vec[Bool]

  val state = RegInit(UInt(0))
  val pending = RegInit(UInt(0xf, width = 4))

  io.gpio := state
  interrupts := pending.toBools

  regmap(
    0 -> Seq(
      RegField(params.num, state)),
    4 -> Seq(
      RegField.w1ToClear(4, pending, state)))
}

// Create a concrete TL2 version of the abstract Example slave
class TLExample(params: ExampleParams)(implicit p: Parameters)
  extends TLRegisterRouter(params.address, "somedev", Seq("ucbbar,random-interface"), 4)(
  new TLRegBundle(params, _)    with ExampleBundle)(
  new TLRegModule(params, _, _) with ExampleModule)

////////

/** This is an example of how to create a blackboxed, register-mapped slave device
  * that also acts as a TL2 master.
  */

import uncore.devices._

case class EwMaSAParams(
    numRegs: Int = 8,
    regWidth: Int = 32,
    numInterrupts: Int = 4,
    baseSlaveAddress: BigInt    = 0x05200000L,
    targetMasterAddress: BigInt = 0x80000000L) {
  def size: Int = regWidth * numRegs
}

class ExampleWithMasterAndSlaveAgents(params: EwMaSAParams)(implicit p: Parameters) extends LazyModule {
  // For DTS printing
  val device = new SimpleDevice("example", Seq("riscv,example0"))

  // Slave agent node containing memory-mapped control registers
  val regNode = TLRegisterNode(
    address = AddressSet(params.baseSlaveAddress, params.size-1),
    device = device, "reg",
    beatBytes = params.regWidth)

  // Interrupt agent node
  val intNode = IntSourceNode(IntSourcePortSimple(
    num = params.numInterrupts,
    resources = Seq(Resource(device, "int"))))

  // Master agent node, only has one outstanding transaction
  val masterNode = TLClientNode(TLClientParameters())

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val int    = intNode.bundleOut
      val slave  = regNode.bundleIn
      val master = masterNode.bundleOut
    }

    /** Interrupt interface */

    // Internal state
    val pending = RegInit(UInt(0, width = params.numInterrupts))

    // Mapping to interrupt ios
    io.int.zipWithIndex.foreach { case (int, i) => int := pending(i) }

    /** Slave interface */

    // Internal state
    val regs = Seq.fill(params.numRegs)(Reg(init=UInt(0, width = params.regWidth)))

    // Simple register map is all the same width
    def makeRegFields(s: Seq[UInt]) = s.map(r => RegField(params.regWidth, r))
    regNode.regmap(0 -> makeRegFields(regs))

    /** Master interface */

    // Useful helper functions can be found on outgoing edge and bundle
    val out = io.master(0)
    val edge = masterNode.edgesOut(0)
    val maxTransfer = edge.manager.maxTransfer
    val beatBytes = edge.manager.beatBytes
    val dataBits = edge.bundle.dataBits

    // Progress within each operation
    val a = out.a.bits
    val (a_first, a_last, req_done) = edge.firstlast(out.a)
    val d = out.d.bits
    val (d_first, d_last, resp_done) = edge.firstlast(out.d)
    val inflight = Reg(init = Bool(false))
    when(req_done)  { inflight := Bool(true) }
    when(resp_done) { inflight := Bool(false) }

    // Contents of the outgoing operation
    val src = UInt(0) // Only a single in-flight transaction
    val addr = UInt(params.targetMasterAddress) // Where we are writing to
    val size = UInt(4) // Log2(16) = 4
    val data = LFSR16Seed(0) // 16 bits of noise
    
    // Generate a PutFull channel A operation
    val (pflegal, pfbits) = edge.Put(src, addr, size, data)
    out.a.valid := pflegal && edge.manager.containsSafe(addr) && !inflight
    out.a.bits  := pfbits

    // We're always ready to accept an ack on D
    out.d.ready := Bool(true)

    // Tie-off unused channels
    out.b.ready := Bool(true)
    out.c.valid := Bool(false)
    out.e.valid := Bool(false)
  }
}
