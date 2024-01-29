// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.interrupts.{IntSourceNode, IntSourcePortSimple}

case class APBRegisterNode(address: AddressSet, concurrency: Int = 0, beatBytes: Int = 4, undefZero: Boolean = true, executable: Boolean = false)(implicit valName: ValName)
  extends SinkNode(APBImp)(Seq(APBSlavePortParameters(
    Seq(APBSlaveParameters(
      address       = Seq(address),
      executable    = executable,
      supportsWrite = true,
      supportsRead  = true)),
    beatBytes  = beatBytes)))
{
  require (address.contiguous)

  // Calling this method causes the matching APB bundle to be
  // configured to route all requests to the listed RegFields.
  def regmap(mapping: RegField.Map*) = {
    val (apb, _) = this.in(0)

    val indexBits = log2Up((address.mask+1)/beatBytes)
    val params = RegMapperParams(indexBits, beatBytes)
    val in = Wire(Decoupled(new RegMapperInput(params)))
    val out = RegMapper(beatBytes, concurrency, undefZero, in, mapping:_*)

    // Only send the request to the RR once
    val taken = RegInit(false.B)
    when (in.fire)  { taken := true.B  }
    when (out.fire) { taken := false.B }

    in.bits.read  := !apb.pwrite
    in.bits.index := apb.paddr >> log2Ceil(beatBytes)
    in.bits.data  := apb.pwdata
    in.bits.mask  := Mux(apb.pwrite, apb.pstrb, ((1<<beatBytes) - 1).U)

    in.valid := apb.psel && !taken
    out.ready := apb.penable

    apb.pready  := out.valid
    apb.pslverr := false.B
    apb.prdata  := out.bits.data
  }
}

/** Mix this trait into a RegisterRouter to be able to attach its register map to an AXI4 bus */
trait HasAPBControlRegMap { this: RegisterRouter =>
  // Externally, this node should be used to connect the register control port to a bus
  val controlNode = APBRegisterNode(
    address = address.head,
    concurrency = concurrency,
    beatBytes = beatBytes,
    undefZero = undefZero,
    executable = executable)

  // Backwards-compatibility default node accessor with no clock crossing
  lazy val node: APBInwardNode = controlNode

  // Internally, this function should be used to populate the control port with registers
  protected def regmap(mapping: RegField.Map*): Unit = { controlNode.regmap(mapping:_*) }
}
