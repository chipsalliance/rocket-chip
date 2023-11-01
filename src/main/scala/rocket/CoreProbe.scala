package freechips.rocketchip.rocket

import chisel3._
import chisel3.probe._
import chisel3.util.Valid
import chisel3.util.experimental.decode.DecodeBundle

/** Instantiate all probe signal for the [[Rocket]] core
  * we don't use boring utils, thus all reference must be accessible in the rocket core.
  */
class CoreProbe(rocket: Rocket) {
  class RFEvent extends Bundle {
    val address = UInt(5.W)
    val data = UInt(rocket.xLen.W)
    val isWrite = Bool()
  }

  class ID extends Bundle {
    def decoder = rocket.rocketImpl.decoder
    val pc = UInt(rocket.vaddrBitsExtended.W)
    val instruction = UInt(32.W)
    val result: DecodeBundle = chisel3.chiselTypeOf(rocket.rocketImpl.decoderModule.output)
  }

  // IF Stage
  //   - Send IF Request to frontend
  class IFRequestEvent extends Bundle {
    val pc = UInt(rocket.vaddrBitsExtended.W)
    // TODO: add IF request type event:
    //       - exception
    //       - [m|s]ret
    //       - replay
    //       - flush or branch misprediction
  }
  val ifRequestEvent = IO(Output(Probe(Valid(new IFRequestEvent))))
  define(ifRequestEvent.valid, ProbeValue(rocket.io.imem.req.valid))
  define(ifRequestEvent.bits.pc, ProbeValue(rocket.io.imem.req.bits.pc))

  // ID Stage:
  //   - Got IF response(latched);
  //   - send to IBUF(RVCExpander);
  //   - Got ID result
  //   - read RF1 RF2
  class IFResponseEvent extends Bundle {
    val pc = UInt(rocket.vaddrBitsExtended.W)
    // data is always 32 in the rocket core, if enabling compressed instructions, mask is 2, otherwise 1
    val data = UInt(32.W)
    val mask = UInt((if(rocket.usingCompressed)2 else 1).W)
    // TODO: add IF request type event:
    //       - exception
    //       - [m|s]ret
    //       - replay
    //       - flush or branch misprediction
  }
  val ifResponseEvent: Valid[IFResponseEvent] = IO(Output(Probe(Valid(new IFResponseEvent))))
  define(ifResponseEvent.valid,
    ProbeValue(
      rocket.io.imem.resp.valid &&
        !rocket.io.imem.resp.bits.replay &&
        !rocket.io.imem.resp.bits.xcpt.ae.inst &&
        !rocket.io.imem.resp.bits.xcpt.pf.inst &&
        !rocket.io.imem.resp.bits.xcpt.gf.inst
    )
  )
  define(ifResponseEvent.bits.data, ProbeValue(rocket.io.imem.resp.bits.data))
  define(ifResponseEvent.bits.mask, ProbeValue(rocket.io.imem.resp.bits.mask))

  class IDEvent extends Bundle {
    val decode: ID = new ID
    val compressed: Bool = Bool()
    // TODO: add id_xcpt
  }
  val idEvent = IO(Output(Probe(Valid(new IDEvent))))
  define(idEvent.valid,
    ProbeValue(
      !rocket.rocketImpl.id_illegal_insn &&
        !rocket.rocketImpl.ibuf.io.kill
      // TODO: ctrl_killd?
    )
  )
  define(idEvent.bits.decode.pc, ProbeValue(rocket.rocketImpl.ibuf.io.pc))
  define(idEvent.bits.decode.instruction, ProbeValue(rocket.rocketImpl.id_inst(0)))
  define(idEvent.bits.decode.result, ProbeValue(rocket.rocketImpl.id_ctrl))
  define(idEvent.bits.compressed, ProbeValue(rocket.rocketImpl.ibuf.io.inst(0).bits.rvc))

  val rs1ReadEvent = IO(Output(Probe(Valid(new RFEvent))))
  define(rs1ReadEvent.valid, ProbeValue(rocket.rocketImpl.id_ren(0)))
  define(rs1ReadEvent.bits.address, ProbeValue(rocket.rocketImpl.id_raddr(0)))
  define(rs1ReadEvent.bits.data, ProbeValue(rocket.rocketImpl.id_rs(0)))
  define(rs1ReadEvent.bits.isWrite, ProbeValue(false.B))

  val rs2ReadEvent = IO(Output(Probe(Valid(new RFEvent))))
  define(rs2ReadEvent.valid, ProbeValue(rocket.rocketImpl.id_ren(1)))
  define(rs2ReadEvent.bits.address, ProbeValue(rocket.rocketImpl.id_raddr(1)))
  define(rs2ReadEvent.bits.data, ProbeValue(rocket.rocketImpl.id_rs(1)))
  define(rs2ReadEvent.bits.isWrite, ProbeValue(false.B))

  // EXE Stage

  // MEM Stage

  // WB Stage

  // OoO WB Stage
  // FP Stage
}
