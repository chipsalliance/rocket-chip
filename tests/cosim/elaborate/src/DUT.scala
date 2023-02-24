package cosim.elabotate

import chisel3._
import freechips.rocketchip.diplomacy.{AddressSet, BundleBridgeSource, InModuleBody, LazyModule, RegionType, SimpleLazyModule, TransferSizes}
import freechips.rocketchip.interrupts.{IntSinkNode, IntSinkPortSimple, IntSourceNode, IntSourcePortSimple}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink.{TLManagerNode, TLSlaveParameters, TLSlavePortParameters}
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile.{NMI, PriorityMuxHartIdFromSeq, RocketTile}

class DUT(p: Parameters) extends Module {
  implicit val implicitP = p
  val tileParams = p(RocketTileParamsKey)
  val ldut = LazyModule(new SimpleLazyModule {
    implicit val implicitP = p
    val rocketTile = LazyModule(new RocketTile(tileParams, RocketCrossingParams(), PriorityMuxHartIdFromSeq(Seq(tileParams))))
    val masterNode = TLManagerNode(Seq(TLSlavePortParameters.v1(
      Seq(TLSlaveParameters.v1(
        address = List(AddressSet(0x0, 0xffffffffL)),
        regionType = RegionType.UNCACHED,
        executable = true,
        supportsGet = TransferSizes(1, 64),
        supportsAcquireT = TransferSizes(1, 64),
        supportsAcquireB = TransferSizes(1, 64),
        supportsPutPartial = TransferSizes(1, 64),
        supportsPutFull = TransferSizes(1, 64),
        supportsLogical = TransferSizes(1, 64),
        supportsArithmetic = TransferSizes(1, 64),
        fifoId = Some(0))),
      beatBytes = 8,
      endSinkId = 4,
      minLatency = 1
    )))
    masterNode :=* rocketTile.masterNode
    val memory = InModuleBody {
      masterNode.makeIOs()
    }

    val intNode = IntSourceNode(IntSourcePortSimple())
    rocketTile.intInwardNode :=* intNode
    val intIn = InModuleBody {
      intNode.makeIOs()
    }

    val haltNode = IntSinkNode(IntSinkPortSimple())
    haltNode :=* rocketTile.haltNode
    val haltOut = InModuleBody {
      haltNode.makeIOs()
    }

    val ceaseNode = IntSinkNode(IntSinkPortSimple())
    ceaseNode :=* rocketTile.ceaseNode
    val ceaseOut = InModuleBody {
      ceaseNode.makeIOs()
    }

    val wfiNode = IntSinkNode(IntSinkPortSimple())
    wfiNode :=* rocketTile.wfiNode
    val wfiOut = InModuleBody {
      wfiNode.makeIOs()
    }
    val resetVectorNode = BundleBridgeSource(() => UInt(32.W))
    rocketTile.resetVectorNode := resetVectorNode
    val resetVector = InModuleBody {
      resetVectorNode.makeIO()
    }
    val hartidNode = BundleBridgeSource(() => UInt(4.W))
    rocketTile.hartIdNode := hartidNode
    InModuleBody {
      hartidNode.bundle := 0.U
    }
    val nmiNode = BundleBridgeSource(Some(() => new NMI(32)))
    rocketTile.nmiNode := nmiNode
    val nmi = InModuleBody {
      nmiNode.makeIO()
    }
  })
  chisel3.experimental.DataMirror.fullModulePorts(
    // instantiate the LazyModule
    Module(ldut.module)
  ).filterNot(_._2.isInstanceOf[Aggregate]).foreach { case (name, ele) =>
    if (!(name == "clock" || name == "reset")) {
      chisel3.experimental.DataMirror.directionOf(ele) match {
        case ActualDirection.Output =>
          val io = IO(Output(chiselTypeOf(ele))).suggestName(name)
          println(s"output $name")
          io := ele
        case ActualDirection.Input =>
          val io = IO(Input(chiselTypeOf(ele))).suggestName(name)
          println(s"input $name")
          ele := io
      }
    }
  }
}
