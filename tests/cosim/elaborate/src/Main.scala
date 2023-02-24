package cosim.elabotate

import chisel3.aop.Select
import chisel3.aop.injecting.InjectingAspect
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.{CIRCTTarget, CIRCTTargetAnnotation, ChiselStage, FirtoolOption}
import firrtl.options.TargetDirAnnotation
import firrtl.{AnnotationSeq, ChirrtlEmitter, EmitAllModulesAnnotation}
import freechips.rocketchip.devices.debug.DebugModuleKey
import freechips.rocketchip.diplomacy.MonitorsEnabled
import freechips.rocketchip.subsystem.{CacheBlockBytes, SystemBusKey, SystemBusParams}
import mainargs._
import org.chipsalliance.cde.config.{Config, Field}
import freechips.rocketchip.rocket.{DCacheParams, FrontendModule, ICacheModule, ICacheParams, MulDivParams, Rocket, RocketCoreParams}
import freechips.rocketchip.tile.RocketTileParams



object RocketTileParamsKey extends Field[RocketTileParams]

object Main {
  @main
  def elaborate(
                 @arg(name = "dir") dir: String,
               ): Unit = {
    (new ChiselStage).transform(AnnotationSeq(Seq(
      TargetDirAnnotation(dir),
      new ChiselGeneratorAnnotation(() => {
        new DUT(
          new Config((site, here, up) => {
            case MonitorsEnabled => false
            case freechips.rocketchip.tile.XLen => 64
            case freechips.rocketchip.tile.XLen => 64
            case freechips.rocketchip.tile.MaxHartIdBits => 4
            case freechips.rocketchip.tile.MaxHartIdBits => 4
            case freechips.rocketchip.rocket.PgLevels => if (site(freechips.rocketchip.tile.XLen) == 64) 3 else 2
            case freechips.rocketchip.rocket.PgLevels => if (site(freechips.rocketchip.tile.XLen) == 64) 3 else 2
            case RocketTileParamsKey => RocketTileParams(
              core = RocketCoreParams(mulDiv = Some(MulDivParams(
                mulUnroll = 8,
                mulEarlyOut = true,
                divEarlyOut = true))),
              dcache = Some(DCacheParams(
                rowBits = site(SystemBusKey).beatBits,
                nMSHRs = 0,
                blockBytes = site(CacheBlockBytes))),
              icache = Some(ICacheParams(
                rowBits = site(SystemBusKey).beatBits,
                blockBytes = site(CacheBlockBytes))))
            case SystemBusKey => SystemBusParams(
              beatBytes = site(freechips.rocketchip.tile.XLen) / 8,
              blockBytes = site(CacheBlockBytes))
            case DebugModuleKey => None
          })
        )
      }),
      firrtl.passes.memlib.InferReadWriteAnnotation,
      CIRCTTargetAnnotation(CIRCTTarget.Verilog),
      EmitAllModulesAnnotation(classOf[ChirrtlEmitter]),
      FirtoolOption("--disable-annotation-unknown"),
      InjectingAspect(
        { dut: DUT => Select.collectDeep(dut){ case icache : ICacheModule => icache } },
        {
          icache : ICacheModule =>
            chisel3.experimental.Trace.traceName(icache.s2_miss)
        }
      )
    )))
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
