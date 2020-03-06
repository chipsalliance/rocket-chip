// See LICENSE.SiFive for license details.

package freechips.rocketchip.monitors

import chisel3.aop.injecting._

import chisel3.experimental.{BaseModule, FixedPoint}
import chisel3.{RawModule, VecInit}

import chisel3.aop.injecting.InjectingAspect
import chisel3.aop._
import chisel3._
import firrtl.options.TargetDirAnnotation
import freechips.rocketchip.rocket._
import freechips.rocketchip.stage.{ConfigsAnnotation, TopModuleAnnotation}
import freechips.rocketchip.system.{RocketChipStage, TestHarness}
import chisel3.internal.firrtl._
import freechips.rocketchip.util.HasRocketChipStageUtils
import freechips.rocketchip.tilelink._

case class DummyCode () extends RawModule {
  val results: Seq[Int] = Seq[Int](0, 1, 2, 3, 4, 5, 6)
  val values = VecInit(results.map(_.U))
  val counter = RegInit(0.U(results.length.W))
  counter := counter + 1.U
  when(counter >= values.length.U) {
    stop()
  }.otherwise {
//    when(reset.asBool() === false.B) {
      printf("values(%d) = %d\n", counter, values(counter))
      assert(counter === values(counter))
//    }
  }
}


case object TLMonitorInjToDcache extends InjectorAspect[RawModule, DCacheModule](
  {top: RawModule => Select.collectDeep(top) { case d: DCacheModule => d }},
  {d: DCacheModule => 
    // attach TLMonitor here
    printf("SULTAN from object TLMonitorInjToDcache")
    val dummyWire = Wire(UInt(3.W)).suggestName("hello")
    dummyWire := 5.U
    dontTouch(dummyWire)
  }
)
//case object TLMonitorInjToDcache extends InjectorAspect (
//  {top: RawModule => Select.collectDeep(top) { case d: DCache => d }},
//  {d: DCache =>  
//    // attach TLMonitor here
//    println(s"SULTAN from object TLMonitorInjToDcache")
//  })

//{


//  injMonitor(mon:       {case a: TLMoniotr => true})
//            (whereToGo: {case a: DCache => true}),
//            (port:      {case a: HellaCacheBundle => true})

//(
//    selectRoots: TLMonitor => Iterable[DCache],
//    injection: TLMonitor => Unit
//)
//

// (
//    selectRoots: TLMonitor => Iterable[DCache],
//    injection: TLMonitor => Unit)
