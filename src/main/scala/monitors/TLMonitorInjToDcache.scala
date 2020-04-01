// See LICENSE.SiFive for license details.

package freechips.rocketchip.monitors

import chisel3.aop.injecting._

import chisel3.experimental.{BaseModule, FixedPoint}
import chisel3.{RawModule, VecInit}

import chisel3.aop.injecting.InjectingAspect
import chisel3.aop._
import chisel3._
import firrtl.options.TargetDirAnnotation
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.stage.{ConfigsAnnotation, TopModuleAnnotation}
//import freechips.rocketchip.system.{RocketChipStage, TestHarness}
import freechips.rocketchip.system._
import chisel3.internal.firrtl._
//import freechips.rocketchip.util.HasRocketChipStageUtils
import freechips.rocketchip.util.SelectDiplomacy
import freechips.rocketchip.tilelink._
import freechips.rocketchip.formal._
import freechips.rocketchip.diplomacy._
import scala.reflect.ClassTag

/** API to access Diplomacy Node data:
  *  {{{
  *     val baseNodes = SelectDiplomacy.collectBaseNodes()
  *     val impMod = SelectDiplomacy.collectImpModules()
  *     val lazyMod = SelectDiplomacy.collectLazyModules()
  * 
  *     val mixedNodes = SelectDiplomacy.mixedNodes()
  *     val mixedCustomNodes = SelectDiplomacy.mixedCustomNodes()
  *     val customNodes = SelectDiplomacy.customNodes()
  *     val junctionNodes = SelectDiplomacy.junctionNodes()
  *     val mixedAdapterNodes = SelectDiplomacy.mixedAdapterNodes()
  *     val adapterNodes = SelectDiplomacy.adapterNodes()
  *     val identityNodes = SelectDiplomacy.identityNodes()
  *     val ephemeralNodes = SelectDiplomacy.ephemeralNodes()
  *     val mixedNexusNodes = SelectDiplomacy.mixedNexusNodes()
  *     val nexusNodes = SelectDiplomacy.nexusNodes()
  *     val sourceNodes = SelectDiplomacy.sourceNodes()
  *     val sinkNodes = SelectDiplomacy.sinkNodes()
  *     val mixedTestNodes = SelectDiplomacy.mixedTestNodes()
  * 
  *     val csNode = getSrcNodes[DCache]()
  *     val cmNode = getManagementNodes[DCache()
  *  }}}
 */

case class AOPTestModule (wide: Int) extends Module {
  val io = IO(new Bundle {
    val inc = Input(Bool())
    val random = Output(UInt(wide.W))
  })
  io.random := 0.U
}

case object TLMonitorInjToDcache extends InjectorAspect[RawModule, DCacheModule](
  {top: RawModule => Select.collectDeep(top) { case d: DCacheModule => d }},
  {d: DCacheModule => 
    // attach TLMonitor here   
    printf("TLMonitorInjToDcache: message from the object for .v file")
    val dummyWire = Wire(UInt(3.W)).suggestName("aopTestWire")
    dummyWire := 5.U
    dontTouch(dummyWire)

    println(s"DEBUG from object TLMonitorInjToDcache test moule inject")
    val aopTestMod = Module(new AOPTestModule(16))
    aopTestMod.io.inc := 1.U
    dontTouch(aopTestMod.io.inc)

    println(s"DEBUG from object TLMonitorInjToDcache TLMonitor inject")
    val clientSrcNodes = SelectDiplomacy.getSrcNodes[DCache]()

     println(s"DEBUG from object TLMonitorInjToDcache TLMonitor inject")
    val clientNode = SelectDiplomacy.clientTLNode("dcache.node").head
   val (bundle, edge) = clientNode.out.head
   
   val args = TLMonitorArgs(edge)
  
    val aopTLMon = Module(new TLMonitor(args))
    aopTLMon.io.in := bundle
  })


