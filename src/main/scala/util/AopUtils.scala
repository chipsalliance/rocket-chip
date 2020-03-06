// See LICENSE.SiFive for license details.

package freechips.rocketchip.aoputil

import java.io.{File, FileWriter}
import scala.util.matching.Regex

import chisel3.experimental.{BaseModule, FixedPoint}
import chisel3.{RawModule, VecInit}

import chisel3.aop.injecting.InjectingAspect
import chisel3._
import firrtl.options.TargetDirAnnotation
import freechips.rocketchip.rocket._
import freechips.rocketchip.stage.{ConfigsAnnotation, TopModuleAnnotation}
import freechips.rocketchip.system.{RocketChipStage, TestHarness}
import chisel3.internal.firrtl._
import freechips.rocketchip.util.HasRocketChipStageUtils

trait AopCallBase
case object ShowModules extends AopCallBase
case class ShowModulePorts(moduleInstName: String) extends AopCallBase

//case class ModulePutMonitor(m: BaseModule => component) extends AopCallBase

case class AopInjectMonitor(modName: Regex, monitor: String, io: String) extends AopCallBase

case class InjectingMonitor[T <: BaseModule, M <: BaseModule](m: T => M)

object InjectModules {

  def monInject[T](collector: PartialFunction[BaseModule, T]): Unit = {
//  def monInject[T](collector: PartialFunction[BaseModule, T]): Iterable[T] = {
//    items.collect{case m: DCache => println(s"SULTAN DCache: ${m.getClass.getName}")}
    println(s"SULTAN partial func: ${collector.getClass.getName}")
//    circuit.components.collect{collector}
  }

  def monFind(a: BaseModule) = {
    println(s"SULTAN monFind: ${a.getClass.getName}")
  }

//  ModuleToPutMonitor(case a: Dcache => a)

  var injItems: Seq[AopCallBase] = Seq[AopCallBase]()

  def register(items: Seq[AopCallBase]) = {
    items.collect{case i: AopCallBase => println(s"SULTAN info register ${i.getClass.getName}")}
    injItems = items
  }

//  def getIrrevocables(io: Data): Seq[IrrevocableIO[_]] = {
//    io match {
//      case a: IrrevocableIO[_] => Seq(a)
//      case x: Aggregate => x.getElements.flatMap(getIrrevocables)
//      case other => Nil
//    }
//  }

//  def getModByName(pat: Regex, circuit: Circuit) = {
//    val mod = circuit.components.collect{c => 
//      c.name match {
//        case pat(a) => c
//      }
//    }
//    mod.map(_.id)
//  }
//
//  def getModByType(t: Class[_], circuit: Circuit) = {
//    val mod = circuit.components.collect{
//      case c => println(s"SULTAN TYPE CHECK c: ${c.getClass.getName}"); c
//    }
//    mod.map(_.id)
//  }

//  var circuit = Null

  def inj(c: Circuit) = {
    println(s"SULTAN info in inj method")
//    circuit = c
//
//    injItems.foreach{
//      case ShowModules =>
//        circuit.components.foreach{a => println(s"COMPONENT ${a.name}\t\t${a.getClass.getName}")}
//      case ShowModulePorts(a) =>
//        val m = circuit.components.foreach{b =>
//          if(b.name == a)
//            Select.ios(b.id).foreach{i => println(s"COMP IOS\t\t${i}")}
//        }
//      case AopInjectMonitor(ins, m, p) =>
//        println(s"INJECT MONITOR ins ${ins} monitor ${m} port ${p}")
//
        val modInjAspect = InjectingAspect(
          {dut: TestHarness => Seq(dut.dut)},
          {dut: freechips.rocketchip.system.ExampleRocketSystemModuleImp[freechips.rocketchip.system.ExampleRocketSystem] =>
            val dummyWire = Wire(UInt(3.W)).suggestName("hello")
            dummyWire := 5.U
            dontTouch(dummyWire)
          })

        val dirName = System.getProperty("user.dir") + "/emulator"
        println(s"SULTAN dirName: ${dirName}")

//        val dir = new File(dirName)
//        if (!dir.exists()) dir.mkdirs()

        new RocketChipStage().run(Seq(
          new TargetDirAnnotation(dirName),
          new TopModuleAnnotation(Class.forName("freechips.rocketchip.system.TestHarness")),
          new ConfigsAnnotation(Seq("freechips.rocketchip.system.DefaultConfig")),
          modInjAspect
        ))

/***********
        val mods = getModByName(ins, circuit)
//        mods.foreach{a => println(s"SULTAN FIND_MODULE ${a}")}
        InjectingAspect(
          //            (dc: DCache) => Select.collectDeep(dc) { case i: RawModule if Select.ios(i).flatMap(getIrrevocables).nonEmpty => i },
          (th:DCache) => getModByName(ins, circuit).collect{case i: RawModule if Select.ios(i).flatMap(getIrrevocables).nonEmpty => i },
          //      (t: DefModule) => tgts.collect{case i: RawModule if Select.ios(i).flatMap(getIrrevocables).nonEmpty => i },
          (instance: RawModule) => {
            val irrevocablePorts = Select.ios(instance).flatMap(getIrrevocables)
            irrevocablePorts.foreach { irrevocable: IrrevocableIO[_] =>
              // Inject logic to calculate irrevocable condition
              val canDeassert = RegInit(true.B)

              when(irrevocable.ready) {
                canDeassert := false.B
              }
              when(irrevocable.valid) {
                canDeassert := true.B
              }
              assert(~(~irrevocable.ready & canDeassert), s"ERROR! SULTAN Irrevocable IO ${irrevocable.toTarget.serialize} deasserted ready prior to valid handshake!")
            }
          })
 */
//    }
  }

}

object AopInjModules extends HasRocketChipStageUtils {

  println(s"SULTAN AopInjModule")
  
  injTest({case i:Int => i.toDouble})

  injTest1({()=>
    {case i:Int => i.toDouble}
  })

  injTest2({()=>
    {println("SULTAN from caller")}
  })

  injMonitor({case a: DCache => true})
}

//  InjectModules.register(Seq(
//    ShowModules,
//    ShowModulePorts("DCache"),
//    AopInjectMonitor("DCache$".r, "TLMonitor", "HellaCacheBundle")
//  ))

// AopInjectMonitorChild("DCache$".r, "TLMonitor", "HellaCacheBundle", <child-name>)
// AopInjectMonitorParent("DCache$".r, "TLMonitor", "HellaCacheBundle", <parent-name>)
// AopInjectMonitorAncestor("DCache$".r, "TLMonitor", "HellaCacheBundle", <ancestor-name>)
// AopInjectMonitorDescendant("DCache$".r, "TLMonitor", "HellaCacheBundle".<descendant-name>)

