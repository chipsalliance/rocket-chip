// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3.aop.injecting.InjectingAspect
import chisel3.experimental.{BaseModule, FixedPoint}
import chisel3._
import firrtl._
import firrtl.options.TargetDirAnnotation
import freechips.rocketchip.stage.{ConfigsAnnotation, TopModuleAnnotation}
import freechips.rocketchip.system.{RocketChipStage, TestHarness}

import java.io.{File, FileWriter}
import Chisel.throwException
import freechips.rocketchip.aoputil._

import freechips.rocketchip.diplomacy.{LazyModuleImp, LazyModule, BaseNode}
import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3.internal.firrtl._
import freechips.rocketchip.system.{RocketChipStage, TestHarness}
import chisel3.aop._

trait HasRocketChipStageUtils {

  def getConfig(fullConfigClassNames: Seq[String]): Config = {
    new Config(fullConfigClassNames.foldRight(Parameters.empty) { case (currentName, config) =>
      val currentConfig = try {
        Class.forName(currentName).newInstance.asInstanceOf[Config]
      } catch {
        case e: java.lang.ClassNotFoundException =>
          throwException(s"""Unable to find part "$currentName" from "$fullConfigClassNames", did you misspell it?""", e)
      }
      currentConfig ++ config
    })
  }

  def enumerateROMs(circuit: Circuit): String = {
    val res = new StringBuilder
    val configs =
      circuit.components flatMap { m =>
        m.id match {
          case rom: BlackBoxedROM => Some((rom.name, ROMGenerator.lookup(rom)))
          case _ => None
        }
      }
    configs foreach { case (name, c) =>
      res append s"name ${name} depth ${c.depth} width ${c.width}\n"
    }
    res.toString
  }

  def writeOutputFile(targetDir: String, fname: String, contents: String): File = {
    val f = new File(targetDir, fname)
    val fw = new FileWriter(f)
    fw.write(contents)
    fw.close
    f
  }

  def injTest(pf:PartialFunction[Int, Double]) = {
    println(s"SULTAN Test int 5 to ${pf(5)}")
  }

  def injTest1(f: () => PartialFunction[Int, Double]) = {
    println(s"SULTAN Test1 int 7 to ${f()(7)}")
  }

  def injTest2(f: () => Unit) = {
    println("SULTAN from injTest2")
    f()
  }

  var optCircuit: Option[Circuit] = None

  def injectModule(targetDir: String, c: Circuit) {
    println(s"SULTAN circuit IN  ${c.getClass.getName}")
    optCircuit = Option(c)
    println(s"SULTAN info injectModule")

    optCircuit match {
      case Some(x) => println(s"SULTAN x: ${x.getClass.getName}")
    }
  }

  def injMonitor(pf: PartialFunction[BaseModule, Boolean]) = {
    println(s"SULTAN pf ${pf.getClass.getName}")


  }

/*
    val dip = SelectDiplomacy(circuit).render(targetDir, "DiplomacyGraph", DOTGRAPH)
    val impModules: List[LazyModuleImp] = SelectDiplomacy.collectImpModules()
    val str = SelectDiplomacy.collectImpModules(targetDir, "collectImp.txt")
    val str1 = SelectDiplomacy.collectBaseNodes(targetDir, "collectBaseNodes.txt")
    val str2 = SelectDiplomacy.collectLazyModules(targetDir, "collectLazyModules.txt")

    println(s"SULTAN info injectModule")
//    println(s"SULTAN info AopInjModule.abcd ${AopInjModules.abcd}")

//    circuit.components.foreach{a => println(s"SULTAN COMPONENT ${a.name} >>> ${a.getClass.getName}")}
//    circuit.annotations.foreach{a => println(s"SULTAN ANNO ${a} >>> ${a.getClass.getName}")}

    impModules.foreach { m => println(s"SULTAN IMP MODULE ${m.name}")}
    val baseNodes: List[BaseNode] = SelectDiplomacy.collectBaseNodes()
    baseNodes.foreach{ n =>
      println(s"DEBUG_AOP: SULTAN BASE_NODE name: ${n.name} valName; ${n.valName} formatNode: +++${n.formatNode}---")
      println("INPUTS")
      n.inputs.foreach{println}
      println("OUTPUTS")
      n.outputs.foreach{println}
    }

//    val pat = "(DCache$).*".r
//    val tgts = getModByName(pat, circuit)
//    tgts.foreach{a => println(s"SULTAN FIND_MODULE ${a}")}
//    Select.ios(tgts.head).foreach{i => println(s"SULTAN IOS for first match ${i}")}
 */


//    InjectModules.inj(circuit)

//    println(s"SULTAN info Injecting Aspect for statements")
//    val modInjAspect = InjectingAspect(
//      {dut: TestHarness => Seq(dut.dut)},
//      {dut: freechips.rocketchip.system.ExampleRocketSystemModuleImp[freechips.rocketchip.system.ExampleRocketSystem] =>
//        val dummyWire = Wire(UInt(3.W)).suggestName("hello")
//        dummyWire := 5.U
//        dontTouch(dummyWire)
//      })

//    val dirName = System.getProperty("user.dir") + "/emulator"
//    println(s"SULTAN dirName: ${dirName}")

//        val dir = new File(dirName)
//        if (!dir.exists()) dir.mkdirs()

//        new RocketChipStage().run(Seq(
//          new TargetDirAnnotation(dirName),
//          new TopModuleAnnotation(Class.forName("freechips.rocketchip.system.TestHarness")),
//          new ConfigsAnnotation(Seq("freechips.rocketchip.system.DefaultConfig")),
//          modInjAspect
//        ))
   



//    val pat1 = "(RocketTile)|(Rocket)|(ExampleRocketSystem)".r
//    val pat1 = "Rocket$".r
//    val tgts1 = getModByName(pat1, circuit)
//    tgts1.foreach{a => println(s"SULTAN ROCKET ${a}")}

//    val targetDef = impModules(0)
//    val mon = new DummyMonitor()
//    val monAspect = InjectingAspect( () => circuit, () => mon)


/**********
    InjectingAspect(

      (th: TestHarness) => Select.collectDeep(th) { case i: RawModule if Select.ios(i).flatMap(getIrrevocables).nonEmpty => i },
//      (th:TestHarness) => getModByName(pat, circuit).collect{case i: RawModule if Select.ios(i).flatMap(getIrrevocables).nonEmpty => i },
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
 * ************/
//    InjectMonitor.pr()
//  }
}

object ElaborationArtefacts {
  var files: Seq[(String, () => String)] = Nil

  def add(extension: String, contents: => String) {
    files = (extension, () => contents) +: files
  }

  def contains(extension: String): Boolean = {
    files.foldLeft(false)((t, s) => {s._1 == extension | t})
  }
}

class DummyMonitor() extends Module {
  val io = new Bundle {}
}

/*
case class DummyCode () extends RawModule {
  val results: Seq[Int] = Seq[Int](0, 1, 2, 3, 4, 5, 6)
  val values = VecInit(results.map(_.U))
  val counter = RegInit(0.U(results.length.W))
  counter := counter + 1.U
  when(counter >= values.length.U) {
    stop()
  }.otherwise {
    when(reset.asBool() === false.B) {
      printf("values(%d) = %d\n", counter, values(counter))
      assert(counter === values(counter))
    }
  }
}
 */

/***********
      (th: TestHarness) => Seq(th),
      (dut: DummyCode) => {
        for(i <- 0 until dut.values.length) {
          dut.values(i) := i.U
        }
      })



class AspectTester(results: Seq[Int]) extends BasicTester {
  val values = VecInit(results.map(_.U))
  val counter = RegInit(0.U(results.length.W))
  counter := counter + 1.U
  when(counter >= values.length.U) {
    stop()
  }.otherwise {
    when(reset.asBool() === false.B) {
      printf("values(%d) = %d\n", counter, values(counter))
      assert(counter === values(counter))
    }
  }
}
 * *********/
