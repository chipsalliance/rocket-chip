// See LICENSE.SiFive for license details.

package freechips.rocketchip.aoputil

import java.io.{File, FileWriter}
import scala.util.matching.Regex

import Chisel._
import Chisel.throwException
import freechips.rocketchip.config.Parameters

import freechips.rocketchip.diplomacy.{LazyModuleImp, LazyModule, BaseNode}
import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3.experimental.{BaseModule, FixedPoint}
import chisel3.internal.HasId
import chisel3.internal.firrtl._
import chisel3.{RawModule, VecInit}
import chisel3.util._
import chisel3.aop._
import chisel3.aop.injecting._
import freechips.rocketchip.rocket._
import freechips.rocketchip.unittest._

//import freechips.rocketchip.unittest._

//import chisel3._
//import chisel3.experimental.{BaseModule, FixedPoint}
//   *** import chisel3.internal.HasId
//import chisel3.internal.firrtl._
//import firrtl.annotations.ReferenceTarget

trait AopCallBase
case object ShowModules extends AopCallBase
case class ShowModulePorts(moduleInstName: String) extends AopCallBase

case class AopInjectMonitor(modName: Regex, monitor: String, io: String) extends AopCallBase

object InjectModules {

  var injItems: Seq[AopCallBase] = Seq[AopCallBase]()

  def register(items: Seq[AopCallBase]) = {
    items.collect{case i: AopCallBase => println(s"SULTAN info register ${i.getClass.getName}")}
    injItems = items
  }

  def pr() = {
    "DEBUG_AOP: from InjectMonitor.pr()"
  }

  def getIrrevocables(io: Data): Seq[IrrevocableIO[_]] = {
    io match {
      case a: IrrevocableIO[_] => Seq(a)
      case x: Aggregate => x.getElements.flatMap(getIrrevocables)
      case other => Nil
    }
  }

  def getModByName(pat: Regex, circuit: Circuit) = {
    val mod = circuit.components.collect{c => 
      c.name match {
        case pat(a) => c
      }
    }
    mod.map(_.id)
  }

  def inj(circuit: Circuit) = {
    println(s"SULTAN info in inj method")

    injItems.foreach{
      case ShowModules =>
        circuit.components.foreach{a => println(s"COMPONENT ${a.name}\t\t${a.getClass.getName}")}
      case ShowModulePorts(a) =>
        val m = circuit.components.foreach{b =>
          if(b.name == a)
            Select.ios(b.id).foreach{i => println(s"COMP IOS\t\t${i}")}
        }
      case AopInjectMonitor(ins, m, p) =>
        println(s"INJECT MONITOR ins ${ins} monitor ${m} port ${p}")
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
    }
  }

}
