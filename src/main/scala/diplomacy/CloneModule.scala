// See LICENSE.SiFive for license details.

// !!! HACK TO WORK-AROUND MISSING CHISEL FEATURE
// !!! We need to be inside the chisel3 package to access Builder

package chisel3.shim

import Chisel._
import chisel3.experimental.{RawModule, MultiIOModule, BaseModule}
import chisel3.internal.Builder
import chisel3.core.UserModule
import chisel3.internal.firrtl.{Command, DefInstance}
import scala.collection.immutable.ListMap
import scala.collection.mutable.ArrayBuffer

class ClonePorts protected[shim](elts: Data*) extends Record
{
  val elements = ListMap(elts.map(d => d.instanceName -> d.chiselCloneType): _*)
  def apply(field: String) = elements(field)
  override def cloneType = (new ClonePorts(elts: _*)).asInstanceOf[this.type]
}

class CloneModule private (model: RawModule) extends BlackBox
{
  import CloneModule._
  override def desiredName = model.name
  val io = IO(new ClonePorts(model.getPorts.map(_.id): _*))
}

object CloneModule
{
  def apply(model: BaseModule): ClonePorts = {
    // Create the 'BlackBox' stand-in
    val mod = Module(new CloneModule(model.asInstanceOf[RawModule]))
    // Rewrite the instance definition to be the original module
    // (this is needed because the original module gets clobbered by DCE + constant prop)
    val method = classOf[UserModule].getDeclaredMethod("_commands")
    method.setAccessible(true)
    val commands = method.invoke(Builder.forcedUserModule).asInstanceOf[ArrayBuffer[Command]]
    val victimIdx = commands.lastIndexWhere {
      case DefInstance(_, kill, _) => mod eq kill
      case _ => false
    }
    val victim = commands(victimIdx).asInstanceOf[DefInstance]
    val standin = new DefInstance(victim.sourceInfo, model, victim.ports) {
      override def name = victim.name
    }
    commands.update(victimIdx, standin)
    // Wire it up
    model match {
      case _: MultiIOModule =>
        mod.io("clock") := Module.clock
        mod.io("reset") := Module.reset
      case _: RawModule => // Do nothing
    }
    mod.io
  }
}
