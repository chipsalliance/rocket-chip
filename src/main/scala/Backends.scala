// See LICENSE for license details.

package rocketchip

import Chisel._
import RocketChipBackend._
import scala.collection.mutable.HashMap

object RocketChipBackend {
  val initMap = new HashMap[Module, Bool]()
}

class RocketChipBackend extends VerilogBackend
{
  initMap.clear()
  override def emitPortDef(m: MemAccess, idx: Int) = {
    val res = new StringBuilder()
    for (node <- m.mem.inputs) {
      if(node.name.contains("init"))
         res.append("    .init(" + node.name + "),\n")
    }
    (if (idx == 0) res.toString else "") + super.emitPortDef(m, idx)
  }

  def addMemPin(c: Module) = {
    for (m <- Driver.components) {
      m bfs { _ match {
        case mem: Mem[_] if mem.seqRead =>
          connectMemPin(m, mem)
        case _ =>
      } }
    }
  }

  def connectInitPin(c: Module) {
    initMap(c) = c.addPin(Bool(INPUT), "init")
    if (!(initMap contains c.parent)) connectInitPin(c.parent)
    initMap(c) := initMap(c.parent)
  }

  def connectMemPin(c: Module, mem: Mem[_]) {
    if (!(initMap contains c)) connectInitPin(c)
    mem.inputs += initMap(c)
  }

  def addTopLevelPin(c: Module) = {
    initMap(c) = c.addPin(Bool(INPUT), "init")
  }

  transforms += addTopLevelPin
  transforms += addMemPin
}

class Fame1RocketChipBackend extends RocketChipBackend with Fame1Transform

