// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import chisel3.experimental.RawModule

class LogicalTreeRegistry {
  val fList: List[() => (RawModule, RawModule)] = List[() => (RawModule, RawModule)]()

  def register(f: () => (RawModule, RawModule)): Unit = {
    fList.+:(f)
  }

  def makePairs(): List[(RawModule, RawModule)] = {
    fList.map{
      case f => f()
    }
  }

  def makeTree():

}
