// See LICENSE.SiFive for license details.

package rocketchip

import config._
import junctions._
import diplomacy._
import uncore.devices._
import tile.XLen
import coreplex._
import uncore.tilelink2._
import util._

import java.nio.file.{Files, Paths}
import java.nio.{ByteBuffer, ByteOrder}

class RangeManager {
  private var finalized = false
  private val l = collection.mutable.ListBuffer[(String, Int)]()
  def add(name: String, element: Int) = { require(!finalized); l += (name -> element) }
  def rangeMap = {
    finalized = true
    (l map {
      var sum = 0
      x => { sum += x._2; (x._1 -> (sum-x._2, sum)) }
    }).toMap
  }
  def range(name: String) = rangeMap(name)
  def print = {
    rangeMap.toSeq.sortBy(_._2).foreach { case (name, (start, end)) =>
      println(s"${name} on int ${start}-${end-1}")
    }
  }
  def sum = {
    finalized = true
    l.map(_._2).sum
  }
}

class ResourceManager[T] {
  private var finalized = false
  private val l = collection.mutable.ArrayBuffer[T]()
  def add(element: T) = { require(!finalized); l += element }
  def add(list: Seq[T]) = { require(!finalized); l ++= list }
  def get: Seq[T] = { finalized = true; l }
}

class GlobalVariable[T] {
  private var assigned = false
  private var variable: T = _
  def assign(value: T) = { require(!assigned); assigned = true; variable = value }
  def get: T = { require(assigned); variable }
}

object GenerateBootROM {
  def apply(p: Parameters, address: BigInt, dts: String) = {
    val romdata = Files.readAllBytes(Paths.get(p(BootROMFile)))
    val rom = ByteBuffer.wrap(romdata)

    rom.order(ByteOrder.LITTLE_ENDIAN)

    require(address == address.toInt)
    val dtsAddr = address.toInt + rom.capacity
    require(rom.getInt(12) == 0,
      "DTS address position should not be occupied by code")
    rom.putInt(12, dtsAddr)
    rom.array() ++ (dts.getBytes.toSeq)
  }
}
