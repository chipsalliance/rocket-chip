// See LICENSE.SiFive for license details.

package freechips.rocketchip.unittest

import chisel3._
import chisel3.util._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import scala.util.control.Breaks._
import org.scalatest.{BeforeAndAfterAllConfigMap, ConfigMap}

import freechips.rocketchip.config._
import freechips.rocketchip.util.HasGeneratorUtilities
import org.w3c.dom.DOMConfiguration

import freechips.rocketchip.util.PlusArg

trait StringToConfig {
  def apply(value: String) : Config
}

object TargetDir extends chipsalliance.rocketchip.config.Field[String]
class TargetDir extends StringToConfig {
  def apply(value: String) : Config = new Config((site, here, up) => { case TargetDir => value })
}

object SimTimeout extends chipsalliance.rocketchip.config.Field[Int]
class SimTimeout extends StringToConfig {
  def apply(value: String) : Config = new Config((site, here, up) => { case SimTimeout => value.toInt })
}

class TestHarnessTester(val maxCycles: Int)(c: TestHarness) extends PeekPokeTester(c) {
  breakable{
    for(i <- 0 to maxCycles) {
      if (peek(c.io.success) == 1) {
        println("==========----------     SUCCESS!     ----------==========")
        break
      }
      else {
        step(1)
      }
    }
  }
  expect(c.io.success, 1, s"($maxCycles) cycles time out!!!")
}

object SimWithTreadle extends chipsalliance.rocketchip.config.Field[Boolean](false)
class SimWithTreadle extends StringToConfig {
  def apply(value: String) : Config = new Config((site, here, up) => { case SimWithTreadle => Seq("1", "true", "yes") contains value.toLowerCase })
}

class UnittestSpec extends ChiselFlatSpec with HasGeneratorUtilities with BeforeAndAfterAllConfigMap {
  var configNames = Array("freechips.rocketchip.unittest.ScatterGatherTestConfig")
  var argsConfig = Parameters.empty

  override def beforeAll(configMap: ConfigMap) = {
    PlusArg.configMap = configMap
    if (configMap.get("config").isDefined) {
      configNames = configMap.getRequired[String]("config").split(",")
    }
    configMap.keys.map{ k =>
      try {
        Class.forName(k) match {
          case s2cClass: Class[StringToConfig] => {
            val s2c = s2cClass.newInstance
            argsConfig = argsConfig ++ s2c(configMap.getRequired[String](k))
          }
        }
      } catch {
        case e: java.lang.ClassNotFoundException => println(e)
      }
    }
  }

  def runUnitTestWithConfig(configName: String) = {
    println (s"\nBegin TestHarness with $configName \n")
    val config: Config = new Config(argsConfig ++ getConfig(Seq(configName)))
    val params: Parameters = config.toInstance
    val td = params(TargetDir)
    val timeout = params(SimTimeout)
    PlusArg.simWithTreadle = params(SimWithTreadle)
    iotesters.Driver.execute(Array("-td", td), () => new TestHarness()(params)) {
      c => new TestHarnessTester(timeout)(c)
    } should be (true)
  }

  "Unit Test Harness" should "finish success" in {
    configNames map (runUnitTestWithConfig _)
  }
}
