// See LICENSE.SiFive for license details.

package generatorTests.phases

import freechips.rocketchip.config.Config
import freechips.rocketchip.diplomacy.BigIntHexContext
import freechips.rocketchip.stage.ConfigsAnnotation
import freechips.rocketchip.stage.phases.AddDefaultTests
import freechips.rocketchip.subsystem.ExtMem
import freechips.rocketchip.system.DefaultConfig
import freechips.rocketchip.tile.XLen

import org.scalatest.{FlatSpec, Matchers}

object AddDefaultTestsSpec {

  /** A configuration that is not SV39 aligned */
  class NotSV39AlignedConfig extends Config(
    new Config(
      (site, here, up) => {
        case ExtMem => up(ExtMem, site).map(b => b.copy(master=b.master.copy(base = x"7000_0000")))
      }
    ) ++ new DefaultConfig)

  /** A configuration that is RV128 */
  class RV128Config extends Config(
    new Config(
      (site, here, up) => {
        case XLen => 128
      }
    ) ++ new DefaultConfig)

}

class AddDefaultTestsSpec extends FlatSpec with Matchers {

  import AddDefaultTestsSpec._

  behavior of "freechips.rocketchip.stage.phases.AddDefaultTests"

  it should "throw an IllegalArgumentException on an RV128 config" in {
    val annotations = Seq(new ConfigsAnnotation(Seq((new RV128Config).getClass.getName)))
    assertThrows[IllegalArgumentException] {
      (new AddDefaultTests).GenerateSystemTestSuites(annotations)
    }
  }

  it should "include '*-dirty' tests if the configuration is SV39 superpage aligned" in {
    val annotations = Seq(new ConfigsAnnotation(Seq((new DefaultConfig).getClass.getName)))
    val out = (new AddDefaultTests).GenerateSystemTestSuites(annotations)
    out.toString should include ("dirty")
  }

  it should "not include '*-dirty' tests if the configuration isn't SV39 superpage aligned" in {
    val annotations = Seq(new ConfigsAnnotation(Seq((new NotSV39AlignedConfig).getClass.getName)))
    val out = (new AddDefaultTests).GenerateSystemTestSuites(annotations)
    out.toString should not include ("dirty")
  }

}
