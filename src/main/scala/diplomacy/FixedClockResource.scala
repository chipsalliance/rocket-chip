// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

class FixedClockResource(val name: String, val freqMHz: Double, val prefix: String = "soc/")
{
  val device = new Device {
    def describe(resources: ResourceBindings) =
      Description(prefix + name, Map(
        "#clock-cells"       -> Seq(ResourceInt(0)),
        "clock-frequency"    -> Seq(ResourceInt(freqMHz * 1000000)),
        "clock-output-names" -> Seq(ResourceString(name)),
        "compatible"         -> Seq(ResourceString("fixed-clock"))))
  }

  // Just bind something to ensure it shows up
  ResourceBinding { Resource(device, "nil").bind(ResourceInt(42)) }

  def bind(dev: Device) {
    ResourceBinding { Resource(dev, "clocks").bind(ResourceReference(device.label)) }
  }
}
