package freechips.rocketchip

package object resources {
  type PropertyOption = Option[(String, Seq[ResourceValue])]
  type PropertyMap = Iterable[(String, Seq[ResourceValue])]

  implicit class BigIntHexContext(private val sc: StringContext) extends AnyVal {
    def x(args: Any*): BigInt = {
      val orig = sc.s(args: _*)
      BigInt(orig.replace("_", ""), 16)
    }
  }

  implicit class IntToProperty(x: Int) {
    def asProperty: Seq[ResourceValue] = Seq(ResourceInt(BigInt(x)))
  }

  implicit class BigIntToProperty(x: BigInt) {
    def asProperty: Seq[ResourceValue] = Seq(ResourceInt(x))
  }

  implicit class StringToProperty(x: String) {
    def asProperty: Seq[ResourceValue] = Seq(ResourceString(x))
  }

  implicit class DeviceToProperty(x: Device) {
    def asProperty: Seq[ResourceValue] = Seq(ResourceReference(x.label))
  }
}
