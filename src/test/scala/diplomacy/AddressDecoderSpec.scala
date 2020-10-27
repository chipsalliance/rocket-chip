package diplomacyTest

import freechips.rocketchip.diplomacy._
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AddressDecoderSpec extends AnyFlatSpec with Matchers with PrivateMethodTester {
  implicit def toBigInt(str: String) = BigInt(Integer.parseInt(str, 2))
  val decorateToStringValue = PrivateMethod[String]('decorateToStringValue)

  behavior of "AddressDecoder"
  val port0 = Seq(
    AddressSet("00", "100"),
    AddressSet("01", "100")
  )
  val port1 = Seq(
    AddressSet("10", "100"),
    AddressSet("11", "100")
  )

  "AddressDecoder" should "partition port0 and port1" in {
    AddressDecoder(Seq(port0, port1)) should equal(toBigInt("10"))
  }
}