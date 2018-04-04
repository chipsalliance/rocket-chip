// See LICENSE.SiFive for license details.

package freechips.rocketchip.regmapper

import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods.{pretty, render}

object RegMappingAnnotation {
  def serialize(base: BigInt, name: String, mapping: RegField.Map*): String = {
    val regDescs = mapping.flatMap { case (byte, seq) =>
      seq.map(_.width).scanLeft(0)(_ + _).zip(seq).map { case (bit, f) =>
        val anonName = s"unnamedRegField${byte.toHexString}_${bit}"
        (f.desc.map{ _.name}.getOrElse(anonName)) -> f.toJson(byte, bit)
      }
    }

    pretty(render(
      ("peripheral" -> (
        ("displayName" -> name) ~
        ("baseAddress" -> s"0x${base.toInt.toHexString}") ~
        ("regfields" -> regDescs)))))
  }
}
