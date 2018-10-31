// See LICENSE for license details.

package freechips.rocketchip.diplomaticobjectmodel

import java.io.{File, FileWriter}
import java.lang.management.OperatingSystemMXBean

//import freechips.rocketchip.diplomaticobjectmodel.model.OMBaseInstructionSetSerializer
import freechips.rocketchip.diplomaticobjectmodel.model.OMBaseType
import org.json4s.jackson.JsonMethods.pretty
import org.json4s.jackson.Serialization
import org.json4s._


case object OMBaseTypeSerializer extends Serializer[OMBaseType] {
  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), OMBaseType] = ???

  def serialize(implicit formats: Formats): PartialFunction[Any, JValue] = {
    case x: OMBaseType =>
      println("OMBaseType")
      import JsonDSL._
      import org.json4s.jackson.JsonMethods._
      Extraction.decompose(x)(DefaultFormats) merge render(( "_types" -> x.getTypes )) 
  }
}

object DiplomaticObjectModelUtils {

  def toJson(json: Any): String = {
    implicit val formats = Serialization.formats(NoTypeHints) + OMBaseTypeSerializer
    pretty(Extraction.decompose(json))
  }

  def writeJsonFile(msg: String, filename: String, json: Map[String, Any]) : Unit = {
    val writer = new FileWriter(new File(filename))
    writer.write(toJson(json))
    writer.close()
  }

}
