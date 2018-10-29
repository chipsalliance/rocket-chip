// See LICENSE for license details.

package freechips.rocketchip.diplomaticobjectmodel

import java.io.{File, FileWriter}

import org.json4s.jackson.JsonMethods.pretty
import org.json4s.jackson.Serialization
import org.json4s.{Extraction, NoTypeHints}

object DiplomaticObjectModelUtils {

  implicit class RichBoolean(val b: Boolean) extends AnyVal {
    final def option[A](a: => A): Option[A] = if (b) Some(a) else None
  }

  def toJson(json: Any): String = {
    implicit val formats = Serialization.formats(NoTypeHints)
    pretty(Extraction.decompose(json))
  }

  def writeJsonFile(msg: String, filename: String, json: Map[String, Any]) : Unit = {
    val writer = new FileWriter(new File(filename))
    writer.write(toJson(json))
    writer.close()
  }

}
