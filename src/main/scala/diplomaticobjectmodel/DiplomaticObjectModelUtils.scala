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

  def writeJsonFile(msg: String, filename: String, json: Map[String, Any]) : Unit = {
    implicit val formats = Serialization.formats(NoTypeHints)
    val writer = new FileWriter(new File(filename))
    writer.write(pretty(Extraction.decompose(json)))
    writer.close()
  }

}
