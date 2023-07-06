// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

object ElaborationArtefacts {
  var files: Seq[(String, () => String)] = Nil

  def add(extension: String, contents: => String): Unit = {
    files = (extension, () => contents) +: files
  }

  def contains(extension: String): Boolean = {
    files.foldLeft(false)((t, s) => {s._1 == extension | t})
  }
}
